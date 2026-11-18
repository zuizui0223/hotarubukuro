#!/usr/bin/env python3
"""Render exploratory Figure 3 from completed Bombus spatial-match outputs.

Run after python run_pipeline.py reproduce --no-resume:
  python scripts/plot_bombus_assemblage_figure3.py

This script performs no model fitting, resampling, or hypothesis selection.
"""
import argparse
import io
from pathlib import Path
import zipfile
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

SUMMARY="bombus_spatial_guardrail/community_spatial_match_summary.csv"
DETAIL="bombus_spatial_guardrail/community_spatial_match_transition_detail.csv"
CELLS="analysis_cells/analysis_cells_1km.csv"
RADII=[5,10,25]

def load(root, artifact):
    paths=[SUMMARY,DETAIL,CELLS]
    if artifact:
        with zipfile.ZipFile(artifact) as f:
            return [pd.read_csv(io.BytesIO(f.read(p))) for p in paths]
    return [pd.read_csv(root/p) for p in paths]

def validate(summary, details, cells):
    def choose(df):
        return df.loc[(df.metric=="occurrence_reference_hellinger") &
                      (df.background_window_km==50) &
                      (df.requested_n_controls==20)].copy()
    s=choose(summary).sort_values("radius_km")
    d=choose(details)
    if s.radius_km.tolist()!=RADII:
        raise ValueError("Expected the frozen 5/10/25 km match specifications")
    grouped=d.groupby("radius_km").matched_excess
    if not np.array_equal(grouped.size().reindex(RADII).to_numpy(),s.n_transition_edges.to_numpy()):
        raise ValueError("Transition counts disagree")
    if not np.allclose(grouped.mean().reindex(RADII).to_numpy(),s.mean_matched_excess.to_numpy(),atol=1e-10):
        raise ValueError("Matched excess disagree")
    coords=cells.drop_duplicates("exact_site_id").set_index("exact_site_id")[["longitude","latitude"]]
    mids=[]
    for edge in d.loc[d.radius_km==10,"transition_edge_id"]:
        parts=str(edge).split("__")
        if len(parts)!=3 or parts[0]!="r10":
            raise ValueError("Invalid transition edge id: "+str(edge))
        p=coords.loc[parts[1:]].mean()
        mids.append((float(p.longitude),float(p.latitude)))
    return s,d,pd.DataFrame(mids,columns=["longitude","latitude"])

def draw(s,d,m,cells,path):
    fig=plt.figure(figsize=(8.2,8.1))
    gs=fig.add_gridspec(2,1,height_ratios=[1.04,1],left=.15,right=.96,top=.87,bottom=.19,hspace=.43)
    plum="#8E4B7D";navy="#1D455C"
    fig.suptitle("Predicted bumblebee assemblage turnover at floral-colour transitions",
                 x=.15,y=.97,ha="left",fontsize=13.6,weight="bold")
    a=fig.add_subplot(gs[0])
    try:
        from mpl_toolkits.basemap import Basemap
        geo=Basemap(projection="cyl",llcrnrlon=128.2,llcrnrlat=31,
                    urcrnrlon=144.1,urcrnrlat=42.1,resolution="l",ax=a)
        geo.fillcontinents(color="#F1F3F3",lake_color="white",zorder=0)
        geo.drawcoastlines(linewidth=.6,color="#9CA9AE",zorder=1)
    except ImportError:
        a.set_facecolor("#F7F8F8")
    a.scatter(cells.longitude,cells.latitude,s=5,color="#AAB8BC",alpha=.4,lw=0,
              label="Flower-record cells (n=%d)"%len(cells),zorder=2)
    a.scatter(m.longitude,m.latitude,s=20,color=plum,alpha=.85,edgecolor="white",
              linewidth=.3,label="10-km transitions (n=%d)"%len(m),zorder=3)
    a.set_xlim(128.2,144.1);a.set_ylim(31,42.1);a.set_aspect("equal",adjustable="box")
    a.set_xticks([130,134,138,142]);a.set_yticks([32,36,40])
    a.set_xlabel("Longitude (°E)");a.set_ylabel("Latitude (°N)")
    a.text(0,1.03,"a  Distribution of photographed colour transitions",
           transform=a.transAxes,weight="bold")
    a.legend(loc="lower right",fontsize=8,framealpha=.96)
    b=fig.add_subplot(gs[1]);rng=np.random.default_rng(20261117)
    for i,r in enumerate(RADII):
        values=d.loc[d.radius_km==r,"matched_excess"].to_numpy()
        b.scatter(i+rng.uniform(-.15,.15,len(values)),values,color=plum,alpha=.18,s=10,lw=0)
        q=np.quantile(values,[.25,.5,.75])
        b.plot([i,i],[q[0],q[2]],color=plum,lw=4)
        b.scatter([i],[q[1]],facecolor="white",edgecolor=plum,s=32,zorder=5)
        row=s.loc[s.radius_km==r].iloc[0]
        b.scatter([i],[row.mean_matched_excess],marker="D",facecolor=navy,
                  edgecolor="white",s=74,zorder=6)
    b.axhline(0,color="#718289",linestyle="--",linewidth=1)
    b.set_xlim(-.5,2.5);b.set_ylim(-.32,.38)
    b.set_xticks(range(3),[f"{r} km\nn={int(v.n_transition_edges)} | q={v.BH_q_across_scales_within_match_spec:.4f}"
                            for r,v in zip(RADII,s.itertuples())])
    b.set_ylabel("Matched Hellinger turnover excess")
    b.set_xlabel("Maximum colour-transition distance")
    b.text(0,1.03,"b  Turnover excess relative to matched controls",
           transform=b.transAxes,weight="bold")
    b.spines[["top","right"]].set_visible(False)
    b.grid(axis="y",color="#D9E0E2",alpha=.7)
    fig.text(.15,.095,"Diamond = mean; open circle = median; thick line = interquartile range.",fontsize=9)
    fig.text(.15,.076,"q = BH-adjusted p across three scales; 5 km is not significant.",fontsize=9)
    fig.text(.15,.057,"SDM predictions, not visitation or selection; scales share observations.",fontsize=9)
    path.parent.mkdir(parents=True,exist_ok=True)
    fig.savefig(path,dpi=300,bbox_inches="tight",facecolor="white")
    plt.close(fig)

def main():
    p=argparse.ArgumentParser()
    p.add_argument("--results-root",type=Path,default=Path("results"))
    p.add_argument("--artifact",type=Path,help="Optional manuscript-replay artifact ZIP")
    p.add_argument("--output",type=Path,default=Path("results/figures/Figure3_assemblage.png"))
    args=p.parse_args()
    s,d,cells=load(args.results_root,args.artifact)
    summary,details,mids=validate(s,d,cells)
    draw(summary,details,mids,cells,args.output)
    print("Figure 3 reproduced:",args.output,"edges",summary.n_transition_edges.tolist(),
          "q",summary.BH_q_across_scales_within_match_spec.round(6).tolist())

if __name__=="__main__":
    main()
