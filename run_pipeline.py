#!/usr/bin/env python3
"""One-command public analysis reproduction for the hotarubukuro project.

This entry point intentionally reproduces analysis only.  Manuscripts, journal
submission files, author metadata, original third-party photographs and private
paper bundles are outside the public repository boundary.

Usage
-----
    python run_pipeline.py audit
    python run_pipeline.py reproduce

The reproduce profile starts from the committed derived ``Data_S1.csv`` and
rebuilds public environmental inputs, Bombus SDMs and the current statistical
analysis chain.  Live public-source retrieval is explicit and recorded; it is
not an exact archival substitute for third-party services that can change.
"""

from __future__ import annotations

import argparse
import csv
import datetime as dt
import hashlib
import json
import os
import shutil
import subprocess
import sys
import time
import urllib.request
from pathlib import Path
from typing import Iterable, Sequence

ROOT = Path(__file__).resolve().parent
RESULTS = ROOT / "results"
CACHE = ROOT / ".repro_cache"
MANIFEST = RESULTS / "analysis_reproduction" / "run_manifest.json"
WORLDPOP_URL = (
    "https://data.worldpop.org/GIS/Population/Global_2000_2020_1km/2020/"
    "JPN/jpn_ppp_2020_1km_Aggregated.tif"
)

EXPECTED_GIT_BLOBS = {
    "Data_S1.csv": "74b951898814f4ed15f314061e3129d8b05823d5",
    "Code_S1.py": "85019b8c05d7de1271b9e3279baa847f658f57ee",
}

THREAD_ENV = {
    "HOTARUBUKURO_R_SEED": "20260725",
    "OMP_NUM_THREADS": "1",
    "OPENBLAS_NUM_THREADS": "1",
    "MKL_NUM_THREADS": "1",
    "VECLIB_MAXIMUM_THREADS": "1",
    "NUMEXPR_NUM_THREADS": "1",
    "GOTO_NUM_THREADS": "1",
    "BLIS_NUM_THREADS": "1",
    "INLA_NUM_THREADS": "1",
    "OMP_DYNAMIC": "FALSE",
}

PUBLIC_CACHE = CACHE / "public_environment"
MLIT_CACHE = CACHE / "mlit_l03_2021"
DID_CACHE = CACHE / "mlit_did_2015"
GBIF_CACHE = CACHE / "bombus_gbif"
WORLDPOP = PUBLIC_CACHE / "population_count_Japan_crop.tif"

RASTER_ALIASES = {
    "chelsa_bio05.tif": "bio5_Japan_crop_30s.tif",
    "chelsa_bio10.tif": "bio10_Japan_crop_30s.tif",
    "chelsa_bio04.tif": "bio4_Japan_crop_30s.tif",
    "chelsa_gdd5.tif": "gdd5_Japan_crop_30s.tif",
    "chelsa_cmimean.tif": "CMI_Japan_crop_30s.tif",
    "chelsa_bio12.tif": "bio12_Japan_crop_30s.tif",
    "chelsa_bio14.tif": "bio14_Japan_crop_30s.tif",
    "chelsa_bio15.tif": "bio15_Japan_crop_30s.tif",
    "chelsa_rsdsmean.tif": "RSDS_Japan_crop_30s.tif",
    "soilgrids_bdod_0_5cm_mean.tif": "bdod_0-5cm_mean_30s.tif",
    "soilgrids_cfvo_0_5cm_mean.tif": "cfvo_0-5cm_mean_30s.tif",
    "soilgrids_sand_0_5cm_mean.tif": "sand_0-5cm_mean_30s.tif",
    "soilgrids_silt_0_5cm_mean.tif": "silt_0-5cm_mean_30s.tif",
    "soilgrids_nitrogen_0_5cm_mean.tif": "nitrogen_0-5cm_mean_30s.tif",
    "soilgrids_ocd_0_5cm_mean.tif": "ocd_0-5cm_mean_30s.tif",
    "soilgrids_soc_0_5cm_mean.tif": "soc_0-5cm_mean_30s.tif",
    "soilgrids_phh2o_0_5cm_mean.tif": "phh2o_0-5cm_mean_30s.tif",
    "elevation_30s.tif": "elevation_Japan_crop.tif",
}

REQUIRED_FILES = [
    "Data_S1.csv",
    "Code_S1.py",
    "config/pipeline.yml",
    "config/raster_sources.csv",
    "config/bombus_sdm.yml",
    "dependencies/r-packages.csv",
    "dependencies/r-version.txt",
    "R/pipeline_support.R",
    "R/environment_spatial.R",
    "R/natural_biotic_covariates.R",
    "R/phenotype_hurdle.R",
    "R/multiscale_hotspots.R",
    "R/natural_predictive_model.R",
    "R/continuous_colour_isolation.R",
    "source_build/download_rasters.R",
    "source_build/prepare_rasters.R",
    "source_build/fetch_bombus_occurrences.R",
    "source_build/build_bombus_sdm_mainland.R",
    "source_build/build_human_raster.R",
    "scripts/build_environment_input.R",
    "scripts/run_environment_spatial.R",
    "scripts/run_natural_biotic_covariates.R",
    "scripts/run_phenotype_hurdle.R",
    "scripts/run_multiscale_hotspots.R",
    "scripts/run_natural_predictive_model.R",
    "scripts/build_bombus_occurrence_reference_support.R",
    "scripts/run_bombus_local_sharp_transition.R",
    "scripts/run_human_landscape_features.R",
    "scripts/run_local_human_context.R",
    "scripts/run_did_sensitivity.R",
    "scripts/run_broad_environment_spatial_audit.R",
    "scripts/build_fixed_space_null_cache.R",
    "scripts/fit_broad_supported_term_distance_space_null.R",
    "scripts/fit_final8_presence_null.R",
    "analysis_sensitivity/run_continuous_colour_isolation_human_context.R",
]


def utc_now() -> str:
    return dt.datetime.now(dt.timezone.utc).isoformat(timespec="seconds")


def git_blob_sha(path: Path) -> str:
    data = path.read_bytes()
    return hashlib.sha1(f"blob {len(data)}\0".encode() + data).hexdigest()


def sha256(path: Path) -> str:
    h = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(4 * 1024 * 1024), b""):
            h.update(block)
    return h.hexdigest()


def display(command: Sequence[str]) -> str:
    return " ".join(subprocess.list2cmdline([part]) for part in command)


class ReproductionError(RuntimeError):
    pass


class Pipeline:
    def __init__(self, *, dry_run: bool, resume: bool, skip_setup: bool) -> None:
        self.dry_run = dry_run
        self.resume = resume
        self.skip_setup = skip_setup
        self.env = os.environ.copy()
        self.env.update(THREAD_ENV)
        self.env.update(
            {
                "HOTARUBUKURO_ROOT": str(ROOT),
                "HOTARUBUKURO_PUBLIC_CACHE": str(PUBLIC_CACHE),
                "HOTARUBUKURO_MLIT_CACHE": str(MLIT_CACHE),
                "HOTARUBUKURO_DID_CACHE": str(DID_CACHE),
                "HOTARUBUKURO_WORLDPOP_RASTER": str(WORLDPOP),
                "HOTARUBUKURO_INPUT_ROOT": str(ROOT),
            }
        )
        self.records: list[dict[str, object]] = []

    def audit(self) -> None:
        missing = [item for item in REQUIRED_FILES if not (ROOT / item).is_file()]
        if missing:
            raise ReproductionError("Missing required files: " + ", ".join(missing))
        for rel, expected in EXPECTED_GIT_BLOBS.items():
            observed = git_blob_sha(ROOT / rel)
            if observed != expected:
                raise ReproductionError(
                    f"{rel} changed: expected git blob {expected}, observed {observed}"
                )
        with (ROOT / "Data_S1.csv").open("r", encoding="utf-8-sig", newline="") as handle:
            reader = csv.reader(handle)
            header = next(reader)
            rows = sum(1 for _ in reader)
        required_columns = {"observation_id", "latitude", "longitude"}
        missing_columns = sorted(required_columns - set(header))
        if missing_columns:
            raise ReproductionError("Data_S1.csv missing columns: " + ", ".join(missing_columns))
        if rows != 1965:
            raise ReproductionError(f"Data_S1.csv row count changed: expected 1965, observed {rows}")
        print("PASS public reproducibility contract")
        print(f"Data_S1 rows={rows} git_blob={EXPECTED_GIT_BLOBS['Data_S1.csv']}")
        print(f"Code_S1 git_blob={EXPECTED_GIT_BLOBS['Code_S1.py']}")

    def run(self, name: str, command: Sequence[str], outputs: Iterable[str] = ()) -> None:
        output_paths = [ROOT / item for item in outputs]
        if self.resume and output_paths and all(path.exists() for path in output_paths):
            print(f"\n==> {name}: already complete; skipping")
            self.records.append({"stage": name, "status": "SKIPPED_COMPLETE", "finished_at": utc_now()})
            return
        print(f"\n==> {name}\n    $ {display(command)}")
        started = utc_now()
        if self.dry_run:
            self.records.append({"stage": name, "status": "DRY_RUN", "started_at": started, "finished_at": utc_now()})
            return
        subprocess.run(list(command), cwd=ROOT, env=self.env, check=True)
        missing = [str(path.relative_to(ROOT)) for path in output_paths if not path.exists()]
        if missing:
            raise ReproductionError(f"Stage {name} did not create: {', '.join(missing)}")
        self.records.append({"stage": name, "status": "SUCCESS", "started_at": started, "finished_at": utc_now()})

    def prepare_aliases(self) -> None:
        print("\n==> prepare_analysis_raster_aliases")
        if self.dry_run:
            self.records.append({"stage": "prepare_analysis_raster_aliases", "status": "DRY_RUN"})
            return
        source_root = ROOT / "data" / "processed" / "rasters"
        PUBLIC_CACHE.mkdir(parents=True, exist_ok=True)
        missing = []
        for source_name, alias in RASTER_ALIASES.items():
            source = source_root / source_name
            target = PUBLIC_CACHE / alias
            if not source.is_file():
                missing.append(source_name)
                continue
            if target.exists() or target.is_symlink():
                target.unlink()
            try:
                target.symlink_to(source.resolve())
            except OSError:
                shutil.copy2(source, target)
        if missing:
            raise ReproductionError("Prepared raster outputs missing: " + ", ".join(missing))
        self.records.append({"stage": "prepare_analysis_raster_aliases", "status": "SUCCESS", "finished_at": utc_now()})

    def ensure_worldpop(self) -> None:
        print("\n==> worldpop_2020")
        if self.dry_run:
            self.records.append({"stage": "worldpop_2020", "status": "DRY_RUN"})
            return
        PUBLIC_CACHE.mkdir(parents=True, exist_ok=True)
        if not WORLDPOP.is_file() or WORLDPOP.stat().st_size == 0:
            partial = WORLDPOP.with_suffix(".part")
            for attempt in range(1, 6):
                try:
                    with urllib.request.urlopen(WORLDPOP_URL, timeout=1800) as response, partial.open("wb") as out:
                        shutil.copyfileobj(response, out)
                    partial.replace(WORLDPOP)
                    break
                except Exception:
                    partial.unlink(missing_ok=True)
                    if attempt == 5:
                        raise
                    time.sleep(min(2 ** attempt, 30))
        prov = CACHE / "worldpop_provenance.json"
        prov.parent.mkdir(parents=True, exist_ok=True)
        prov.write_text(
            json.dumps(
                {"source_url": WORLDPOP_URL, "sha256": sha256(WORLDPOP), "bytes": WORLDPOP.stat().st_size},
                indent=2,
            ) + "\n",
            encoding="utf-8",
        )
        self.records.append({"stage": "worldpop_2020", "status": "SUCCESS", "finished_at": utc_now(), "sha256": sha256(WORLDPOP)})

    def reproduce(self) -> None:
        self.audit()
        CACHE.mkdir(parents=True, exist_ok=True)
        RESULTS.mkdir(parents=True, exist_ok=True)
        if not self.skip_setup:
            self.run(
                "restore_r_environment",
                ["Rscript", "scripts/setup_r_environment.R", "--report-dir", "results/analysis_reproduction/environment", "--scopes", "analysis,acquisition,reproducibility,testing"],
                ["results/analysis_reproduction/environment/package_versions.csv"],
            )
        self.run("download_public_rasters", ["Rscript", "source_build/download_rasters.R"], ["data/processed/raster_download_manifest.csv"])
        self.run("prepare_public_rasters", ["Rscript", "source_build/prepare_rasters.R", "--no-download"], ["data/processed/raster_manifest.csv"])
        self.prepare_aliases()
        self.ensure_worldpop()
        self.run(
            "fetch_bombus_occurrences",
            ["Rscript", "source_build/fetch_bombus_occurrences.R", "--output-dir", str(GBIF_CACHE), "--refresh", "false"],
            [str((GBIF_CACHE / "ardens_gbif.csv").relative_to(ROOT))],
        )
        self.run(
            "build_bombus_sdm",
            ["Rscript", "source_build/build_bombus_sdm_mainland.R", "--config", "config/bombus_sdm.yml", "--occurrence-dir", str(GBIF_CACHE), "--raster-dir", "data/processed/rasters", "--output-dir", "results/bombus_sdm_source_build", "--flower-data", "Data_S1.csv"],
            ["results/bombus_sdm_source_build/predictions/ardens.tif", "results/bombus_sdm_source_build/predictions/diversus.tif"],
        )
        self.run(
            "build_human_raster",
            ["Rscript", "source_build/build_human_raster.R", "--observation-csv", "Data_S1.csv", "--cache-dir", str(MLIT_CACHE), "--output-dir", "results/public_rasters/mlit_human_forest_edge_2021"],
            ["results/public_rasters/mlit_human_forest_edge_2021/mlit_human_forest_edge_1km.tif"],
        )
        self.run(
            "build_environment_input",
            ["Rscript", "scripts/build_environment_input.R", "--raw-colour-csv", "Data_S1.csv", "--cache-root", str(PUBLIC_CACHE), "--output-csv", "results/environment_v3/ecological_input_v2.csv"],
            ["results/environment_v3/ecological_input_v2.csv"],
        )
        self.run(
            "environment_spatial",
            ["Rscript", "scripts/run_environment_spatial.R", "--anomaly-csv", "results/environment_v3/ecological_input_v2.csv", "--raw-colour-csv", "Data_S1.csv", "--bombus-dir", "results/bombus_sdm_source_build/predictions", "--H-raster", str(WORLDPOP), "--R-raster", "results/public_rasters/mlit_human_forest_edge_2021/mlit_human_forest_edge_1km.tif", "--N-raster", str(PUBLIC_CACHE / "gdd5_Japan_crop_30s.tif"), "--A-raster", "results/public_rasters/mlit_human_forest_edge_2021/mlit_major_road_distance_1km.tif", "--output-dir", "results/ecological_v9_final_public_HRNA_50km", "--run-inla", "false", "--author-review-confirmed", "true"],
            ["results/ecological_v9_final_public_HRNA_50km/analysis_data.csv"],
        )
        self.run(
            "natural_biotic_covariates",
            ["Rscript", "scripts/run_natural_biotic_covariates.R", "--analysis-data", "results/ecological_v9_final_public_HRNA_50km/analysis_data.csv", "--occurrence-dir", str(GBIF_CACHE), "--raw-colour-csv", "Data_S1.csv", "--output-dir", "results/ecological_v10_final_mechanism_HRNA", "--tail-bootstraps", "199"],
            ["results/ecological_v10_final_mechanism_HRNA/analysis_data_mechanism_v3.csv"],
        )
        self.run(
            "two_part_phenotype",
            ["Rscript", "scripts/run_phenotype_hurdle.R", "--analysis-data", "results/ecological_v10_final_mechanism_HRNA/analysis_data_mechanism_v3.csv", "--output-dir", "results/ecological_v11_pigmentation_hurdle", "--run-inla", "true"],
            ["results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv"],
        )
        self.run(
            "one_km_cell_context",
            ["Rscript", "scripts/run_multiscale_hotspots.R", "--input", "results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv", "--output-dir", "results/ecological_v15_multiscale_hotspots", "--environment-cache", str(PUBLIC_CACHE), "--worldpop-raster", str(WORLDPOP), "--bootstrap", "1000"],
            ["results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv"],
        )
        self.run(
            "bombus_occurrence_reference_support",
            ["Rscript", "scripts/build_bombus_occurrence_reference_support.R", "--cells", "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv", "--sdm-root", "results/bombus_sdm_source_build", "--output", "results/bombus_occurrence_reference_support"],
            ["results/bombus_occurrence_reference_support/cell_effective_bombus_support.csv"],
        )
        self.run(
            "local_bombus_boundaries",
            ["Rscript", "scripts/run_bombus_local_sharp_transition.R", "--cells", "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv", "--support", "results/bombus_occurrence_reference_support/cell_effective_bombus_support.csv", "--radii", "5,10,25", "--thresholds", "1,0.75,0.5", "--k", "5", "--randomisations", "100000", "--seed", "20260808", "--output", "results/ecological_v18_bombus_local_sharp_transition"],
            ["results/ecological_v18_bombus_local_sharp_transition/sharp_transition_summary.csv"],
        )
        self.run(
            "natural_presence_1000",
            ["Rscript", "scripts/run_natural_predictive_model.R", "--observations", "results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv", "--cells", "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv", "--output", "results/ecological_v16_predictive_replication", "--draws", "1000", "--components", "national_environment_spde_presence", "--force", "true", "--seed", "20260725"],
            ["results/ecological_v16_predictive_replication/checkpoints/national_environment_spde_presence_draws1000.rds"],
        )
        self.run(
            "human_landscape_features",
            ["Rscript", "scripts/run_human_landscape_features.R", "--cells", "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv", "--observations", "results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv", "--mlit-dir", "results/public_rasters/mlit_human_forest_edge_2021", "--checkpoint-root", "results/ecological_v16_predictive_replication/checkpoints", "--output", "results/ecological_v19_human_landscape_extremes", "--max-draws", "1000"],
            ["results/ecological_v19_human_landscape_extremes/landscape_cell_features.csv"],
        )
        self.run(
            "local_human_context",
            ["Rscript", "scripts/run_local_human_context.R", "--cells", "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv", "--observations", "results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv", "--base-features", "results/ecological_v19_human_landscape_extremes/landscape_cell_features.csv", "--worldpop-raster", str(WORLDPOP), "--mlit-dir", "results/public_rasters/mlit_human_forest_edge_2021", "--mlit-cache", str(MLIT_CACHE), "--checkpoint-root", "results/ecological_v16_predictive_replication/checkpoints", "--output", "results/ecological_v21_local_human_neighbourhood", "--max-draws", "1000"],
            ["results/ecological_v21_local_human_neighbourhood/human_neighbourhood_cell_features.csv"],
        )
        self.run(
            "did_context",
            ["Rscript", "scripts/run_did_sensitivity.R", "--cells", "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv", "--features", "results/ecological_v21_local_human_neighbourhood/human_neighbourhood_cell_features.csv", "--checkpoint-root", "results/ecological_v16_predictive_replication/checkpoints", "--template", "results/public_rasters/mlit_human_forest_edge_2021/mlit_human_forest_edge_1km.tif", "--did-cache", str(DID_CACHE), "--output", "results/ecological_v22_did_human_context", "--max-draws", "1000"],
            ["results/ecological_v22_did_human_context/did_cell_context.csv"],
        )
        self.run(
            "final_broad_environment_spde",
            ["Rscript", "scripts/run_broad_environment_spatial_audit.R", "--input-csv", "results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv", "--landscape-csv", "results/ecological_v19_human_landscape_extremes/landscape_cell_features.csv", "--raster-dir", "data/processed/rasters", "--output-dir", "results/broad_environment_spatial_audit", "--bootstrap-reps", "4000", "--seed", "20260814"],
            ["results/broad_environment_spatial_audit"],
        )
        self.run(
            "fixed_space_only_cache",
            ["Rscript", "scripts/build_fixed_space_null_cache.R", "--cells=results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv", "--output=results/broad_space_null_geobin_sensitivity_v23", "--samples=500", "--seed=20260725", "--max-pairs-per-fold=15000"],
            ["results/broad_space_null_geobin_sensitivity_v23/fixed_heldout_pair_table.csv", "results/broad_space_null_geobin_sensitivity_v23/heldout_space_null_site_draws.rds"],
        )
        self.run(
            "supported_environment_distance",
            ["Rscript", "scripts/fit_broad_supported_term_distance_space_null.R", "--cells=results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv", "--cache-dir=results/broad_space_null_geobin_sensitivity_v23", "--output=results/broad_supported_term_distance_space_null"],
            ["results/broad_supported_term_distance_space_null/primary_supported_term_distance_test.csv"],
        )
        self.run(
            "final8_presence_10000",
            ["Rscript", "scripts/fit_final8_presence_null.R", "--cells", "results/ecological_v15_multiscale_hotspots/multiscale_hotspot_cells_1km.csv", "--output", "results/final8_presence_null", "--draws", "10000", "--seed", "20260725"],
            ["results/final8_presence_null/final8_presence_draws10000.rds"],
        )
        self.run(
            "continuous_colour_isolation",
            ["Rscript", "analysis_sensitivity/run_continuous_colour_isolation_human_context.R", "--reference-root", ".", "--final8-root", "results/final8_presence_null", "--output", "results/continuous_colour_isolation_human_context", "--draws", "10000", "--permutations", "2000", "--seed", "20260725"],
            ["results/continuous_colour_isolation_human_context/continuous_isolation_validation.json"],
        )
        self.write_manifest("SUCCESS")

    def write_manifest(self, status: str, error: str | None = None) -> None:
        if self.dry_run:
            return
        MANIFEST.parent.mkdir(parents=True, exist_ok=True)
        payload = {
            "status": status,
            "generated_utc": utc_now(),
            "data_s1_git_blob": git_blob_sha(ROOT / "Data_S1.csv"),
            "code_s1_git_blob": git_blob_sha(ROOT / "Code_S1.py"),
            "source_boundary": "committed derived Data_S1 plus live/pinned public environmental and occurrence sources",
            "privacy_boundary": "no manuscript, submission bundle, author metadata or original third-party photographs",
            "stages": self.records,
            "error": error,
        }
        MANIFEST.write_text(json.dumps(payload, indent=2) + "\n", encoding="utf-8")


def main() -> int:
    parser = argparse.ArgumentParser()
    sub = parser.add_subparsers(dest="command", required=True)
    sub.add_parser("audit", help="validate the public reproducibility contract")
    reproduce = sub.add_parser("reproduce", help="rebuild the public analysis from Data_S1")
    reproduce.add_argument("--dry-run", action="store_true")
    reproduce.add_argument("--no-resume", action="store_true", help="rerun stages even when outputs exist")
    reproduce.add_argument("--skip-setup", action="store_true", help="do not install/restore declared R packages")
    args = parser.parse_args()

    pipeline = Pipeline(
        dry_run=bool(getattr(args, "dry_run", False)),
        resume=not bool(getattr(args, "no_resume", False)),
        skip_setup=bool(getattr(args, "skip_setup", False)),
    )
    try:
        if args.command == "audit":
            pipeline.audit()
        else:
            pipeline.reproduce()
        return 0
    except Exception as error:
        if args.command == "reproduce":
            pipeline.write_manifest("FAILED", f"{type(error).__name__}: {error}")
        print(f"ERROR: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
