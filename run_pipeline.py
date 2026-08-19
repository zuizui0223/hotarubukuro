#!/usr/bin/env python3
"""Single publication analysis entry point.

`audit` verifies the committed public contract. `reproduce` rebuilds the final
submission analysis from Data_S1.csv plus declared public sources. Superseded
exploratory pipelines are deliberately not callable from here.
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
PUBLIC_CACHE = CACHE / "public_environment"
MLIT_CACHE = CACHE / "mlit_l03_2021"
GBIF_CACHE = CACHE / "bombus_gbif"
WORLDPOP = PUBLIC_CACHE / "population_count_Japan_crop.tif"
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
    "Data_S1.csv", "Code_S1.py", "config/pipeline.yml", "config/raster_sources.csv",
    "config/bombus_sdm.yml", "dependencies/r-packages.csv", "dependencies/r-version.txt",
    "R/pipeline_support.R", "R/environment_spatial.R", "R/natural_biotic_covariates.R",
    "R/phenotype_hurdle.R", "R/analysis_cells.R", "R/natural_predictive_model.R",
    "R/continuous_colour_isolation.R", "R/local_pair_graph.R", "R/human_raster_features.R",
    "source_build/download_rasters.R", "source_build/prepare_rasters.R",
    "source_build/fetch_bombus_occurrences.R", "source_build/build_bombus_sdm_mainland.R",
    "source_build/build_human_raster.R", "scripts/build_environment_input.R",
    "scripts/run_environment_spatial.R", "scripts/run_natural_biotic_covariates.R",
    "scripts/run_phenotype_hurdle.R", "scripts/build_analysis_cells.R",
    "scripts/build_bombus_occurrence_reference_support.R",
    "scripts/run_bombus_local_sharp_transition.R",
    "scripts/run_bombus_spatial_replication_test.R", "scripts/build_broad_landscape_context.R",
    "scripts/run_broad_environment_spatial_audit.R", "scripts/build_fixed_space_null_cache.R",
    "scripts/fit_broad_supported_term_distance_space_null.R", "scripts/fit_final8_presence_null.R",
    "scripts/run_continuous_colour_isolation.R",
]


def utc_now() -> str:
    return dt.datetime.now(dt.timezone.utc).isoformat(timespec="seconds")


def git_blob_sha(path: Path) -> str:
    data = path.read_bytes()
    return hashlib.sha1(f"blob {len(data)}\0".encode() + data).hexdigest()


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(4 * 1024 * 1024), b""):
            digest.update(block)
    return digest.hexdigest()


class ReproductionError(RuntimeError):
    pass


class Pipeline:
    def __init__(self, dry_run: bool, resume: bool, skip_setup: bool) -> None:
        self.dry_run = dry_run
        self.resume = resume
        self.skip_setup = skip_setup
        self.records: list[dict[str, object]] = []
        self.env = os.environ.copy()
        self.env.update(THREAD_ENV)
        self.env.update(
            HOTARUBUKURO_ROOT=str(ROOT),
            HOTARUBUKURO_PUBLIC_CACHE=str(PUBLIC_CACHE),
            HOTARUBUKURO_MLIT_CACHE=str(MLIT_CACHE),
            HOTARUBUKURO_WORLDPOP_RASTER=str(WORLDPOP),
            HOTARUBUKURO_INPUT_ROOT=str(ROOT),
        )

    def audit(self) -> None:
        missing = [path for path in REQUIRED_FILES if not (ROOT / path).is_file()]
        if missing:
            raise ReproductionError("Missing publication files: " + ", ".join(missing))
        for rel, expected in EXPECTED_GIT_BLOBS.items():
            observed = git_blob_sha(ROOT / rel)
            if observed != expected:
                raise ReproductionError(f"{rel} blob changed: {observed} != {expected}")
        with (ROOT / "Data_S1.csv").open("r", encoding="utf-8-sig", newline="") as handle:
            reader = csv.reader(handle)
            header = next(reader)
            rows = sum(1 for _ in reader)
        if rows != 1965:
            raise ReproductionError(f"Data_S1 row count changed: {rows} != 1965")
        missing_columns = {"observation_id", "latitude", "longitude"} - set(header)
        if missing_columns:
            raise ReproductionError("Data_S1 missing: " + ", ".join(sorted(missing_columns)))
        print("PASS submission reproducibility contract")
        print(f"Data_S1 rows={rows}; blob={EXPECTED_GIT_BLOBS['Data_S1.csv']}")

    def run(self, name: str, command: Sequence[str], outputs: Iterable[str] = ()) -> None:
        paths = [ROOT / output for output in outputs]
        if self.resume and paths and all(path.exists() for path in paths):
            print(f"\n==> {name}: already complete")
            self.records.append({"stage": name, "status": "SKIPPED_COMPLETE"})
            return
        print(f"\n==> {name}\n    $ {' '.join(command)}")
        started = utc_now()
        if self.dry_run:
            self.records.append({"stage": name, "status": "DRY_RUN", "started_at": started})
            return
        subprocess.run(list(command), cwd=ROOT, env=self.env, check=True)
        absent = [str(path.relative_to(ROOT)) for path in paths if not path.exists()]
        if absent:
            raise ReproductionError(f"{name} did not create: {', '.join(absent)}")
        self.records.append(
            {"stage": name, "status": "SUCCESS", "started_at": started, "finished_at": utc_now()}
        )

    def prepare_aliases(self) -> None:
        if self.dry_run:
            return
        source_root = ROOT / "data" / "processed" / "rasters"
        PUBLIC_CACHE.mkdir(parents=True, exist_ok=True)
        missing: list[str] = []
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

    def ensure_worldpop(self) -> None:
        if self.dry_run:
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
                    time.sleep(min(2**attempt, 30))
        (CACHE / "worldpop_provenance.json").write_text(
            json.dumps(
                {"source_url": WORLDPOP_URL, "sha256": sha256(WORLDPOP), "bytes": WORLDPOP.stat().st_size},
                indent=2,
            ) + "\n",
            encoding="utf-8",
        )

    def reproduce(self) -> None:
        self.audit()
        CACHE.mkdir(parents=True, exist_ok=True)
        RESULTS.mkdir(parents=True, exist_ok=True)
        if not self.skip_setup:
            self.run(
                "restore_r_environment",
                ["Rscript", "scripts/setup_r_environment.R", "--report-dir",
                 "results/analysis_reproduction/environment", "--scopes",
                 "analysis,acquisition,reproducibility,testing"],
                ["results/analysis_reproduction/environment/package_versions.csv"],
            )
        self.run("download_public_rasters", ["Rscript", "source_build/download_rasters.R"],
                 ["data/processed/raster_download_manifest.csv"])
        self.run("prepare_public_rasters", ["Rscript", "source_build/prepare_rasters.R", "--no-download"],
                 ["data/processed/raster_manifest.csv"])
        self.prepare_aliases()
        self.ensure_worldpop()
        self.run(
            "fetch_bombus_occurrences",
            ["Rscript", "source_build/fetch_bombus_occurrences.R", "--output-dir", str(GBIF_CACHE),
             "--refresh", "false"],
            [str((GBIF_CACHE / "ardens_gbif.csv").relative_to(ROOT))],
        )
        self.run(
            "build_bombus_sdm",
            ["Rscript", "source_build/build_bombus_sdm_mainland.R", "--config", "config/bombus_sdm.yml",
             "--occurrence-dir", str(GBIF_CACHE), "--raster-dir", "data/processed/rasters",
             "--output-dir", "results/bombus_sdm_source_build", "--flower-data", "Data_S1.csv"],
            ["results/bombus_sdm_source_build/predictions/ardens.tif",
             "results/bombus_sdm_source_build/predictions/diversus.tif"],
        )
        self.run(
            "build_forest_reference_raster",
            ["Rscript", "source_build/build_human_raster.R", "--observation-csv", "Data_S1.csv",
             "--cache-dir", str(MLIT_CACHE), "--output-dir",
             "results/public_rasters/mlit_human_forest_edge_2021"],
            ["results/public_rasters/mlit_human_forest_edge_2021/mlit_human_forest_edge_1km.tif"],
        )
        self.run(
            "build_environment_input",
            ["Rscript", "scripts/build_environment_input.R", "--raw-colour-csv", "Data_S1.csv",
             "--cache-root", str(PUBLIC_CACHE), "--output-csv", "results/environment_v3/ecological_input_v2.csv"],
            ["results/environment_v3/ecological_input_v2.csv"],
        )
        self.run(
            "environment_spatial",
            ["Rscript", "scripts/run_environment_spatial.R", "--anomaly-csv",
             "results/environment_v3/ecological_input_v2.csv", "--raw-colour-csv", "Data_S1.csv",
             "--bombus-dir", "results/bombus_sdm_source_build/predictions", "--H-raster", str(WORLDPOP),
             "--R-raster", "results/public_rasters/mlit_human_forest_edge_2021/mlit_human_forest_edge_1km.tif",
             "--N-raster", str(PUBLIC_CACHE / "gdd5_Japan_crop_30s.tif"), "--A-raster",
             "results/public_rasters/mlit_human_forest_edge_2021/mlit_major_road_distance_1km.tif",
             "--output-dir", "results/ecological_v9_final_public_HRNA_50km", "--run-inla", "false",
             "--author-review-confirmed", "true"],
            ["results/ecological_v9_final_public_HRNA_50km/analysis_data.csv"],
        )
        self.run(
            "natural_biotic_covariates",
            ["Rscript", "scripts/run_natural_biotic_covariates.R", "--analysis-data",
             "results/ecological_v9_final_public_HRNA_50km/analysis_data.csv", "--occurrence-dir", str(GBIF_CACHE),
             "--raw-colour-csv", "Data_S1.csv", "--output-dir", "results/ecological_v10_final_mechanism_HRNA",
             "--tail-bootstraps", "199"],
            ["results/ecological_v10_final_mechanism_HRNA/analysis_data_mechanism_v3.csv"],
        )
        self.run(
            "two_part_phenotype",
            ["Rscript", "scripts/run_phenotype_hurdle.R", "--analysis-data",
             "results/ecological_v10_final_mechanism_HRNA/analysis_data_mechanism_v3.csv", "--output-dir",
             "results/ecological_v11_pigmentation_hurdle", "--run-inla", "true"],
            ["results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv"],
        )
        cells = "results/analysis_cells/analysis_cells_1km.csv"
        phenotype = "results/ecological_v11_pigmentation_hurdle/analysis_data_pigmentation_hurdle.csv"
        self.run(
            "build_final_analysis_cells",
            ["Rscript", "scripts/build_analysis_cells.R", "--input", phenotype,
             "--environment-cache", str(PUBLIC_CACHE), "--output", "results/analysis_cells"],
            [cells, "results/analysis_cells/analysis_cell_contract.csv"],
        )
        support = "results/bombus_occurrence_reference_support/cell_effective_bombus_support.csv"
        self.run(
            "bombus_occurrence_reference_support",
            ["Rscript", "scripts/build_bombus_occurrence_reference_support.R", "--cells", cells,
             "--sdm-root", "results/bombus_sdm_source_build", "--output", "results/bombus_occurrence_reference_support"],
            [support],
        )
        self.run(
            "local_bombus_boundaries",
            ["Rscript", "scripts/run_bombus_local_sharp_transition.R", "--cells", cells, "--support", support,
             "--radii", "5,10,25", "--thresholds", "1,0.75,0.5", "--k", "5", "--randomisations",
             "100000", "--seed", "20260808", "--output", "results/bombus_local_boundaries"],
            ["results/bombus_local_boundaries/sharp_transition_summary.csv"],
        )
        self.run(
            "bombus_spatial_guardrail",
            ["Rscript", "scripts/run_bombus_spatial_replication_test.R", "--cells", cells, "--support", support,
             "--radii", "5,10,25", "--background-windows", "25,50,100", "--control-counts", "10,20,50",
             "--k", "5", "--randomisations", "100000", "--seed", "20260809", "--output",
             "results/bombus_spatial_guardrail"],
            ["results/bombus_spatial_guardrail/interpretation_summary.csv"],
        )
        self.run(
            "build_broad_landscape_context",
            ["Rscript", "scripts/build_broad_landscape_context.R", "--observations", phenotype, "--mlit-dir",
             "results/public_rasters/mlit_human_forest_edge_2021", "--output",
             "results/broad_landscape_context/forest_fraction_context.csv"],
            ["results/broad_landscape_context/forest_fraction_context.csv"],
        )
        self.run(
            "final_broad_environment_spde",
            ["Rscript", "scripts/run_broad_environment_spatial_audit.R", "--input-csv", phenotype,
             "--landscape-csv", "results/broad_landscape_context/forest_fraction_context.csv", "--raster-dir",
             "data/processed/rasters", "--output-dir", "results/broad_environment_spatial_audit",
             "--bootstrap-reps", "4000", "--seed", "20260814"],
            ["results/broad_environment_spatial_audit/analysis_manifest.json"],
        )
        self.run(
            "fixed_space_only_reference",
            ["Rscript", "scripts/build_fixed_space_null_cache.R", f"--cells={cells}",
             "--output=results/broad_space_null", "--samples=500", "--seed=20260725",
             "--max-pairs-per-fold=15000"],
            ["results/broad_space_null/fixed_heldout_pair_table.csv",
             "results/broad_space_null/heldout_space_null_site_draws.rds"],
        )
        self.run(
            "supported_environment_distance",
            ["Rscript", "scripts/fit_broad_supported_term_distance_space_null.R", f"--cells={cells}",
             "--cache-dir=results/broad_space_null", "--output=results/broad_supported_term_distance"],
            ["results/broad_supported_term_distance/primary_supported_term_distance_test.csv"],
        )
        final8 = "results/final8_presence_null/final8_presence_draws10000.rds"
        self.run(
            "final8_natural_maps",
            ["Rscript", "scripts/fit_final8_presence_null.R", "--cells", cells, "--output",
             "results/final8_presence_null", "--draws", "10000", "--seed", "20260725"],
            [final8],
        )
        self.run(
            "continuous_colour_isolation",
            ["Rscript", "scripts/run_continuous_colour_isolation.R", "--cells", cells, "--worldpop",
             str(WORLDPOP), "--final8", final8, "--output", "results/continuous_colour_isolation",
             "--draws", "10000", "--permutations", "2000", "--chunk-size", "250", "--seed", "20260725"],
            ["results/continuous_colour_isolation/validation.txt"],
        )
        self.write_manifest("SUCCESS")

    def write_manifest(self, status: str, error: str | None = None) -> None:
        if self.dry_run:
            return
        MANIFEST.parent.mkdir(parents=True, exist_ok=True)
        MANIFEST.write_text(
            json.dumps(
                {
                    "status": status,
                    "generated_utc": utc_now(),
                    "data_s1_git_blob": git_blob_sha(ROOT / "Data_S1.csv"),
                    "code_s1_git_blob": git_blob_sha(ROOT / "Code_S1.py"),
                    "stages": self.records,
                    "excluded_legacy": [
                        "hotspot/candidate ranking", "16-event local-departure detector",
                        "DID human context", "MLIT land-cover candidate classification",
                        "coefficient-weighted Broad space-null variants", "development-only interaction screens",
                    ],
                    "error": error,
                },
                indent=2,
            ) + "\n",
            encoding="utf-8",
        )


def main() -> int:
    parser = argparse.ArgumentParser()
    sub = parser.add_subparsers(dest="command", required=True)
    sub.add_parser("audit")
    reproduce = sub.add_parser("reproduce")
    reproduce.add_argument("--dry-run", action="store_true")
    reproduce.add_argument("--no-resume", action="store_true")
    reproduce.add_argument("--skip-setup", action="store_true")
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
    except Exception as exc:
        pipeline.write_manifest("FAILED", str(exc))
        print(f"ERROR: {exc}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    raise SystemExit(main())
