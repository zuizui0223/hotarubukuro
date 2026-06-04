# hotarubukuro

**From hiking trails to a trait database: georeferenced phenotyping reveals hidden structure and drivers of floral color variation, illustrated in *Campanula punctata***

## Overview

This repository contains all code and data associated with a nationwide study of floral colour variation in the spotted bellflower (*Campanula punctata* Lam.) across the Japanese archipelago. The study develops a georeferenced phenotyping workflow that links time-stamped flower photographs from a hiking platform (YAMAP) to GPS track logs, enabling continuous floral colour quantification at a national scale (n = 1,926 records).

## Repository Structure

```
hotarubukuro/
├── Code_S1.py                 # GPS georeferencing: match photo timestamps to GPX track logs
├── Code_S2.py                 # Image processing: extract dominant petal RGB via k-means clustering
├── Code_S3.R                  # Main analysis pipeline (R script)
├── Code_S3.Rmd                # Main analysis pipeline (R Markdown, same content as Code_S3.R)
├── Code_S4.R                  # Species distribution models (SDM) for five Bombus species
├── Data_S1.csv                # Georeferenced flower colour dataset (latitude, longitude, R, G, B)
└── bee5/                      # SDM output rasters (TIFF files per species)
```

## Workflow

```
[Field data — YAMAP hiking photos + GPX logs]
        ↓
Code_S1.py
  Match photo timestamps → nearest GPS point → latitude/longitude
        ↓
Code_S2.py
  Segment petal region → k-means clustering → dominant RGB value per image
        ↓
Data_S1.csv  (latitude, longitude, R, G, B per record)
        ↓
Code_S3.R / Code_S3.Rmd  — Main analysis
  ├─ Colour space conversion: RGB → CIELAB (L*, a*, b*), chroma C*, HSV
  ├─ Confirmatory factor analysis (CFA) → latent floral colour index (Pigment factor)
  ├─ Environmental PCA: temperature, precipitation/dryness, soil physical, soil nutrient, topography
  ├─ Multicollinearity filtering (r ≥ 0.7 threshold)
  ├─ Spatial Bayesian model (SPDE–INLA, Matérn covariance) → fixed effects estimation
  └─ Quantile GAM (qGAM, τ = 0.75–0.95) → upper-tail drivers (human population density)
        ↓
Code_S4.R  — Pollinator context
  MaxNet SDM for Bombus ardens, beaticola, consobrinus, diversus, honshuensis
  → Cumulative species richness map (bee5/)
```

## Key Results

- The spatial random field accounted for **31.7%** of total floral colour variance; fixed environmental effects accounted for **9.5%**; residual variance **58.8%**.
- Temperature, precipitation–dryness, topography, and a pollinator-associated predictor retained significant effects after spatial correction.
- Human population density showed quantile-dependent effects: effect size increased from **0.419** (τ = 0.75) to **2.488** (τ = 0.95), suggesting anthropogenic influence is concentrated in the upper tail of colour variation.

## Dependencies

### Python
```
opencv-python
pandas
scikit-learn
numpy
openpyxl
```

### R
```r
install.packages(c(
  "lavaan", "qgam",
  "sf", "terra", "rnaturalearth", "ggspatial",
  "ggplot2", "patchwork", "dplyr", "tidyr",
  "ENMeval", "maxnet", "corrplot"
))

# INLA requires separate installation:
install.packages("INLA",
  repos = c(getOption("repos"),
            INLA = "https://inla.r-inla-download.org/R/stable"))
```

## Data

`Data_S1.csv` contains floral colour records collected during the flowering seasons of 2023–2025. Records are restricted to wild individuals photographed along hiking routes. Cultivated or ornamental individuals were excluded during image curation.

| Column | Description |
|--------|-------------|
| `date` | Collection date |
| `latitude` | Decimal degrees (WGS84) |
| `longitude` | Decimal degrees (WGS84) |
| `R` | Red channel (0–255) |
| `G` | Green channel (0–255) |
| `B` | Blue channel (0–255) |

## Citation

> Zhang [year in prep]. From hiking trails to a trait database: georeferenced phenotyping reveals hidden structure and drivers of floral color variation, illustrated in *Campanula punctata*. *New Phytologist*.

## License

Code: MIT License  
Data: CC BY 4.0
