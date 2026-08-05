# Public environmental and human-context data

This registry records the public spatial inputs used by the publication
pipeline. All continuous environmental layers were cropped to Japan and
aligned to a 30-arc-second (approximately 1-km) EPSG:4326 grid before values
were extracted or used to fit the *Bombus* ENMeval models. Bilinear
interpolation was used for continuous climate and elevation layers. SoilGrids
subsets were obtained from the native 250-m product, masked using valid
bulk-density cells, and aggregated to the common grid.

## Natural-environment predictors

| Provider and version | Variables used | Native / analysis grain | Role | Provider page |
|---|---|---:|---|---|
| CHELSA v2.1 climatologies, 1981–2010 | BIO5, BIO10, growing degree-days above 5 °C, climate moisture index, BIO12, BIO14, BIO15, BIO4, and surface downwelling short-wave radiation | 30 arc-seconds / 30 arc-seconds | Temperature, precipitation/moisture, seasonality, and radiation predictors; BIO10, BIO12, and radiation also entered the multiscale environmental decomposition | <https://www.chelsa-climate.org/datasets/chelsa_bioclim> and <https://envidat.ch/metadata/chelsa-climatologies> |
| ISRIC SoilGrids 2.0 | Mean bulk density, coarse fragments, sand, silt, total nitrogen, organic-carbon density, soil organic carbon, and pH in water for 0–5 cm | 250 m / 30 arc-seconds | Physical and nutrient soil axes and soil pH in the national input | <https://docs.isric.org/globaldata/soilgrids/SoilGrids_faqs.html> and <https://docs.isric.org/globaldata/soilgrids/wcs.html> |
| WorldClim 2.1 | Elevation | 30 arc-seconds / 30 arc-seconds | Elevation and derived slope, terrain ruggedness index, and roughness; elevation also entered the multiscale environmental decomposition | <https://www.worldclim.org/data/worldclim21.html> |

For the locked natural predictive model, elevation, CHELSA BIO10, CHELSA
BIO12, and CHELSA radiation were represented at two scales. At each observed
1-km cell, the pipeline calculated a 50-km neighbourhood mean and the
cell-minus-neighbourhood deviation, then summarized the four broad-scale and
four within-neighbourhood variables with two response-blind principal
components per scale. These four components were the fixed environmental
effects in the national environment-plus-SPDE models. The 25- and 100-km
decompositions were sensitivity analyses.

The complete raster file list and MD5 hashes for the original national
environment table are stored in
`results/environment_v3/environment_input_manifest.json`. The four-layer
multiscale subset and its hashes are stored in
`results/ecological_v15_multiscale_hotspots/multiscale_environment_provenance.csv`.

## Human-landscape characterization

| Provider and version | Variables used | Grain | Role | Provider page |
|---|---|---:|---|---|
| WorldPop, Japan 2020, unconstrained/unadjusted population count | People per raster cell | approximately 1 km; summed within 5, 10, 25, and 50 km | Post-selection population context around locally unexpected pigmented cells and their white-flowered neighbours | <https://data.worldpop.org/GIS/Population/Global_2000_2020_1km_UNadj/2020/JPN/> |
| Ministry of Land, Infrastructure, Transport and Tourism (MLIT), National Land Numerical Information L03-b, 2021 | 100-m land-use classes | summarized to the study 1-km cells | Post-selection settlement, cultivation, forest–managed-land interface, and major-road context | <https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-L03-b-2021.html> |
| MLIT, National Land Numerical Information A16, 2015 | Densely Inhabited District polygons | distance summarized on the study 1-km grid | Sensitivity analysis of proximity to dense settlement | <https://nlftp.mlit.go.jp/ksj/gml/datalist/KsjTmplt-A16-v2_3.html> |

Human-context variables were deliberately withheld from the definition of
pigmented-isolate candidates and their white-flowered comparison
neighbourhoods. They characterize candidates after response-blind geographic,
environmental, and sampling-support rules have been applied; they are not
evidence of planting, horticultural origin, escape, or introgression.

## Predicted *Bombus* community

Occurrence records for five focal *Bombus* species were downloaded from GBIF,
deduplicated, date-filtered, and modelled from the public environmental layers
with ENMeval. The publication pipeline reselects model settings by AICc and
does not treat previously supplied species TIFFs as data. Species predictions
were converted to a community fingerprint containing total ranked habitat
support and two Hellinger-composition principal components. These values
represent relative predicted habitat suitability, not abundance, visitation
frequency, or pollination effectiveness.

The occurrence cache is under
`results/bombus_occurrence_phenology_cache/`; selected settings and prediction
hashes are under `results/enmeval_aicc_reselected/` and
`results/ecological_v9_final_public_HRNA_50km/bombus_prediction_manifest.csv`.

## Core references

- Brun, P., Zimmermann, N. E., Hari, C., Pellissier, L., and Karger, D. N.
  (2022). Global climate-related predictors at kilometre resolution for the
  past and future. *Earth System Science Data*, 14, 5573–5603.
  <https://doi.org/10.5194/essd-14-5573-2022>
- Fick, S. E., and Hijmans, R. J. (2017). WorldClim 2: new 1-km spatial
  resolution climate surfaces for global land areas. *International Journal
  of Climatology*, 37, 4302–4315. <https://doi.org/10.1002/joc.5086>
- Karger, D. N., et al. (2017). Climatologies at high resolution for the
  earth's land surface areas. *Scientific Data*, 4, 170122.
  <https://doi.org/10.1038/sdata.2017.122>
- Poggio, L., de Sousa, L. M., Batjes, N. H., Heuvelink, G. B. M., Kempen,
  B., Ribeiro, E., and Rossiter, D. (2021). SoilGrids 2.0: producing soil
  information for the globe with quantified spatial uncertainty. *SOIL*, 7,
  217–240. <https://doi.org/10.5194/soil-7-217-2021>

Access and provenance records were checked on 24 July 2026.
