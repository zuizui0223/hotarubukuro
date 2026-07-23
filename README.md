# hotarubukuro

Nationwide image phenotyping and spatial ecological analysis of floral colour in
*Campanula punctata*.

## Current study design

The author isolated/cropped each focal flower and visually confirmed the petal
colour-extraction region before scripted colour calculation. The current primary
responses are (i) response-blind optical pigmentation presence and (ii) CIELAB
a* intensity conditional on flowers classified as pigmented. White-flower a*
variation is not treated as anthocyanin quantity. Dark/pink extremes and
automatic mask/shape warnings are retained; only unformable colour measurements
and exact duplicate images are excluded.

The inferential workflow has three layers:

1. **Natural baseline:** response-blind environmental axes plus PC-prior
   INLA-SPDE quantify measured gradients and unresolved spatial biogeography.
2. **Bombus challenge:** freshly generated ENMeval predictions and frozen public
   occurrence records separate landscape concordance, shared geography, and
   locally remaining variation. Pre-existing Bombus TIFFs are not analysis
   inputs.
3. **Human-interface challenge:** population exposure H and an independently
   defined MLIT forest--human interface R test a prespecified H-by-R prediction
   on the original two-part outcomes. Road access A and observation date are
   sensitivities. Natural-model cross-fitted residuals are used only for
   residual-tail convergence checks and candidate ranking, never as
   horticultural labels.

## Current results

- Final locked pipeline: `results/final_analysis_pipeline/`
- Base: `results/ecological_v9_final_public_HRNA_50km/`
- Mechanism extension: `results/ecological_v10_final_mechanism_HRNA/`
- Response-corrected analysis: `results/ecological_v11_pigmentation_hurdle/`
- Public MLIT layers: `results/public_rasters/mlit_human_forest_edge_2021/`
- Local white-neighbourhood human context:
  `results/ecological_v21_local_human_neighbourhood/`
- DID settlement-context sensitivity:
  `results/ecological_v22_did_human_context/`
- Rendered report: `final_v11.html`

The response-blind a* mixture separates 966 white-like and 957 pigmented
observations at a*=4.94. Bombus proxies show strong broad co-geography, but W/A
add essentially no spatial-fold information for pigmentation presence after
environment and space. W retains a small negative gradient within pigmented
flowers (held-out delta R-squared about 0.01); A does not. A weak western H-by-R
association occurs for pigmentation presence, but it does not strengthen in the
extreme residual tail and early-flowering convergence is absent. The current
data therefore rank horticultural candidates but do not support cultivar origin
or garden escape at population level.

The later v21/v22 local-discontinuity workflow holds human variables out of
candidate selection and compares each pigmented isolate with its own
environment-similar observed-white neighbours in 1,000 natural predictive maps.
Five-km population and an independent 2015 DID layer show a consistent local
settlement-proximity direction, but neither passes a strict 0.05 family-wise
threshold. These results prioritize follow-up cells and do not identify
planting, garden escape, horticultural origin, or introgression.

See:

- `FINAL_ANALYSIS_PIPELINE.md` for the locked final workflow, included results,
  and explicit exclusions;
- `ANALYSIS_REVIEW_V11.md` for the reviewer-facing validity audit;
- `MANUSCRIPT_STORY_V11.md` for the paper narrative and claim language;
- `MECHANISM_WORKFLOW_V3.md` for locked hypotheses and anti-circularity rules;
- `PUBLIC_RASTER_DESIGN.md` for H/R/N/A definitions;
- `ANALYSIS_REVIEW_V22.md` for the current human-settlement continuation;
- `results/README.md` for current versus development output directories.

## Main files

```text
Data_S1.csv
final.Rmd                         legacy full analysis draft; preserved
final_v11.Rmd                     manuscript synthesis of the v11 hurdle results
scripts/ecological_analysis_v2.R  base ecological/SPDE implementation
scripts/ecological_mechanism_v3.R Bombus and horticultural evidence workflow
scripts/pigmentation_hurdle_v4.R  two-part response and residual-tail workflow
scripts/mlit_human_forest_edge.R  response-blind MLIT R and A construction
scripts/run_ecological_analysis_v2.R
scripts/run_ecological_mechanism_v3.R
scripts/run_pigmentation_hurdle_v4.R
scripts/run_local_human_neighbourhood_v14.R
scripts/run_did_human_context_v15.R
scripts/render_final_v11.R
tests/testthat/
```

`final.Rmd` is retained as the historical, full executable draft. The v10 report
and v11 report are deliberately separate so that old embedded results cannot
silently overwrite the response-corrected outputs.

## Reproducibility

Render the current report from the repository root:

```powershell
& 'C:\Program Files\R\R-4.5.3\bin\Rscript.exe' scripts/render_final_v11.R
```

The render helper automatically finds the Pandoc bundled with RStudio when it is
not already on `PATH`.

Run the test suite:

```powershell
& 'C:\Program Files\R\R-4.5.3\bin\Rscript.exe' -e "testthat::test_dir('tests/testthat')"
```

On Windows, run INLA with `TEMP` and `TMP` set to an ASCII-only directory before
starting R. INLA's external executable can fail to create a working directory
when the temporary path contains Japanese characters.

The base runner requires an anomaly input and the directory of freshly selected
ENMeval prediction rasters. The mechanism runner defaults to the locked v9 base
and requires the frozen Bombus occurrence directory through `--occurrence-dir`
or `HOTARUBUKURO_BOMBUS_OCCURRENCE_DIR`.

## Key dependencies

R packages include `INLA`, `sf`, `terra`, `ENMeval`, `maxnet`, `mclust`, `qgam`, `mgcv`,
`testthat`, and `rmarkdown`. Python image/georeferencing utilities use `opencv-
python`, `pandas`, `scikit-learn`, `numpy`, and `openpyxl`.

## License

Code: MIT License. Data: CC BY 4.0.
