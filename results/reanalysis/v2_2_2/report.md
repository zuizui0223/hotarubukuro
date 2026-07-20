# Reviewed public-data reanalysis

Analysis version: `public-reanalysis-v1`.

## Data flow

- 1,965 source photographs.
- 2 duplicate-photo/different-coordinate records hard-excluded.
- 1,963 primary photograph records retained; 1,955 complete for the six-predictor model.
- 1,887 records have complete values for all five legacy *Bombus* suitability rasters.
- 1,925 exact-site sensitivity units.
- 19 public predictor layers validated and joined by stable ID.

## Main result

For apparent CIELAB a*:

- Environment-only blocked Q²: 0.2156 on the full complete cohort.
- Environment-only blocked Q²: 0.2337 on the *Bombus*-complete cohort.
- Environment + *Bombus* suitability blocked Q²: 0.2521 on that same cohort.
- Increment attributable to adding the legacy suitability sum: 0.0184 Q².

The six colour-estimation methods give a narrow environment-only Q² range
(0.2156–0.2234), so the predictive result is not driven by one colour summary.

## Interpretation limit

`Bombus_suitability_sum` is a sum of climate-based legacy SDM predictions. It is
not observed richness, visitation, geographic occupancy, or realized pollinator
availability. In particular, the current model does not encode structural
absence on the Izu Islands and therefore does not test the proposed
Izu-white-flower availability hypothesis. The modest increment may partly
reflect environmental information reused by the legacy SDMs.

WorldPop is treated as an access/sampling-intensity proxy. These models describe
camera-rendered apparent flower colour; they do not estimate calibrated pigment,
reflectance, pollinator vision, adaptation, or causality.

## Remaining limitations

Manual adjudication of 785 automated review flags is incomplete. Coordinates
were remapped from the workbook but not independently reconstructed from GPX.
White balance is not identifiable from the available cut-outs. The five
*Bombus* rasters are structurally valid but remain `legacy_unverifiable`.
