# Result — supported environmental distance versus spatial continuity

The final environment + SPDE models were not refitted. They only fixed the supported term sets used to calculate unweighted environmental distance.

| Response | Supported terms | Observed contrast | Space-null median | Excess | One-sided P |
|---|---|---:|---:|---:|---:|
| pigmentation_state | env_Temperature_PC1 | 0.100608 | 0.048475 | +0.052133 | 0.009980 |
| conditional_intensity | env_precip_PC1, env_TemperatureSeasonality, env_topo_PC1, int_thermal_variability | 0.046626 | 0.019459 | +0.027167 | 0.259481 |

Design: five held-out geographical folds, five geographical-distance strata per fold, up to 15,000 pairs per fold and 500 posterior-predictive maps from an intercept + stationary Matérn SPDE.

Workflow run: `32213509187`.

Interpretation ceiling: this is an unsigned supporting comparison. Direction comes from the accepted final-model coefficients; the test does not establish selection, adaptation, genetics, drift, plasticity or direct physiology.
