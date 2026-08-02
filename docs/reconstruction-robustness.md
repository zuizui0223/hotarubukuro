# Reconstruction robustness analysis

## What this is, and what it is not

This analysis does **not** reproduce the published manuscript. It cannot: the
published analysis inputs no longer exist anywhere, which
`docs/established-inputs.md` establishes by search rather than assumption.

It asks a different and answerable question:

> Do the manuscript's conclusions still hold when the entire pipeline is rebuilt
> from nothing but declared, reproducible public sources?

The analysis population is the **public reconstruction**: 1909 observations
(955 white-like, 954 pigmented) derived from `Data_S1.csv` and the pinned
CHELSA, SoilGrids, WorldClim, WorldPop, MLIT and GBIF sources, with every input
checksummed in an immutable Release snapshot. The published analysis used 1923
observations (966, 957). The difference arises in `R/environment_spatial.R`,
where the analysis population is defined by `complete.cases` over the extracted
environmental covariates, so a coverage difference in any public raster changes
which observations survive.

The two analyses are therefore not the same analysis, and their numbers are not
expected to match. Treating agreement in the fourth decimal as the goal would be
the wrong test. What is being tested is whether the *conclusions* survive.

## How the frozen audits are handled

The pipeline's frozen upstream audits exist to confirm that a run is operating
on the published inputs. Under this analysis they are not, by design. Rather
than delete the audits or fake a pass, the pipeline runs them with
`--baseline reconstruction`:

| check | published mode | reconstruction mode |
|---|---|---|
| `analysis_n` | must equal 1923 | `not_applicable`, records observed vs published |
| `species_warning_isolated` | must be exactly 2 rows on `bee_consobrinus_ns` fold 5 | re-expressed as `species_warnings_remain_isolated`: warnings absent, or confined to one predictor and one fold |
| `population_grain` | must equal 1307 cells | `not_applicable`, records observed vs published |
| every other check | enforced | enforced, unchanged |

Eight of the ten phenotype checks are dataset-independent — the binary response,
the conditional-intensity domain, the measurement-count consistency, the twelve
INLA models with finite WAIC, the cross-fitted residual completeness — and all
of them are enforced in both modes. `not_applicable` is reported as its own
state and is never counted as a pass.

The re-expressed warning check deserves naming explicitly, because it is the one
place where a check's *form* changed. The published check asserted a specific
historical outcome. The scientific requirement behind it is that convergence
trouble stays isolated rather than becoming widespread, since a warning in many
folds would undermine the cross-fitted predictions the later stages rest on.
That requirement is what the reconstruction mode enforces.

## The discordance diagnostic

On top of the rebuilt pipeline, this analysis runs the bidirectional local
colour-state discordance diagnostic (`v23`, implemented in
`R/local_state_asymmetry.R`; "asymmetry" in the code identifiers and
"discordance" in prose refer to the same thing).

It reuses the locked primary local-isolate graph and asks whether a pigmented
cell surrounded by white neighbours is more common than a white cell surrounded
by pigmented neighbours, beyond the discordance that the fitted natural
environment-plus-SPDE baseline and the observed sampling design already produce.
The comparison is against 1000 cross-fitted natural predictive maps drawn from
the same posterior the locked pipeline produces.

It is a symmetry diagnostic, not a pre-specified test, and it has no published
counterpart — the comparison reports it as `new_analysis` rather than scoring it
against a number that does not exist.

## What is compared

`scripts/compare_reconstruction_to_published.R` writes
`reproducibility/reconstruction_vs_published.csv` and a readable report, in five
sections:

1. **Sample size** — observation and 1-km cell counts per model.
2. **Environmental model** — cross-fitted AUC and RMSE per component, and the
   national *Bombus* AUC gain.
3. **Local *Bombus*** — the 25-km partial turnover slopes and their corrected
   p-values, for both hurdle stages.
4. **Local discordance** — the new diagnostic and its position against the
   natural baseline.
5. **Human context** — isolate counts and their natural-null p-value, the
   population-scale and DID contrasts, and the DID-proximate fraction.

Every row carries two verdicts:

- `agreement` — whether the value is numerically close, on a scale stated per
  quantity. Informative, not decisive.
- `conclusion` — whether the claim the manuscript makes still holds: the sign of
  an effect, which side of a threshold a test lands on, whether discrimination
  stays useful. `robust` or `differs`. **This is the column that matters.**

The published reference values are committed under `inputs/published_reference/`
so they cannot be overwritten by a run. They are copies of the committed
publication outputs, all ten of which `scripts/verify_locked_artifacts.py`
verifies against the MD5s recorded in the publication lock — so the comparison
is anchored to the real publication rather than to a later rebuild.

## Relationship to PR17

PR17 preserves the historical published analysis and the reproducible pipeline
infrastructure. It deliberately does not re-baseline anything onto the
reconstruction, and its `inputs/numerical_reference.csv` and
`inputs/established_input_expectations.csv` still hold the published values.

This analysis changes none of that. It adds a second, clearly labelled mode
alongside it.
