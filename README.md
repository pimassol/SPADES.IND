# SPADES.IND

Energy-driven generation of privacy-aware synthetic microdata. Version 0.1.8.

## Three response scenarios

- `target="diagnosis", mode="classification"`: categorical fidelity and an
  absolute accuracy gap from a fixed RF guide.
- `target="income", mode="regression", target_bins=10L,
  regression_metric="RMSE"`: categorical fidelity and a normalized RMSE gap.
  MAE is also supported. The numerical response is sampled within its bin at
  each landing proposal; accepted values are retained in the release.
- `target=NULL, mode="none"`: fidelity-only landing, without a response guide.

All scenarios use BN proposals with plausibility, privacy and diversity terms
in flight. Numerical predictors are reconstructed after landing within their
original intervals. Duplicate synthetic contents are retained.

## Workflow and evaluation

`spades_synthesise()` runs the complete workflow. Staged interfaces provide
preparation, model fitting, flight with checkpoint recovery, landing and
reconstruction. Supply training data only; reserve a real holdout for evaluation.

`spades_fidelity_audit()` assesses categorical and numerical fidelity.
`spades_privacy_audit()` and `spades_mia_stability()` evaluate membership attacks.
`spades_target_stability()` repeats target selection on a fixed pool.
`spades_external_utility()` compares XT, bagged CART and XGB against real-data
baselines using a fixed model seed and a shared real holdout.

## User guides

- `vignette("workflow", package="SPADES.IND")`: the three generation scenarios.
- `vignette("evaluation", package="SPADES.IND")`: fidelity, privacy, stability
  and external utility.
- `vignette("paper-case-study", package="SPADES.IND")`: classification study protocol.

Function arguments are documented in the installed help pages. The script
`examples/three_scenarios.R` demonstrates all three scenarios with artificial data.

## Checks and collaboration

`source("scripts/check_package.R")` builds the package and vignettes and runs
the small test suites. knitr, rmarkdown and Pandoc are required. PDF manual
checking additionally requires TeX and `options(SPADES.check_manual=TRUE)`.

See CONTRIBUTING.md for issue reports. Keep generated outputs outside the
source checkout. Historical research-module accessors are retained for helper
dependencies and compatibility; the supported workflow is described above.

## Paper input data

The supplied 9,000-row training file and 1,000-row holdout are installed under
`extdata/paper`, with original SHA-256 checksums and a variable inventory.
`examples/load_paper_data.R` verifies and reads them without fitting models.
See the accompanying data README for categorical codes and literal Missing labels.

## PDF manual

The detailed English user and reference manual is in
`manual/SPADES_IND_User_Manual.pdf`. This repository PDF complements the
installed help and vignettes; it is excluded from the R source-package build.

## Saved paper results

[Paper results index](paper/README.md) links the original input split and the
completed fidelity, membership privacy, ten-target stability and external
utility results. These files are retained unchanged and excluded from the R build.

## Repository execution scripts

See [scripts/WORKFLOWS.md](scripts/WORKFLOWS.md) for explicit generation, audit, fixed-pool stability and fixed-seed utility runners. Start with `scripts/verify_runners_small.R`; loading the runners does not execute the paper experiments.
