# Classification paper: inputs and saved results

This directory indexes the completed fixed-pool, ten-target classification
study. The original input split is in `../inst/extdata/paper`: 9,000 training
rows and 1,000 holdout rows. The user manual is in `../manual`.

## Results

The 20 supplied files in `results/` are preserved byte for byte. Their original
README, source manifest, protocols and session information are retained.
`checksums_sha256.txt` records the hashes of all 20 supplied files.
Original absolute paths in `sources_manifest.csv` are historical provenance,
not paths required to read these tables from this repository.

| Analysis | Files |
|---|---|
| Approved release, categorical fidelity | fidelity_approved_final_fidelity.csv |
| Approved release, numerical fidelity | fidelity_approved_final_numeric.csv |
| Ten target runs | study_targets_summary.csv, study_stability_summary.csv |
| Fidelity components across targets | study_fidelity_components.csv, study_fidelity_component_stability.csv |
| Numerical fidelity across targets | study_numeric_fidelity.csv |
| Membership-attack stability | study_privacy_stability.csv |
| External utility: 33 evaluations | utility_metrics_33_evaluations.csv |
| Paired synthetic-minus-real gaps | utility_paired_gaps_30.csv |
| Across-target summaries | utility_across_targets.csv, utility_across_targets_paired_gaps.csv |
| Class-specific utility | utility_class_metrics.csv |
| Additional utility summaries | utility_summary_mean_sd.csv, utility_paired_gaps_summary.csv |
| Protocols and provenance | study_protocol.rds, utility_protocol.rds, utility_sessionInfo.txt, sources_manifest.csv, READ_ME.txt |

The external comparison contains ten synthetic targets and three model types,
with one fixed model seed: thirty synthetic evaluations and three real baselines.
Targets share the flight pool and the real holdout. Across-target variability
is not model-seed variability; overlapping MIA splits are not independent
replications. See the original READ_ME.txt for the experimental interpretation.

The historical result files refer to the software environment used to produce
them, including SPADES.IND 0.1.5. The current package source is version 0.1.8.
Adding results to this repository does not change or recompute them.

## Reading results

From the repository root:

```r
metrics <- read.csv("paper/results/utility_metrics_33_evaluations.csv")
targets <- read.csv("paper/results/study_targets_summary.csv")
protocol <- readRDS("paper/results/utility_protocol.rds")
```

This directory contains inputs references, aggregate results and provenance;
it does not contain historical execution scripts, fitted models or prediction
checkpoints. Portable execution runners and a small verification entry point are documented in `../scripts/WORKFLOWS.md`. They do not reconstruct missing historical fitted objects. The contents
of `paper/` are tracked in Git but excluded from the R package build. No paper
experiment is run by installation or package checks.
