# Trying SPADES.IND and reporting issues

The repository supports collaboration on classification, regression and
fidelity-only synthetic-data generation.

## Useful bug reports

Use the private repository issue tracker. Include:

- package version and `sessionInfo()`;
- operating system and R version;
- the exact function call, seed, controls, worker and chain counts;
- the complete error message and the last relevant progress lines;
- a small reproducible artificial example when possible;
- whether the run was new, resumed or reusing cached results.

Do not upload confidential data, model caches or record-level membership links.
Describe expected and observed behavior separately. Report numerical-bin
crossings, missing values in output or non-reproducible checkpoint continuation
with the saved schema and a minimal artificial example.

## Package checks

Run `source("scripts/check_package.R")` from the extracted repository.
The script builds the English vignettes and runs package checks and small tests.
It does not run the paper study. knitr, rmarkdown and Pandoc are required for
vignette building. The PDF manual requires a TeX installation and is optional:
set `options(SPADES.check_manual=TRUE)` in the calling session to request it.

Keep generated outputs outside the source tree. Changes to sampling, energy
terms or reconstruction require explicit numerical verification. A passing
software test is not a statistical privacy or utility guarantee.
