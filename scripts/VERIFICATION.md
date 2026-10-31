# Verification record

- All 34 repository R files parsed successfully with the tree-sitter R grammar.
- 71 implementation, test, documentation, input, manual and archived-result files match the previous bundle byte for byte.
- Both bundled paper CSVs match the checksums used by the loader.
- Runner calls reviewed against the current public API signatures.
- New runners contain no superassignment or direct global-environment access.
- R/Rscript is unavailable in the authoring environment; no new runtime PASS is claimed.
- Run `verify_runners_small.R` on R to obtain the runtime result. It stops on missing dependencies, failing tests or detected skipped suites.
- Historical R CMD check results apply to the unchanged package source; the new repository runners require the new smoke check.
