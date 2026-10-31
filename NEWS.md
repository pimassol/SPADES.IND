# SPADES.IND 0.1.8

- Experimental response landing jointly selects categorical pool rows and numeric responses drawn within bins per proposal.
- RMSE or MAE guide gap, normalized by training response SD; guide uses categorical predictors and real numeric response.
- Accepted responses retained; other numerical variables reconstructed after landing.
- Main workflow supports classification, regression and no response. Quantile deciles default for continuous response.
- Existing categorical SA kernel and flight unchanged. Main-workflow reconstruction timing is now explicitly post-landing.
- Dynamic proposal has a uniform-within-bin base measure; no assertion of uniform Lebesgue-space target or incremental privacy guarantee.

# SPADES.IND 0.1.7

- Main workflow explicitly supports categorical response, continuous response, or no response.
- Continuous response defaults to training quantile deciles; repeated boundaries are merged, with actual bins and counts retained.
- Regression guide uses fixed reconstructed numeric predictors and response. Pool numeric rows are retained in output.
- Numeric-coded categorical responses are not binned when classification is requested.
- Regression remains experimental. No new differential-privacy claim or resampling inside SA.

# SPADES.IND 0.1.6

- Add spades_external_utility for released-data classification or regression.
- One fixed seed across targets; XT, bootstrap CART and XGB; no automatic tuning.
- Per-fit checkpoints, strict input/configuration fingerprints, and descriptive target variability.
- Numeric and categorical predictors are user-configurable; training-only encoders.
- No changes to flight, landing, reconstruction or privacy algorithms.
- Correct development check script to target 0.1.6.

# SPADES.IND 0.1.4
- parallel declared only in Imports; unused C++ local variable removed.
- Scoped withr seed guards replace handwritten global RNG assignments.
- rngtools handles exact checkpoint RNG state inside the scoped guard.
- Fidelity accumulator uses an explicit private environment instead of <<-.
- Legacy R seed setters also use scoped guards; no direct GlobalEnv access in R/.
- Runtime tests cover caller state restoration on success and error.
- New imports: withr >= 2.5.0 and rngtools; checkpoint requests include their versions.

# SPADES.IND 0.1.3
- Fix raw data-frame utility calls: absent internal missing-policy metadata uses
  mandatory NA/__MISSING__ markers, rather than failing on NULL.
- Retain strict validation for user-supplied missing_tokens.
- Regression tests cover raw classification/regression and missing pool rejection.
- No changes to fitted-model algorithms, energies, SA or checkpoint state logic.

# SPADES.IND 0.1.2
- Complete-case row filtering at preparation, shared by all fitting and F/U references.
- Actual NA/NaN and text NA/__MISSING__ excluded; configurable additional labels.
- Literal Missing no longer silently removed inside BN fitting.
- Cleaned numeric response remains aligned in full regression workflow.
- Downstream incomplete references/pools rejected instead of filtered independently.
- Flight transition/energy rules, checkpoint mechanism and landing C++ unchanged.

# SPADES.IND 0.1.1
- Optional per-chain flight checkpoints with saved RNG, archive and static cache.
- spades_resume_flight loads the original fitted model; no training on resume.
- Immutable checked generations, previous-generation recovery and run lock.
- Controlled pauses via run_for; worker count may change on resume.
- Existing flight transition rules and landing code unchanged.

# SPADES.IND 0.1.0
- Port approved BN-cancelled plausibility/privacy/diversity flight, retain duplicates.
- Independent chain archives and PSOCK scheduling; bounded static-score cache.
- Port C++ single-index SA with mean marginal/pair/conditional fidelity.
- Optional fixed-guide accuracy or normalized RMSE utility; fidelity-only mode.
- Materialize numeric pool values once and export exactly selected rows.
- Explicit controls, compact console logs, standalone workflow and validation scripts.
- Keep original SPADES directory and case-study outputs untouched.
