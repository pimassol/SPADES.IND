SPADES RESULTS: corrected numeric reconstruction; fixed flight pool; 10 landing targets; one fixed model seed.
Categorical fidelity: mean TVD over predefined marginal, pair and conditional terms. Numeric fidelity: original intervals preserved.
External utility: real-trained and synthetic-trained XT, bagging CART and XGB evaluated on the SAME 1000 real holdout rows.
Hyperparameters fixed in utility_protocol.rds; encoders fitted separately on each training dataset. No test tuning or early stopping.
Model seeds paired between real and each synthetic target; each real fit reused across targets.
Across-target SD is computed from ten target results at ONE fixed model seed; no model-seed variability is estimated.
Targets share reference, pool and holdout; repeated MIA splits overlap. SD/ranges are descriptive, not independent confidence intervals.
The holdout was inspected previously; results are exploratory. No target selection by test performance.
MIA results assess listed attacks, not formal privacy. Guide accuracy is distinct from external utility.
Prediction checkpoints and private MIA data remain in their original directories; this folder contains aggregate paper tables and provenance.
Current source package: SPADES.IND 0.1.5. Package validation logs should accompany any later software-release claim.
