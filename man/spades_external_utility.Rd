\name{spades_external_utility}
\alias{spades_external_utility}
\title{External Utility and Fixed-Seed Target Stability}
\description{Train external predictive models on real and released synthetic datasets, then evaluate all models on the same real holdout. This function is independent of synthesis and does not tune models on the holdout.}
\usage{
spades_external_utility(reference, synthetic, holdout, target,
  out_dir, predictors = NULL, mode = c("classification", "regression"),
  models = c("XT", "Bagging_CART", "XGB"), seed = 20261026L,
  threads = 1L, numeric = NULL, control = list(),
  missing_tokens = "NA", verbose = TRUE)
}
\arguments{
\item{reference}{Complete real training data.frame.}
\item{synthetic}{Complete released data.frame or named list of released data.frames, after numeric reconstruction. Dataset names must be unique and different from Real.}
\item{holdout}{Complete real test data.frame, reserved from generator and predictive model training.}
\item{target}{Response column name.}
\item{out_dir}{Dedicated output directory. Existing results are reused only when inputs and configuration match.}
\item{predictors}{Predictor names; defaults to all reference columns except the response. Exclude identifiers explicitly.}
\item{mode}{Classification or regression. Classification requires every response class in every dataset, including holdout, so macro metrics are defined. Regression requires numeric responses.}
\item{models}{Subset of XT, Bagging_CART and XGB. Requires ranger, rpart and xgboost respectively.}
\item{seed}{One nonnegative integer, reset for every model and dataset within a scope that preserves caller RNG state. Multiple model seeds are deliberately not supported.}
\item{threads}{Positive thread count for XT and XGB. CART bootstrap trees run sequentially.}
\item{numeric}{Numeric predictor names, inferred from reference column types by default. Other predictors are treated as categorical, including ordered factors.}
\item{control}{Named list overriding trees (500), min_node (5, at most 10), nrounds (300), eta (0.05), max_depth (4). All other model settings are fixed and recorded in protocol.rds.}
\item{missing_tokens}{Text labels treated as missing, in addition to actual missing values and internal missing markers. Incomplete evaluation inputs are rejected, not silently filtered.}
\item{verbose}{Print per-fit progress and compact final summary.}
}
\details{
Each training dataset has a separate encoder. Numeric predictors use training means and standard deviations; categorical predictors use training-only one-hot encoding. Previously unseen holdout categories map to all-zero indicators and their counts are recorded. Matrices are dense; high-cardinality inputs can require substantial memory.

XT uses probability forests for classification and regression forests otherwise, with extra-randomized splits, square-root feature count, no bootstrap and full sampling. Bagging CART fits bootstrap rpart trees with cp=0, minsplit=10, maxdepth=30, no cross-validation and all encoded predictors eligible. XGB uses histogram trees, subsample=0.8, colsample_bytree=0.8, min_child_weight=1, lambda=1 and alpha=0, with multiclass soft probabilities or squared-error regression. No test watchlist or early stopping is used.

The real baseline is fitted once per model and reused for comparisons with every synthetic target. Numeric reconstruction, flight and landing are never run. Target SD and ranges are descriptive and conditional on the supplied pool, holdout, model seed and synthetic datasets; reconstruction variability may contribute. Shared holdout predictions are not independent experiments. A fixed seed does not establish deterministic equivalence across hardware or library versions.

Completed evaluations are committed to individual checkpoints. An interrupted fit restarts; completed fits are reused. Configuration or input changes cause rejection of an existing output directory. Concurrent access is blocked by RUNNING.lock. After a process crash, remove a stale lock only after verifying that the run is inactive. Old standalone case-study checkpoint formats are not imported by this API. Existing standalone results remain valid and need not be rerun.

Checkpoints contain holdout responses and predictions and must be kept private. Summaries are written to metrics.csv, paired_gaps.csv and across_targets.csv. Protocol and session information are retained.
}
\value{Invisibly, a list with metrics, paired synthetic-minus-real gaps, descriptive across-target summary, preprocessing metadata and protocol. Classification metrics are accuracy, balanced accuracy, macro F1, log loss and the sum-over-classes Brier score. Regression metrics are RMSE, MAE and R squared (NA for constant holdout response). SD is NA for one synthetic target.}
\examples{
\dontrun{
# Supply independent, complete datasets and exclude identifier columns.
ans <- spades_external_utility(real_train, list(s1 = synthetic_1,
  s2 = synthetic_2), real_test, target = "response",
  out_dir = tempfile("utility_"), threads = 2L)
ans$summary
}
}
