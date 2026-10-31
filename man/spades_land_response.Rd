\name{spades_land_response}
\alias{spades_land_response}
\title{Landing with a Continuous Response Drawn per Proposal}
\description{Select categorical pool rows while sampling a continuous response within its categorical interval at each proposal. The response is retained on acceptance.}
\usage{
spades_land_response(pool, prepared, n, target,
  metric = c("RMSE", "MAE"), seed = 1122L, rf_seed = 1011L,
  threads = 1L, control = list(), pairs = list(),
  conditional_on = NULL, keep_trace = TRUE, verbose = TRUE,
  initial_indices = NULL, initial_y = NULL)
}
\arguments{
\item{pool}{Categorical data.frame or spades_flight object.}
\item{prepared}{Prepared real training data with a numeric target and quantile discretizer. The main workflow defaults to ten response quantile bins; ties merge boundaries.}
\item{n}{Number of records selected without replacement by pool index. Duplicate row contents are retained.}
\item{target}{Numeric response column name.}
\item{metric}{RMSE or MAE, computed with a fixed regression forest guide.}
\item{seed}{Landing seed, allowing two subsequent seed offsets.}
\item{rf_seed}{Regression forest seed.}
\item{threads}{Guide training and prediction threads. SA is sequential.}
\item{control}{L (1000000), pilot (400), log_every (10000).}
\item{pairs}{Variable pairs for fidelity.}
\item{conditional_on}{Fidelity conditioning column, or NULL for largest reference cardinality.}
\item{keep_trace}{Retain progress trace.}
\item{verbose}{Print progress.}
\item{initial_indices}{Optional distinct pool row indices.}
\item{initial_y}{Optional numerical responses matching those rows' bins. Otherwise sampled uniformly inside the bins.}
}
\details{
The guide is trained once on categorical predictors and the original real numeric response. It is never refitted during SA. Pool predictions are cached in memory. Fidelity uses categorical marginal, selected pairwise and conditional terms, exactly as categorical landing. Utility is the absolute difference between selected-target and real-reference guide error divided by real response SD. The real reference error is evaluated on guide training rows and is not external predictive validation.

A proposal selects one target position, one currently unselected pool index and an independent uniform response in that index's bin. Integer-valued input responses use admissible integers uniformly. If all pool rows are selected, a proposal refreshes only the selected position's response. Rejection restores the complete old categorical/numeric state. The pilot evaluates and rejects proposals without modifying initial state. Only a single absolute or squared error contribution changes per step. Other numerical variables are not reconstructed here.

At temperature T, the target is proportional to exp(-H/T) relative to uniform pool-index selection and the product of within-bin uniform response laws. The proposal ratio cancels this base-measure ratio. Consequently acceptance uses exp(-delta H/T), without an interval-width correction. This is not a claim that the proposal is symmetric with respect to Lebesgue measure when bin widths differ. An energy density defined relative to Lebesgue measure instead would require a different Hastings ratio.

The final response values must be used directly, not reconstructed again. The main spades_synthesise workflow does this and reconstructs other numeric columns after selection. Arbitrary within-bin response values can adapt to the guide without preserving within-bin conditional structure: external evaluation measures predictive performance on released numerical data. Uniform random reconstruction is not a privacy guarantee. Interrupted-SA checkpoint resume is not supported by this kernel.
}
\value{A spades_response_landing object inheriting spades_landing, with categorical data, indices, response_values, initial_response_values, response_errors, fidelity components, energy summary, guide metadata and optional trace.}
