\name{spades_synthesise}
\alias{spades_synthesise}
\title{Generate Synthetic Data With an Optional Response}
\description{Prepare a complete-case reference, fit generation models, generate a categorical pool, select a target by simulated annealing, and reconstruct numerical values.}
\usage{
spades_synthesise(data, n, target = NULL,
  mode = c("auto", "classification", "regression", "none"),
  n_pool = max(1000L, 12L * n), n_bins = 20L,
  chains = 6L, workers = 6L, seed = 4123L,
  pairs = NULL, conditional_on = NULL,
  fit_control = list(), flight_control = list(), landing_control = list(),
  cache_dir = NULL, verbose = TRUE, missing_tokens = "NA",
  target_bins = 10L, regression_metric = c("RMSE", "MAE"))
}
\arguments{
\item{data}{Training data frame only. Do not include the external holdout. Supported columns and complete-case filtering are described in \code{\link{spades_prepare}}.}
\item{n}{Positive integer number of synthetic rows, no larger than the pool.}
\item{target}{Name of the response variable, or \code{NULL} when there is no response.}
\item{mode}{With \code{"auto"}, a numeric response selects regression, any other response selects classification, and a missing target selects \code{"none"}. Specify classification explicitly for numerical class codes.}
\item{n_pool}{Requested number of pool rows. Duplicate row contents are retained.}
\item{n_bins}{Requested number of bins for numerical variables other than the regression response.}
\item{chains}{Number of logical flight chains.}
\item{workers}{Flight worker count; also used for structure and utility random-forest threads.}
\item{seed}{Nonnegative integer master seed, with room for workflow offsets. Caller RNG state is preserved.}
\item{pairs}{List of two-column character vectors defining bivariate fidelity components. \code{NULL} requests automatic RF-based selection; \code{list()} disables these components.}
\item{conditional_on}{Single conditioning column. If \code{NULL}, use the highest-cardinality prepared column; column order resolves ties.}
\item{fit_control}{Named model-fitting overrides: \code{n_fake}, \code{nrounds}, \code{bn_restart}, \code{bn_blacklist}, and \code{nthread}.}
\item{flight_control}{Named flight controls accepted by \code{spades_flight}, including \code{burnin}, \code{thin}, and \code{log_every}. See the workflow vignette and flight documentation.}
\item{landing_control}{Named overrides: \code{L} (default 1000000), \code{pilot} (400), and \code{log_every} (10000). Each must be a positive integer.}
\item{cache_dir}{Optional flight cache directory. For explicit interruption and checkpoint control use the staged flight API; see \code{\link{spades_resume_flight}}. The main function fits generation models before calling flight.}
\item{verbose}{Print progress.}
\item{missing_tokens}{Text labels treated as missing in addition to actual missing values and internal markers. Affected rows are removed before any fitting.}
\item{target_bins}{Requested quantile bins for a regression response (default ten). Coincident boundaries are merged.}
\item{regression_metric}{Regression guide error, RMSE or MAE.}
}
\details{
All modes use BN proposals with plausibility, distance-based privacy and diversity penalties in flight. Exact categorical reference matches are rejected; duplicate synthetic contents are retained. Diversity is evaluated against a chain-local archive, which makes flight adaptive.

Landing fidelity is the arithmetic mean of individual marginal, selected-pair and conditional TVD components. Conditional TVD is weighted by reference stratum proportions; an absent target stratum has discrepancy one. This is neither a maximum nor an equal-weight mean of three component families. Landing does not add a diversity penalty.

Classification adds the absolute accuracy gap from a fixed RF guide. Regression adds the absolute RMSE or MAE gap divided by the reference response standard deviation. Guide reference performance is measured on its training rows and is not external predictive validation. With no response, utility is zero.

For regression the pool and fidelity remain categorical. A guide uses categorical predictors and real numerical responses. Each proposal samples a numerical response uniformly inside its proposed bin (uniform over admissible integers for integer-valued responses). Acceptance retains that value; rejection retains the previous state. Final response values are not sampled again. Other numerical columns are reconstructed after landing, respecting their original bins. See \code{\link{spades_land_response}}.

Selection is without replacement in pool indices, not in distinct row contents. The returned target is the final SA state. Neither globally optimal fidelity nor a formal privacy guarantee is asserted.
}
\value{A list containing \code{data} (released values), \code{categorical} (selected categorical records), \code{landing}, \code{pool}, \code{prepared}, \code{model}, \code{mode}, \code{seed}, \code{filtering}, and \code{response_protocol}.}
\seealso{\code{\link{spades_prepare}}, \code{\link{spades_land_response}}, \code{\link{spades_fidelity_audit}}, \code{\link{spades_privacy_audit}}, \code{\link{spades_external_utility}}.}
\examples{
# See vignette("workflow", package = "SPADES.IND") for three small examples.
# Explicitly choose classification when numeric values encode class labels:
# result <- spades_synthesise(training, n = 1000, target = "class",
#                             mode = "classification")
}
