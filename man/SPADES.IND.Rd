\name{SPADES.IND}
\alias{SPADES.IND-package}
\alias{spades_fit}
\alias{spades_flight}
\alias{spades_flight_auto}
\alias{spades_structure_auto}
\alias{spades_land}
\alias{spades_land_rf}
\alias{spades_reconstruct}
\alias{spades_utility}
\alias{spades_modules}
\alias{spades_functions}
\alias{spades_function_index}
\alias{spades_check_dependencies}
\title{Energy-driven synthetic individual data}
\description{Preparation, BN and XGB fitting, adaptive flight, incremental landing,
optional fixed-guide classification or regression utility, and numeric reconstruction.}
\details{Use \code{target=NULL, mode="none"} for fidelity-only selection. Categorical targets use \code{mode="classification"}. Continuous targets use \code{mode="regression"}, \code{target_bins=10L} and \code{regression_metric="RMSE"} or \code{"MAE"}. Continuous responses are sampled inside their bins at every landing proposal; accepted values are retained. Other numeric columns are reconstructed after landing. Fidelity and regression-guide predictors remain categorical. See \code{\link{spades_land_response}}.
Use \code{spades_synthesise} for the complete workflow. Supply training data
only. Preparation excludes records with actual NA or textual missing markers before
all fitting. Extra markers are configured with \code{missing_tokens}; the same
complete reference is used throughout. The response is optional. For numeric responses use regression; convert
numeric class labels to factors for classification. Flight retains duplicate
synthetic records and rejects exact categorical matches to the reference.
The flight BN term cancels the independence Hastings correction. Dynamic diversity
uses a chain-local archive. Landing minimizes mean marginal, selected-pair and
conditional total variation distance plus optional absolute accuracy gap or
normalized RMSE gap. No diversity penalty is added in landing.
See \code{\link{spades_synthesise}} and
\code{vignette("workflow", package="SPADES.IND")} for the supported workflow and limitations. The extracted legacy research modules are separate from this workflow.}
\value{Preparation, fitting, flight and landing return structured lists. The complete
workflow returns a list with data, categorical, landing, pool, prepared and model.
Reconstruction returns a data frame. Utility returns scores, reference, scale and model.}
\author{Pierpaolo Massoli}
