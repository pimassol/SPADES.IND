\name{spades_prepare}
\alias{spades_prepare}
\title{Prepare a common complete-case reference}
\description{Remove records with missing values before binning or any model fitting.
Convert supported columns to the categorical representation used by the workflow.}
\usage{
spades_prepare(data, nominal = NULL, ordinal = NULL, numeric = NULL,
               n_bins = 20L, method = c("quantile", "equal_width"),
               missing_tokens = "NA", verbose = TRUE)
}
\arguments{
\item{data}{Nonempty data frame of supported raw columns.}
\item{nominal, ordinal, numeric}{Optional column names partitioning the data.
Defaults infer numeric columns and ordered factors, treating the remainder as nominal.}
\item{n_bins}{Integer at least two, requested numeric bin count.}
\item{method}{Numeric binning method.}
\item{missing_tokens}{Additional textual missing labels. Actual NA/NaN and textual
NA and \code{__MISSING__} are always removed. Matching is case-sensitive after whitespace trimming.}
\item{verbose}{Print the input, excluded and used record counts.}
}
\details{A single row mask is applied before fitting numeric bins. The cleaned
original and categorical frames are row aligned. Literal Missing is a normal
category unless declared in missing_tokens. The downstream workflow uses this
same reference and does not generate synthetic missingness.}
\value{A spades_prepared list containing data, original, retained_rows, filtering,
missing_policy, schema, numeric discretizers and controls.}
