\name{spades_audits}
\alias{spades_reconstruct_bins}
\alias{spades_fidelity_audit}
\alias{spades_mia_candidates}
\alias{spades_mia_stability}
\alias{spades_privacy_audit}
\alias{spades_target_stability}
\title{Reconstruction, Fidelity, Membership Audits and Fixed-Pool Target Stability}
\description{Checks original-bin reconstruction and fidelity, evaluates fixed-release membership attacks, and repeats landing on a fixed pool. These empirical checks do not establish anonymity or differential privacy.}
\usage{
spades_reconstruct_bins(categorical, discretizers,
  integer_columns = character(), seed = 4123L)
spades_fidelity_audit(reference, categorical, released, reference_values,
  discretizers = list(), pairs = list(), conditional_on = NULL)
spades_mia_candidates(members, nonmembers, seed = 20261030L)
spades_mia_stability(audit, out_dir, seeds = 20261100L + seq_len(20L),
  threads = 6L, bootstrap = 1000L, verbose = TRUE)
spades_privacy_audit(members, nonmembers, released, out_dir,
  numeric = names(members)[vapply(members, is.numeric, logical(1))],
  candidates = NULL, seeds = 20261100L + seq_len(20L),
  threads = 6L, verbose = TRUE)
spades_target_stability(pool, reference, reference_values, nonmembers,
  discretizers, integer_columns, n, out_dir, utility = NULL,
  pairs = list(), conditional_on = NULL,
  seeds = 20262000L + seq_len(10L),
  control = list(L = 1000000L, pilot = 400L, log_every = 100000L),
  mia_seeds = 20261100L + seq_len(20L), threads = 6L,
  verbose = TRUE, pool_values = NULL)
}
\arguments{
\item{categorical}{Complete categorical target data frame.}
\item{discretizers}{Named saved discretizers with method, levels and breaks.}
\item{integer_columns}{Names of numeric variables reconstructed as integers.}
\item{seed}{Local random seed, preserving caller RNG state.}
\item{reference}{Complete categorical reference data frame.}
\item{released}{Final reconstructed synthetic data frame, including duplicate contents.}
\item{reference_values}{Original complete reference values in the same row and column order.}
\item{pairs}{List of two-variable fidelity pairs.}
\item{conditional_on}{Conditioning variable; defaults to highest cardinality.}
\item{members}{Original records used for generation.}
\item{nonmembers}{Original records excluded from generation.}
\item{audit}{Saved privacy cache with features and binary labels.}
\item{out_dir}{Explicit local output directory; use a short path on Windows. Files contain private candidate indices.}
\item{seeds}{Distinct seeds for attacks or target replications, respectively.}
\item{threads}{Threads for the membership random forests.}
\item{bootstrap}{First-split stratified AUC bootstrap repetitions.}
\item{verbose}{Print progress.}
\item{numeric}{Numeric distance columns; other columns use equality distances.}
\item{candidates}{Reusable candidate plan from spades_mia_candidates.}
\item{pool}{Fixed categorical pool; no new flight is run.}
\item{n}{Target size, at least five.}
\item{utility}{Fixed landing utility specification or NULL.}
\item{control}{Landing iteration, pilot and progress controls.}
\item{mia_seeds}{Same membership split seeds for every target.}
\item{pool_values}{For regression, fixed numeric pool used to compute utility scores. Selected values are reused, not reconstructed again.}
}
\details{All frames must have matching column order and no missing or nonfinite values. Conditional fidelity is weighted by reference stratum proportions. Numeric Wasserstein distance is normalized by reference range; KS and quantile differences are also reported. Reconstruction must reproduce every original bin.

Membership attacks operate on balanced unique record profiles. Profiles shared between members and nonmembers are excluded and counted. At least 200 eligible profiles per class are required. Distances use all released rows. Numeric differences are divided by reference range and clipped at one; Hamming uses exact equality. Attacks use separate 40/30/30 percent training, calibration and test partitions, fixed score directions, and calibration-only thresholds and selection. The RF uses 500 trees and minimum node size ten. Controls include permuted training labels and a hypothetical exact-copy release. Only the first split has bootstrap intervals; overlapping split variability is descriptive. Candidate-source differences may confound membership results.

The target study preserves the fixed pool, fidelity structure and utility guide. Completed targets, features and attack splits are cached with input fingerprints. An interrupted unfinished landing restarts that target. A lock prevents concurrent target studies. After a crash a stale lock must be removed only after confirming that no process remains. Regression requires the exact fixed numeric pool underlying utility scores. No external predictive utility models are fitted by these functions.}
\value{Reconstruction returns a data frame. Fidelity returns component, summary and numeric tables. Candidate selection returns row indices and labels. Privacy and stability return metric lists and write explicit audit/checkpoint files. The target study invisibly returns per-target and aggregate summaries.}
