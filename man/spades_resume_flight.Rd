\name{spades_resume_flight}
\alias{spades_resume_flight}
\title{Resume or reuse a saved flight}
\description{Continue the original flight from per-chain checkpoints without model
refitting. Completed flights are loaded directly.}
\usage{
spades_resume_flight(checkpoint_dir, workers = 6L, verbose = TRUE,
                     checkpoint_every = 10000L, run_for = NULL)
}
\arguments{
\item{checkpoint_dir}{Directory created by spades_flight with checkpoint_dir set.}
\item{workers}{Number of execution workers. Logical chain count is unchanged.}
\item{verbose}{Print console progress.}
\item{checkpoint_every}{Checkpoint interval in MH transitions per chain.}
\item{run_for}{Optional positive limit on additional transitions per chain, for a controlled pause.}
}
\details{The original model, request and RNG states are reused. The R and dependency
versions must match. An abrupt stop loses only work after the last committed
checkpoint. A stale RUNNING.lock must only be removed after confirming that the
previous run and its workers have stopped. See RESUME.md in the source package.}
\value{A spades_flight object, or spades_flight_paused if run_for stops before completion.}
