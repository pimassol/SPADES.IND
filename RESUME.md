# Flight reuse and interruption recovery (0.1.4)

The algorithm is unchanged: BN proposal, plausibility/privacy/diversity energy,
chain-local archive, repeated states retained. Landing is unchanged.

## Start, save, resume

Use a new directory for each planned flight. `model` is a fitted object returned
by spades_fit; train it once on the user's training data. No case-study names,
paths or saved historical models are required by these APIs.

```r
library(SPADES.IND)
pool <- spades_flight(
  model, n_pool = 110000L, chains = 6L, workers = 6L, seed = 123L,
  checkpoint_dir = "path/to/flight_run_01",
  checkpoint_every = 10000L,
  control = list(log_every = 10000L)
)
```

After an interruption, including a new R session, use only:

```r
library(SPADES.IND)
pool <- spades_resume_flight("path/to/flight_run_01", workers = 6L)
```

The original fitted model is loaded from request.rds; no call to spades_fit or
spades_synthesise is required. A completed run returns its saved pool immediately.
Checkpoint interval and worker count may change; logical chains, seeds, model,
requested total size, energy controls and software environment must stay fixed.
This is continuation to the original total, not extension to a larger pool.
Existing 0.1.0 runs cannot be resumed retroactively without these checkpoints.

Checkpoint_every counts MH iterations PER CHAIN, including burn-in. Each chain
saves at initialization, every interval, and completion. At an abrupt interruption,
at most the iterations since its last checkpoint must be replayed. This does not
repeat training or the completed portion. Archive, current state, energy scores,
static-score cache, counters and RNG are saved together. Initialization before
its first successful checkpoint may need to be repeated.

Two committed generations are retained per chain. A temporary interrupted write
is ignored. A corrupt latest generation is quarantined and the previous one is
used. If neither is valid, execution stops rather than silently restarting.
Checkpoint writes use immutable filenames and rename on the same filesystem;
this is not a guarantee against disk failure or loss of unflushed OS buffers.
Use a local persistent directory, with enough space for model and chain archives.

A RUNNING.lock directory prevents two callers writing the same run. Normal exit
or an R error releases it after stopping workers. After a process/PC crash it
may remain. FIRST stop any surviving workers of that run, THEN remove only:

```r
unlink("path/to/flight_run_01/RUNNING.lock", recursive = TRUE)
```

Do not remove a live run's lock. Checkpoint files and request.rds must be retained.

## Controlled pause

```r
paused <- spades_flight(model, 110000L, chains=6L, workers=6L,
  checkpoint_dir="path/to/flight_run_01", run_for=5000L)
# Continue later with spades_resume_flight(...).
```

run_for is the maximum additional transitions per chain in this invocation.
A paused call returns spades_flight_paused, not a pool suitable for landing.
Different chains may have reached different checkpoints after an abrupt stop.
Each resumes from its own saved state; completed chains require no transitions.

## Scope

The one-call spades_synthesise workflow still fits fresh models when invoked.
For long restartable work, use the staged prepare/structure/fit/flight/land API
and resume the flight with spades_resume_flight. Save RF structure and utility
objects with saveRDS if needed for later stages. No new fitting is triggered by
spades_resume_flight. This release does not implement landing checkpoints.

Exact continuation requires the same R, SPADES.IND, XGB, BN and Matrix versions
and numerical platform. RNG kind is restored from the request. Native XGB model
bytes are stored, avoiding serialization of external pointers.

Checkpoints are version-locked. Finish an in-progress 0.1.1 flight under 0.1.1;
0.1.2 does not silently reinterpret or migrate that saved request. Existing
completed case-study outputs are not modified by installing this update.

Version locks also apply to 0.1.3; old checkpoint files are not migrated.

Version 0.1.4 also records withr/rngtools versions in checkpoint requests.
