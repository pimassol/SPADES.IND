# Execution scripts

All scripts in this directory are repository tools, excluded from the built R package.
`workflow_runners.R` defines functions only. Source it into a private environment.
No paper run starts when loading it. Use short output paths on Windows.

## Validate first

In a fresh R session, install this exact source if needed using
`scripts/install_and_verify_utility.R`, then source
`scripts/verify_runners_small.R`. Required dependencies must be installed; missing
ones stop verification rather than producing a successful result with skipped tests.

Verification runs the seven package suites in clean R processes, then small
runner checks for classification, dynamic regression and no response (two workers),
original-bin fidelity, completed-workflow reuse and changed-request rejection,
MIA/cache reuse, two short fixed-pool landings and three external models with one
fixed seed. Only artificial data are used. Logs and a final `VALIDATION_PASS.txt`
are written beside the repository. A runtime failure stops immediately.

## Load the runners

```r
local({
  repo <- "D:/linea MEB/DATI_SINTETICI/SPADES_GitHub_scripts/SPADES.IND"
  api <- new.env(parent=baseenv())
  sys.source(file.path(repo,"scripts","workflow_runners.R"), envir=api)
  api$require_spades()
  # Invoke an explicit operation below here.
})
```

Read the bundled split without depending on installed example data:

```r
paper <- api$load_paper_inputs(repo)
data <- paper$training
holdout <- paper$holdout
```

## Generate a new dataset

`data` is an input data frame containing training rows only. The holdout is not
an argument to generation. For classification use `target="diagnosis"` and
`mode="classification"`; numeric-coded class labels require this explicit mode.
For regression use a numeric target and `mode="regression"`; target bins default
to deciles and the accepted continuous response is retained. Without a response,
use `target=NULL, mode="none"`.

```r
w <- api$run_generation(data, out_dir="D:/SPADES_runs/new_run", n=9000L,
  target="diagnosis", mode="classification", n_pool=110000L,
  chains=6L, workers=6L, seed=4123L,
  fit_control=list(nthread=6L),
  flight_control=list(log_every=10000L),
  landing_control=list(L=1000000L,pilot=400L,log_every=10000L))
```

This is a **new fit and new experiment**, not a replay of the archived paper.
BN, XGB and RF are fitted from the supplied training data. All controls are saved.
`Missing` in the supplied paper CSVs is a literal category unless explicitly
listed in `missing_tokens`; declaring it missing changes the reference population.
Do not silently reinterpret the archived experiment.

A completed identical generation request is reused. Changed inputs, controls,
versions or a modified release CSV are rejected. An incomplete main workflow is
not automatically rerun. The saved final workflow deliberately omits the fitted
XGB pointer: it is for release audits and fixed-pool landing, not model restoration.
The existing stage API provides model-aware flight checkpoints:

```r
pool <- SPADES.IND::spades_flight(model,n_pool=110000L,chains=6L,workers=6L,
  checkpoint_dir="D:/SPADES_runs/flight_checkpoint",checkpoint_every=10000L)
pool <- api$resume_flight("D:/SPADES_runs/flight_checkpoint",workers=6L)
```

Use `model` returned by `spades_fit` for the first call. Resume uses the saved
request and does not refit. A lock must not be removed while another process is
running. Flight checkpoint reuse is distinct from completed-workflow reuse.

## Audit the final release and repeat landing only

```r
fidelity <- api$audit_fidelity(w)
privacy <- api$run_privacy(w,holdout,"D:/SPADES_runs/privacy",threads=6L)
stability <- api$run_target_stability(w,holdout,"D:/SPADES_runs/targets",threads=6L)
```

Fidelity uses categorical records and verifies that the reconstructed release
returns to the identical bins. Numeric fidelity is reported separately. Privacy
uses the reconstructed release, all member/nonmember rows for descriptive counts,
and the package's eligible balanced candidate profiles for membership attacks.
The defaults use 20 attack splits, and ten target landings on the **same pool**.
No flight stability run is performed. Dynamic regression requires repeated
`spades_land_response` calls; the fixed-score stability runner rejects that mode.

## External utility: three models, ten targets, one model seed

```r
targets <- setNames(lapply(1:10,function(i)
  readRDS(file.path("D:/SPADES_runs/targets",sprintf("target_%02d",i),"released.rds"))),
  sprintf("target_%02d",1:10))
u <- api$run_external_utility(w$prepared$original,targets,holdout,
  target="diagnosis",out_dir="D:/SPADES_runs/utility",threads=6L)
```

This performs 30 synthetic fits plus three real-reference fits on the unchanged
holdout, with one fixed model seed and fixed hyperparameters. Completed fits are
reused by the package cache. There is no seed grid. Use `mode="regression"` for
numeric-response datasets. No external utility applies when no response exists.

## Archived paper outputs

`paper/results/` contains the previously supplied results and provenance checksums.
The runners do not overwrite those files or claim that newly fitted models reproduce
the historical realization exactly. The large historical pool, fitted guide,
checkpoint objects and ten target RDS files are not included in the supplied
results ZIP. Reusing those historical objects requires their original files;
aggregate CSV results cannot reconstruct them.
