# Source into a private environment. Loading this file performs no computation.
require_spades <- function() {
  deps <- c("SPADES.IND", "Rcpp", "withr", "rngtools", "bnlearn", "ranger", "rpart", "xgboost")
  missing <- deps[!vapply(deps, requireNamespace, logical(1), quietly=TRUE)]
  if(length(missing)) stop("Missing dependencies: ", paste(missing, collapse=", "))
  if(as.character(utils::packageVersion("SPADES.IND")) != "0.1.8")
    stop("These runners require SPADES.IND 0.1.8.")
  invisible(TRUE)
}

# A completed workflow is reused only for an identical request and environment.
# Incomplete main workflows are never silently restarted.
run_generation <- function(data, out_dir, n, target=NULL, mode="none", ...) {
  require_spades()
  dots <- list(...)
  if(length(dots) && (is.null(names(dots)) || any(!nzchar(names(dots))) || anyDuplicated(names(dots))))
    stop("Additional arguments must have unique names.")
  allowed <- setdiff(names(formals(SPADES.IND::spades_synthesise)), c("data","n","target","mode"))
  if(any(!names(dots) %in% allowed)) stop("Unknown generation argument.")
  request <- list(data=data,n=n,target=target,mode=mode,controls=dots,
    versions=vapply(c("SPADES.IND","bnlearn","ranger","xgboost"),
      function(x) as.character(utils::packageVersion(x)), character(1)), R=R.version.string)
  result_file <- file.path(out_dir,"workflow.rds")
  request_file <- file.path(out_dir,"request.rds")
  lock <- paste0(out_dir,".lock")
  dir.create(dirname(out_dir), recursive=TRUE, showWarnings=FALSE)
  if(!dir.create(lock, showWarnings=FALSE)) stop("Workflow locked: ",lock)
  on.exit(unlink(lock,recursive=TRUE),add=TRUE)
  if(dir.exists(out_dir)) {
    if(!file.exists(request_file) || !identical(readRDS(request_file),request))
      stop("Existing output does not match this request; use a new output directory.")
    if(!file.exists(result_file)) stop("Incomplete workflow: use the stage API and an existing flight checkpoint when available; no automatic restart.")
    ans <- readRDS(result_file)
    csv <- file.path(out_dir,"synthetic_data.csv")
    if(!file.exists(csv) || !identical(unname(tools::md5sum(csv)),ans$release_md5))
      stop("Release CSV missing or modified.")
    cat("REUSE | completed workflow; no training or sampling.\n")
    return(ans)
  }
  if(!dir.create(out_dir,recursive=TRUE)) stop("Cannot create output directory.")
  saveRDS(request,request_file)
  ans <- do.call(SPADES.IND::spades_synthesise,c(list(data=data,n=n,target=target,mode=mode),dots))
  # The fitted XGB external pointer is not serialized as a reusable model.
  ans$model <- NULL
  audit <- audit_fidelity(ans)
  saveRDS(audit,file.path(out_dir,"fidelity.rds"))
  utils::write.csv(audit$components,file.path(out_dir,"fidelity_components.csv"),row.names=FALSE)
  utils::write.csv(audit$summary,file.path(out_dir,"fidelity_summary.csv"),row.names=FALSE)
  utils::write.csv(audit$numeric,file.path(out_dir,"numeric_fidelity.csv"),row.names=FALSE)
  csv <- file.path(out_dir,"synthetic_data.csv")
  utils::write.csv(ans$data,csv,row.names=FALSE)
  ans$release_md5 <- unname(tools::md5sum(csv))
  tmp <- paste0(result_file,".tmp")
  saveRDS(ans,tmp)
  if(!file.rename(tmp,result_file)) stop("Cannot commit workflow result.")
  writeLines(utils::capture.output(utils::sessionInfo()),file.path(out_dir,"sessionInfo.txt"))
  ans
}

audit_fidelity <- function(workflow) {
  p <- workflow$prepared; l <- workflow$landing
  SPADES.IND::spades_fidelity_audit(p$data,workflow$categorical,workflow$data,p$original,
    discretizers=p$numeric_discretizers,pairs=l$pairs,conditional_on=l$conditional_on)
}

run_privacy <- function(workflow, holdout, out_dir, threads=6L,
                        seeds=20261100L+seq_len(20L)) {
  require_spades()
  # Uses reconstructed release, not categorical pool; holdout is never fitted.
  audit_fidelity(workflow)
  SPADES.IND::spades_privacy_audit(workflow$prepared$original,holdout,workflow$data,
    out_dir=out_dir,numeric=workflow$prepared$schema$numeric,seeds=seeds,threads=threads)
}

run_target_stability <- function(workflow, holdout, out_dir, n=nrow(workflow$data),
    seeds=20262000L+seq_len(10L),threads=6L,
    control=list(L=1000000L,pilot=400L,log_every=10000L),
    mia_seeds=20261100L+seq_len(20L)) {
  require_spades()
  if(!workflow$mode %in% c("classification","none"))
    stop("Use repeated spades_land_response calls for dynamic regression, preserving each accepted response.")
  p <- workflow$prepared; l <- workflow$landing
  pool <- if(inherits(workflow$pool,"spades_flight")) workflow$pool$data else workflow$pool
  SPADES.IND::spades_target_stability(pool,p$data,p$original,holdout,
    p$numeric_discretizers,p$integer_columns,n,out_dir,utility=l$utility,
    pairs=l$pairs,conditional_on=l$conditional_on,seeds=seeds,control=control,
    mia_seeds=mia_seeds,threads=threads)
}

run_external_utility <- function(reference, targets, holdout, target, out_dir,
    mode="classification", seed=20261026L, threads=6L, control=list()) {
  require_spades()
  SPADES.IND::spades_external_utility(reference,targets,holdout,target,out_dir,
    mode=mode,models=c("XT","Bagging_CART","XGB"),seed=seed,threads=threads,control=control)
}

resume_flight <- function(checkpoint_dir, workers=6L, checkpoint_every=10000L) {
  require_spades()
  SPADES.IND::spades_resume_flight(checkpoint_dir,workers=workers,
    checkpoint_every=checkpoint_every,verbose=TRUE)
}

load_paper_inputs <- function(repo) {
  paths <- file.path(repo,"inst","extdata","paper",c("training_data.csv","test_real_1000.csv"))
  expected <- c("07131c4af34dd46d4e45b91139e8e18d","02d72a44c49529b065f82d824e9223b7")
  if(!all(file.exists(paths)) || !identical(unname(tools::md5sum(paths)),expected))
    stop("Paper input files missing or checksum mismatch.")
  read_one <- function(path) {
    d <- utils::read.csv(path,colClasses="character",na.strings=NULL,check.names=FALSE)
    d$age <- as.integer(d$age)
    if(anyNA(d$age)) stop("Invalid age values.")
    d
  }
  training <- read_one(paths[1]);holdout <- read_one(paths[2])
  stopifnot(nrow(training)==9000L,nrow(holdout)==1000L,ncol(training)==8L,
    identical(names(training),names(holdout)))
  list(training=training,holdout=holdout,target="diagnosis",mode="classification")
}
