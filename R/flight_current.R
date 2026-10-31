# Approved BN-tilted flight. Diversity archive belongs to a chain, not a worker.
.spades_controls <- function(defaults, control) {
  if (!is.list(control) || (length(control) && (is.null(names(control)) ||
      anyNA(names(control)) || anyDuplicated(names(control)) ||
      any(!names(control) %in% names(defaults)) || any(vapply(control,is.null,logical(1))))))
    stop('Unknown, duplicated or NULL control.')
  utils::modifyList(defaults, control)
}
.spades_restore_xgb <- function(raw) {
  p <- tempfile(fileext='.model'); on.exit(unlink(p))
  writeBin(raw$bytes,p); fit <- xgboost::xgb.load(p)
  for(nm in names(raw$prediction_fields)) fit[[nm]] <- raw$prediction_fields[[nm]]
  fit
}
.spades_distance <- function(code, archive) {
  if(!nrow(archive)) return(Inf)
  z <- integer(nrow(archive))
  for(j in seq_along(code)) z <- z + (archive[,j] != code[j])
  min(z)/length(code)
}
.spades_current_chain <- function(job, model, cfg, verbose) {
 .spades_with_seed(job$seed, {
  f <- .spades_flight_functions(); fit <- .spades_restore_xgb(model$xgb_raw)
  real <- model$prepared$data; cols <- names(real); lev <- lapply(real,levels)
  rc <- do.call(cbind,lapply(real,as.integer)); k <- ncol(rc)
  realkeys <- new.env(hash=TRUE,parent=emptyenv())
  for(key in apply(rc,1,paste,collapse=':')) assign(key,TRUE,realkeys)
  cache <- new.env(hash=TRUE,parent=emptyenv())
  candidate <- function() {
    rec <- bnlearn::rbn(model$fit_bn,n=1)[,cols,drop=FALSE]
    rec <- f$cast_record_to_schema(rec,model$factor_schema)
    code <- vapply(cols,function(v) match(as.character(rec[[v]]),lev[[v]]),integer(1))
    if(anyNA(code)) stop('BN generated a value outside the schema.')
    key <- paste(code,collapse=':')
    if(exists(key,realkeys,inherits=FALSE)) return(NULL)
    if(exists(key,cache,inherits=FALSE)) return(get(key,cache,inherits=FALSE))
    mm <- f$build_candidate_sparse_row(rec_df=rec,factor_schema=model$factor_schema,
      municipality_col=character(),nominal_cols=model$prepared$schema$nominal,
      ordinal_cols=model$prepared$schema$ordinal,numeric_cols=model$prepared$schema$numeric,
      ordinal_numeric_maps=model$ordinal_numeric_maps,design_colnames=model$design_colnames)
    p <- as.numeric(predict(fit,xgboost::xgb.DMatrix(mm)))
    if(length(p)!=1L || !is.finite(p) || p<0 || p>1) stop('Invalid XGB score.')
    p <- f$clamp(p); d <- .spades_distance(code,rc)
    ans <- list(code=code,p=p,d=d,H=cfg$lambda_plausibility*(1-p)+cfg$lambda_privacy*exp(-d/cfg$tau_privacy))
    if(length(cache)>=cfg$cache_size) rm(list=ls(cache,all.names=TRUE),envir=cache)
    assign(key,ans,cache);ans
  }
  limit <- cfg$burnin+cfg$thin*job$n
  state <- if(!is.null(job$checkpoint)) .spades_checkpoint_read(job$checkpoint,job$key) else NULL
  if(is.null(state)) {
    current <- NULL
    for(i in seq_len(10000L)) {current <- candidate();if(!is.null(current)) break}
    if(is.null(current)) stop('No admissible initial state found in 10000 proposals.')
    archive <- matrix(0L,job$n,k); scores <- matrix(0,job$n,4,
      dimnames=list(NULL,c('p_real','distance_real','static_energy','diversity_at_collection')))
    collected <- 0L; accepted <- 0L; invalid <- 0L; cd <- 0; completed <- 0L
  } else {
    current<-state$current;archive<-state$archive;scores<-state$scores
    collected<-state$collected;accepted<-state$accepted;invalid<-state$invalid
    cd<-state$cd;completed<-state$iteration
    if(completed>limit || nrow(archive)!=job$n) stop('Checkpoint size mismatch.')
    list2env(state$cache,envir=cache)
    rngtools::RNGseed(state$rng)
    if(verbose) {cat(sprintf('Chain %d | resumed at iteration %d/%d | records %d/%d\n',
      job$id,completed,limit,collected,job$n));flush.console()}
  }
  checkpoint <- function(iteration) {
    if(!is.null(job$checkpoint)) .spades_checkpoint_write(job$checkpoint,job$key,list(
      iteration=iteration,current=current,archive=archive,scores=scores,collected=collected,
      accepted=accepted,invalid=invalid,cd=cd,cache=as.list(cache,all.names=TRUE),
      rng=rngtools::RNGseed()))
  }
  if(is.null(state)) checkpoint(0L)
  end <- if(is.null(job$run_for))limit else min(limit,completed+as.double(job$run_for))
  if(completed<end) for(iter in seq.int(completed+1L,end)) {
    proposal <- candidate()
    if(is.null(proposal)) invalid <- invalid+1L else {
      pd <- if(collected) exp(-.spades_distance(proposal$code,archive[seq_len(collected),,drop=FALSE])/cfg$tau_diversity) else 0
      # -log(q_BN) in target cancels the independent BN proposal ratio.
      log_alpha <- current$H-proposal$H+cfg$lambda_diversity*(cd-pd)
      if(log(runif(1))<min(0,log_alpha)) {current<-proposal;cd<-pd;accepted<-accepted+1L}
    }
    if(iter>cfg$burnin && (iter-cfg$burnin)%%cfg$thin==0L) {
      collected<-collected+1L;archive[collected,]<-current$code
      scores[collected,]<-c(current$p,current$d,current$H,cd)
      cd<-1 # Current state was just appended: exact zero distance to archive.
    }
    if(!is.null(job$checkpoint) && (iter%%job$checkpoint_every==0L || iter==end)) checkpoint(iter)
    if(verbose && (iter%%cfg$log_every==0L || iter==end)) {
      cat(sprintf('Chain %d | iteration %d/%d | records %d/%d | acceptance %.3f\n',
        job$id,iter,limit,collected,job$n,accepted/iter));flush.console()
    }
  }
  if(end<limit) return(list(paused=TRUE,chain=job$id,iteration=end,records=collected))
  records <- as.data.frame(lapply(seq_along(cols),function(j)
    factor(lev[[j]][archive[,j]],levels=lev[[j]],ordered=is.ordered(real[[j]]))),check.names=FALSE)
  names(records)<-cols
  list(data=records,scores=as.data.frame(scores),summary=data.frame(chain=job$id,
    records=collected,iterations=limit,acceptance=accepted/limit,real_proposals_rejected=invalid))
 })
}
spades_flight <- function(model,n_pool,chains=6L,workers=6L,seed=123L,
                          control=list(),cache_dir=NULL,verbose=TRUE,
                          checkpoint_dir=NULL,checkpoint_every=10000L,run_for=NULL) {
 cl<-NULL;lock<-NULL
 on.exit({if(!is.null(cl))try(parallel::stopCluster(cl),silent=TRUE)
   if(!is.null(lock))unlink(lock,recursive=TRUE)},add=TRUE)
 if(!inherits(model,'spades_fit')) stop('Use spades_fit first.')
 .spades_check_prepared(model$prepared)
 n_pool<-.spades_positive_integer(n_pool,'n_pool');chains<-.spades_positive_integer(chains,'chains')
 workers<-.spades_positive_integer(workers,'workers');seed<-.spades_positive_integer(seed,'seed',0L)
 if(seed>.Machine$integer.max-1000*chains) stop('Seed exceeds chain offset range.')
 chains<-min(chains,n_pool);workers<-min(workers,chains)
 cfg<-.spades_controls(list(burnin=1000L,thin=5L,log_every=10000L,cache_size=4096L,
   lambda_plausibility=1,lambda_privacy=1,lambda_diversity=.7,tau_privacy=.25,tau_diversity=.25),control)
 for(nm in c('burnin','thin','log_every','cache_size')) cfg[[nm]]<-.spades_positive_integer(cfg[[nm]],nm,if(nm=='burnin')0L else 1L)
 for(nm in c('lambda_plausibility','lambda_privacy','lambda_diversity','tau_privacy','tau_diversity'))
   if(!is.numeric(cfg[[nm]]) || length(cfg[[nm]])!=1L || !is.finite(cfg[[nm]]) || cfg[[nm]]<0 ||
     (startsWith(nm,'tau') && cfg[[nm]]==0)) stop('Invalid ',nm)
 sizes<-rep(n_pool%/%chains,chains);sizes[seq_len(n_pool%%chains)]<-sizes[seq_len(n_pool%%chains)]+1L
 if(any(cfg$burnin+cfg$thin*as.double(sizes)>.Machine$integer.max)) stop('Too many iterations.')
 checkpoint_every<-.spades_positive_integer(checkpoint_every,'checkpoint_every')
 if(!is.null(run_for)) {
   run_for<-.spades_positive_integer(run_for,'run_for')
   if(is.null(checkpoint_dir))stop('run_for requires checkpoint_dir.')
 }
 request<-list(model=model,n_pool=n_pool,chains=chains,seed=seed,cfg=cfg,version='0.1.4',R=R.version.string,rng_kind=RNGkind(),
   packages=vapply(c('xgboost','bnlearn','Matrix','withr','rngtools'),function(p)as.character(utils::packageVersion(p)),''))
 key<-.spades_flight_hash(request)
 cachefile<-NULL
 if(!is.null(cache_dir)) {
   dir.create(cache_dir,recursive=TRUE,showWarnings=FALSE)
   cachefile<-file.path(cache_dir,paste0('flight_',key,'.rds'))
 }
 if(!is.null(checkpoint_dir)) {
   dir.create(checkpoint_dir,recursive=TRUE,showWarnings=FALSE)
   checkpoint_dir<-normalizePath(checkpoint_dir,mustWork=TRUE)
   lockpath<-file.path(checkpoint_dir,'RUNNING.lock')
   if(!dir.create(lockpath,showWarnings=FALSE)) stop('Run is locked: ',lockpath,
     '. If the previous R session crashed, first ensure all its workers have stopped, then remove this lock directory.')
   lock<-lockpath
   manifest<-file.path(checkpoint_dir,'request.rds')
   if(file.exists(manifest)) {
     saved<-.spades_read_verified(manifest)
     if(!identical(saved$key,key))stop('Checkpoint belongs to a different model, settings or software environment. Use its original request or a new directory.')
   } else {
     if(length(list.files(checkpoint_dir,pattern='^chain_|^complete')))stop('Missing request for existing checkpoints.')
     .spades_write_new(list(key=key,request=request,cache_dir=cache_dir),manifest)
   }
   completefile<-file.path(checkpoint_dir,'complete.rds')
   if(file.exists(completefile)) {
     if(verbose)cat('REUSE | completed flight; no training or sampling.\n')
     return(.spades_read_verified(completefile))
   }
 }
 if(!is.null(cachefile) && file.exists(cachefile)) {
   if(verbose)cat('REUSE | completed pool cache; no training or sampling.\n')
   ans<-readRDS(cachefile)
   if(!is.null(checkpoint_dir)) .spades_write_new(ans,completefile)
   return(ans)
 }
 jobs<-lapply(seq_len(chains),function(i)list(id=i,n=sizes[i],seed=seed+1000L*i,rng_kind=RNGkind(),
   key=key,checkpoint=if(is.null(checkpoint_dir))NULL else file.path(checkpoint_dir,sprintf('chain_%02d',i)),
   checkpoint_every=checkpoint_every,run_for=run_for))
 if(verbose) {cat('FLIGHT | BN + plausibility + privacy + diversity | duplicates retained\n');flush.console()}
 if(workers==1L) out<-lapply(jobs,.spades_current_chain,model=model,cfg=cfg,verbose=verbose) else {
   cl<-parallel::makePSOCKcluster(workers,outfile='')
   parallel::clusterCall(cl,function(paths,kind){.libPaths(paths);do.call(RNGkind,as.list(kind));loadNamespace('SPADES.IND');NULL},.libPaths(),RNGkind())
   out<-parallel::parLapply(cl,jobs,.spades_current_chain,model=model,cfg=cfg,verbose=verbose)
 }
 if(any(vapply(out,function(x)isTRUE(x$paused),logical(1)))) {
   if(verbose)cat('PAUSED | checkpoints saved. Continue with spades_resume_flight(checkpoint_dir).\n')
   return(structure(list(checkpoint_dir=checkpoint_dir,progress=lapply(out,function(x)
     if(isTRUE(x$paused)) x else x$summary)),class='spades_flight_paused'))
 }
 ans<-structure(list(data=do.call(rbind,lapply(out,`[[`,'data')),
   scores=do.call(rbind,lapply(out,`[[`,'scores')),summary=do.call(rbind,lapply(out,`[[`,'summary')),
   controls=cfg,seed=seed,chains=chains,reference_hash=.spades_flight_hash(model$prepared$data)),class='spades_flight')
 rownames(ans$data)<-NULL
 if(!is.null(checkpoint_dir)) .spades_write_new(ans,completefile)
 if(!is.null(cachefile)) {tmp<-tempfile(tmpdir=cache_dir);saveRDS(ans,tmp);if(!file.rename(tmp,cachefile)) {unlink(tmp);warning('Unable to save flight cache.')}}
 ans
}
spades_flight_auto <- function(model,n_pool,...,structure=NULL) {
 if(is.null(structure)) structure<-spades_structure_auto(model$prepared)
 ans<-spades_flight(model,n_pool,...);ans$structure<-structure;ans
}
