local({
  library(SPADES.IND)
  ns <- asNamespace('SPADES.IND')
  metric <- get('.spades_external_metrics',ns)
  encoder <- get('.spades_external_encode',ns)
  p <- matrix(c(.8,.2,.4,.6),nrow=2,byrow=TRUE)
  m <- metric(p,c('a','b'),c('a','b'))$values
  stopifnot(m['accuracy']==1,abs(m['Brier_sum']-.2)<1e-12,
    abs(m['log_loss']+mean(log(c(.8,.6))))<1e-12)
  mr <- metric(c(1,3),c(1,2))$values
  stopifnot(abs(mr['RMSE']-sqrt(.5))<1e-12,mr['MAE']==.5,mr['R_squared']==-1)
  z <- encoder(data.frame(a=c(0,2),b=c('x','y')),data.frame(a=100,b='new'),c('a','b'),'a')
  stopifnot(z$schema$a$mean==1,z$unknown$unseen_test_records==1,all(z$test[1,2:3]==0))
  available <- c(XT='ranger',Bagging_CART='rpart',XGB='xgboost')
  models <- names(available)[vapply(available,requireNamespace,logical(1),quietly=TRUE)]
  if(!length(models)){cat('SKIP fitting: optional model packages unavailable.\n');return(invisible(NULL))}
  withr::local_seed(891L)
  make <- function(n) {
    a <- stats::rnorm(n); b <- rep(c('north','south'),length.out=n)
    data.frame('continuous predictor'=a,'group label'=b,
      response=ifelse(a+stats::rnorm(n,sd=.4)>0,'yes','no'),check.names=FALSE)
  }
  ref <- make(80); test <- make(40); syn <- list(first=make(80),second=make(80))
  td <- tempfile('external_test_');dir.create(td);on.exit(unlink(td,recursive=TRUE),add=TRUE)
  # Preload optional packages before taking session-state snapshots.
  snap <- function()list(seed=get('.Random.seed',globalenv()),kind=RNGkind(),wd=getwd(),options=options(),search=search())
  before <- snap()
  call <- function(out,reference=ref,synthetic=syn,mode='classification')spades_external_utility(
    reference,synthetic,test,'response',out,models=models,mode=mode,seed=17L,threads=1L,
    control=list(trees=5L,nrounds=3L),verbose=FALSE)
  a <- call(file.path(td,'classification'))
  stopifnot(nrow(a$metrics)==3L*length(models),all(a$metrics$seed==17L),identical(before,snap()))
  # Independently recompute summaries and paired real baselines.
  for(mod in models){
    v <- a$metrics$accuracy[a$metrics$model==mod & a$metrics$dataset!='Real']
    sr <- a$summary[a$summary$model==mod & a$summary$metric=='accuracy',]
    stopifnot(isTRUE(all.equal(sr$sd,sd(v))),sr$targets==2L)
  }
  files <- list.files(file.path(td,'classification','checkpoints'),full.names=TRUE)
  hashes <- tools::md5sum(files); times <- file.info(files)$mtime
  # A completed cache must not invoke model fitting.
  trace('.spades_external_fit',tracer=quote(stop('Unexpected refit')),where=ns,print=FALSE)
  b <- tryCatch(call(file.path(td,'classification')),finally=untrace('.spades_external_fit',where=ns))
  stopifnot(identical(a$metrics,b$metrics),identical(hashes,tools::md5sum(files)),identical(times,file.info(files)$mtime),identical(before,snap()))
  # Incomplete run: retain completed fits and recompute only the missing one.
  saved <- readRDS(files[1]);unlink(files[1]);c <- call(file.path(td,'classification'))
  stopifnot(identical(saved$metrics,readRDS(files[1])$metrics),identical(before,snap()))
  changed <- ref;changed[1,1] <- changed[1,1]+1
  stopifnot(inherits(try(call(file.path(td,'classification'),reference=changed),silent=TRUE),'try-error'))
  bad <- syn;bad[[1]][1,1] <- NA_real_
  stopifnot(inherits(try(call(file.path(td,'bad'),synthetic=bad),silent=TRUE),'try-error'))
  ref$response <- ref[[1]]*2+seq_len(nrow(ref))/100
  test$response <- test[[1]]*2+seq_len(nrow(test))/100
  syn <- lapply(syn,function(d){d$response<-d[[1]]*2+seq_len(nrow(d))/100;d})
  r <- call(file.path(td,'regression'),mode='regression')
  stopifnot(all(is.finite(r$metrics$RMSE)),nrow(r$metrics)==3L*length(models),identical(before,snap()))
  cat('PASS: external utility metrics, training-only encoding, fixed-seed classification/regression, checkpoint reuse, interrupted-run recovery and session state.\n')
})
