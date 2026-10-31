# External evaluation of released data. Never called by generation functions.
.spades_external_encode <- function(train, test, predictors, numeric) {
  x <- matrix(numeric(0), nrow(train), 0L)
  xt <- matrix(numeric(0), nrow(test), 0L)
  schema <- list(); unknown <- list()
  for (v in predictors) {
    if (v %in% numeric) {
      mu <- mean(train[[v]]); sig <- stats::sd(train[[v]])
      if (!is.finite(sig) || sig == 0) sig <- 1
      x <- cbind(x, (train[[v]] - mu)/sig)
      xt <- cbind(xt, (test[[v]] - mu)/sig)
      schema[[v]] <- list(mean=mu, sd=sig)
    } else {
      a <- as.character(train[[v]]); b <- as.character(test[[v]])
      lev <- sort(unique(a))
      x <- cbind(x, outer(a,lev,'==')*1)
      xt <- cbind(xt, outer(b,lev,'==')*1)
      schema[[v]] <- list(levels=lev)
      unknown[[length(unknown)+1L]] <- data.frame(variable=v, unseen_test_records=sum(!b %in% lev))
    }
  }
  colnames(x) <- colnames(xt) <- paste0('x',seq_len(ncol(x)))
  if(any(!is.finite(x)) || any(!is.finite(xt))) stop('Nonfinite encoded predictors.')
  list(x=x, test=xt, schema=schema, unknown=if(length(unknown))do.call(rbind,unknown) else
    data.frame(variable=character(),unseen_test_records=integer()))
}

.spades_external_metrics <- function(pred, truth, classes=NULL) {
  if (is.null(classes)) {
    if(length(pred)!=length(truth)||any(!is.finite(pred)))stop('Invalid regression predictions.')
    residual <- pred-truth; den <- sum((truth-mean(truth))^2)
    return(list(values=c(RMSE=sqrt(mean(residual^2)), MAE=mean(abs(residual)),
      R_squared=if(den>0)1-sum(residual^2)/den else NA_real_), prediction=as.numeric(pred)))
  }
  p <- as.matrix(pred); k <- length(classes)
  if(nrow(p)!=length(truth)||ncol(p)!=k||any(!is.finite(p))||any(p<0)||
     any(rowSums(p)<=0)||max(abs(rowSums(p)-1))>1e-5)stop('Invalid class probabilities.')
  p <- p/rowSums(p); colnames(p) <- classes
  y <- factor(as.character(truth),levels=classes)
  label <- factor(classes[max.col(p,ties.method='first')],levels=classes)
  cm <- table(truth=y,prediction=label)
  recall <- diag(cm)/rowSums(cm)
  precision <- ifelse(colSums(cm)>0,diag(cm)/colSums(cm),0)
  f1 <- ifelse(precision+recall>0,2*precision*recall/(precision+recall),0)
  values <- c(accuracy=mean(label==y),balanced_accuracy=mean(recall),macro_F1=mean(f1),
    log_loss=-mean(log(pmax(p[cbind(seq_along(y),as.integer(y))],1e-15))),
    Brier_sum=mean(rowSums((p-diag(k)[as.integer(y),,drop=FALSE])^2)))
  list(values=values,prediction=label,probabilities=p,confusion=cm,
    by_class=data.frame(class=classes,n=as.integer(rowSums(cm)),recall=recall,precision=precision,F1=f1,row.names=NULL))
}

.spades_external_fit <- function(model,z,y,classes,seed,threads,control,verbose) {
  classification <- !is.null(classes)
  .spades_with_seed(seed, {
    if(model=='XT') {
      fit <- ranger::ranger(x=as.data.frame(z$x),y=y,probability=classification,
        num.trees=control$trees,mtry=max(1L,floor(sqrt(ncol(z$x)))),min.node.size=control$min_node,
        splitrule='extratrees',num.random.splits=1L,replace=FALSE,sample.fraction=1,
        num.threads=threads,seed=seed,importance='none',verbose=FALSE)
      p <- predict(fit,data=as.data.frame(z$test),num.threads=threads)$predictions
      if(classification)p <- p[,classes,drop=FALSE]
    } else if(model=='Bagging_CART') {
      train <- data.frame(y=y,z$x,check.names=FALSE); test <- as.data.frame(z$test)
      p <- if(classification)matrix(0,nrow(test),length(classes),dimnames=list(NULL,classes)) else numeric(nrow(test))
      for(b in seq_len(control$trees)) {
        idx <- sample.int(nrow(train),nrow(train),replace=TRUE)
        fit <- rpart::rpart(y~.,data=train[idx,,drop=FALSE],method=if(classification)'class' else 'anova',
          control=rpart::rpart.control(cp=0,minsplit=10L,minbucket=control$min_node,maxdepth=30L,xval=0L))
        pb <- predict(fit,newdata=test,type=if(classification)'prob' else 'vector')
        if(classification)p[,colnames(pb)] <- p[,colnames(pb),drop=FALSE]+pb else p <- p+pb
        if(verbose && b%%100L==0L){cat('  CART',b,'/',control$trees,'\n');flush.console()}
      }
      p <- p/control$trees
    } else {
      pars <- list(eta=control$eta,max_depth=control$max_depth,min_child_weight=1,
        subsample=.8,colsample_bytree=.8,lambda=1,alpha=0,tree_method='hist',nthread=threads,verbosity=0)
      if(classification){pars$objective <- 'multi:softprob';pars$num_class <- length(classes);pars$eval_metric <- 'mlogloss'}
      else {pars$objective <- 'reg:squarederror';pars$eval_metric <- 'rmse'}
      dm <- xgboost::xgb.DMatrix(z$x,label=if(classification)as.integer(y)-1L else y)
      dt <- xgboost::xgb.DMatrix(z$test)
      fit <- xgboost::xgb.train(params=pars,data=dm,nrounds=control$nrounds,verbose=0)
      p <- predict(fit,dt)
      if(classification){
        if(is.null(dim(p)))p <- matrix(p,ncol=length(classes),byrow=TRUE)
        if(!identical(dim(p),c(nrow(z$test),length(classes))))stop('Unexpected XGB prediction shape.')
        colnames(p) <- classes
      }
    }
    p
  })
}

spades_external_utility <- function(reference, synthetic, holdout, target,
  out_dir, predictors=NULL, mode=c('classification','regression'),
  models=c('XT','Bagging_CART','XGB'), seed=20261026L, threads=1L,
  numeric=NULL, control=list(), missing_tokens='NA', verbose=TRUE) {
  mode <- match.arg(mode)
  if(!is.character(target)||length(target)!=1L||is.na(target)||!nzchar(target))stop('target must name one response column.')
  if(!is.data.frame(reference)||!target%in%names(reference))stop('Reference response missing.')
  if(is.null(predictors))predictors <- setdiff(names(reference),target)
  if(!is.character(predictors)||!length(predictors)||anyNA(predictors)||anyDuplicated(predictors)||
     target%in%predictors||!all(predictors%in%names(reference)))stop('Invalid predictors.')
  if(is.data.frame(synthetic))synthetic <- list(Synthetic_01=synthetic)
  if(!is.list(synthetic)||!length(synthetic)||!all(vapply(synthetic,is.data.frame,logical(1))))stop('synthetic must be a data.frame or list of data.frames.')
  if(is.null(names(synthetic)))names(synthetic) <- sprintf('Synthetic_%02d',seq_along(synthetic))
  if(anyNA(names(synthetic))||any(!nzchar(names(synthetic)))||anyDuplicated(names(synthetic))||'Real'%in%names(synthetic))stop('Synthetic dataset names must be unique, nonempty and different from Real.')
  frames <- c(list(Real=reference),synthetic,list(Holdout=holdout))
  cols <- c(predictors,target); tokens <- .spades_missing_tokens(missing_tokens)
  for(i in seq_along(frames)) {
    d <- frames[[i]]
    if(!is.data.frame(d)||!nrow(d)||anyDuplicated(names(d))||!all(cols%in%names(d)))stop('Invalid dataset at position ',i)
    d <- d[,cols,drop=FALSE]
    if(!all(vapply(d,function(v)is.factor(v)||is.character(v)||is.logical(v)||(is.numeric(v)&&!is.object(v)),logical(1))))stop('Unsupported column type.')
    if(any(.spades_missing_rows(d,tokens)))stop('Incomplete evaluation data; supply explicitly cleaned data. No silent row removal.')
    if(any(vapply(d,function(v)is.numeric(v)&&any(!is.finite(v)),logical(1))))stop('Nonfinite numeric data.')
    frames[[i]] <- d
  }
  if(is.null(numeric))numeric <- predictors[vapply(reference[predictors],is.numeric,logical(1))]
  if(!is.character(numeric)||anyNA(numeric)||anyDuplicated(numeric)||!all(numeric%in%predictors))stop('Invalid numeric predictor names.')
  if(!all(vapply(frames,function(d)all(vapply(d[numeric],is.numeric,logical(1))),logical(1))))stop('Declared numeric predictors must be numeric in every dataset.')
  # An undeclared numeric predictor is explicitly treated as categorical.
  classes <- if(mode=='classification')sort(unique(as.character(reference[[target]]))) else NULL
  if(mode=='classification') {
    if(length(classes)<2L)stop('At least two reference response classes required.')
    for(d in frames)if(!setequal(unique(as.character(d[[target]])),classes))stop('Each dataset, including holdout, must contain all reference response classes.')
  } else {
    if(!all(vapply(frames,function(d)is.numeric(d[[target]]),logical(1))))stop('Regression responses must be numeric.')
    if(nrow(reference)<2L||stats::sd(reference[[target]])==0)stop('Reference regression response must vary.')
  }
  if(!is.character(models)||!length(models)||anyNA(models)||anyDuplicated(models)||!all(models%in%c('XT','Bagging_CART','XGB')))stop('Unknown or duplicate models.')
  if(length(seed)!=1L||!is.numeric(seed)||!is.finite(seed)||seed<0||seed>.Machine$integer.max||seed!=floor(seed))stop('Exactly ONE nonnegative integer seed required.')
  seed <- as.integer(seed); threads <- .spades_positive_integer(threads,'threads')
  cfg <- .spades_controls(list(trees=500L,min_node=5L,nrounds=300L,eta=.05,max_depth=4L),control)
  for(nm in c('trees','min_node','nrounds','max_depth'))cfg[[nm]] <- .spades_positive_integer(cfg[[nm]],nm)
  if(cfg$min_node>10L)stop('min_node must not exceed fixed CART minsplit=10.')
  if(length(cfg$eta)!=1L||!is.numeric(cfg$eta)||!is.finite(cfg$eta)||cfg$eta<=0||cfg$eta>1)stop('eta must be in (0,1].')
  deps <- unname(c(XT='ranger',Bagging_CART='rpart',XGB='xgboost')[models])
  for(pkg in deps)if(!requireNamespace(pkg,quietly=TRUE))stop('Install optional dependency: ',pkg)
  if(!is.character(out_dir)||length(out_dir)!=1L||is.na(out_dir)||!nzchar(out_dir))stop('Invalid out_dir.')
  if(!dir.exists(out_dir)&&!dir.create(out_dir,recursive=TRUE))stop('Cannot create out_dir.')
  lock <- file.path(out_dir,'RUNNING.lock')
  if(!dir.create(lock,showWarnings=FALSE))stop('Output locked. Verify that no run is active before removing a stale RUNNING.lock.')
  on.exit(unlink(lock,recursive=TRUE),add=TRUE)
  protocol <- list(version=as.character(utils::packageVersion('SPADES.IND')),R=R.version.string,
    input_hash=.spades_flight_hash(frames),datasets=names(synthetic),target=target,predictors=predictors,numeric=numeric,
    mode=mode,models=models,seed=seed,threads=threads,control=cfg,missing_tokens=tokens,
    packages=setNames(vapply(deps,function(p)as.character(utils::packageVersion(p)),character(1)),deps),
    encoding='training-only standardized numeric and one-hot categorical; unseen categories all-zero',
    fixed_hyperparameters=list(XT=list(mtry='floor(sqrt(encoded p))',random_splits=1L,replace=FALSE,sample_fraction=1),
      Bagging_CART=list(cp=0,minsplit=10L,maxdepth=30L,xval=0L,bootstrap='training row count'),
      XGB=list(min_child_weight=1,subsample=.8,colsample_bytree=.8,lambda=1,alpha=0,tree_method='hist')))
  .spades_audit_manifest(protocol,out_dir)
  cpdir <- file.path(out_dir,'checkpoints');dir.create(cpdir,showWarnings=FALSE)
  train <- frames[seq_len(length(frames)-1L)]; test <- frames[[length(frames)]]
  results <- list(); preprocessing <- list(); total <- length(train)*length(models); done <- 0L
  for(i in seq_along(train)) {
    z <- NULL; y <- if(mode=='classification')factor(as.character(train[[i]][[target]]),levels=classes) else train[[i]][[target]]
    for(model in models) {
      path <- file.path(cpdir,sprintf('%s_%03d.rds',model,i));done <- done+1L
      if(file.exists(path)) {
        ans <- readRDS(path)
        if(!identical(ans$dataset,names(train)[i])||!identical(ans$model,model)||!identical(ans$seed,seed)||
           !identical(ans$truth,test[[target]]))stop('Checkpoint identity mismatch.')
        met <- .spades_external_metrics(if(mode=='classification')ans$probabilities else ans$prediction,ans$truth,classes)
        if(!isTRUE(all.equal(met$values,ans$metrics,tolerance=1e-12)))stop('Checkpoint metric mismatch.')
        preprocessing[[i]] <- ans$preprocessing
        if(verbose)cat('REUSE',done,'/',total,'|',model,'|',names(train)[i],'\n')
      } else {
        if(is.null(z))z <- .spades_external_encode(train[[i]],test,predictors,numeric)
        if(verbose){cat('FIT',done,'/',total,'|',model,'|',names(train)[i],'| seed',seed,'\n');flush.console()}
        started <- proc.time()[3]
        p <- .spades_external_fit(model,z,y,classes,seed,threads,cfg,verbose)
        met <- .spades_external_metrics(p,test[[target]],classes)
        ans <- c(list(model=model,dataset=names(train)[i],seed=seed,truth=test[[target]],metrics=met$values,
          seconds=unname(proc.time()[3]-started),preprocessing=z[c('schema','unknown')]),met[setdiff(names(met),'values')])
        .spades_audit_save(ans,path);preprocessing[[i]] <- ans$preprocessing
      }
      results[[length(results)+1L]] <- ans
      if(verbose){cat(' ',names(ans$metrics)[1],sprintf('%.5f',ans$metrics[1]),'\n');flush.console()}
    }
  }
  names(preprocessing) <- names(train)
  rows <- do.call(rbind,lapply(results,function(a)data.frame(model=a$model,dataset=a$dataset,seed=a$seed,as.list(a$metrics),seconds=a$seconds)))
  rownames(rows) <- NULL; measures <- names(results[[1]]$metrics)
  syn <- rows[rows$dataset!='Real',,drop=FALSE]; real <- rows[rows$dataset=='Real',,drop=FALSE]
  gaps <- syn[,c('model','dataset','seed'),drop=FALSE]
  for(m in measures)gaps[[paste0(m,'_gap')]] <- syn[[m]]-real[[m]][match(syn$model,real$model)]
  summary <- do.call(rbind,lapply(models,function(model)do.call(rbind,lapply(measures,function(m){
    v <- syn[[m]][syn$model==model]; r <- real[[m]][real$model==model]
    data.frame(model=model,metric=m,targets=length(v),real=r,mean=mean(v),sd=stats::sd(v),min=min(v),max=max(v),mean_gap=mean(v)-r)
  }))))
  utils::write.csv(rows,file.path(out_dir,'metrics.csv'),row.names=FALSE)
  utils::write.csv(gaps,file.path(out_dir,'paired_gaps.csv'),row.names=FALSE)
  utils::write.csv(summary,file.path(out_dir,'across_targets.csv'),row.names=FALSE)
  writeLines(capture.output(utils::sessionInfo()),file.path(out_dir,'sessionInfo.txt'))
  writeLines(c('External utility; one fixed model seed, same holdout for all training datasets.',
    'No tuning, early stopping, generator fitting or target selection. Encoders fitted on each training dataset.',
    'SD and ranges describe variation across supplied synthetic targets conditional on this holdout and model seed.',
    'These are not independent confidence intervals or privacy guarantees. Reconstruction variation may also contribute.',
    'Checkpoints contain holdout truth and predictions: retain privately. Completed fits are reused; an interrupted fit is restarted.',
    'Bagging CART uses sequential bootstrap trees. threads controls ranger and XGBoost only.'),file.path(out_dir,'README.txt'))
  if(verbose){print(summary,row.names=FALSE);cat('EXTERNAL UTILITY COMPLETED:',out_dir,'\n')}
  invisible(list(metrics=rows,gaps=gaps,summary=summary,preprocessing=preprocessing,protocol=protocol))
}
