# Numeric values are materialized once, before regression utility is computed.
spades_reconstruct <- function(data,prepared,seed=4123L) {
 if(!inherits(prepared,'spades_prepared')) stop('Use spades_prepare first.')
 .spades_check_prepared(prepared)
 if(inherits(data,'spades_landing')||inherits(data,'spades_flight')) data<-data$data
 .spades_assert_complete(data,prepared$missing_policy$tokens,'Reconstruction data')
 if(!is.data.frame(data)||!identical(names(data),names(prepared$data))) stop('Data/schema mismatch.')
 for(v in names(data)) if(anyNA(data[[v]])||any(!as.character(data[[v]])%in%levels(prepared$data[[v]]))) stop('Unknown category: ',v)
 .spades_with_seed(seed, {
   out<-data
   for(v in prepared$schema$numeric) {
     disc<-prepared$numeric_discretizers[[v]];labs<-as.character(data[[v]])
     z<-rep(NA_real_,length(labs));ok<-labs!='__MISSING__'
     if(identical(disc$method,'constant')) z[ok]<-disc$constant_value
     else if(any(ok)) {
       bins<-match(labs[ok],setdiff(disc$levels,'__MISSING__'))
       lo<-disc$breaks[bins];hi<-disc$breaks[bins+1L]
       if(v%in%prepared$integer_columns) {
         # Bins are right-closed, first bin includes the lower bound.
         lower<-ifelse(bins==1L,ceiling(lo),floor(lo)+1)
         upper<-floor(hi)
         if(identical(disc$method,"equal_width")) {lower<-ceiling(lo);upper<-ifelse(bins==length(disc$breaks)-1L,floor(hi),ceiling(hi)-1)}
         if(any(lower>upper)) stop('No integer value in bin for ',v)
         z[ok]<-lower+floor(runif(sum(ok))*(upper-lower+1))
       } else {
         z[ok]<-lo+runif(sum(ok))*(hi-lo)
         # Protect right-closed bin identity at an exactly sampled lower boundary.
         boundary<-!identical(disc$method,"equal_width") & bins>1L & z[ok]<=lo
         zz<-z[ok];zz[boundary]<-lo[boundary]+(hi[boundary]-lo[boundary])/2;z[ok]<-zz
       }
     }
     out[[v]]<-z
   }
   for(v in setdiff(names(out),prepared$schema$numeric)) {
     z<-as.character(out[[v]]);z[z=='__MISSING__']<-NA_character_
     orig<-prepared$original[[v]]
     if(is.factor(orig)) z<-factor(z,levels=levels(orig),ordered=is.ordered(orig))
     else if(is.logical(orig)) z<-as.logical(z)
     out[[v]]<-z
   }
   for(v in prepared$schema$numeric) {
     if(!identical(.spades_bin_labels(out[[v]],prepared$numeric_discretizers[[v]]),as.character(data[[v]]))) stop('Reconstruction original-bin invariant failed: ',v)
   }
   .spades_assert_complete(out,prepared$missing_policy$tokens,'Reconstructed data')
   out
 })
}
spades_utility <- function(reference,pool,target,predictors=NULL,
                           mode=c('classification','regression'),seed=1011L,threads=1L,
                           reference_values=NULL,pool_values=NULL) {
 mode<-match.arg(mode)
 prepared_input<-if(inherits(reference,'spades_prepared'))reference else NULL
 if(inherits(reference,'spades_prepared')) {
   .spades_check_prepared(reference)
   if(is.null(reference_values))reference_values<-reference$original
   else if(!identical(reference_values,reference$original))stop('reference_values must equal the cleaned prepared$original, in the same row order.')
   reference<-reference$data
 }
 if(inherits(pool,'spades_flight')) pool<-pool$data
 .spades_assert_complete(reference,prepared_input$missing_policy$tokens,'Utility reference')
 .spades_assert_complete(pool,prepared_input$missing_policy$tokens,'Utility pool')
 if(length(target)!=1L||!is.character(target)||!target%in%names(reference)) stop('Invalid response variable.')
 if(is.null(predictors)) predictors<-setdiff(names(reference),target)
 if(!length(predictors)||anyNA(predictors)||anyDuplicated(predictors)||target%in%predictors||!all(predictors%in%names(reference))) stop('Invalid predictors.')
 threads<-.spades_positive_integer(threads,'threads')
 if(!requireNamespace('ranger',quietly=TRUE)) stop('Install ranger.')
 r<-reference[,predictors,drop=FALSE];p<-pool[,predictors,drop=FALSE]
 for(v in predictors) {
   lev<-unique(as.character(r[[v]]));r[[v]]<-factor(as.character(r[[v]]),levels=lev)
   p[[v]]<-factor(as.character(p[[v]]),levels=lev)
 }
 if(anyNA(r)||anyNA(p)) stop('Utility predictors contain missing/unknown categorical codes.')
 if(mode=='classification') {
   y<-factor(as.character(reference[[target]]));yp<-factor(as.character(pool[[target]]),levels=levels(y))
   if(nlevels(y)<2L||anyNA(yp)) stop('Classification requires at least two known response classes.')
   scale<-1
 } else {
   if(is.null(reference_values)||is.null(pool_values)) stop('Regression requires fixed reference_values and pool_values.')
   if(nrow(reference_values)!=nrow(reference)||nrow(pool_values)!=nrow(pool)) stop('Numeric row alignment mismatch.')
   if(!identical(names(reference_values),names(reference))||!identical(names(pool_values),names(pool)))stop('Numeric column alignment mismatch.')
   if(!is.null(prepared_input)) {
     helper<-spades_functions('core')
     for(v in prepared_input$schema$numeric) {
       disc<-prepared_input$numeric_discretizers[[v]]
       if(!identical(as.character(helper$apply_numeric_discretizer(pool_values[[v]],disc)),as.character(pool[[v]]))) stop('Pool numeric/category mismatch: ',v)
       if(!identical(as.character(helper$apply_numeric_discretizer(reference_values[[v]],disc)),as.character(reference[[v]]))) stop('Reference numeric/category mismatch: ',v)
     }
   }
   # Regression guide sees the fixed reconstructed numeric predictors.
   for(v in predictors) {
     if(is.numeric(reference_values[[v]])) {
       if(!is.numeric(pool_values[[v]])||any(!is.finite(c(reference_values[[v]],pool_values[[v]]))))stop('Invalid numeric regression predictor: ',v)
       r[[v]]<-reference_values[[v]];p[[v]]<-pool_values[[v]]
     }
   }
   y<-reference_values[[target]];yp<-pool_values[[target]]
   if(!is.numeric(y)||!is.numeric(yp)||any(!is.finite(c(y,yp)))) stop('Regression response must be finite numeric, without missing values.')
   scale<-sd(y);if(!is.finite(scale)||scale<=0) stop('Regression response must have positive variance.')
 }
 .spades_with_seed(seed, {
   fit<-ranger::ranger(x=r,y=y,probability=mode=='classification',num.trees=500L,
     min.node.size=5L,importance='impurity',respect.unordered.factors='order',num.threads=threads,seed=seed)
   pr<-predict(fit,data=r,num.threads=threads)$predictions
   pp<-predict(fit,data=p,num.threads=threads)$predictions
   if(mode=='classification') {
     cr<-colnames(pr)[max.col(pr,ties.method='first')];cp<-colnames(pp)[max.col(pp,ties.method='first')]
     ref<-mean(cr==as.character(y));scores<-as.numeric(cp==as.character(yp))
   } else {ref<-sqrt(mean((pr-y)^2));scores<-(pp-yp)^2}
   list(mode=mode,scores=scores,reference=ref,scale=scale,model=fit,target=target,
     predictors=predictors,seed=seed,reference_evaluation='training rows; fixed guide; not external validation')
 })
}
spades_land_rf <- function(pool,reference,n,target,predictors=NULL,seed=1122L,
                           rf_seed=1011L,rf_threads=1L,control=list(),pairs=list(),conditional_on=NULL,
                           keep_trace=TRUE,verbose=TRUE) {
 u<-spades_utility(reference,pool,target,predictors,seed=rf_seed,threads=rf_threads)
 spades_land(pool,reference,n,seed,control,pairs,conditional_on,u,keep_trace,verbose)
}
spades_synthesise <- function(data,n,target=NULL,mode=c('auto','classification','regression','none'),
                              n_pool=max(1000L,12L*n),n_bins=20L,chains=6L,workers=6L,
                              seed=4123L,pairs=NULL,conditional_on=NULL,
                              fit_control=list(),flight_control=list(),landing_control=list(),
                              cache_dir=NULL,verbose=TRUE,missing_tokens="NA",target_bins=10L,regression_metric=c("RMSE","MAE")) {
 mode<-match.arg(mode);n<-.spades_positive_integer(n,'n')
 seed<-.spades_positive_integer(seed,'seed',0L)
 if(seed>.Machine$integer.max-10000L)stop('Seed too large for workflow offsets.')
 if(!is.null(target)&&(!is.character(target)||length(target)!=1L||is.na(target)||!target%in%names(data)))stop('Unknown target.')
 if(mode=='auto') mode<-if(is.null(target))'none' else if(is.numeric(data[[target]]))'regression' else 'classification'
 if(mode!='none'&&is.null(target))stop('Specify target.')
 if(mode=='none'&&!is.null(target))stop('Use target=NULL with mode=none.')
 prepared<-.spades_prepare_target(data,target,mode,n_bins,target_bins,missing_tokens,verbose)
 if(is.null(pairs)) {
   st<-spades_structure_auto(prepared,seed=seed,threads=workers)
   pairs<-st$pairs
 }
 allowed<-c('n_fake','nrounds','bn_restart','bn_blacklist','nthread')
 if(!is.list(fit_control)||(length(fit_control)&&(is.null(names(fit_control))||anyDuplicated(names(fit_control))||any(!names(fit_control)%in%allowed))))stop('Invalid fit_control.')
 model<-do.call(spades_fit,c(list(prepared=prepared,seed=seed),fit_control))
 pool<-spades_flight(model,n_pool,chains,workers,seed+1L,flight_control,cache_dir,verbose)
 if(mode=='regression') {
   regression_metric<-match.arg(regression_metric)
   landing<-spades_land_response(pool,prepared,n,target,metric=regression_metric,
     seed=seed+4L,rf_seed=seed+3L,threads=workers,control=landing_control,pairs=pairs,
     conditional_on=conditional_on,verbose=verbose)
   synthetic<-.spades_finalize_response(landing,prepared,seed+2L)
   prepared$response_protocol$utility<-paste('absolute',regression_metric,'gap / reference SD')
 } else {
   u<-if(mode=='none')NULL else spades_utility(prepared,pool,target,mode='classification',seed=seed+3L,threads=workers)
   landing<-spades_land(pool,prepared,n,seed+4L,landing_control,pairs,conditional_on,u,verbose=verbose)
   synthetic<-spades_reconstruct(landing$data,prepared,seed+2L)
 }
 rownames(synthetic)<-NULL
 list(data=synthetic,categorical=landing$data,landing=landing,pool=pool,
   prepared=prepared,model=model,mode=mode,seed=seed,filtering=prepared$filtering,
   response_protocol=prepared$response_protocol)
}

# Response discretization is learned solely from the cleaned training reference.
.spades_prepare_target <- function(data,target,mode,n_bins,target_bins,missing_tokens,verbose) {
 if(mode=='regression'&&(!is.numeric(data[[target]])||is.object(data[[target]])))stop('Continuous target must be numeric.')
 numeric_cols<-names(data)[vapply(data,is.numeric,logical(1))]
 if(mode=='classification')numeric_cols<-setdiff(numeric_cols,target)
 prepared<-spades_prepare(data,n_bins=n_bins,numeric=numeric_cols,missing_tokens=missing_tokens,verbose=verbose)
 protocol<-list(target=target,mode=mode,utility=if(mode=='none')'none' else if(mode=='classification')'absolute accuracy gap' else 'absolute RMSE gap / reference SD')
 if(mode=='regression') {
   target_bins<-.spades_positive_integer(target_bins,'target_bins')
   if(target_bins<2L)stop('target_bins must be at least two.')
   y<-prepared$original[[target]]
   if(!is.finite(sd(y))||sd(y)<=0)stop('Continuous target must have positive variance after cleaning.')
   probs<-seq(0,1,length.out=target_bins+1L)
   breaks<-unique(as.numeric(stats::quantile(y,probs=probs,type=7,names=FALSE)))
   disc<-list(method='quantile',breaks=breaks,
     levels=sprintf('response_%02d',seq_len(length(breaks)-1L)),prefix='response')
   helper<-spades_functions('core')
   prepared$data[[target]]<-helper$apply_numeric_discretizer(y,disc)
   prepared$numeric_discretizers[[target]]<-disc
   protocol$requested_bins<-target_bins
   protocol$actual_bins<-length(breaks)-1L
   protocol$breaks<-breaks
   protocol$counts<-table(prepared$data[[target]])
   protocol$reconstruction<-'Uniform within original interval (uniform over admissible integers for integer-valued input); at every SA proposal; accepted response values retained; other numeric columns reconstructed after landing.'
   protocol$status<-'Experimental regression support; not validated by the classification case study. Random reconstruction is not a privacy guarantee.'
   if(verbose){cat('RESPONSE | quantile bins requested:',target_bins,'| actual:',protocol$actual_bins,'| dynamic response reconstruction | RMSE or MAE gap
');flush.console()}
 }
 prepared$response_protocol<-protocol
 prepared
}
