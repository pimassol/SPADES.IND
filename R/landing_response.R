# Experimental joint selection of categorical rows and within-bin response values.
spades_land_response <- function(pool,prepared,n,target,metric=c('RMSE','MAE'),
 seed=1122L,rf_seed=1011L,threads=1L,control=list(),pairs=list(),
 conditional_on=NULL,keep_trace=TRUE,verbose=TRUE,initial_indices=NULL,initial_y=NULL) {
 metric<-match.arg(metric)
 if(!inherits(prepared,'spades_prepared'))stop('prepared must come from spades_prepare or the main workflow.')
 .spades_check_prepared(prepared)
 if(!is.character(target)||length(target)!=1L||is.na(target)||!target%in%prepared$schema$numeric)stop('Response must be a prepared numeric variable.')
 if(inherits(pool,'spades_flight')) {
   if(!is.null(pool$reference_hash)&&!identical(pool$reference_hash,.spades_flight_hash(prepared$data)))stop('Flight/reference mismatch.')
   if(!is.null(pool$structure)&&!length(pairs))pairs<-pool$structure$pairs
   pool<-pool$data
 }
 spec<-.spades_fidelity_specs(pool,prepared$data,pairs,conditional_on)
 disc<-prepared$numeric_discretizers[[target]]
 if(!identical(disc$method,'quantile'))stop('Response requires quantile bins with positive widths.')
 bin<-match(as.character(pool[[target]]),disc$levels)
 lo<-as.numeric(disc$breaks[bin]);hi<-as.numeric(disc$breaks[bin+1L])
 integer_y<-target%in%prepared$integer_columns
 if(integer_y){lo<-ifelse(bin==1L,ceiling(lo),floor(lo)+1);hi<-floor(hi)}
 if(anyNA(bin)||any(!is.finite(c(lo,hi)))||any(if(integer_y)lo>hi else lo>=hi))stop('Invalid response interval.')
 n<-.spades_positive_integer(n,'n');M<-nrow(pool)
 if(n>M)stop('Target exceeds pool size.')
 seed<-.spades_positive_integer(seed,'seed',0L);rf_seed<-.spades_positive_integer(rf_seed,'rf_seed',0L)
 if(seed>.Machine$integer.max-2L)stop('Seed too large.')
 threads<-.spades_positive_integer(threads,'threads')
 cfg<-.spades_controls(list(L=1000000L,pilot=400L,log_every=10000L),control)
 for(nm in names(cfg))cfg[[nm]]<-.spades_positive_integer(cfg[[nm]],nm)
 if(!requireNamespace('ranger',quietly=TRUE))stop('Install ranger.')
 predictors<-setdiff(names(pool),target)
 r<-prepared$data[,predictors,drop=FALSE];p<-pool[,predictors,drop=FALSE]
 for(v in predictors){lev<-unique(as.character(r[[v]]));r[[v]]<-factor(as.character(r[[v]]),levels=lev);p[[v]]<-factor(as.character(p[[v]]),levels=lev)}
 if(anyNA(p))stop('Unknown predictor categories.')
 y<-prepared$original[[target]];scale<-sd(y)
 if(!is.numeric(y)||any(!is.finite(y))||!is.finite(scale)||scale<=0)stop('Response must be finite numeric with positive SD.')
 guide<-.spades_with_seed(rf_seed,{
   fit<-ranger::ranger(x=r,y=y,num.trees=500L,min.node.size=5L,respect.unordered.factors='order',num.threads=threads,seed=rf_seed)
   list(model=fit,real=as.numeric(predict(fit,data=r,num.threads=threads)$predictions),
     pool=as.numeric(predict(fit,data=p,num.threads=threads)$predictions))
 })
 loss<-function(y,p)if(metric=='RMSE')(y-p)^2 else abs(y-p)
 error<-function(z)if(metric=='RMSE')sqrt(mean(z)) else mean(z)
 reference_error<-error(loss(y,guide$real))
 if(any(!is.finite(c(guide$pool,reference_error,loss(lo,guide$pool),loss(hi,guide$pool)))))stop('Nonfinite response loss; rescale response.')
 if(is.null(initial_indices))initial_indices<-.spades_with_seed(seed,sample.int(M,n))
 if(length(initial_indices)!=n||anyNA(initial_indices)||any(initial_indices!=floor(initial_indices))||any(initial_indices<1|initial_indices>M)||anyDuplicated(initial_indices))stop('Invalid initial pool indices.')
 ii<-as.integer(initial_indices)
 if(is.null(initial_y)) initial_y<-.spades_with_seed(seed+1L,{
   u<-runif(n)
   if(integer_y)lo[ii]+floor(u*(hi[ii]-lo[ii]+1)) else {
     yy<-lo[ii]+u*(hi[ii]-lo[ii]);bad<-yy<=lo[ii];yy[bad]<-lo[ii][bad]+(hi[ii][bad]-lo[ii][bad])/2;yy
   }
 })
 if(!is.numeric(initial_y)||length(initial_y)!=n||any(!is.finite(initial_y))||
    (integer_y&&any(initial_y!=floor(initial_y)))||
    !identical(.spades_bin_labels(initial_y,disc),as.character(pool[[target]][ii])))stop('Initial response values do not match selected bins.')
 initial_loss<-loss(initial_y,guide$pool[ii])
 args<-list(X=spec$X,specs=spec$specs,group=spec$group,gw=spec$gw,predictions=guide$pool,
   lo=lo,hi=hi,integer_y=integer_y,rmse=metric=='RMSE',initial=ii,initial_y=initial_y,
   L=cfg$L,pilot=cfg$pilot,log_every=cfg$log_every,reference=reference_error,scale=scale,verbose=verbose)
 if(verbose){cat('LANDING | categorical fidelity +',metric,'gap | response drawn per proposal |',cfg$L,'iterations\n');flush.console()}
 ans<-.spades_with_seed(seed+2L,.Call('_SPADES_IND_sa_response',args,PACKAGE='SPADES.IND'))
 indices<-ans$indices;numeric_y<-ans$response_values
 vals<-.spades_fidelity_evaluate(spec,indices);before<-.spades_fidelity_evaluate(spec,ii)
 final_loss<-loss(numeric_y,guide$pool[indices])
 if(abs(mean(vals)-ans$F)>1e-8||abs(mean(final_loss)-ans$mean_score)>1e-8*(1+mean(final_loss)))stop('Independent response landing verification failed.')
 if(!identical(.spades_bin_labels(numeric_y,disc),as.character(pool[[target]][indices])))stop('Accepted response bin invariant failed.')
 errors<-c(error(initial_loss),error(final_loss));gaps<-abs(errors-reference_error)/scale
 summary<-data.frame(stage=c('initial','selected'),F_mean=c(mean(before),mean(vals)),utility=gaps)
 summary$H<-summary$F_mean+summary$utility
 summary$duplicates<-c(sum(duplicated(pool[ii,,drop=FALSE])),sum(duplicated(pool[indices,,drop=FALSE])))
 structure(list(data=pool[indices,,drop=FALSE],indices=indices,initial_indices=ii,
   response_values=numeric_y,initial_response_values=initial_y,response=target,metric=metric,
   summary=summary,response_errors=data.frame(stage=summary$stage,error=errors,reference=reference_error),
   components=data.frame(term=spec$labels,initial=before,selected=vals),
   utility=list(mode='dynamic_regression',metric=metric,reference=reference_error,scale=scale,
     predictions=guide$pool,model=guide$model,predictors=predictors,reference_evaluation='training rows, fixed categorical-predictor guide'),
   trace=if(keep_trace)ans$trace else NULL,controls=cfg,seed=seed,
   temperature=c(initial=ans$T0,final=ans$Tf),pairs=spec$pairs,conditional_on=spec$conditional_on,
   proposal_measure='Uniform pool subsets and independent uniform response within each selected bin; energy tilt relative to this base measure.',
   output_state='final'),class=c('spades_response_landing','spades_landing'))
}

.spades_finalize_response <- function(landing,prepared,seed) {
 # Do not draw and overwrite the response: draw only the other numeric columns.
 pr<-prepared;pr$schema$numeric<-setdiff(pr$schema$numeric,landing$response)
 out<-spades_reconstruct(landing$data,pr,seed)
 out[[landing$response]]<-landing$response_values
 for(v in prepared$schema$numeric)if(!identical(.spades_bin_labels(out[[v]],prepared$numeric_discretizers[[v]]),as.character(landing$data[[v]])))stop('Final reconstruction bin mismatch: ',v)
 out
}
