local({
 library(SPADES.IND)
 withr::local_seed(92L)
 ns<-asNamespace('SPADES.IND');prep<-get('.spades_prepare_target',ns)
 fid<-get('.spades_fidelity_specs',ns);evaluate<-get('.spades_fidelity_evaluate',ns)
 # Test the native incremental kernel independently of a fitted model.
 raw<-data.frame(x=rep(letters[1:3],20),y=c(seq(0,1,length.out=30),seq(10,100,length.out=30)))
 pr<-prep(raw,'y','regression',4L,4L,'NA',FALSE);pool<-pr$data
 spec<-fid(pool,pr$data,list(c('x','y')),'x');disc<-pr$numeric_discretizers$y
 bins<-match(as.character(pool$y),disc$levels);lo<-disc$breaks[bins];hi<-disc$breaks[bins+1L]
 pred<-seq(1,20,length.out=nrow(pool));initial<-1:15;iy<-(lo[initial]+hi[initial])/2
 for(rmse in c(TRUE,FALSE))for(full in c(FALSE,TRUE)) {
   ii<-if(full)seq_len(nrow(pool)) else initial; yy<-(lo[ii]+hi[ii])/2
   args<-list(X=spec$X,specs=spec$specs,group=spec$group,gw=spec$gw,predictions=pred,
     lo=lo,hi=hi,integer_y=FALSE,rmse=rmse,initial=as.integer(ii),initial_y=yy,
     L=60L,pilot=10L,log_every=1L,reference=3,scale=20,verbose=FALSE)
   run<-function()withr::with_seed(18L,.Call('_SPADES_IND_sa_response',args,PACKAGE='SPADES.IND'))
   before<-.Random.seed;a<-run();b<-run()
   stopifnot(identical(a,b),identical(before,.Random.seed),identical(a$initial_response_values,yy),!anyDuplicated(a$indices))
   err<-if(rmse)(a$response_values-pred[a$indices])^2 else abs(a$response_values-pred[a$indices])
   stopifnot(abs(mean(err)-a$mean_score)<1e-8,abs(mean(evaluate(spec,a$indices))-a$F)<1e-8,
     identical(get('.spades_bin_labels',ns)(a$response_values,disc),as.character(pool$y[a$indices])))
   last<-tail(a$trace,1)[[1]]
   expected<-mean(evaluate(spec,a$indices))+abs((if(rmse)sqrt(mean(err)) else mean(err))-3)/20
   stopifnot(abs(last['H']-expected)<1e-8)
 }
 # Algebra check for unequal continuous bin widths under the stated base measure.
 wold<-2;wnew<-20;delta<-.12;T<-.3
 log_target_ratio<--delta/T+log(wold/wnew)
 log_proposal_ratio<-log(wnew/wold)
 stopifnot(abs(log_target_ratio+log_proposal_ratio+delta/T)<1e-12)
 if(requireNamespace('ranger',quietly=TRUE)) {
   raw$y<-seq_len(nrow(raw))
   pr<-prep(raw,'y','regression',4L,10L,'NA',FALSE)
   for(metric in c('RMSE','MAE')) {
     a<-spades_land_response(pr$data,pr,15,'y',metric=metric,control=list(L=50L,pilot=10L,log_every=10L),verbose=FALSE)
     stopifnot(all(a$response_values==floor(a$response_values)))
     out<-get('.spades_finalize_response',ns)(a,pr,33L)
     stopifnot(identical(out$y,a$response_values))
     pp<-a$utility$predictions[a$indices]
     err<-if(metric=='RMSE')sqrt(mean((out$y-pp)^2)) else mean(abs(out$y-pp))
     stopifnot(abs(err-a$response_errors$error[2])<1e-10)
   }
 }
 cat('PASS: dynamic response kernel, RMSE/MAE, full-pool refresh, categorical fidelity, bin invariance, integer response, reproducibility and preserved final response.\n')
})
