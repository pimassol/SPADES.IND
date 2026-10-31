local({
 library(SPADES.IND)
 deps<-c('bnlearn','ranger','xgboost')
 if(!all(vapply(deps,requireNamespace,logical(1),quietly=TRUE))){cat('SKIP main workflow: optional generation dependencies unavailable.\n');return(invisible(NULL))}
 withr::local_seed(444L)
 d<-data.frame(x=rnorm(120),g=rep(letters[1:8],15),h=rep(LETTERS[1:6],20))
 d$y<-10+3*d$x+as.integer(factor(d$g))+rnorm(120)
 run<-function(data,target,mode)spades_synthesise(data,n=8L,target=target,mode=mode,n_pool=20L,
   n_bins=4L,chains=1L,workers=1L,seed=84L,pairs=list(c('x','y')),conditional_on='g',
   fit_control=list(n_fake=30L,nrounds=3L,bn_restart=0L,nthread=1L),
   flight_control=list(burnin=3L,thin=1L,log_every=20L),
   landing_control=list(L=30L,pilot=10L,log_every=30L),verbose=FALSE)
 for(mode in c('classification','regression','none')) {
   input<-d;if(mode=='classification')input$y<-factor(ifelse(d$y>median(d$y),'high','low'))
   target<-if(mode=='none')NULL else 'y'
   ans<-run(input,target,mode)
   stopifnot(nrow(ans$data)==8L,identical(names(ans$data),names(input)),!anyNA(ans$data))
   for(v in ans$prepared$schema$numeric)stopifnot(identical(
     get('.spades_bin_labels',asNamespace('SPADES.IND'))(ans$data[[v]],ans$prepared$numeric_discretizers[[v]]),
     as.character(ans$categorical[[v]])))
   if(mode=='regression')stopifnot(identical(ans$data$y,ans$landing$response_values),ans$landing$utility$mode=='dynamic_regression')
   if(mode=='none')stopifnot(ans$landing$utility$mode=='none',all(ans$landing$summary$utility==0))
   if(mode=='classification')stopifnot(ans$landing$utility$mode=='classification')
 }
 cat('PASS: public main workflow classification, dynamic regression, and no response on small artificial data.\n')
})
