library(SPADES.IND)
set.seed(71)
r<-data.frame(group=factor(sample(letters[1:4],80,TRUE)),a=factor(sample(1:3,80,TRUE)),b=factor(sample(1:3,80,TRUE)))
p<-r[sample.int(80,200,TRUE),,drop=FALSE]
set.seed(91);old<-.Random.seed
run<-function(u=NULL,seed=123L)spades_land(p,r,30,seed=seed,pairs=list(c('a','b')),
 utility=u,control=list(L=300L,pilot=40L,log_every=50L),verbose=FALSE)
a<-run();stopifnot(identical(old,.Random.seed),nrow(a$data)==30L,!anyDuplicated(a$indices),
 nrow(a$components)==6L,all(a$summary$utility==0),identical(a$indices,run()$indices))
b<-run(list(mode='classification',scores=as.numeric(p$a==p$b),reference=.7,scale=1))
stopifnot(abs(b$summary$utility[2]-abs(mean((p$a==p$b)[b$indices])-.7))<1e-10)
c<-run(list(mode='regression',scores=seq_len(200)/200,reference=.6,scale=2))
stopifnot(abs(c$summary$utility[2]-abs(sqrt(mean((seq_len(200)/200)[c$indices]))-.6)/2)<1e-10)
all<-spades_land(p,r,nrow(p),verbose=FALSE);stopifnot(length(all$indices)==nrow(p))
x<-data.frame(a=factor(c('a','b',NA,'a')),integer=c(1L,3L,8L,NA),continuous=c(.1,.9,2.7,NA))
for(method in c('quantile','equal_width')) {
 prepared<-spades_prepare(x,n_bins=3,method=method)
 restored<-spades_reconstruct(prepared$data,prepared)
 f<-spades_functions('core')
 for(v in prepared$schema$numeric)stopifnot(identical(as.character(f$apply_numeric_discretizer(restored[[v]],prepared$numeric_discretizers[[v]])),as.character(prepared$data[[v]])))
 stopifnot(all(is.na(restored$integer)|restored$integer==floor(restored$integer)))
}
# Numerically verify cancellation of BN in independence Hastings.
q<-c(.08,.17);h<-c(.4,.9);d<-c(.2,.8)
H<--log(q)+h+.7*d
stopifnot(abs((H[1]-H[2]+log(q[1])-log(q[2]))-(h[1]-h[2]+.7*(d[1]-d[2])))<1e-12)
if(requireNamespace('ranger',quietly=TRUE)) {
 # Raw data-frame API must still reject textual/actual missing pool values.
 for(marker in c('NA',NA_character_)) {
   bad<-p;bad$b<-as.character(bad$b);bad$b[1L]<-marker
   err<-tryCatch({spades_utility(r,bad,'b');NULL},error=function(e)e)
   stopifnot(inherits(err,'error'),grepl('missing values or missing labels',conditionMessage(err),fixed=TRUE))
 }

 u<-spades_utility(r,p,'b',threads=1)
 stopifnot(length(u$scores)==nrow(p),all(u$scores%in%c(0,1)))
 rv<-r;pv<-p;rv$b<-seq_len(nrow(r))/10;pv$b<-seq_len(nrow(p))/10
 u<-spades_utility(r,p,'b',mode='regression',reference_values=rv,pool_values=pv)
 stopifnot(length(u$scores)==nrow(p),all(is.finite(u$scores)))
}
cat('PASS: incremental landing, all utility modes, row indices, RNG, reconstruction and Hastings algebra.\n')
