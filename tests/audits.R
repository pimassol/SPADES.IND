library(SPADES.IND)
ns<-asNamespace('SPADES.IND')
bins<-get('.spades_bin_labels',ns)
d<-list(method='quantile',levels=c('a','b'),breaks=c(0,2,4))
x<-data.frame(age=rep(c('a','b'),each=100),g=rep(c('x','y'),100))
z<-spades_reconstruct_bins(x,list(age=d),'age',seed=12)
stopifnot(identical(bins(z$age,d),x$age),all(z$age[x$age=='b']%in%3:4))
e<-list(method='equal_width',levels=c('a','b'),breaks=c(0,2,4))
z2<-spades_reconstruct_bins(x,list(age=e),'age',seed=12)
stopifnot(identical(bins(z2$age,e),x$age),all(z2$age[x$age=='a']%in%0:1))
f<-spades_fidelity_audit(x,x,z,z,list(age=d),conditional_on='g')
stopifnot(f$summary$F_mean==0,all(f$components$before==f$components$after))
bad<-z;bad$age[1]<-4
stopifnot(inherits(try(spades_fidelity_audit(x,x,bad,z,list(age=d),conditional_on='g'),silent=TRUE),'try-error'))
# Independent R reference for compiled mixed/Hamming distances and exact frequency.
r<-data.frame(age=c(0,1,2,3,4,5),g=c('a','a','b','b','c','c'))
q<-data.frame(age=c(1,2.5),g=c('a','c'))
features<-get('.spades_audit_features',ns)(q,r,r,'age',FALSE)
for(i in seq_len(nrow(q))){h<-((q$age[i]!=r$age)+(q$g[i]!=r$g))/2;g<-(abs(q$age[i]-r$age)/5+(q$g[i]!=r$g))/2
 stopifnot(abs(features$Hamming_min[i]-min(h))<1e-12,abs(features$mixed_min[i]-min(g))<1e-12,
 abs(features$mixed_knn5[i]-mean(sort(g)[1:5]))<1e-12,abs(features$exact_frequency[i]-mean(h==0))<1e-12)}
auc<-get('.spades_auc',ns)
stopifnot(auc(c(0,1),c(0,1))==1,auc(c(0,1),c(1,0))==0,auc(c(0,1),c(1,1))==.5)
cat('PASS: original bins, fidelity invariance, rejected bin crossing, compiled distance reference and AUC.\n')
