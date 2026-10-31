.spades_audit_frame <- function(x, columns=NULL) {
 if(!is.data.frame(x)||!nrow(x)||!ncol(x)||anyDuplicated(names(x))||any(!nzchar(names(x)))) stop('Expected a nonempty data.frame with unique column names.')
 if(!is.null(columns)&&!identical(names(x),columns)) stop('Audit column order/schema mismatch.')
 if(anyNA(x)||any(vapply(x,function(v) is.numeric(v)&&any(!is.finite(v)),logical(1)))) stop('Audit inputs must be complete and finite.')
 .spades_assert_complete(x,context='Audit input')
 x
}
.spades_audit_key <- function(x) {
 apply(as.data.frame(lapply(x,function(v){v<-as.character(v);paste0(nchar(v,type='bytes'),':',v)}),stringsAsFactors=FALSE),1,paste,collapse='|')
}
.spades_bin_labels <- function(x,d) {
 if(identical(d$method,'constant')) {if(any(x!=d$constant_value))stop('Value outside constant bin.');return(rep(as.character(d$levels[1]),length(x)))}
 if(!d$method%in%c('quantile','equal_width')||is.null(d$breaks))stop('Unsupported discretizer.')
 lev<-setdiff(as.character(d$levels),'__MISSING__');br<-d$breaks
 if(length(br)!=length(lev)+1L||any(!is.finite(br))||any(diff(br)<=0))stop('Invalid saved breaks.')
 lev[cut(x,breaks=br,include.lowest=TRUE,right=d$method!='equal_width',labels=FALSE)]
}
spades_reconstruct_bins <- function(categorical,discretizers,integer_columns=character(),seed=4123L) {
 .spades_audit_frame(categorical)
 if(!is.list(discretizers)||!all(names(discretizers)%in%names(categorical))||!all(integer_columns%in%names(discretizers)))stop('Invalid reconstruction schema.')
 .spades_with_seed(seed,{
  out<-categorical
  for(v in names(discretizers)) {
   d<-discretizers[[v]];lab<-as.character(categorical[[v]])
   if(identical(d$method,'constant'))z<-rep(d$constant_value,length(lab)) else {
    lev<-setdiff(as.character(d$levels),'__MISSING__');ii<-match(lab,lev)
    if(anyNA(ii))stop('Unknown bin: ',v)
    br<-d$breaks;lo<-br[ii];hi<-br[ii+1L]
    if(any(!is.finite(c(lo,hi)))||any(hi<=lo))stop('Invalid intervals.')
    if(v%in%integer_columns) {
     if(identical(d$method,'equal_width')) {low<-ceiling(lo);up<-ifelse(ii==length(lev),floor(hi),ceiling(hi)-1)}
     else {low<-ifelse(ii==1L,ceiling(lo),floor(lo)+1);up<-floor(hi)}
     if(any(low>up))stop('No admissible integer in bin: ',v)
     z<-low+floor(runif(length(lab))*(up-low+1))
    } else {
     z<-lo+runif(length(lab))*(hi-lo)
     bad<-is.na(.spades_bin_labels(z,d))|.spades_bin_labels(z,d)!=lab
     z[bad]<-lo[bad]+(hi[bad]-lo[bad])/2
    }
   }
   if(!identical(.spades_bin_labels(z,d),lab))stop('Original-bin invariant failed: ',v)
   out[[v]]<-z
  }
  out
 })
}
spades_fidelity_audit <- function(reference,categorical,released,reference_values,discretizers=list(),pairs=list(),conditional_on=NULL) {
 .spades_audit_frame(reference);.spades_audit_frame(categorical,names(reference))
 .spades_audit_frame(released,names(reference));.spades_audit_frame(reference_values,names(reference))
 if(nrow(categorical)!=nrow(released)||nrow(reference)!=nrow(reference_values))stop('Row alignment mismatch.')
 numeric<-names(discretizers);rebin<-released
 if(!all(numeric%in%names(reference)))stop('Invalid discretizer names.')
 for(v in names(reference)) {
  if(v%in%numeric) {
   if(!is.numeric(released[[v]])||!is.numeric(reference_values[[v]]))stop('Expected numeric original/reconstructed column.')
   if(!identical(.spades_bin_labels(reference_values[[v]],discretizers[[v]]),as.character(reference[[v]])))stop('Reference bin alignment failed: ',v)
   rebin[[v]]<-.spades_bin_labels(released[[v]],discretizers[[v]])
  } else if(!identical(as.character(reference[[v]]),as.character(reference_values[[v]])))stop('Reference row alignment failed: ',v)
  if(!identical(as.character(rebin[[v]]),as.character(categorical[[v]])))stop('Reconstruction row/bin mismatch: ',v)
 }
 s<-.spades_fidelity_specs(categorical,reference,pairs,conditional_on)
 vals<-.spades_fidelity_evaluate(s,seq_len(nrow(categorical)))
 s2<-.spades_fidelity_specs(as.data.frame(lapply(rebin,as.character)),reference,pairs,conditional_on)
 after<-.spades_fidelity_evaluate(s2,seq_len(nrow(rebin)))
 if(max(abs(vals-after))>1e-12)stop('Fidelity changed after reconstruction.')
 nums<-lapply(numeric,function(v){
  r<-reference_values[[v]];z<-released[[v]];grid<-sort(unique(c(r,z)))
  w<-sum(abs(ecdf(r)(head(grid,-1))-ecdf(z)(head(grid,-1)))*diff(grid))
  scale<-diff(range(r));probs<-c(.05,.25,.5,.75,.95)
  data.frame(variable=v,W1=w,W1_range_normalized=if(scale>0)w/scale else NA_real_,
   KS=max(abs(ecdf(r)(grid)-ecdf(z)(grid))),quantile_probability=probs,
   real_quantile=as.numeric(quantile(r,probs)),synthetic_quantile=as.numeric(quantile(z,probs)))
 })
 list(components=data.frame(term=s$labels,before=vals,after=after),
  summary=data.frame(F_mean=mean(vals),F_max=max(vals),changed_bins=0L,
    duplicates_categorical=sum(duplicated(categorical)),duplicates_released=sum(duplicated(released))),
  numeric=if(length(nums))do.call(rbind,nums)else data.frame())
}
.spades_audit_features <- function(query,release,reference,numeric,verbose=TRUE) {
 cols<-names(reference);.spades_audit_frame(query,cols);.spades_audit_frame(release,cols)
 if(nrow(release)<5L||!all(numeric%in%cols))stop('At least five release rows and valid numeric columns required.')
 lev<-lapply(cols,function(v) unique(c(as.character(query[[v]]),as.character(release[[v]]))))
 encode<-function(z) do.call(cbind,lapply(seq_along(cols),function(j) match(as.character(z[[j]]),lev[[j]])))
 q<-encode(query);s<-encode(release);qn<-matrix(0,nrow(query),length(cols));sn<-matrix(0,nrow(release),length(cols))
 for(v in numeric){j<-match(v,cols);r<-reference[[v]]
  if(!is.numeric(r)||!is.numeric(query[[v]])||!is.numeric(release[[v]]))stop('Numeric audit column is not numeric: ',v)
  scale<-diff(range(r));if(scale==0) {if(any(c(query[[v]],release[[v]])!=r[1]))stop('Constant numeric reference with differing values.');scale<-1}
  qn[,j]<-(query[[v]]-min(r))/scale;sn[,j]<-(release[[v]]-min(r))/scale
 }
 ans<-matrix(NA_real_,nrow(query),8)
 for(a in seq.int(1L,nrow(query),by=250L)){
  ix<-a:min(a+249L,nrow(query));ans[ix,]<-.Call(`_SPADES_IND_audit_dist`,q[ix,,drop=FALSE],s,qn[ix,,drop=FALSE],sn,cols%in%numeric)
  if(verbose){cat('Distances:',max(ix),'/',nrow(query),'\n');flush.console()}
 }
 colnames(ans)<-c('Hamming_min','mixed_min','Hamming_knn5','mixed_knn5','exact_frequency','near_frequency','Hamming_kernel','mixed_kernel')
 as.data.frame(ans)
}
