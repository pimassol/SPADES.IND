.spades_fidelity_specs <- function(pool,real,pairs,conditional_on) {
 .spades_assert_complete(real,context='Fidelity reference')
 .spades_assert_complete(pool,context='Fidelity pool')
 cols<-names(real);M<-nrow(pool);nr<-nrow(real)
 if(!is.data.frame(pool)||!is.data.frame(real)||!M||!nr||!length(cols)||
    !identical(names(pool),cols)||anyNA(pool)||anyNA(real)||anyDuplicated(cols)) stop('Invalid categorical pool/reference.')
 if(any(!vapply(c(pool,real),function(z)is.factor(z)||is.character(z)||is.logical(z),logical(1)))) stop('Use prepared categorical data.')
 lev<-lapply(real,function(z)sort(unique(as.character(z))))
 rc<-do.call(cbind,lapply(cols,function(v)match(as.character(real[[v]]),lev[[v]])-1L))
 pc<-do.call(cbind,lapply(cols,function(v)match(as.character(pool[[v]]),lev[[v]])-1L))
 if(anyNA(pc)) stop('Pool values outside reference domains.')
 if(is.null(conditional_on)) conditional_on<-cols[which.max(lengths(lev))]
 if(length(conditional_on)!=1L||!conditional_on%in%cols) stop('Invalid conditioning variable.')
 if(!is.list(pairs)||any(!vapply(pairs,function(p)is.character(p)&&length(p)==2L&&all(p%in%cols)&&p[1]!=p[2],logical(1)))) stop('Invalid pairs.')
 keys<-vapply(pairs,function(p)paste(sort(match(p,cols)),collapse=':'),'')
 pairs<-pairs[!duplicated(keys)]
 b<-match(conditional_on,cols);G<-length(lev[[b]]);group<-pc[,b];rg<-rc[,b];gn<-tabulate(rg+1L,G);gw<-gn/nr
 acc<-new.env(parent=emptyenv());acc$specs<-list();acc$labels<-character()
 add<-function(label,pi,ri,size,conditional=FALSE,levels=0L) {
   if(!is.finite(size)||size>1e7) stop('Fidelity contingency table exceeds 10 million cells; reduce bins/cardinality.')
   target<-tabulate(ri+1L,size)/nr
   if(conditional) target<-as.numeric(t(matrix(tabulate(ri+1L,size),nrow=G,byrow=TRUE)/pmax(gn,1L)))
   acc$specs[[length(acc$specs)+1L]]<-list(id=as.integer(pi),target=target,conditional=conditional,levels=as.integer(levels))
   acc$labels<-c(acc$labels,label)
 }
 for(j in seq_along(cols)) add(paste0('marginal:',cols[j]),pc[,j],rc[,j],length(lev[[j]]))
 for(p in pairs) {a<-match(p[1],cols);b2<-match(p[2],cols);B<-length(lev[[b2]])
   add(paste0('pair:',paste(p,collapse=' / ')),pc[,a]*B+pc[,b2],rc[,a]*B+rc[,b2],as.double(length(lev[[a]]))*B)}
 for(j in setdiff(seq_along(cols),b)) {B<-length(lev[[j]])
   add(paste0('conditional:',cols[j],' | ',conditional_on),group*B+pc[,j],rg*B+rc[,j],as.double(G)*B,TRUE,B)}
 list(X=pc,specs=acc$specs,group=as.integer(group),gw=gw,labels=acc$labels,pairs=pairs,conditional_on=conditional_on)
}
.spades_fidelity_evaluate <- function(spec,idx) {
 n<-length(idx);G<-length(spec$gw);ng<-tabulate(spec$group[idx]+1L,G)
 vapply(spec$specs,function(s) {
   ct<-tabulate(s$id[idx]+1L,length(s$target))
   if(!s$conditional) return(.5*sum(abs(ct/n-s$target)))
   z<-matrix(ct,nrow=G,byrow=TRUE);r<-matrix(s$target,nrow=G,byrow=TRUE)
   d<-.5*rowSums(abs(z/pmax(ng,1L)-r));d[ng==0]<-1;sum(spec$gw*d)
 },numeric(1))
}
spades_land <- function(pool,reference,n,seed=1122L,control=list(),
                         pairs=list(),conditional_on=NULL,utility=NULL,keep_trace=TRUE,
                         verbose=TRUE,initial_indices=NULL) {
 if(inherits(reference,'spades_prepared')) {
   .spades_check_prepared(reference)
   pd<-if(inherits(pool,'spades_flight'))pool$data else pool
   .spades_assert_complete(pd,reference$missing_policy$tokens,'Landing pool')
 }
 if(inherits(pool,'spades_flight')) {
   rr<-if(inherits(reference,'spades_prepared'))reference$data else reference
   if(!is.null(pool$reference_hash)&&!identical(pool$reference_hash,.spades_flight_hash(rr)))stop('Flight/reference mismatch.')
   if(!is.null(pool$structure)&&!length(pairs)) pairs<-pool$structure$pairs
   pool<-pool$data
 }
 if(inherits(reference,'spades_prepared')) reference<-reference$data
 n<-.spades_positive_integer(n,'n');seed<-.spades_positive_integer(seed,'seed',0L)
 if(seed==.Machine$integer.max) stop('Seed must allow +1 offset.')
 if(n>nrow(pool)) stop('Target exceeds pool size.')
 cfg<-.spades_controls(list(L=1000000L,pilot=400L,log_every=10000L),control)
 for(nm in names(cfg)) cfg[[nm]]<-.spades_positive_integer(cfg[[nm]],nm)
 spec<-.spades_fidelity_specs(pool,reference,pairs,conditional_on)
 M<-nrow(pool)
 if(is.null(utility)) utility<-list(mode='none',scores=rep(0,M),reference=0,scale=1)
 if(!is.list(utility)||!utility$mode%in%c('none','classification','regression')||
   !is.numeric(utility$scores)||length(utility$scores)!=M||any(!is.finite(utility$scores))||
   length(utility$reference)!=1L||!is.finite(utility$reference)||length(utility$scale)!=1L||
   !is.finite(utility$scale)||utility$scale<=0) stop('Invalid utility specification.')
 if(utility$mode=='classification' && (any(!utility$scores%in%c(0,1))||utility$reference<0||utility$reference>1)) stop('Classification scores must be binary hits.')
 if(utility$mode=='regression' && (any(utility$scores<0)||utility$reference<0)) stop('Regression scores must be squared errors.')
 if(utility$mode=='none') {utility$scores<-rep(0,M);utility$reference<-0;utility$scale<-1}
 if(is.null(initial_indices)) initial_indices<-.spades_with_seed(seed,sample.int(M,n))
 if(length(initial_indices)!=n||anyNA(initial_indices)||any(initial_indices!=floor(initial_indices))||
   any(initial_indices<1|initial_indices>M)||anyDuplicated(initial_indices)) stop('Invalid initial pool indices.')
 uvalue<-function(ii) {v<-mean(utility$scores[ii]);if(utility$mode=='regression')v<-sqrt(v);abs(v-utility$reference)/utility$scale}
 if(verbose) {cat('LANDING | F_mean + utility | mode:',utility$mode,'| iterations:',cfg$L,'\n');flush.console()}
 if(n==M) {
   ii<-initial_indices;vals<-.spades_fidelity_evaluate(spec,ii)
   ans<-list(indices=ii,F=mean(vals),mean_score=mean(utility$scores[ii]),trace=list(),T0=NA_real_,Tf=NA_real_)
 } else ans<-.spades_with_seed(seed+1L,spades_sa_cpp(spec$X,spec$specs,spec$group,spec$gw,
   utility$scores,utility$reference,as.integer(initial_indices),cfg$L,cfg$pilot,cfg$log_every,
   '',function(...)invisible(NULL),utility$mode=='regression',utility$scale,verbose))
 ii<-ans$indices;vals<-.spades_fidelity_evaluate(spec,ii)
 if(abs(mean(vals)-ans$F)>1e-8) stop('Final independent fidelity verification failed.')
 if(abs(mean(utility$scores[ii])-ans$mean_score)>1e-8*(1+abs(ans$mean_score))) stop('Final utility verification failed.')
 before<-.spades_fidelity_evaluate(spec,initial_indices)
 summary<-data.frame(stage=c('initial','selected'),F_mean=c(mean(before),mean(vals)),
   utility=c(uvalue(initial_indices),uvalue(ii)))
 summary$H<-summary$F_mean+summary$utility
 summary$duplicates<-c(sum(duplicated(pool[initial_indices,,drop=FALSE])),sum(duplicated(pool[ii,,drop=FALSE])))
 structure(list(data=pool[ii,,drop=FALSE],indices=ii,initial_indices=initial_indices,
   summary=summary,components=data.frame(term=spec$labels,initial=before,selected=vals),
   utility=utility,trace=if(keep_trace)ans$trace else NULL,
   controls=cfg,seed=seed,temperature=c(initial=ans$T0,final=ans$Tf),
   pairs=spec$pairs,conditional_on=spec$conditional_on,output_state='final'),class='spades_landing')
}
