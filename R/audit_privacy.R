.spades_audit_save <- function(x,path) {
 tmp<-paste0(path,'.tmp');saveRDS(x,tmp)
 if(file.exists(path))stop('Refusing to replace completed checkpoint: ',path)
 if(!file.rename(tmp,path))stop('Checkpoint commit failed: ',path)
 invisible(path)
}
.spades_audit_manifest <- function(config,out) {
 if(!dir.exists(out)&&!dir.create(out,recursive=TRUE))stop('Cannot create output directory.')
 if(nchar(normalizePath(out,winslash='/',mustWork=TRUE),type='chars')>180L)stop('Use a shorter audit output directory (<=180 characters).')
 p<-file.path(out,'protocol.rds')
 if(file.exists(p)){if(!identical(readRDS(p),config))stop('Existing output configuration/input mismatch: ',out)}else .spades_audit_save(config,p)
}
spades_mia_candidates <- function(members,nonmembers,seed=20261030L) {
 .spades_audit_frame(members);.spades_audit_frame(nonmembers,names(members))
 km<-.spades_audit_key(members);kn<-.spades_audit_key(nonmembers);shared<-intersect(km,kn)
 im<-which(!duplicated(km)&!km%in%shared);io<-which(!duplicated(kn)&!kn%in%shared);n<-min(length(im),length(io))
 if(n<200L)stop('At least 200 unique unambiguous candidates per class required.')
 .spades_with_seed(seed,{
  im<-im[sample.int(length(im),n)];io<-io[sample.int(length(io),n)]
  list(member_rows=im,nonmember_rows=io,labels=rep(c(1L,0L),each=n),
   shared_member_rows=sum(km%in%shared),shared_nonmember_rows=sum(kn%in%shared),
   reference_hash=.spades_flight_hash(list(members,nonmembers)),seed=seed)
 })
}
.spades_auc <- function(y,s){n1<-sum(y==1);n0<-sum(y==0);(sum(rank(s)[y==1])-n1*(n1+1)/2)/(n1*n0)}
.spades_threshold <- function(s,y,alpha=NULL){
 cuts<-c(Inf,sort(unique(s),decreasing=TRUE));tp<-vapply(cuts,function(t)mean(s[y==1]>=t),numeric(1));fp<-vapply(cuts,function(t)mean(s[y==0]>=t),numeric(1))
 if(is.null(alpha))return(cuts[which.max(tp-fp)])
 ok<-which(fp<=alpha);cuts[ok[which.max(tp[ok])]]
}
spades_mia_stability <- function(audit,out_dir,seeds=20261100L+seq_len(20L),threads=6L,bootstrap=1000L,verbose=TRUE) {
 if(!requireNamespace('ranger',quietly=TRUE))stop('Install ranger.')
 f<-audit$features;y<-audit$labels
 if(!is.data.frame(f)||nrow(f)!=length(y)||anyNA(f)||!all(y%in%c(0,1))||any(table(factor(y,levels=0:1))<200L))stop('Invalid audit cache.')
 if(!length(seeds)||anyNA(seeds)||anyDuplicated(seeds)||any(seeds<0|seeds> .Machine$integer.max-100L)||any(seeds!=floor(seeds)))stop('Invalid seeds.')
 threads<-.spades_positive_integer(threads,'threads');bootstrap<-.spades_positive_integer(bootstrap,'bootstrap')
 config<-list(hash=.spades_flight_hash(list(f,y)),seeds=seeds,threads=threads,bootstrap=bootstrap,
  version=as.character(utils::packageVersion('SPADES.IND')),ranger=as.character(utils::packageVersion('ranger')),R=R.version.string)
 .spades_audit_manifest(config,out_dir)
 fixed<-data.frame(near_Hamming=-f$Hamming_min,far_Hamming=f$Hamming_min,near_mixed=-f$mixed_min,far_mixed=f$mixed_min,
 dense_knn=-f$mixed_knn5,sparse_knn=f$mixed_knn5,exact_match=f$exact_frequency,reverse_exact_match=-f$exact_frequency,
 kernel_density=f$mixed_kernel,reverse_kernel_density=-f$mixed_kernel)
 files<-character(length(seeds))
 for(b in seq_along(seeds)){
  p<-file.path(out_dir,sprintf('split_%02d.rds',b));files[b]<-p
  if(file.exists(p)){if(verbose)cat('REUSE MIA split',b,'\n');next}
  ans<-.spades_with_seed(seeds[b],{
   tr<-ca<-te<-integer()
   for(cl in 0:1){ids<-which(y==cl);ids<-ids[sample.int(length(ids))];n<-length(ids);nt<-floor(.4*n);nc<-floor(.3*n)
    tr<-c(tr,ids[seq_len(nt)]);ca<-c(ca,ids[nt+seq_len(nc)]);te<-c(te,ids[(nt+nc+1L):n])}
   scores<-fixed
   fit<-ranger::ranger(x=f[tr,,drop=FALSE],y=factor(y[tr],levels=0:1),probability=TRUE,num.trees=500,min.node.size=10,num.threads=threads,seed=seeds[b])
   scores$RF_attack<-predict(fit,data=f,num.threads=threads)$predictions[,'1']
   fit0<-ranger::ranger(x=f[tr,,drop=FALSE],y=factor(sample(y[tr]),levels=0:1),probability=TRUE,num.trees=500,min.node.size=10,num.threads=threads,seed=seeds[b]+100L)
   scores$RF_permuted_control<-predict(fit0,data=f,num.threads=threads)$predictions[,'1']
   eligible<-setdiff(names(scores),'RF_permuted_control');winner<-eligible[which.max(vapply(scores[eligible],function(s).spades_auc(y[ca],s[ca]),numeric(1)))]
   rows<-list()
   for(nm in names(scores)){
    s<-scores[[nm]];st<-s[te];yt<-y[te];ci<-c(NA_real_,NA_real_)
    if(b==1L){boot<-replicate(bootstrap,{ii<-c(sample(which(yt==1),sum(yt==1),TRUE),sample(which(yt==0),sum(yt==0),TRUE));.spades_auc(yt[ii],st[ii])});ci<-quantile(boot,c(.025,.975),names=FALSE)}
    for(rule in c('Youden','FPR05','FPR01')){
     alpha<-switch(rule,Youden=NULL,FPR05=.05,FPR01=.01);t<-.spades_threshold(s[ca],y[ca],alpha)
     tp<-sum(st[yt==1]>=t);fp<-sum(st[yt==0]>=t);np<-sum(yt==1);nn<-sum(yt==0)
     rows[[length(rows)+1L]]<-data.frame(split=b,seed=seeds[b],attack=nm,rule=rule,selected_on_calibration=nm==winner,
      AUC=.spades_auc(yt,st),AUC_lo=ci[1],AUC_hi=ci[2],TP=tp,FP=fp,n_member=np,n_nonmember=nn,TPR=tp/np,FPR=fp/nn,
      FPR_upper95=binom.test(fp,nn)$conf.int[2],balanced_accuracy=(tp/np+1-fp/nn)/2,advantage=tp/np-fp/nn,threshold=t)
    }
   }
   list(metrics=do.call(rbind,rows),winner=winner,train=tr,calibration=ca,test=te,
    test_scores=if(b==1L)scores[te,,drop=FALSE]else NULL,test_labels=if(b==1L)y[te]else NULL)
  })
  .spades_audit_save(ans,p)
  if(verbose){z<-ans$metrics;z<-z[z$attack=='far_mixed'&z$rule=='Youden',];cat('MIA split',b,'/',length(seeds),'| inverse AUC',round(z$AUC,4),'| selected',ans$winner,'\n');flush.console()}
 }
 all<-do.call(rbind,lapply(files,function(p)readRDS(p)$metrics))
 summ<-function(z,label)data.frame(attack=label,rule=z$rule[1],splits=nrow(z),AUC_mean=mean(z$AUC),AUC_sd=sd(z$AUC),
 AUC_min=min(z$AUC),AUC_max=max(z$AUC),BA_mean=mean(z$balanced_accuracy),advantage_mean=mean(z$advantage),TPR_mean=mean(z$TPR),FPR_mean=mean(z$FPR))
 groups<-split(all,interaction(all$attack,all$rule,drop=TRUE));s<-do.call(rbind,lapply(groups,function(z)summ(z,z$attack[1])))
 chosen<-all[all$selected_on_calibration,];sel<-do.call(rbind,lapply(split(chosen,chosen$rule),function(z)summ(z,'Selected_on_calibration')));s<-rbind(s,sel);rownames(s)<-NULL
 utils::write.csv(all,file.path(out_dir,'all_splits.csv'),row.names=FALSE);utils::write.csv(s,file.path(out_dir,'summary.csv'),row.names=FALSE)
 first<-readRDS(files[1]);grDevices::pdf(file.path(out_dir,'ROC_first_split.pdf'),width=7,height=7)
 tryCatch({graphics::plot(c(0,1),c(0,1),type='n',xlab='False positive rate',ylab='True positive rate');graphics::abline(0,1,lty=2)
  pal<-grDevices::hcl.colors(ncol(first$test_scores),'Dark 3')
  for(j in seq_along(first$test_scores)){sc<-first$test_scores[[j]];yy<-first$test_labels;cuts<-c(Inf,sort(unique(sc),decreasing=TRUE),-Inf)
   graphics::lines(vapply(cuts,function(t)mean(sc[yy==0]>=t),numeric(1)),vapply(cuts,function(t)mean(sc[yy==1]>=t),numeric(1)),col=pal[j])}
  graphics::legend('bottomright',legend=names(first$test_scores),col=pal,lty=1,cex=.6)
 },finally=grDevices::dev.off())
 writeLines(c('Fixed-release MIA; known labelled auxiliary candidates; not shadow-model attacks.',
 'Score directions fixed; thresholds and attack selection use calibration only. Train/calibration/test: 40/30/30 percent.',
 'First-split AUC bootstrap intervals condition on release and candidates; other split ranges/SD are descriptive.',
 'Repeated splits overlap and are not independent replications. No pooled binomial inference across splits.',
 'The real holdout was previously used for utility assessment; this is exploratory validation.',
 'Low-FPR targets apply to calibration; inspect actual test FPR and denominators.',
 'Exact profiles shared across real sources excluded from profile-membership attacks and reported.',
 'No detected attack advantage does not establish anonymity or differential privacy.',
 'All RDS checkpoints contain private candidate information: keep local.'),file.path(out_dir,'README.txt'))
 list(summary=s,metrics=all)
}
spades_privacy_audit <- function(members,nonmembers,released,out_dir,numeric=names(members)[vapply(members,is.numeric,logical(1))],
 candidates=NULL,seeds=20261100L+seq_len(20L),threads=6L,verbose=TRUE) {
 .spades_audit_frame(members);.spades_audit_frame(nonmembers,names(members));.spades_audit_frame(released,names(members))
 if(is.null(candidates))candidates<-spades_mia_candidates(members,nonmembers)
 if(!identical(candidates$reference_hash,.spades_flight_hash(list(members,nonmembers))))stop('Candidates/reference mismatch.')
 config<-list(inputs=.spades_flight_hash(list(members,nonmembers,released)),candidates=candidates,numeric=numeric,
  version=as.character(utils::packageVersion('SPADES.IND')))
 .spades_audit_manifest(config,out_dir);p<-file.path(out_dir,'features_PRIVATE.rds')
 if(file.exists(p)){audit<-readRDS(p);if(verbose)cat('REUSE privacy features; no distance recomputation.\n')}else{
  q<-rbind(members[candidates$member_rows,,drop=FALSE],nonmembers[candidates$nonmember_rows,,drop=FALSE])
  f<-.spades_audit_features(q,released,members,numeric,verbose)
  # Distances from ALL released rows to ALL real training rows; not candidate subset.
  near<-.spades_audit_features(released,members,members,numeric,verbose)
  km<-.spades_audit_key(members);kn<-.spades_audit_key(nonmembers);ks<-.spades_audit_key(released)
  copy_score<-as.integer(.spades_audit_key(q)%in%km);positive<-.spades_auc(candidates$labels,copy_score)
  if(positive!=1)stop('Deliberately leaky copy-release positive control failed.')
  counts<-data.frame(dataset=c('members','nonmembers','synthetic'),rows=c(nrow(members),nrow(nonmembers),nrow(released)),
   duplicates=c(sum(duplicated(km)),sum(duplicated(kn)),sum(duplicated(ks))),
   exact_release_matches=c(sum(km%in%ks),sum(kn%in%ks),length(ks)))
  distance_summary<-do.call(rbind,lapply(c('Hamming_min','mixed_min'),function(v)data.frame(metric=v,mean=mean(near[[v]]),min=min(near[[v]]),q05=unname(quantile(near[[v]],.05)),median=median(near[[v]]),max=max(near[[v]]))))
  diversity<-.spades_with_seed(6621L,{
   n<-nrow(released);i<-sample.int(n,10000L,TRUE);j<-sample.int(n-1L,10000L,TRUE);j<-j+(j>=i)
   dd<-rowMeans(vapply(names(released),function(v)as.character(released[[v]][i])!=as.character(released[[v]][j]),logical(length(i))))
   data.frame(metric='Hamming_sampled_distinct_row_pairs',pairs=length(i),mean=mean(dd),min=min(dd),median=median(dd),max=max(dd))
  })
  audit<-list(features=f,labels=candidates$labels,candidates=candidates,counts=counts,nearest_real=distance_summary,diversity=diversity,
   exact_links=list(member_rows=which(km%in%ks),nonmember_rows=which(kn%in%ks),synthetic_to_member=which(ks%in%km)),positive_control_AUC=positive)
  .spades_audit_save(audit,p)
 }
 utils::write.csv(audit$counts,file.path(out_dir,'counts.csv'),row.names=FALSE)
 utils::write.csv(audit$nearest_real,file.path(out_dir,'nearest_real.csv'),row.names=FALSE)
 utils::write.csv(audit$diversity,file.path(out_dir,'diversity.csv'),row.names=FALSE)
 ans<-spades_mia_stability(audit,file.path(out_dir,'mia'),seeds,threads,verbose=verbose)
 list(counts=audit$counts,nearest_real=audit$nearest_real,diversity=audit$diversity,positive_control_AUC=audit$positive_control_AUC,mia=ans)
}
