spades_target_stability <- function(pool,reference,reference_values,nonmembers,discretizers,integer_columns,
 n,out_dir,utility=NULL,pairs=list(),conditional_on=NULL,seeds=20262000L+seq_len(10L),
 control=list(L=1000000L,pilot=400L,log_every=100000L),mia_seeds=20261100L+seq_len(20L),threads=6L,verbose=TRUE,pool_values=NULL) {
 .spades_audit_frame(pool);.spades_audit_frame(reference,names(pool));.spades_audit_frame(reference_values,names(pool));.spades_audit_frame(nonmembers,names(pool))
 if(!length(seeds)||anyDuplicated(seeds)||anyNA(seeds)||any(seeds!=floor(seeds))||any(seeds<0|seeds>.Machine$integer.max-100000L))stop('Invalid target seeds.')
 n<-.spades_positive_integer(n,'n')
 if(n<5L||n>nrow(pool))stop('Target must contain at least five rows and fit in pool.')
 regression<-!is.null(utility)&&identical(utility$mode,'regression')
 if(regression){
  if(is.null(pool_values))stop('Regression requires the fixed pool_values used for utility scores.')
  .spades_audit_frame(pool_values,names(pool))
  if(nrow(pool_values)!=nrow(pool))stop('Regression pool_values row mismatch.')
  invisible(spades_fidelity_audit(reference,pool,pool_values,reference_values,discretizers,pairs,conditional_on))
 }
 config<-list(inputs=.spades_flight_hash(list(pool,reference,reference_values,nonmembers)),discretizers=discretizers,integer_columns=integer_columns,
 n=n,utility=.spades_flight_hash(utility),pool_values_hash=.spades_flight_hash(pool_values),pairs=pairs,conditional_on=conditional_on,seeds=seeds,control=control,mia_seeds=mia_seeds,threads=threads,
 version=as.character(utils::packageVersion('SPADES.IND')),R=R.version.string)
 .spades_audit_manifest(config,out_dir)
 lock<-file.path(out_dir,'RUNNING.lock')
 if(!dir.create(lock,showWarnings=FALSE))stop('Study already locked; ensure no process is running before removing stale RUNNING.lock.')
 on.exit(unlink(lock,recursive=TRUE),add=TRUE)
 candidates<-spades_mia_candidates(reference_values,nonmembers)
 rows<-list();components<-list();numeric_results<-list();privacy_results<-list()
 for(b in seq_along(seeds)) {
  td<-file.path(out_dir,sprintf('target_%02d',b));dir.create(td,showWarnings=FALSE)
  lp<-file.path(td,'landing.rds');rp<-file.path(td,'released.rds');fp<-file.path(td,'fidelity.rds')
  if(verbose){cat('\nTARGET',b,'/',length(seeds),'| fixed pool | seed',seeds[b],'\n');flush.console()}
  if(file.exists(lp)){land<-readRDS(lp);if(verbose)cat('REUSE completed landing.\n')}else{
   started<-proc.time()[3]
   land<-spades_land(pool,reference,n,seeds[b],control,pairs,conditional_on,utility,keep_trace=TRUE,verbose=verbose)
   land$elapsed_seconds<-unname(proc.time()[3]-started)
   .spades_audit_save(land,lp)
  }
  if(file.exists(rp)){released<-readRDS(rp);if(verbose)cat('REUSE reconstructed target.\n')}else{
   released<-if(regression)pool_values[land$indices,,drop=FALSE]else spades_reconstruct_bins(land$data,discretizers,integer_columns,seeds[b]+100000L)
   .spades_audit_save(released,rp)
  }
  # Always verify saved artifacts against their pool rows and original intervals.
  if(!identical(land$data,pool[land$indices,,drop=FALSE]))stop('Saved landing/pool row mismatch.')
  fid<-spades_fidelity_audit(reference,land$data,released,reference_values,discretizers,pairs,conditional_on)
  if(!file.exists(fp)).spades_audit_save(fid,fp)
  utils::write.csv(released,file.path(td,'synthetic_data.csv'),row.names=FALSE)
  check<-utils::read.csv(file.path(td,'synthetic_data.csv'),check.names=FALSE,stringsAsFactors=FALSE)
  if(!identical(.spades_audit_key(check),.spades_audit_key(released)))stop('Released CSV roundtrip mismatch.')
  privacy<-spades_privacy_audit(reference_values,nonmembers,released,file.path(td,'privacy'),names(discretizers),candidates,mia_seeds,threads,verbose)
  pr<-file.path(td,'privacy_summary.rds');if(!file.exists(pr)).spades_audit_save(privacy,pr)
  s<-land$summary[land$summary$stage=='selected',]
  selected<-privacy$mia$summary;selected<-selected[selected$attack=='Selected_on_calibration'&selected$rule=='Youden',]
  rows[[b]]<-data.frame(target=b,seed=seeds[b],F_mean=fid$summary$F_mean,F_max=fid$summary$F_max,
    landing_seconds=land$elapsed_seconds,acceptance=if(length(land$trace))as.numeric(tail(land$trace,1)[[1]]['accepted'])/land$controls$L else NA_real_,
    utility_gap=s$utility,H=s$H,initial_H=land$summary$H[land$summary$stage=='initial'],
    guide_score=if(is.null(utility))NA_real_ else mean(utility$scores[land$indices]),
    duplicates_categorical=fid$summary$duplicates_categorical,duplicates_released=fid$summary$duplicates_released,
    exact_training_matches=privacy$counts$exact_release_matches[privacy$counts$dataset=='members'],
    nearest_real_Hamming_mean=privacy$nearest_real$mean[privacy$nearest_real$metric=='Hamming_min'],
    sampled_diversity_mean=privacy$diversity$mean,selected_MIA_AUC=selected$AUC_mean,selected_MIA_advantage=selected$advantage_mean)
  components[[b]]<-data.frame(target=b,fid$components)
  if(nrow(fid$numeric))numeric_results[[length(numeric_results)+1L]]<-data.frame(target=b,fid$numeric)
  privacy_results[[b]]<-data.frame(target=b,privacy$mia$summary)
  # Refresh compact summaries after every fully audited target.
  utils::write.csv(do.call(rbind,rows),file.path(out_dir,'targets_summary.csv'),row.names=FALSE)
  if(verbose){cat('TARGET COMPLETED',b,'| F',round(s$F_mean,6),'| utility gap',round(s$utility,6),'\n');flush.console()}
 }
 tab<-do.call(rbind,rows);metrics<-setdiff(names(tab),c('target','seed'))
 agg<-do.call(rbind,lapply(metrics,function(v){z<-tab[[v]];z<-z[is.finite(z)];data.frame(metric=v,mean=if(length(z))mean(z)else NA_real_,sd=if(length(z)>1)sd(z)else NA_real_,min=if(length(z))min(z)else NA_real_,max=if(length(z))max(z)else NA_real_)}))
 utils::write.csv(agg,file.path(out_dir,'stability_summary.csv'),row.names=FALSE)
 comp<-do.call(rbind,components);utils::write.csv(comp,file.path(out_dir,'fidelity_components.csv'),row.names=FALSE)
 ca<-do.call(rbind,lapply(split(comp,comp$term),function(z)data.frame(term=z$term[1],mean=mean(z$after),sd=sd(z$after),min=min(z$after),max=max(z$after))))
 utils::write.csv(ca,file.path(out_dir,'fidelity_component_stability.csv'),row.names=FALSE)
 if(length(numeric_results))utils::write.csv(do.call(rbind,numeric_results),file.path(out_dir,'numeric_fidelity.csv'),row.names=FALSE)
 utils::write.csv(do.call(rbind,privacy_results),file.path(out_dir,'privacy_stability.csv'),row.names=FALSE)
 writeLines(c('FIXED-POOL TARGET STABILITY. No flight generation or model fitting for synthesis.',
 'Each target has an independent specified landing/reconstruction seed; generation inputs and RF guide fixed.',
 'All privacy attacks use the same candidate records and same split seeds across targets.',
 'MIA split averages are correlated within and across targets. Report descriptive summaries, not independent-sample confidence intervals.',
 'Classification utility is the fixed guide score, not external predictive validation. No XT/CART/XGB tests.',
 'Completed landing, reconstruction, distances and attack splits are reused. An interrupted landing restarts only that unfinished target.',
 'Sampling retains duplicate row contents; selected pool indices are without replacement.',
 'Private audit files contain membership labels and reference row indices; do not release.'),file.path(out_dir,'README.txt'))
 if(verbose){print(tab,row.names=FALSE);cat('FIXED-POOL STUDY COMPLETED:',out_dir,'\n')}
 invisible(list(targets=tab,summary=agg))
}
