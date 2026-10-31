# Immutable generation files: an interrupted write cannot overwrite the last good state.
.spades_write_new <- function(value,path) {
 if(file.exists(path))stop('Refusing to overwrite committed file: ',path)
 dir.create(dirname(path),recursive=TRUE,showWarnings=FALSE)
 tmp<-tempfile(pattern='.pending_',tmpdir=dirname(path))
 on.exit(unlink(tmp),add=TRUE)
 envelope<-list(value=value,hash=.spades_flight_hash(value))
 saveRDS(envelope,tmp,compress=FALSE,version=2)
 if(!file.rename(tmp,path))stop('Cannot commit checkpoint: ',path)
 invisible(path)
}
.spades_read_verified <- function(path) {
 z<-readRDS(path)
 if(!is.list(z)||is.null(z$hash)||!identical(z$hash,.spades_flight_hash(z$value)))
   stop('Checkpoint integrity check failed: ',path)
 z$value
}
.spades_checkpoint_read <- function(directory,key) {
 paths<-sort(list.files(directory,pattern='^state_[0-9]+[.]rds$',full.names=TRUE),decreasing=TRUE)
 for(path in paths) {
   z<-tryCatch(.spades_read_verified(path),error=function(e)NULL)
   if(is.null(z)) {
     warning('Unreadable checkpoint; trying previous generation: ',path)
     bad<-tempfile(pattern='corrupt_',tmpdir=directory)
     if(!file.rename(path,bad))stop('Cannot quarantine corrupt checkpoint: ',path)
     next
   }
   if(!identical(z$key,key))stop('Checkpoint request mismatch: ',path)
   return(z$state)
 }
 if(length(paths))stop('No valid checkpoint remains: ',directory)
 NULL
}
.spades_checkpoint_write <- function(directory,key,state) {
 dir.create(directory,recursive=TRUE,showWarnings=FALSE)
 path<-file.path(directory,sprintf('state_%010.0f.rds',state$iteration))
 if(file.exists(path))stop('Checkpoint generation already exists: ',path)
 .spades_write_new(list(key=key,state=state),path)
 paths<-sort(list.files(directory,pattern='^state_[0-9]+[.]rds$',full.names=TRUE),decreasing=TRUE)
 if(length(paths)>2L)unlink(paths[-seq_len(2L)])
 invisible(NULL)
}
# Resume with the saved fitted model and exact original request; never call spades_fit.
spades_resume_flight <- function(checkpoint_dir,workers=6L,verbose=TRUE,
                                  checkpoint_every=10000L,run_for=NULL) {
 saved<-.spades_read_verified(file.path(checkpoint_dir,'request.rds'))
 r<-saved$request
 if(!identical(saved$key,.spades_flight_hash(r)))stop('Request fingerprint mismatch.')
 if(!identical(r$version,as.character(utils::packageVersion('SPADES.IND'))) ||
    !identical(r$R,R.version.string) ||
    !identical(r$packages,vapply(names(r$packages),function(p)as.character(utils::packageVersion(p)),'')))
   stop('Resume requires the original R and SPADES.IND/XGB/BN/Matrix versions.')
 .spades_with_rng_kind(r$rng_kind,
   spades_flight(r$model,r$n_pool,r$chains,workers,r$seed,r$cfg,saved$cache_dir,verbose,
     checkpoint_dir=checkpoint_dir,checkpoint_every=checkpoint_every,run_for=run_for))
}
