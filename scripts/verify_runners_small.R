local({
  frames <- vapply(sys.frames(),function(e) if(is.null(e$ofile)) "" else as.character(e$ofile)[1],character(1))
  frames <- frames[nzchar(frames)]
  if(!length(frames)) stop("Run with source(path/to/verify_runners_small.R).")
  pkg <- dirname(dirname(normalizePath(tail(frames,1),winslash="/",mustWork=TRUE)))
  api <- new.env(parent=baseenv())
  sys.source(file.path(pkg,"scripts","workflow_runners.R"),envir=api)
  api$require_spades()
  paper <- api$load_paper_inputs(pkg)
  cat("PASS | repository paper inputs: hashes, schema and fixed split; no fitting.\n")
  rm(paper)
  out <- tempfile("SPADES_runner_validation_",tmpdir=dirname(pkg))
  dir.create(out)
  cat("VALIDATION | small artificial data only; all model dependencies required.\n")
  exe <- file.path(R.home("bin"),if(.Platform$OS.type=="windows")"Rscript.exe" else "Rscript")
  tests <- list.files(file.path(pkg,"tests"),pattern="[.]R$",full.names=TRUE)
  if(length(tests)!=7L) stop("Expected seven package test suites.")
  for(test in tests) {
    log <- file.path(out,paste0(basename(test),".log"))
    status <- system2(exe,c("--vanilla",shQuote(test)),stdout=log,stderr=log)
    lines <- readLines(log,warn=FALSE)
    cat(tail(lines,5L),sep="\n")
    if(status!=0L || any(grepl("^SKIP",trimws(lines)))) stop("Failed or skipped tests: ",log)
  }
  withr::local_seed(444L)
  d <- data.frame(x=rnorm(120),g=rep(letters[1:8],15),h=rep(LETTERS[1:6],20))
  d$y <- 10+3*d$x+as.integer(factor(d$g))+rnorm(120)
  for(mode in c("classification","regression","none")) {
    input <- d
    if(mode=="classification") input$y <- factor(ifelse(d$y>median(d$y),"high","low"))
    args <- list(data=input,out_dir=file.path(out,mode),n=8L,
      target=if(mode=="none")NULL else "y",mode=mode,n_pool=20L,n_bins=4L,
      chains=2L,workers=2L,seed=84L,pairs=list(c("x","y")),conditional_on="g",
      fit_control=list(n_fake=30L,nrounds=3L,bn_restart=0L,nthread=1L),
      flight_control=list(burnin=3L,thin=1L,log_every=20L),
      landing_control=list(L=30L,pilot=10L,log_every=30L),verbose=FALSE)
    a <- do.call(api$run_generation,args)
    b <- do.call(api$run_generation,args)
    stopifnot(identical(a,b),nrow(a$data)==8L,api$audit_fidelity(a)$summary$changed_bins==0L)
    changed <- args; changed$seed <- 85L
    stopifnot(inherits(try(do.call(api$run_generation,changed),silent=TRUE),"try-error"))
    cat("PASS runner |",mode,"| two workers, reconstruction, exact completed reuse, changed request rejected.\n")
  }
  # Small, independent membership audit; enough unique profiles in each class.
  m <- data.frame(x=rnorm(260),g=rep(letters[1:5],52))
  h <- data.frame(x=rnorm(260),g=rep(letters[1:5],52))
  prepared <- SPADES.IND::spades_prepare(m,n_bins=4L,verbose=FALSE)
  land <- SPADES.IND::spades_land(prepared$data,prepared,30L,seed=71L,
    control=list(L=30L,pilot=10L,log_every=30L),verbose=FALSE)
  w <- list(prepared=prepared,landing=land,pool=prepared$data,mode="none",
    categorical=land$data,data=SPADES.IND::spades_reconstruct(land$data,prepared,seed=72L))
  pa <- api$run_privacy(w,h,file.path(out,"privacy"),seeds=c(7101L,7102L),threads=2L)
  pb <- api$run_privacy(w,h,file.path(out,"privacy"),seeds=c(7101L,7102L),threads=2L)
  stopifnot(isTRUE(all.equal(pa,pb)))
  api$run_target_stability(w,h,file.path(out,"stability"),seeds=c(8101L,8102L),
    threads=2L,control=list(L=30L,pilot=10L,log_every=30L),mia_seeds=7101L)
  targets <- lapply(1:2,function(i)readRDS(file.path(out,"stability",sprintf("target_%02d",i),"released.rds")))
  stopifnot(all(vapply(targets,nrow,integer(1))==30L))
  cat("PASS | MIA, exact cache reuse and two short landings on one fixed pool.\n")
  # Exercise the external runner itself: three models, one fixed seed.
  make <- function(n) { x<-rnorm(n);data.frame(x=x,g=rep(c("a","b"),length.out=n),
    y=ifelse(x+rnorm(n)>0,"yes","no")) }
  ref<-make(80); ho<-make(40); syn<-list(first=make(80),second=make(80))
  u<-api$run_external_utility(ref,syn,ho,"y",file.path(out,"utility"),
    threads=1L,control=list(trees=5L,nrounds=3L))
  stopifnot(nrow(u$metrics)==9L,all(u$metrics$seed==20261026L))
  writeLines("PASS: all required package suites and runner smoke checks completed.",file.path(out,"VALIDATION_PASS.txt"))
  cat("ALL RUNNER CHECKS PASSED. No paper experiments rerun.\nLogs:",out,"\n")
})
