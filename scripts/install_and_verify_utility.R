local({
  # Locate this script from source(), without user-specific directories.
  candidates <- vapply(sys.frames(),function(e)if(is.null(e$ofile))'' else as.character(e$ofile)[1],character(1))
  candidates <- candidates[nzchar(candidates)]
  if(!length(candidates))stop('Run this entry point with source(path/to/install_and_verify_utility.R).')
  pkg <- dirname(dirname(normalizePath(tail(candidates,1),winslash='/',mustWork=TRUE)))
  desc <- read.dcf(file.path(pkg,'DESCRIPTION'))
  if(desc[1,'Package']!='SPADES.IND'||desc[1,'Version']!='0.1.8')stop('Unexpected package source.')
  deps <- c('Rcpp','withr','rngtools','ranger','rpart','xgboost','bnlearn')
  absent <- deps[!vapply(deps,requireNamespace,logical(1),quietly=TRUE)]
  if(length(absent))stop('Install required test dependencies first: ',paste(absent,collapse=', '))
  if('SPADES.IND'%in%loadedNamespaces())stop('Restart R before installation: SPADES.IND DLL is loaded.')
  cat('INSTALL | SPADES.IND 0.1.8 | external utility, one fixed seed\n')
  utils::install.packages(pkg,repos=NULL,type='source',INSTALL_opts='--preclean')
  if(as.character(utils::packageVersion('SPADES.IND'))!='0.1.8')stop('Installation failed.')
  exe <- file.path(R.home('bin'),if(.Platform$OS.type=='windows')'Rscript.exe' else 'Rscript')
  logdir <- tempfile('SPADES_018_validation_',tmpdir=dirname(pkg));dir.create(logdir)
  for(nm in c('invariants.R','session_state.R','audits.R','external_utility.R','response_modes.R','response_dynamic.R','three_scenarios.R')) {
    cat('TEST |',nm,'| small artificial data only\n');flush.console()
    logfile <- file.path(logdir,paste0(nm,'.log'))
    status <- system2(exe,c('--vanilla',shQuote(file.path(pkg,'tests',nm))),stdout=logfile,stderr=logfile)
    cat(tail(readLines(logfile,warn=FALSE),12L),sep='\n')
    if(status!=0L)stop('Test failed: ',nm,'; full log: ',logfile)
  }
  cat('\nINSTALLATION AND TARGETED TESTS PASSED.\n')
  cat('No production flight, landing, MIA or external case-study models rerun.\n')
  cat('Logs:',logdir,'\nFull R CMD check and documentation consolidation remain the next steps.\n')
})
