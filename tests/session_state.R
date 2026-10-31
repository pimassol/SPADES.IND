# Package functions must not leak objects, RNG changes, options or working directory.
local({
 library(SPADES.IND)
 ns<-asNamespace('SPADES.IND')
 seeded<-get('.spades_with_seed',ns)
 with_kind<-get('.spades_with_rng_kind',ns)
 snapshot<-function()list(
   names=ls(globalenv(),all.names=TRUE),
   seed=if(exists('.Random.seed',globalenv(),inherits=FALSE))get('.Random.seed',globalenv(),inherits=FALSE) else NULL,
   kind=RNGkind(),options=options(),wd=getwd(),search=search())
 # Load dependencies before comparing snapshots: first-time namespace initialization
 # is not part of the package's statistical computation.
 loadNamespace('withr');loadNamespace('rngtools')
 verify<-function() {
   before<-snapshot()
   a<-seeded(81L,runif(20L))
   stopifnot(identical(before,snapshot()))
   expected<-withr::with_preserve_seed({set.seed(81L);runif(20L)})
   stopifnot(identical(a,expected),identical(before,snapshot()))
   err<-try(seeded(82L,{runif(5L);stop('intentional test error')}),silent=TRUE)
   stopifnot(inherits(err,'try-error'),identical(before,snapshot()))
   kind<-c("L'Ecuyer-CMRG",'Inversion','Rejection')
   invisible(with_kind(kind,runif(4L)))
   stopifnot(identical(before,snapshot()))
   err<-try(with_kind(kind,{runif(4L);stop('intentional kind error')}),silent=TRUE)
   stopifnot(inherits(err,'try-error'),identical(before,snapshot()))
   state<-seeded(81L,{runif(7L);rngtools::RNGseed()})
   continued<-seeded(999L,{rngtools::RNGseed(state);runif(13L)})
   stopifnot(identical(continued,a[8:20]),identical(before,snapshot()))
   # Exercise the public SA API and its private fidelity accumulator as well.
   ref<-data.frame(a=factor(rep(letters[1:4],10L)),b=factor(rep(LETTERS[1:5],8L)))
   pool<-ref[rep(seq_len(nrow(ref)),2L),,drop=FALSE]
   z<-spades_land(pool,ref,20L,control=list(L=80L,pilot=10L,log_every=80L),verbose=FALSE)
   stopifnot(nrow(z$data)==20L,identical(before,snapshot()))
 }
 # In a clean R test process this also exercises the initially absent-seed case.
 initial<-snapshot()
 verify()
 stopifnot(identical(initial,snapshot()))
 withr::with_preserve_seed({set.seed(197L);verify()})
 stopifnot(identical(initial,snapshot()))
 cat('PASS: caller state preserved on success/error; exact RNG continuation; public landing isolation.\n')
})
