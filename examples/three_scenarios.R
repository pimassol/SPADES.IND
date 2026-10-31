# Run explicitly after installing SPADES.IND. Uses artificial data only.
local({
  if (!requireNamespace("SPADES.IND", quietly = TRUE)) stop("Install SPADES.IND first.")
  withr::local_seed(444L)
  d <- data.frame(x = rnorm(120), group = rep(letters[1:8], 15),
                  category = rep(LETTERS[1:6], 20))
  d$income <- 10 + 3*d$x + as.integer(factor(d$group)) + rnorm(120)
  small_run <- function(data, target, mode) SPADES.IND::spades_synthesise(
    data, n=8L, target=target, mode=mode, n_pool=20L, n_bins=4L,
    chains=1L, workers=1L, seed=84L, pairs=list(), conditional_on="group",
    fit_control=list(n_fake=30L,nrounds=3L,bn_restart=0L,nthread=1L),
    flight_control=list(burnin=3L,thin=1L,log_every=20L),
    landing_control=list(L=30L,pilot=10L,log_every=30L))
  categorical <- d
  categorical$income <- factor(ifelse(d$income > median(d$income), "high", "low"))
  a <- small_run(categorical, "income", "classification")
  b <- small_run(d, "income", "regression")
  c <- small_run(d, NULL, "none")
  print(lapply(list(classification=a, regression=b, no_response=c), function(z) head(z$data)))
  # These tiny settings demonstrate API use; they do not establish statistical quality.
})
