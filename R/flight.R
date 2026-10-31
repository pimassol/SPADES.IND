# Experimental fit/flight API. Legacy adaptive acceptance is retained.

.spades_fake_mixture <- function(X_real, n_fake_xgb) {
make_key <- function(df) {
  apply(df, 1L, paste, collapse = "||")
}

remove_duplicates_vs_real <- function(df, X_real) {
  real_keys <- make_key(X_real)
  fake_keys <- make_key(df)

  keep <- !(fake_keys %in% real_keys)
  df <- df[keep, , drop = FALSE]

  fake_keys <- make_key(df)
  df <- df[!duplicated(fake_keys), , drop = FALSE]

  rownames(df) <- NULL
  df
}


domains <- lapply(X_real, function(x) {
  tab <- table(x, useNA = "no")

  list(
    values = names(tab),
    prob = as.numeric(tab) / sum(tab)
  )
})


n_indep <- as.integer(0.40 * n_fake_xgb)

fake_indep <- as.data.frame(
  lapply(domains, function(d) {
    sample(
      x = d$values,
      size = n_indep,
      replace = TRUE,
      prob = d$prob
    )
  }),
  stringsAsFactors = FALSE
)

names(fake_indep) <- names(X_real)


n_shuffle <- as.integer(0.30 * n_fake_xgb)

idx <- sample(seq_len(nrow(X_real)), n_shuffle, replace = TRUE)
fake_shuffle <- X_real[idx, , drop = FALSE]

n_cols_shuffle <- min(ncol(X_real), max(3L, floor(0.50 * ncol(X_real))))

cols_shuffle <- sample(
  names(fake_shuffle),
  size = n_cols_shuffle,
  replace = FALSE
)

for (j in cols_shuffle) {
  fake_shuffle[[j]] <- sample(fake_shuffle[[j]])
}

rownames(fake_shuffle) <- NULL


n_broken <- as.integer(0.20 * n_fake_xgb)

idx <- sample(seq_len(nrow(X_real)), n_broken, replace = TRUE)
fake_broken <- X_real[idx, , drop = FALSE]

n_break_vars <- min(ncol(X_real), max(3L, floor(0.35 * ncol(X_real))))

for (i in seq_len(nrow(fake_broken))) {
  vars_break <- sample(
    names(fake_broken),
    size = n_break_vars,
    replace = FALSE
  )

  for (v in vars_break) {
    d <- domains[[v]]

    fake_broken[i, v] <- sample(
      x = d$values,
      size = 1L,
      replace = TRUE,
      prob = d$prob
    )
  }
}

rownames(fake_broken) <- NULL


n_hard <- n_fake_xgb -
  n_indep -
  n_shuffle -
  n_broken

idx <- sample(seq_len(nrow(X_real)), n_hard, replace = TRUE)
fake_hard <- X_real[idx, , drop = FALSE]

for (i in seq_len(nrow(fake_hard))) {
  vars_mod <- sample(
    names(fake_hard),
    size = min(ncol(X_real), sample(2:4, 1)),
    replace = FALSE
  )

  for (v in vars_mod) {
    d <- domains[[v]]

    current_val <- as.character(fake_hard[i, v])
    alt_vals <- setdiff(d$values, current_val)

    if (length(alt_vals) > 0L) {
      fake_hard[i, v] <- sample(alt_vals, 1L)
    }
  }
}

rownames(fake_hard) <- NULL


X_fake <- rbind(
  fake_indep,
  fake_shuffle,
  fake_broken,
  fake_hard
)

rownames(X_fake) <- NULL


X_fake <- remove_duplicates_vs_real(
  df = X_fake,
  X_real = X_real
)


attempt <- 0L
while (nrow(X_fake) < n_fake_xgb && attempt < 100L) {
  attempt <- attempt + 1L
  n_need <- n_fake_xgb - nrow(X_fake)

  extra_fake <- as.data.frame(
    lapply(domains, function(d) {
      sample(
        x = d$values,
        size = n_need * 2L,
        replace = TRUE,
        prob = d$prob
      )
    }),
    stringsAsFactors = FALSE
  )

  names(extra_fake) <- names(X_real)

  extra_fake <- remove_duplicates_vs_real(
    df = extra_fake,
    X_real = X_real
  )

  X_fake <- rbind(X_fake, extra_fake)

  fake_keys <- make_key(X_fake)
  X_fake <- X_fake[!duplicated(fake_keys), , drop = FALSE]

  rownames(X_fake) <- NULL
}


if (nrow(X_fake) > n_fake_xgb) {
  X_fake <- X_fake[
    sample(seq_len(nrow(X_fake)), n_fake_xgb),
    ,
    drop = FALSE
  ]
}

rownames(X_fake) <- NULL


for (cc in names(X_real)) {
  X_fake[[cc]] <- factor(
    as.character(X_fake[[cc]]),
    levels = levels(X_real[[cc]]),
    ordered = is.ordered(X_real[[cc]])
  )
}

if (nrow(X_fake) < n_fake_xgb) stop("Insufficient distinct negative records; reduce n_fake.")
X_fake
}

.spades_flight_functions <- function() {
  for (pkg in c("xgboost", "Matrix", "bnlearn")) {
    if (!requireNamespace(pkg, quietly = TRUE)) stop("Install package: ", pkg)
  }
  f <- spades_functions("core", state = list(xgb_ordinal_missing_value = -1))
  for (pkg in c("xgboost", "Matrix", "bnlearn")) {
    for (nm in getNamespaceExports(pkg)) {
      if (!exists(nm, f, inherits = FALSE)) assign(nm, getExportedValue(pkg, nm), f)
    }
  }
  assign("as", methods::as, f)
  f
}

.spades_positive_integer <- function(x, name, minimum = 1L) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) ||
      x < minimum || x > .Machine$integer.max || x != floor(x)) {
    stop(name, " must be an integer >= ", minimum, ".")
  }
  as.integer(x)
}

# Defaults reproduce the V8 training configuration; they are not tuned defaults.
spades_fit <- function(prepared, seed = 123L, n_fake = 15000L,
                       nrounds = 900L, bn_restart = 50L,
                       bn_blacklist = NULL, nthread = 1L) {
  if (!inherits(prepared, "spades_prepared")) stop("Use spades_prepare first.")
  .spades_check_prepared(prepared)
  x <- prepared$data
  if (nrow(x) < 10L || ncol(x) < 2L) stop("At least 10 rows and 2 columns are required.")
  if (anyNA(x)) {
    stop("Prepared data contain NA. Check preparation first.")
  }
  if (any(vapply(x, function(z) length(unique(z)) < 2L, logical(1)))) {
    stop("Constant columns are not supported by this experimental fit.")
  }
  if (any(vapply(x, function(z) any(grepl("||", as.character(z), fixed = TRUE)), logical(1)))) {
    stop("Category labels must not contain the reserved separator ||.")
  }
  n_fake <- .spades_positive_integer(n_fake, "n_fake", 10L)
  nrounds <- .spades_positive_integer(nrounds, "nrounds")
  bn_restart <- .spades_positive_integer(bn_restart, "bn_restart", 0L)
  nthread <- .spades_positive_integer(nthread, "nthread")
  f <- .spades_flight_functions()
  .spades_with_seed(seed, {
    schema <- f$build_factor_schema(x)
    maps <- f$build_ordinal_numeric_maps(schema, prepared$schema$ordinal)
    fake <- .spades_fake_mixture(x, n_fake)
    all <- rbind(x, fake)
    y <- c(rep(1L, nrow(x)), rep(0L, nrow(fake)))
    design <- f$build_xgb_mixed_matrix(all, character(), prepared$schema$nominal,
      prepared$schema$ordinal, prepared$schema$numeric, schema, maps)
    # Stratification ensures both classes occur in training and validation.
    train <- unlist(lapply(split(seq_along(y), y), function(ii) {
      sample(ii, max(1L, floor(0.8 * length(ii))))
    }), use.names = FALSE)
    valid <- setdiff(seq_along(y), train)
    dtrain <- xgboost::xgb.DMatrix(design[train, , drop = FALSE], label = y[train])
    dvalid <- xgboost::xgb.DMatrix(design[valid, , drop = FALSE], label = y[valid])
    params <- list(objective = "binary:logistic", eval_metric = "logloss",
      eta = 0.03, max_depth = 10L, min_child_weight = 4L, subsample = 0.85,
      colsample_bytree = 0.85, gamma = 0.5, lambda = 1.5, alpha = 0,
      tree_method = "hist", nthread = nthread)
    fit <- xgboost::xgb.train(params = params, data = dtrain, nrounds = nrounds,
      watchlist = list(train = dtrain, valid = dvalid),
      early_stopping_rounds = 50L, verbose = 0)
    # Store the native model bytes, not an external pointer, for PSOCK workers.
    raw_model <- list(bytes = xgboost::xgb.save.raw(fit), prediction_fields = list())
    for (nm in c("best_iteration", "best_ntreelimit")) {
      if (!is.null(fit[[nm]])) raw_model$prediction_fields[[nm]] <- fit[[nm]]
    }
    # Every model uses the same complete reference prepared at the boundary.
    # Literal labels such as "Missing" are ordinary categories unless declared.
    bn_data <- droplevels(as.data.frame(lapply(x,function(z)
      factor(as.character(z),levels=levels(z))),check.names=FALSE))
    if(nrow(bn_data)<10L || any(vapply(bn_data,nlevels,integer(1))<2L))
      stop('At least 10 complete rows and two observed levels per variable are required.')
    dag <- bnlearn::hc(bn_data, score = "bic", blacklist = bn_blacklist,
                       restart = bn_restart, perturb = 10L)
    bn <- bnlearn::bn.fit(dag, bn_data, method = "bayes", iss = 10)
    structure(list(prepared = prepared, factor_schema = schema,
      ordinal_numeric_maps = maps, design_colnames = colnames(design),
      xgb_raw = raw_model, fit_bn = bn, seed = seed,
      controls = list(n_fake = n_fake, nrounds = nrounds,
        bn_restart = bn_restart, bn_blacklist = bn_blacklist, nthread = nthread),
      versions = vapply(c("SPADES.IND", "xgboost", "Matrix", "bnlearn"),
        function(p) as.character(utils::packageVersion(p)), character(1))),
      class = "spades_fit")
  })
}

# Hash the full serializable request; native model bytes are part of the key.
.spades_flight_hash <- function(x) {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path), add = TRUE)
  saveRDS(x, path, version = 2, compress = FALSE)
  unname(tools::md5sum(path))
}

