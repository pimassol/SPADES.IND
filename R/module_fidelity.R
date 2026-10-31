# Extracted from distributional_fidelity_diagnostics.R. Original algorithms are unchanged.
# Never source the case-study driver when loading the package.
.spades_fidelity <- local({

to_char_df <- function(df) {
  df <- as.data.frame(df)
  df[] <- lapply(df, function(x) {
    x <- as.character(x)
    x[is.na(x)] <- "<NA>"
    x
  })
  df
}

align_common_cols <- function(real, proposed, tabdiff) {

  real <- as.data.table(real)
  proposed <- as.data.table(proposed)
  tabdiff <- as.data.table(tabdiff)

  common_cols <- Reduce(
    intersect,
    list(names(real), names(proposed), names(tabdiff))
  )

  if (length(common_cols) == 0L) {
    stop("Nessuna colonna comune tra real, proposed e TabDiff.")
  }

  list(
    real = real[, ..common_cols],
    proposed = proposed[, ..common_cols],
    tabdiff = tabdiff[, ..common_cols],
    cols = common_cols
  )
}

safe_prop <- function(x, levels_ref) {
  p <- prop.table(table(factor(x, levels = levels_ref)))
  as.numeric(p)
}

jsd_one_var <- function(real_x, syn_x, eps = 1e-12) {

  lev <- union(unique(real_x), unique(syn_x))

  p <- safe_prop(real_x, lev) + eps
  q <- safe_prop(syn_x, lev) + eps

  p <- p / sum(p)
  q <- q / sum(q)

  m <- 0.5 * (p + q)

  kl_pm <- sum(p * log(p / m))
  kl_qm <- sum(q * log(q / m))

  sqrt(0.5 * kl_pm + 0.5 * kl_qm)
}

marginal_fidelity <- function(real, syn, method_name) {

  cols <- names(real)

  detail <- rbindlist(lapply(cols, function(v) {

    lev <- union(unique(real[[v]]), unique(syn[[v]]))

    pr <- safe_prop(real[[v]], lev)
    ps <- safe_prop(syn[[v]], lev)

    data.table(
      method = method_name,
      variable = v,
      level = lev,
      p_real = pr,
      p_syn = ps,
      abs_diff = abs(pr - ps)
    )
  }))

  summary <- detail[
    ,
    .(
      mean_abs_diff = mean(abs_diff),
      max_abs_diff = max(abs_diff),
      total_variation = 0.5 * sum(abs_diff),
      jensen_shannon = jsd_one_var(
        real[[variable[1L]]],
        syn[[variable[1L]]]
      )
    ),
    by = .(method, variable)
  ]

  list(detail = detail, summary = summary)
}

cramers_v <- function(x, y) {

  tab <- table(x, y)

  if (nrow(tab) < 2L || ncol(tab) < 2L) {
    return(NA_real_)
  }

  chi <- suppressWarnings(chisq.test(tab, correct = FALSE))

  n <- sum(tab)
  r <- nrow(tab)
  k <- ncol(tab)

  sqrt(as.numeric(chi$statistic) / (n * min(r - 1L, k - 1L)))
}

pairwise_cramer <- function(df, method_name) {

  vars <- names(df)
  pairs <- combn(vars, 2L, simplify = FALSE)

  rbindlist(lapply(pairs, function(pp) {

    data.table(
      method = method_name,
      var1 = pp[1L],
      var2 = pp[2L],
      cramer_v = cramers_v(df[[pp[1L]]], df[[pp[2L]]])
    )
  }))
}

association_fidelity <- function(real, syn, method_name) {

  real_cv <- pairwise_cramer(real, "real")
  syn_cv <- pairwise_cramer(syn, method_name)

  out <- merge(
    real_cv[, .(var1, var2, cramer_real = cramer_v)],
    syn_cv[, .(var1, var2, cramer_syn = cramer_v)],
    by = c("var1", "var2"),
    all = TRUE
  )

  out[, method := method_name]
  out[, abs_error := abs(cramer_syn - cramer_real)]

  out[]
}

auc_manual <- function(y, score) {

  pos <- score[y == 1L]
  neg <- score[y == 0L]

  if (length(pos) == 0L || length(neg) == 0L) {
    return(NA_real_)
  }

  mean(outer(pos, neg, FUN = ">")) +
    0.5 * mean(outer(pos, neg, FUN = "=="))
}

prepare_detection_data <- function(real, syn) {

  real_dt <- as.data.table(real)
  syn_dt <- as.data.table(syn)

  real_dt[, source := "real"]
  syn_dt[, source := "synthetic"]

  dat <- rbindlist(
    list(real_dt, syn_dt),
    use.names = TRUE,
    fill = TRUE
  )

  dat[, source := factor(source, levels = c("synthetic", "real"))]

  feature_cols <- setdiff(names(dat), "source")

  for (v in feature_cols) {
    dat[[v]] <- factor(dat[[v]])
  }

  dat
}

logistic_detection_auc <- function(real,
                                   syn,
                                   method_name,
                                   train_frac = 0.70,
                                   n_trees = 500L) {

  dat <- prepare_detection_data(real, syn)

  withr::local_seed(SEED)
  train_idx <- sample.int(nrow(dat), floor(train_frac * nrow(dat)))

  train <- dat[train_idx]
  test <- dat[-train_idx]

  rf_fit <- ranger(
    source ~ .,
    data = train,
    probability = TRUE,
    num.trees = n_trees,
    mtry = max(1L, floor(sqrt(ncol(train) - 1L))),
    min.node.size = 5L,
    respect.unordered.factors = "order",
    seed = SEED
  )

  score <- predict(rf_fit, data = test)$predictions[, "real"]
  truth <- as.integer(test$source == "real")

  auc <- auc_manual(truth, score)

  pred_class <- as.integer(score >= 0.5)

  TP <- sum(pred_class == 1L & truth == 1L)
  FP <- sum(pred_class == 1L & truth == 0L)
  TN <- sum(pred_class == 0L & truth == 0L)
  FN <- sum(pred_class == 0L & truth == 1L)

  data.table(
    method = method_name,
    detection_auc = auc,
    detection_advantage = abs(auc - 0.5),
    detection_accuracy = mean(pred_class == truth),
    detection_balanced_accuracy = 0.5 * (
      TP / max(1L, TP + FN) +
        TN / max(1L, TN + FP)
    )
  )
}

environment()
})
