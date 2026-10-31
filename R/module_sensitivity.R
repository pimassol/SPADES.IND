# Extracted from sensitivity_analysis.R. Original algorithms are unchanged.
# Never source the case-study driver when loading the package.
.spades_sensitivity <- local({

row_key <- function(df) {
  apply(
    as.data.frame(lapply(df, as.character), stringsAsFactors = FALSE),
    1L,
    paste,
    collapse = "||"
  )
}

to_factor_like_real <- function(real_df, pool_df) {
  common_cols <- intersect(names(real_df), names(pool_df))

  real_df <- real_df[, common_cols, drop = FALSE]
  pool_df <- pool_df[, common_cols, drop = FALSE]

  for (cc in common_cols) {
    levs <- sort(unique(c(
      as.character(real_df[[cc]]),
      as.character(pool_df[[cc]])
    )))

    real_df[[cc]] <- factor(as.character(real_df[[cc]]), levels = levs)
    pool_df[[cc]] <- factor(as.character(pool_df[[cc]]), levels = levs)
  }

  list(real = real_df, pool = pool_df)
}

safe_auc <- function(labels, scores) {
  ok <- is.finite(scores) & !is.na(labels)
  labels <- labels[ok]
  scores <- scores[ok]

  if (length(unique(labels)) != 2L) {
    return(NA_real_)
  }

  r <- rank(scores, ties.method = "average")
  n1 <- sum(labels == 1L)
  n0 <- sum(labels == 0L)

  auc <- (sum(r[labels == 1L]) - n1 * (n1 + 1) / 2) / (n1 * n0)
  as.numeric(auc)
}

build_real_marginals <- function(X_real, cols) {
  out <- vector("list", length(cols))
  names(out) <- cols

  for (cc in cols) {
    out[[cc]] <- prop.table(table(as.character(X_real[[cc]])))
  }

  out
}

build_pool_codes <- function(X_pool, real_marginals, cols) {
  levels_list <- vector("list", length(cols))
  names(levels_list) <- cols

  codes_list <- vector("list", length(cols))
  names(codes_list) <- cols

  p_real_list <- vector("list", length(cols))
  names(p_real_list) <- cols

  for (cc in cols) {
    x <- as.character(X_pool[[cc]])

    levs <- union(names(real_marginals[[cc]]), unique(x))
    levs <- sort(levs)

    levels_list[[cc]] <- levs
    codes_list[[cc]] <- match(x, levs)

    p_real <- as.numeric(real_marginals[[cc]][levs])
    p_real[is.na(p_real)] <- 0

    p_real_list[[cc]] <- p_real
  }

  list(
    levels_list = levels_list,
    codes_list = codes_list,
    p_real_list = p_real_list
  )
}

build_subset_counts <- function(idx, pool_code_obj, cols) {
  counts <- vector("list", length(cols))
  names(counts) <- cols

  for (cc in cols) {
    levs <- pool_code_obj$levels_list[[cc]]
    codes <- pool_code_obj$codes_list[[cc]]

    counts[[cc]] <- tabulate(
      codes[idx],
      nbins = length(levs)
    )
  }

  counts
}

compute_objective_from_counts <- function(
  counts,
  p_real_list,
  target_n,
  cols,
  mode = "energy",
  lambda_max = 1.0,
  lambda_mean = 0.5,
  lambda_hot = 5.0,
  tau_drift = 0.08,
  rf_hit_sum = NULL,
  acc_real = NULL,
  lambda_rf_acc = 0.0
) {
  dist_by_var <- numeric(length(cols))
  names(dist_by_var) <- cols

  for (cc in cols) {
    p_syn <- counts[[cc]] / target_n
    p_real <- p_real_list[[cc]]

    dist_by_var[cc] <- mean(abs(p_real - p_syn), na.rm = TRUE)
  }

  H_max <- max(dist_by_var, na.rm = TRUE)
  H_mean <- mean(dist_by_var, na.rm = TRUE)
  H_hot <- sum(pmax(0, dist_by_var - tau_drift)^2, na.rm = TRUE)

  if (is.null(rf_hit_sum) || is.null(acc_real)) {
    acc_syn <- NA_real_
    acc_gap <- NA_real_
    H_rf <- 0
  } else {
    acc_syn <- rf_hit_sum / target_n
    acc_gap <- acc_syn - acc_real
    H_rf <- lambda_rf_acc * (acc_gap^2)
  }

  if (mode == "max") {
    objective_core <- H_max
  } else if (mode == "mean") {
    objective_core <- H_mean
  } else {
    objective_core <-
      lambda_max * H_max +
      lambda_mean * H_mean +
      lambda_hot * H_hot
  }

  objective <- objective_core + H_rf

  list(
    objective = objective,
    objective_core = objective_core,
    dist_by_var = dist_by_var,
    H_max = H_max,
    H_mean = H_mean,
    H_hot = H_hot,
    H_rf = H_rf,
    acc_syn = acc_syn,
    acc_gap = acc_gap
  )
}

update_counts_swap <- function(counts, idx_out, idx_in, pool_code_obj, cols) {
  new_counts <- counts

  for (cc in cols) {
    codes <- pool_code_obj$codes_list[[cc]]

    out_codes <- codes[idx_out]
    in_codes <- codes[idx_in]

    delta_out <- tabulate(
      out_codes,
      nbins = length(new_counts[[cc]])
    )

    delta_in <- tabulate(
      in_codes,
      nbins = length(new_counts[[cc]])
    )

    new_counts[[cc]] <- new_counts[[cc]] - delta_out + delta_in
  }

  new_counts
}

select_subset_sa_marginal <- function(
  X_pool,
  target_n,
  pool_code_obj,
  cols,
  rf_hit_pool,
  acc_real,
  L = 150000L,
  burnin = 750L,
  T0 = 0.75,
  Tf = 1e-5,
  alpha = 0.99975,
  objective_mode = "energy",
  swap_n = 20L,
  seed = 123L,
  lambda_max = 1.5,
  lambda_mean = 1.0,
  lambda_hot = 20.0,
  tau_drift = 0.04,
  lambda_rf_acc = 75.0
) {
  withr::local_seed(seed)

  n_pool <- nrow(X_pool)
  target_n <- as.integer(target_n)

  if (n_pool < target_n) {
    stop("Pool smaller than target.")
  }

  swap_n <- max(
    1L,
    min(
      as.integer(swap_n),
      target_n,
      n_pool - target_n
    )
  )

  idx_current <- sample(
    seq_len(n_pool),
    size = target_n,
    replace = FALSE
  )

  selected <- rep(FALSE, n_pool)
  selected[idx_current] <- TRUE

  counts_current <- build_subset_counts(
    idx = idx_current,
    pool_code_obj = pool_code_obj,
    cols = cols
  )

  rf_hit_sum_current <- sum(rf_hit_pool[idx_current])

  obj_current_list <- compute_objective_from_counts(
    counts = counts_current,
    p_real_list = pool_code_obj$p_real_list,
    target_n = target_n,
    cols = cols,
    mode = objective_mode,
    lambda_max = lambda_max,
    lambda_mean = lambda_mean,
    lambda_hot = lambda_hot,
    tau_drift = tau_drift,
    rf_hit_sum = rf_hit_sum_current,
    acc_real = acc_real,
    lambda_rf_acc = lambda_rf_acc
  )

  obj_current <- obj_current_list$objective
  obj_core_current <- obj_current_list$objective_core
  H_max_current <- obj_current_list$H_max
  H_mean_current <- obj_current_list$H_mean
  H_hot_current <- obj_current_list$H_hot
  H_rf_current <- obj_current_list$H_rf
  acc_syn_current <- obj_current_list$acc_syn
  acc_gap_current <- obj_current_list$acc_gap
  dist_current <- obj_current_list$dist_by_var

  idx_best <- idx_current
  obj_best <- Inf
  dist_best <- dist_current
  H_max_best <- H_max_current
  H_mean_best <- H_mean_current
  H_hot_best <- H_hot_current
  H_rf_best <- H_rf_current
  acc_syn_best <- acc_syn_current
  acc_gap_best <- acc_gap_current

  T <- T0
  n_accept_post_burnin <- 0L
  n_accept_total <- 0L

  for (tt in seq_len(L)) {
    is_burnin <- tt <= burnin

    idx_out_pos <- sample(seq_along(idx_current), size = swap_n)
    idx_out <- idx_current[idx_out_pos]

    idx_not_current <- which(!selected)
    idx_in <- sample(idx_not_current, size = swap_n)

    idx_prop <- idx_current
    idx_prop[idx_out_pos] <- idx_in

    counts_prop <- update_counts_swap(
      counts = counts_current,
      idx_out = idx_out,
      idx_in = idx_in,
      pool_code_obj = pool_code_obj,
      cols = cols
    )

    rf_hit_sum_prop <-
      rf_hit_sum_current -
      sum(rf_hit_pool[idx_out]) +
      sum(rf_hit_pool[idx_in])

    obj_prop_list <- compute_objective_from_counts(
      counts = counts_prop,
      p_real_list = pool_code_obj$p_real_list,
      target_n = target_n,
      cols = cols,
      mode = objective_mode,
      lambda_max = lambda_max,
      lambda_mean = lambda_mean,
      lambda_hot = lambda_hot,
      tau_drift = tau_drift,
      rf_hit_sum = rf_hit_sum_prop,
      acc_real = acc_real,
      lambda_rf_acc = lambda_rf_acc
    )

    obj_prop <- obj_prop_list$objective

    log_alpha <- (obj_current - obj_prop) / max(T, .Machine$double.eps)
    log_alpha_capped <- min(0, log_alpha)

    accept <- log(runif(1)) < log_alpha_capped

    if (accept) {
      n_accept_total <- n_accept_total + 1L

      selected[idx_out] <- FALSE
      selected[idx_in] <- TRUE

      idx_current <- idx_prop
      counts_current <- counts_prop
      rf_hit_sum_current <- rf_hit_sum_prop

      obj_current <- obj_prop
      obj_core_current <- obj_prop_list$objective_core
      H_max_current <- obj_prop_list$H_max
      H_mean_current <- obj_prop_list$H_mean
      H_hot_current <- obj_prop_list$H_hot
      H_rf_current <- obj_prop_list$H_rf
      acc_syn_current <- obj_prop_list$acc_syn
      acc_gap_current <- obj_prop_list$acc_gap
      dist_current <- obj_prop_list$dist_by_var

      if (!is_burnin) {
        n_accept_post_burnin <- n_accept_post_burnin + 1L

        T <- Tf + (T0 - Tf) *
          (alpha ^ n_accept_post_burnin)
      }
    }

    if (!is_burnin && obj_current < obj_best) {
      idx_best <- idx_current
      obj_best <- obj_current

      dist_best <- dist_current
      H_max_best <- H_max_current
      H_mean_best <- H_mean_current
      H_hot_best <- H_hot_current
      H_rf_best <- H_rf_current
      acc_syn_best <- acc_syn_current
      acc_gap_best <- acc_gap_current
    }

    if (tt %% 10000L == 0L) {
      cat(
        " iter =", tt,
        "| H_best =", signif(obj_best, 6),
        "| H_current =", signif(obj_current, 6),
        "| acc_syn =", signif(acc_syn_current, 6),
        "| H_max =", signif(H_max_current, 6),
        "| H_mean =", signif(H_mean_current, 6),
        "| H_hot =", signif(H_hot_current, 6),
        "| H_rf =", signif(H_rf_current, 6),
        "| T =", signif(T, 6),
        "\n"
      )
    }
  }

  if (!is.finite(obj_best)) {
    idx_best <- idx_current
    obj_best <- obj_current
    dist_best <- dist_current
    H_max_best <- H_max_current
    H_mean_best <- H_mean_current
    H_hot_best <- H_hot_current
    H_rf_best <- H_rf_current
    acc_syn_best <- acc_syn_current
    acc_gap_best <- acc_gap_current
  }

  attr(idx_best, "sa_best_energy") <- obj_best
  attr(idx_best, "sa_best_dist_by_var") <- dist_best
  attr(idx_best, "sa_best_H_max") <- H_max_best
  attr(idx_best, "sa_best_H_mean") <- H_mean_best
  attr(idx_best, "sa_best_H_hot") <- H_hot_best
  attr(idx_best, "sa_best_H_rf") <- H_rf_best
  attr(idx_best, "sa_best_acc_syn") <- acc_syn_best
  attr(idx_best, "sa_best_acc_gap") <- acc_gap_best
  attr(idx_best, "sa_acceptance_rate") <- n_accept_total / L
  attr(idx_best, "sa_acceptance_post_burnin") <- n_accept_post_burnin

  idx_best
}

df_to_char_matrix <- function(df) {
  as.matrix(as.data.frame(lapply(df, as.character), stringsAsFactors = FALSE))
}

min_hamming_to_reference <- function(query_df, ref_df, chunk_size = 500L) {
  Q <- df_to_char_matrix(query_df)
  R <- df_to_char_matrix(ref_df)

  nq <- nrow(Q)
  nr <- nrow(R)
  p <- ncol(Q)

  out <- numeric(nq)

  for (i1 in seq(1L, nq, by = chunk_size)) {
    i2 <- min(i1 + chunk_size - 1L, nq)
    q_chunk <- Q[i1:i2, , drop = FALSE]

    d_min <- rep(Inf, nrow(q_chunk))

    for (j in seq_len(nr)) {
      d <- rowMeans(q_chunk != matrix(
        R[j, ],
        nrow = nrow(q_chunk),
        ncol = p,
        byrow = TRUE
      ))

      d_min <- pmin(d_min, d)
    }

    out[i1:i2] <- d_min
  }

  out
}

estimate_synthetic_diversity <- function(X_syn, n_pairs = 100000L, seed = 123L) {
  withr::local_seed(seed)

  n <- nrow(X_syn)
  if (n < 2L) {
    return(NA_real_)
  }

  M <- df_to_char_matrix(X_syn)
  p <- ncol(M)

  i <- sample(seq_len(n), size = n_pairs, replace = TRUE)
  j <- sample(seq_len(n), size = n_pairs, replace = TRUE)

  ok <- i != j
  i <- i[ok]
  j <- j[ok]

  mean(rowMeans(M[i, , drop = FALSE] != M[j, , drop = FALSE]))
}

generate_nonmembers_from_real_marginals <- function(
  real_marginals,
  n,
  seed = 123L
) {
  withr::local_seed(seed)

  out <- data.frame(matrix(NA_character_, nrow = n, ncol = length(real_marginals)))
  names(out) <- names(real_marginals)

  for (cc in names(real_marginals)) {
    probs <- as.numeric(real_marginals[[cc]])
    levs <- names(real_marginals[[cc]])

    out[[cc]] <- sample(
      levs,
      size = n,
      replace = TRUE,
      prob = probs
    )
  }

  out
}

compute_privacy_diagnostics <- function(
  X_syn,
  X_real,
  real_marginals,
  chunk_size = 500L,
  diversity_n_pairs = 100000L,
  mia_nonmember_n = 10000L,
  seed = 123L
) {
  key_syn <- row_key(X_syn)
  key_real <- row_key(X_real)

  exact_overlaps <- sum(key_syn %in% key_real)
  overlap_rate <- exact_overlaps / nrow(X_syn)

  synthetic_duplicates <- sum(duplicated(key_syn))
  duplicate_rate <- synthetic_duplicates / nrow(X_syn)

  d_syn_real <- min_hamming_to_reference(
    query_df = X_syn,
    ref_df = X_real,
    chunk_size = chunk_size
  )

  mean_nn_distance <- mean(d_syn_real)
  share_distance_eq_0 <- mean(d_syn_real == 0)
  share_distance_leq_1 <- mean(d_syn_real <= (1 / ncol(X_syn)))

  synthetic_diversity_mean <- estimate_synthetic_diversity(
    X_syn = X_syn,
    n_pairs = diversity_n_pairs,
    seed = seed
  )

  nonmembers <- generate_nonmembers_from_real_marginals(
    real_marginals = real_marginals,
    n = min(mia_nonmember_n, nrow(X_real)),
    seed = seed + 777L
  )

  nonmembers <- nonmembers[, names(X_real), drop = FALSE]

  d_real_to_syn <- min_hamming_to_reference(
    query_df = X_real,
    ref_df = X_syn,
    chunk_size = chunk_size
  )

  d_non_to_syn <- min_hamming_to_reference(
    query_df = nonmembers,
    ref_df = X_syn,
    chunk_size = chunk_size
  )

  labels <- c(rep(1L, length(d_real_to_syn)), rep(0L, length(d_non_to_syn)))
  scores <- -c(d_real_to_syn, d_non_to_syn)

  mia_auc <- safe_auc(labels, scores)

  best_adv <- NA_real_
  if (is.finite(mia_auc)) {
    thr <- sort(unique(scores))
    if (length(thr) > 200L) {
      thr <- quantile(thr, probs = seq(0, 1, length.out = 200), na.rm = TRUE)
      thr <- sort(unique(as.numeric(thr)))
    }

    adv <- numeric(length(thr))

    for (k in seq_along(thr)) {
      pred <- as.integer(scores >= thr[k])
      tpr <- mean(pred[labels == 1L] == 1L)
      fpr <- mean(pred[labels == 0L] == 1L)
      adv[k] <- tpr - fpr
    }

    best_adv <- max(adv, na.rm = TRUE)
  }

  data.frame(
    exact_overlaps = exact_overlaps,
    overlap_rate = overlap_rate,
    synthetic_duplicates = synthetic_duplicates,
    duplicate_rate = duplicate_rate,
    mean_nn_distance = mean_nn_distance,
    share_distance_eq_0 = share_distance_eq_0,
    share_distance_leq_1 = share_distance_leq_1,
    synthetic_diversity_mean = synthetic_diversity_mean,
    mia_auc = mia_auc,
    mia_advantage = best_adv,
    stringsAsFactors = FALSE
  )
}

scale01_safe <- function(x, reverse = FALSE) {
  rng <- range(x, na.rm = TRUE)

  if (!all(is.finite(rng)) || diff(rng) == 0) {
    z <- rep(0.5, length(x))
  } else {
    z <- (x - rng[1]) / diff(rng)
  }

  if (reverse) {
    z <- 1 - z
  }

  z
}

build_macro_dimensions <- function(summary_df) {
  summary_df$utility_acc_component <-
    scale01_safe(summary_df$acc_syn, reverse = FALSE)

  summary_df$utility_drift_component <-
    scale01_safe(summary_df$H_mean, reverse = TRUE)

  summary_df$utility_maxdrift_component <-
    scale01_safe(summary_df$H_max, reverse = TRUE)

  summary_df$utility_index <- rowMeans(
    summary_df[, c(
      "utility_acc_component",
      "utility_drift_component",
      "utility_maxdrift_component"
    )],
    na.rm = TRUE
  )

  summary_df$privacy_overlap_component <-
    scale01_safe(summary_df$overlap_rate, reverse = TRUE)

  summary_df$privacy_nn_component <-
    scale01_safe(summary_df$mean_nn_distance, reverse = FALSE)

  summary_df$privacy_mia_component <-
    scale01_safe(abs(summary_df$mia_auc - 0.5), reverse = TRUE)

  summary_df$privacy_dup_component <-
    scale01_safe(summary_df$duplicate_rate, reverse = TRUE)

  summary_df$privacy_index <- rowMeans(
    summary_df[, c(
      "privacy_overlap_component",
      "privacy_nn_component",
      "privacy_mia_component",
      "privacy_dup_component"
    )],
    na.rm = TRUE
  )

  summary_df
}

is_pareto <- function(utility, privacy) {
  n <- length(utility)
  out <- rep(TRUE, n)

  for (i in seq_len(n)) {
    dominated <- any(
      utility >= utility[i] &
        privacy >= privacy[i] &
        (utility > utility[i] | privacy > privacy[i])
    )

    out[i] <- !dominated
  }

  out
}

environment()
})
