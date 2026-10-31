# Extracted from synthetic_data_generator_parallel_V8.0.R. Original algorithms are unchanged.
# Never source the case-study driver when loading the package.
.spades_core <- local({

clamp <- function(x, a = 1e-8, b = 1 - 1e-8) {
  pmin(pmax(x, a), b)
}

safe_as_char <- function(x) {
  x_chr <- as.character(x)
  x_chr[is.na(x_chr)] <- "__MISSING__"
  x_chr
}

row_key <- function(df) {
  apply(df, 1, function(z) paste(z, collapse = "||"))
}

split_integer_total <- function(total, k) {
  if (k <= 0L) stop("k deve essere > 0")
  base <- total %/% k
  remn <- total %% k
  out <- rep(base, k)
  if (remn > 0L) out[seq_len(remn)] <- out[seq_len(remn)] + 1L
  out
}

df_to_char_matrix <- function(df) {
  as.matrix(data.frame(
    lapply(df, as.character),
    stringsAsFactors = FALSE,
    check.names = FALSE
  ))
}

infer_ordered_levels <- function(vals_chr) {
  vals_chr_raw <- vals_chr
  vals_chr <- as.character(vals_chr)

  has_missing <- any(is.na(vals_chr_raw)) ||
    any(vals_chr == "__MISSING__", na.rm = TRUE)

  vals_chr[is.na(vals_chr)] <- "__MISSING__"

  u <- unique(vals_chr)
  non_missing <- u[u != "__MISSING__"]

  if (length(non_missing) == 0L) {
    return("__MISSING__")
  }

  num_try <- suppressWarnings(as.numeric(non_missing))

  if (all(!is.na(num_try))) {
    ord <- non_missing[order(num_try)]
  } else {
    ord <- non_missing
  }

  if (has_missing) {
    c(ord, "__MISSING__")
  } else {
    ord
  }
}

min_hamming_to_reference <- function(df_a, df_b, block_size = 1000L) {
  A <- df_to_char_matrix(df_a)
  B <- df_to_char_matrix(df_b)
  nA <- nrow(A)
  out <- rep(NA_real_, nA)
  starts <- seq.int(1L, nA, by = block_size)

  for (s in starts) {
    e <- min(s + block_size - 1L, nA)
    AA <- A[s:e, , drop = FALSE]

    for (i in seq_len(nrow(AA))) {
      cmp <- B != matrix(
        AA[i, ],
        nrow = nrow(B),
        ncol = ncol(B),
        byrow = TRUE
      )
      out[s + i - 1L] <- min(rowMeans(cmp))
    }
  }

  out
}

build_numeric_discretizer <- function(
  x,
  n_bins = 20L,
  method = c("quantile", "equal_width"),
  prefix = "BIN"
) {
  method <- match.arg(method)
  x_num <- suppressWarnings(as.numeric(x))
  has_missing <- any(is.na(x_num))
  x_obs <- x_num[!is.na(x_num)]

  add_missing_level <- function(levs) {
    if (has_missing) {
      c(levs, "__MISSING__")
    } else {
      levs
    }
  }

  if (length(x_obs) == 0L) {
    return(list(
      method = "missing_only",
      breaks = NULL,
      levels = "__MISSING__",
      prefix = prefix
    ))
  }

  if (length(unique(x_obs)) == 1L) {
    levs <- add_missing_level(sprintf("%s_%02d", prefix, 1L))
    return(list(
      method = "constant",
      breaks = NULL,
      levels = levs,
      prefix = prefix,
      constant_value = unique(x_obs)
    ))
  }

  if (method == "quantile") {
    probs <- seq(0, 1, length.out = n_bins + 1L)
    brks <- unique(stats::quantile(
      x_obs,
      probs = probs,
      na.rm = TRUE,
      type = 7
    ))

    if (length(brks) > 2L) {
      levs <- add_missing_level(
        sprintf("%s_%02d", prefix, seq_len(length(brks) - 1L))
      )
      return(list(
        method = "quantile",
        breaks = brks,
        levels = levs,
        prefix = prefix
      ))
    } else {
      method <- "equal_width"
    }
  }

  rng <- range(x_obs, na.rm = TRUE)

  if (!is.finite(rng[1]) || !is.finite(rng[2]) || rng[1] == rng[2]) {
    levs <- add_missing_level(sprintf("%s_%02d", prefix, 1L))
    return(list(
      method = "constant",
      breaks = NULL,
      levels = levs,
      prefix = prefix,
      constant_value = x_obs[1]
    ))
  }

  brks <- seq(rng[1], rng[2], length.out = n_bins + 1L)
  brks[length(brks)] <- brks[length(brks)] + 1e-12

  levs <- add_missing_level(
    sprintf("%s_%02d", prefix, seq_len(length(brks) - 1L))
  )

  list(
    method = "equal_width",
    breaks = brks,
    levels = levs,
    prefix = prefix
  )
}

apply_numeric_discretizer <- function(x, discretizer) {
  x_num <- suppressWarnings(as.numeric(x))
  out <- rep(NA_character_, length(x_num))
  miss <- is.na(x_num)

  if (discretizer$method == "missing_only") {
    out[] <- "__MISSING__"
    return(factor(out, levels = discretizer$levels, ordered = TRUE))
  }

  if (discretizer$method == "constant") {
    out[!miss] <- discretizer$levels[1]
    if (any(miss)) {
      out[miss] <- "__MISSING__"
    }
    return(factor(out, levels = discretizer$levels, ordered = TRUE))
  }

  right_flag <- TRUE
  if (!is.null(discretizer$method) &&
      identical(discretizer$method, "equal_width")) {
    right_flag <- FALSE
  }

  grp <- cut(
    x_num,
    breaks = discretizer$breaks,
    include.lowest = TRUE,
    right = right_flag,
    ordered_result = TRUE
  )

  grp_chr <- as.character(grp)
  levs_grp <- levels(grp)

  levs_nomiss <- discretizer$levels[
    discretizer$levels != "__MISSING__"
  ]

  map <- setNames(
    levs_nomiss[seq_along(levs_grp)],
    levs_grp
  )

  out[!miss] <- unname(map[grp_chr[!miss]])

  if (any(miss)) {
    out[miss] <- "__MISSING__"
  }

  factor(out, levels = discretizer$levels, ordered = TRUE)
}

build_numeric_discretizers <- function(
  df,
  numeric_cols,
  n_bins_numeric = 20L,
  numeric_method = "quantile"
) {
  out <- vector("list", length(numeric_cols))
  names(out) <- numeric_cols

  for (cc in numeric_cols) {
    out[[cc]] <- build_numeric_discretizer(
      x = df[[cc]],
      n_bins = n_bins_numeric,
      method = numeric_method,
      prefix = toupper(cc)
    )
  }

  out
}

make_all_categorical <- function(
  df,
  municipality_col,
  nominal_cols,
  ordinal_cols,
  numeric_cols,
  n_bins_numeric = 20L,
  numeric_method = "quantile",
  numeric_discretizers = NULL
) {
  out <- df[, c(municipality_col, nominal_cols, ordinal_cols, numeric_cols),
            drop = FALSE]

  fac_cols <- c(municipality_col, nominal_cols)

  for (cc in fac_cols) {
    vals <- safe_as_char(out[[cc]])
    out[[cc]] <- factor(vals, ordered = FALSE)
  }

  for (cc in ordinal_cols) {
    levs <- infer_ordered_levels(out[[cc]])
    vals <- safe_as_char(out[[cc]])
    out[[cc]] <- factor(vals, levels = levs, ordered = TRUE)
  }

  if (is.null(numeric_discretizers)) {
    numeric_discretizers <- build_numeric_discretizers(
      df = df,
      numeric_cols = numeric_cols,
      n_bins_numeric = n_bins_numeric,
      numeric_method = numeric_method
    )
  }

  for (cc in numeric_cols) {
    out[[cc]] <- apply_numeric_discretizer(
      x = out[[cc]],
      discretizer = numeric_discretizers[[cc]]
    )
  }

  attr(out, "numeric_discretizers") <- numeric_discretizers
  out
}

generate_fake_records <- function(
  X_real,
  n_fake_target,
  sampling_mode = c("uniform", "empirical"),
  max_iter = 30L,
  oversample_factor = 2.5,
  seed = 123L
) {
  sampling_mode <- match.arg(sampling_mode)
  withr::local_seed(seed)

  real_chr <- data.frame(lapply(X_real, as.character), stringsAsFactors = FALSE)
  real_keys <- unique(row_key(real_chr))

  fake_accum <- NULL
  fake_keys_accum <- character(0)
  iter <- 0L

  while (length(fake_keys_accum) < n_fake_target && iter < max_iter) {
    iter <- iter + 1L

    n_to_draw <- max(
      100L,
      as.integer(ceiling(
        (n_fake_target - length(fake_keys_accum)) * oversample_factor
      ))
    )

    tmp <- vector("list", ncol(real_chr))
    names(tmp) <- names(real_chr)

    for (j in seq_along(tmp)) {
      col_vals <- real_chr[[j]]
      levs <- unique(col_vals)

      if (sampling_mode == "uniform") {
        tmp[[j]] <- sample(levs, size = n_to_draw, replace = TRUE)
      } else {
        tab <- table(col_vals)
        pr <- as.numeric(tab) / sum(tab)
        tmp[[j]] <- sample(
          names(tab),
          size = n_to_draw,
          replace = TRUE,
          prob = pr
        )
      }
    }

    fake_batch <- as.data.frame(tmp, stringsAsFactors = FALSE)
    fake_batch_keys <- row_key(fake_batch)

    keep1 <- !(fake_batch_keys %in% real_keys)
    fake_batch <- fake_batch[keep1, , drop = FALSE]
    fake_batch_keys <- fake_batch_keys[keep1]

    keep2 <- !duplicated(fake_batch_keys)
    fake_batch <- fake_batch[keep2, , drop = FALSE]
    fake_batch_keys <- fake_batch_keys[keep2]

    keep3 <- !(fake_batch_keys %in% fake_keys_accum)
    fake_batch <- fake_batch[keep3, , drop = FALSE]
    fake_batch_keys <- fake_batch_keys[keep3]

    if (nrow(fake_batch) > 0L) {
      if (is.null(fake_accum)) {
        fake_accum <- fake_batch
      } else {
        fake_accum <- rbind(fake_accum, fake_batch)
      }
      fake_keys_accum <- c(fake_keys_accum, fake_batch_keys)
    }

    cat(sprintf(
      "[FAKE GEN] iter=%d  accumulati=%d / %d\n",
      iter,
      length(fake_keys_accum),
      n_fake_target
    ))
  }

  if (length(fake_keys_accum) < n_fake_target) {
    warning(sprintf(
      paste0(
        "Generati solo %d fake unici non coincidenti con i reali, ",
        "inferiori al target %d."
      ),
      length(fake_keys_accum),
      n_fake_target
    ))
  }

  fake_accum <- fake_accum[
    seq_len(min(n_fake_target, nrow(fake_accum))),
    ,
    drop = FALSE
  ]

  X_fake <- fake_accum

  for (cc in names(X_real)) {
    X_fake[[cc]] <- factor(
      X_fake[[cc]],
      levels = levels(X_real[[cc]]),
      ordered = is.ordered(X_real[[cc]])
    )
  }

  X_fake
}

build_sparse_design_matrix <- function(df_cat) {
  df_tmp <- df_cat
  for (cc in names(df_tmp)) {
    if (!is.factor(df_tmp[[cc]])) df_tmp[[cc]] <- factor(df_tmp[[cc]])
  }
  sparse.model.matrix(~ . - 1, data = df_tmp)
}

align_sparse_cols <- function(mm, target_cols) {
  mm <- as(mm, "dgCMatrix")
  miss_cols <- setdiff(target_cols, colnames(mm))

  if (length(miss_cols) > 0L) {
    zero_block <- Matrix(
      0,
      nrow = nrow(mm),
      ncol = length(miss_cols),
      sparse = TRUE
    )
    colnames(zero_block) <- miss_cols
    mm <- cbind(mm, zero_block)
  }

  extra_cols <- setdiff(colnames(mm), target_cols)

  if (length(extra_cols) > 0L) {
    keep <- setdiff(colnames(mm), extra_cols)
    mm <- mm[, keep, drop = FALSE]
  }

  mm[, target_cols, drop = FALSE]
}

build_factor_schema <- function(df_cat) {
  lapply(df_cat, function(x) {
    list(
      levels = levels(x),
      ordered = is.ordered(x)
    )
  })
}

cast_record_to_schema <- function(rec_df, factor_schema) {
  out <- rec_df
  for (cc in names(out)) {
    out[[cc]] <- factor(
      as.character(out[[cc]]),
      levels = factor_schema[[cc]]$levels,
      ordered = factor_schema[[cc]]$ordered
    )
  }
  out
}

build_ordinal_numeric_maps <- function(factor_schema, ordinal_cols) {
  out <- vector("list", length(ordinal_cols))
  names(out) <- ordinal_cols

  for (cc in ordinal_cols) {
    levs <- factor_schema[[cc]]$levels
    levs_nomiss <- levs[levs != "__MISSING__"]

    if (length(levs_nomiss) <= 1L) {
      vals <- rep(0, length(levs_nomiss))
    } else {
      vals <- seq(0, 1, length.out = length(levs_nomiss))
    }

    mp <- setNames(vals, levs_nomiss)

    if ("__MISSING__" %in% levs) {
      mp["__MISSING__"] <- xgb_ordinal_missing_value
    }

    out[[cc]] <- mp
  }

  out
}

ordinal_to_scaled_numeric <- function(x, level_map, missing_value = -1) {
  vals <- as.character(x)
  vals[is.na(vals)] <- "__MISSING__"
  out <- unname(level_map[vals])
  out[is.na(out)] <- missing_value
  as.numeric(out)
}

build_xgb_mixed_matrix <- function(
  df_cat,
  municipality_col,
  nominal_cols,
  ordinal_cols,
  numeric_cols,
  factor_schema,
  ordinal_numeric_maps,
  design_colnames = NULL
) {
  df_cat <- cast_record_to_schema(df_cat, factor_schema)

  cat_cols <- unique(c(municipality_col, nominal_cols, numeric_cols))
  cat_cols <- cat_cols[cat_cols %in% names(df_cat)]

  mm_cat <- NULL
  if (length(cat_cols) > 0L) {
    mm_cat <- build_sparse_design_matrix(df_cat[, cat_cols, drop = FALSE])
  }

  mm_ord <- NULL
  if (length(ordinal_cols) > 0L) {
    ord_mat <- matrix(0, nrow = nrow(df_cat), ncol = length(ordinal_cols))
    colnames(ord_mat) <- paste0("ORDNUM__", ordinal_cols)

    for (j in seq_along(ordinal_cols)) {
      cc <- ordinal_cols[j]
      ord_mat[, j] <- ordinal_to_scaled_numeric(
        x = df_cat[[cc]],
        level_map = ordinal_numeric_maps[[cc]],
        missing_value = xgb_ordinal_missing_value
      )
    }

    mm_ord <- Matrix(ord_mat, sparse = TRUE)
  }

  if (is.null(mm_cat) && is.null(mm_ord)) {
    stop("Nessuna colonna disponibile per la design matrix XGBoost.")
  }

  if (is.null(mm_cat)) {
    mm <- mm_ord
  } else if (is.null(mm_ord)) {
    mm <- mm_cat
  } else {
    mm <- cbind(mm_cat, mm_ord)
  }

  mm <- as(mm, "dgCMatrix")

  if (!is.null(design_colnames)) {
    mm <- align_sparse_cols(mm, design_colnames)
  }

  mm
}

build_candidate_sparse_row <- function(
  rec_df,
  factor_schema,
  municipality_col,
  nominal_cols,
  ordinal_cols,
  numeric_cols,
  ordinal_numeric_maps,
  design_colnames
) {
  rec_df <- cast_record_to_schema(rec_df, factor_schema)

  build_xgb_mixed_matrix(
    df_cat = rec_df,
    municipality_col = municipality_col,
    nominal_cols = nominal_cols,
    ordinal_cols = ordinal_cols,
    numeric_cols = numeric_cols,
    factor_schema = factor_schema,
    ordinal_numeric_maps = ordinal_numeric_maps,
    design_colnames = design_colnames
  )
}

compute_p_real_on_df <- function(
  df_cat,
  xgb_fit,
  factor_schema,
  municipality_col,
  nominal_cols,
  ordinal_cols,
  numeric_cols,
  ordinal_numeric_maps,
  design_colnames
) {
  df_cat <- cast_record_to_schema(df_cat, factor_schema)

  mm <- build_xgb_mixed_matrix(
    df_cat = df_cat,
    municipality_col = municipality_col,
    nominal_cols = nominal_cols,
    ordinal_cols = ordinal_cols,
    numeric_cols = numeric_cols,
    factor_schema = factor_schema,
    ordinal_numeric_maps = ordinal_numeric_maps,
    design_colnames = design_colnames
  )

  p <- as.numeric(predict(xgb_fit, newdata = xgb.DMatrix(mm)))
  clamp(p)
}

logit <- function(p) {
  p <- clamp(p)
  qlogis(p)
}

record_key_one <- function(rec_df) {
  paste(unlist(lapply(rec_df, as.character)), collapse = "||")
}

hamming_min_dist_one <- function(rec_df, ref_mat_char) {
  if (is.null(ref_mat_char) || nrow(ref_mat_char) == 0L) return(1)

  x <- as.character(unlist(
    lapply(rec_df[1, , drop = FALSE], as.character),
    use.names = FALSE
  ))

  cmp <- ref_mat_char != matrix(
    x,
    nrow = nrow(ref_mat_char),
    ncol = ncol(ref_mat_char),
    byrow = TRUE
  )

  d <- rowMeans(cmp)
  min(d)
}

make_key <- function(df) {
  apply(df, 1L, paste, collapse = "|")
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

propose_record_bn <- function(
  fit_bn,
  factor_schema
) {

  rec <- rbn(
    x = fit_bn,
    n = 1L
  )

  rec <- as.data.frame(
    rec,
    stringsAsFactors = FALSE
  )

  rec <- rec[, names(factor_schema), drop = FALSE]

  rec <- cast_record_to_schema(
    rec_df = rec,
    factor_schema = factor_schema
  )

  rec
}

log_q_bn_record <- function(
  rec_df,
  fit_bn,
  eps = 1e-12
) {

  rec_df <- as.data.frame(
    rec_df,
    stringsAsFactors = FALSE
  )

  total_log_q <- 0

  for (node in names(fit_bn)) {

    node_obj <- fit_bn[[node]]

    parents <- node_obj$parents
    prob_tab <- node_obj$prob

    node_val <- as.character(rec_df[[node]][1L])

    if (length(parents) == 0L) {

      p <- prob_tab[node_val]

    } else {

      idx <- c(
        list(node_val),
        lapply(parents, function(pp) {
          as.character(rec_df[[pp]][1L])
        })
      )

      p <- do.call(
        "[",
        c(
          list(prob_tab),
          idx,
          list(drop = TRUE)
        )
      )
    }

    p <- as.numeric(p)

    if (!is.finite(p) || is.na(p) || p <= 0) {
      p <- eps
    }

    total_log_q <- total_log_q + log(p)
  }

  total_log_q
}

build_proposal_schema_no_missing <- function(
  factor_schema,
  forbidden_levels = c("Missing", "__MISSING__")
) {

  proposal_schema <- factor_schema

  for (cc in names(proposal_schema)) {

    proposal_schema[[cc]]$levels <- setdiff(
      proposal_schema[[cc]]$levels,
      forbidden_levels
    )

    if (length(proposal_schema[[cc]]$levels) == 0L) {
      stop("Nessun livello valido per: ", cc)
    }
  }

  proposal_schema
}

propose_record_uniform <- function(
  proposal_schema,
  factor_schema
) {

  vals <- lapply(names(proposal_schema), function(cc) {

    levs <- proposal_schema[[cc]]$levels

    sample(
      x = levs,
      size = 1L,
      replace = TRUE
    )
  })

  names(vals) <- names(proposal_schema)

  rec <- as.data.frame(vals, stringsAsFactors = FALSE)

  cast_record_to_schema(
    rec_df = rec,
    factor_schema = factor_schema
  )
}

compute_soft_diversity_energy <- function(
  min_hamming_syn,
  tau_diversity = 0.25
) {

  out <- exp(-min_hamming_syn / tau_diversity)
  out[is.na(min_hamming_syn) | !is.finite(min_hamming_syn)] <- 0

  out
}

compute_soft_privacy_energy <- function(
  min_hamming_real,
  tau_privacy = 0.25
) {

  out <- exp(-min_hamming_real / tau_privacy)
  out[is.na(min_hamming_real) | !is.finite(min_hamming_real)] <- 0

  out
}

build_generation_marginal_state <- function(
  X_real,
  factor_schema,
  cols
) {

  real_marginals <- vector("list", length(cols))
  names(real_marginals) <- cols

  marginal_levels <- vector("list", length(cols))
  names(marginal_levels) <- cols

  for (cc in cols) {

    levs <- factor_schema[[cc]]$levels
    x_real <- as.character(X_real[[cc]])

    real_tab <- prop.table(table(
      factor(x_real, levels = levs)
    ))

    p_real <- as.numeric(real_tab)
    p_real[is.na(p_real)] <- 0

    real_marginals[[cc]] <- setNames(p_real, levs)
    marginal_levels[[cc]] <- levs
  }

  list(
    real_marginals = real_marginals,
    marginal_levels = marginal_levels
  )
}

init_generation_marginal_counts <- function(
  gen_marginal_levels,
  gen_marginal_cols
) {

  counts <- vector("list", length(gen_marginal_cols))
  names(counts) <- gen_marginal_cols

  for (cc in gen_marginal_cols) {
    counts[[cc]] <- setNames(
      rep(0L, length(gen_marginal_levels[[cc]])),
      gen_marginal_levels[[cc]]
    )
  }

  counts
}

compute_generation_marginal_energy <- function(
  rec_df,
  marginal_counts,
  real_marginals,
  marginal_levels,
  n_collected,
  cols,
  weights = NULL,
  marginal_eps = 1e-8
) {

  if (is.null(weights)) {
    weights <- rep(1, length(cols))
    names(weights) <- cols
  }

  penalties <- numeric(length(cols))
  names(penalties) <- cols

  for (cc in cols) {

    val <- as.character(rec_df[[cc]][1])
    levs <- marginal_levels[[cc]]
    pos <- match(val, levs)

    if (is.na(pos)) {
      penalties[cc] <- 0
      next
    }

    cnt_now <- marginal_counts[[cc]][pos]

    p_syn_after <- (cnt_now + 1) / (n_collected + 1)
    p_real <- real_marginals[[cc]][pos]

    penalties[cc] <- max(0, p_syn_after - p_real)
  }

  weighted.mean(
    penalties,
    w = weights[cols],
    na.rm = TRUE
  )
}

update_generation_marginal_counts <- function(
  marginal_counts,
  rec_df,
  marginal_levels,
  cols
) {

  for (cc in cols) {

    val <- as.character(rec_df[[cc]][1])
    pos <- match(val, marginal_levels[[cc]])

    if (!is.na(pos)) {
      marginal_counts[[cc]][pos] <- marginal_counts[[cc]][pos] + 1L
    }
  }

  marginal_counts
}

build_conditional_marginal_state <- function(
  X_real,
  factor_schema,
  base_col,
  cols
) {

  base_levels <- factor_schema[[base_col]]$levels

  conditional_marginals <- vector("list", length(cols))
  names(conditional_marginals) <- cols

  conditional_levels <- vector("list", length(cols))
  names(conditional_levels) <- cols

  for (cc in cols) {

    x_base <- as.character(X_real[[base_col]])
    x_var <- as.character(X_real[[cc]])

    var_levels <- factor_schema[[cc]]$levels

    tab <- table(
      factor(x_base, levels = base_levels),
      factor(x_var, levels = var_levels)
    )

    row_tot <- rowSums(tab)
    prob <- sweep(tab, 1, row_tot, "/")
    prob[is.na(prob)] <- 0

    conditional_marginals[[cc]] <- prob

    conditional_levels[[cc]] <- list(
      base_levels = base_levels,
      var_levels = var_levels
    )
  }

  list(
    conditional_marginals = conditional_marginals,
    conditional_levels = conditional_levels
  )
}

init_conditional_marginal_counts <- function(
  conditional_levels,
  cols
) {

  counts <- vector("list", length(cols))
  names(counts) <- cols

  for (cc in cols) {

    bl <- conditional_levels[[cc]]$base_levels
    vl <- conditional_levels[[cc]]$var_levels

    counts[[cc]] <- matrix(
      0L,
      nrow = length(bl),
      ncol = length(vl),
      dimnames = list(bl, vl)
    )
  }

  counts
}

compute_conditional_marginal_energy <- function(
  rec_df,
  conditional_counts,
  conditional_marginals,
  conditional_levels,
  base_col,
  cols,
  weights = NULL
) {

  if (is.null(weights)) {
    weights <- rep(1, length(cols))
    names(weights) <- cols
  }

  base_val <- as.character(rec_df[[base_col]][1])

  penalties <- numeric(length(cols))
  names(penalties) <- cols

  for (cc in cols) {

    var_val <- as.character(rec_df[[cc]][1])

    bl <- conditional_levels[[cc]]$base_levels
    vl <- conditional_levels[[cc]]$var_levels

    bpos <- match(base_val, bl)
    vpos <- match(var_val, vl)

    if (is.na(bpos) || is.na(vpos)) {
      penalties[cc] <- 0
      next
    }

    row_count_now <- sum(conditional_counts[[cc]][bpos, ])
    cnt_now <- conditional_counts[[cc]][bpos, vpos]

    p_syn_after <- (cnt_now + 1) / (row_count_now + 1)
    p_real <- conditional_marginals[[cc]][bpos, vpos]

    penalties[cc] <- max(0, p_syn_after - p_real)
  }

  weighted.mean(
    penalties,
    w = weights[cols],
    na.rm = TRUE
  )
}

update_conditional_marginal_counts <- function(
  conditional_counts,
  rec_df,
  conditional_levels,
  base_col,
  cols
) {

  base_val <- as.character(rec_df[[base_col]][1])

  for (cc in cols) {

    var_val <- as.character(rec_df[[cc]][1])

    bl <- conditional_levels[[cc]]$base_levels
    vl <- conditional_levels[[cc]]$var_levels

    bpos <- match(base_val, bl)
    vpos <- match(var_val, vl)

    if (!is.na(bpos) && !is.na(vpos)) {
      conditional_counts[[cc]][bpos, vpos] <-
        conditional_counts[[cc]][bpos, vpos] + 1L
    }
  }

  conditional_counts
}

build_pairwise_state <- function(
  X_real,
  factor_schema,
  pair_cols
) {

  pairwise_marginals <- vector("list", length(pair_cols))
  pairwise_levels <- vector("list", length(pair_cols))

  pair_names <- vapply(
    pair_cols,
    function(z) paste(z, collapse = "__"),
    character(1)
  )

  names(pairwise_marginals) <- pair_names
  names(pairwise_levels) <- pair_names

  for (k in seq_along(pair_cols)) {

    v1 <- pair_cols[[k]][1]
    v2 <- pair_cols[[k]][2]

    pair_name <- pair_names[k]

    l1 <- factor_schema[[v1]]$levels
    l2 <- factor_schema[[v2]]$levels

    tab <- table(
      factor(as.character(X_real[[v1]]), levels = l1),
      factor(as.character(X_real[[v2]]), levels = l2)
    )

    prob <- tab / sum(tab)

    pairwise_marginals[[pair_name]] <- prob

    pairwise_levels[[pair_name]] <- list(
      var1 = v1,
      var2 = v2,
      levels1 = l1,
      levels2 = l2
    )
  }

  list(
    pairwise_marginals = pairwise_marginals,
    pairwise_levels = pairwise_levels
  )
}

init_pairwise_counts <- function(pairwise_levels) {

  counts <- vector("list", length(pairwise_levels))
  names(counts) <- names(pairwise_levels)

  for (nm in names(pairwise_levels)) {

    l1 <- pairwise_levels[[nm]]$levels1
    l2 <- pairwise_levels[[nm]]$levels2

    counts[[nm]] <- matrix(
      0L,
      nrow = length(l1),
      ncol = length(l2),
      dimnames = list(l1, l2)
    )
  }

  counts
}

compute_pairwise_energy <- function(
  rec_df,
  pairwise_counts,
  pairwise_marginals,
  pairwise_levels,
  weights = NULL
) {

  pair_names <- names(pairwise_levels)

  if (is.null(weights)) {
    weights <- rep(1, length(pair_names))
    names(weights) <- pair_names
  }

  penalties <- numeric(length(pair_names))
  names(penalties) <- pair_names

  for (nm in pair_names) {

    v1 <- pairwise_levels[[nm]]$var1
    v2 <- pairwise_levels[[nm]]$var2

    val1 <- as.character(rec_df[[v1]][1])
    val2 <- as.character(rec_df[[v2]][1])

    l1 <- pairwise_levels[[nm]]$levels1
    l2 <- pairwise_levels[[nm]]$levels2

    p1 <- match(val1, l1)
    p2 <- match(val2, l2)

    if (is.na(p1) || is.na(p2)) {
      penalties[nm] <- 0
      next
    }

    n_now <- sum(pairwise_counts[[nm]])
    cnt_now <- pairwise_counts[[nm]][p1, p2]

    p_syn_after <- (cnt_now + 1) / (n_now + 1)
    p_real <- pairwise_marginals[[nm]][p1, p2]

    penalties[nm] <- max(0, p_syn_after - p_real)
  }

  weighted.mean(
    penalties,
    w = weights[pair_names],
    na.rm = TRUE
  )
}

update_pairwise_counts <- function(
  pairwise_counts,
  rec_df,
  pairwise_levels
) {

  for (nm in names(pairwise_levels)) {

    v1 <- pairwise_levels[[nm]]$var1
    v2 <- pairwise_levels[[nm]]$var2

    val1 <- as.character(rec_df[[v1]][1])
    val2 <- as.character(rec_df[[v2]][1])

    p1 <- match(val1, pairwise_levels[[nm]]$levels1)
    p2 <- match(val2, pairwise_levels[[nm]]$levels2)

    if (!is.na(p1) && !is.na(p2)) {
      pairwise_counts[[nm]][p1, p2] <-
        pairwise_counts[[nm]][p1, p2] + 1L
    }
  }

  pairwise_counts
}

score_candidate_energy <- function(
  rec_df,
  xgb_fit,
  factor_schema,
  municipality_col,
  nominal_cols,
  ordinal_cols,
  numeric_cols,
  ordinal_numeric_maps,
  design_colnames,
  real_keys_unique,
  real_ref_mat_char,
  syn_keys_existing = character(0),
  syn_ref_mat_char = NULL,
  lambda_plausibility = 1.0,
  lambda_diversity = 0.70,
  lambda_privacy = 1.00,
  tau_diversity = 0.25,
  tau_privacy = 0.25,
  lambda_gen_marginal = 10.0,
  gen_real_marginals = NULL,
  gen_marginal_counts = NULL,
  gen_marginal_levels = NULL,
  gen_marginal_cols = NULL,
  gen_marginal_weights = NULL,
  lambda_gen_conditional = 0.0,
  gen_conditional_base_col = NULL,
  gen_conditional_cols = NULL,
  gen_conditional_marginals = NULL,
  gen_conditional_counts = NULL,
  gen_conditional_levels = NULL,
  gen_conditional_weights = NULL,
  lambda_gen_pairwise = 0.0,
  gen_pairwise_marginals = NULL,
  gen_pairwise_counts = NULL,
  gen_pairwise_levels = NULL,
  gen_pairwise_weights = NULL,
  n_collected_current = 0L,
  marginal_eps = 1e-8
) {

  rec_df <- cast_record_to_schema(rec_df, factor_schema)
  rec_key <- record_key_one(rec_df)

  exact_real <- rec_key %in% real_keys_unique
  exact_syn <- rec_key %in% syn_keys_existing

  if (exact_real || exact_syn) {
    return(list(
      p_real = 0,
      h1_plausibility = Inf,
      h2_diversity = Inf,
      h3_privacy = Inf,
      h4_marginal = Inf,
      h5_conditional = Inf,
      h6_pairwise = Inf,
      H = Inf,
      exact_real = exact_real,
      exact_syn = exact_syn,
      min_hamming_real = ifelse(exact_real, 0, NA_real_),
      min_hamming_syn = ifelse(exact_syn, 0, NA_real_)
    ))
  }

  mm <- build_candidate_sparse_row(
    rec_df = rec_df,
    factor_schema = factor_schema,
    municipality_col = municipality_col,
    nominal_cols = nominal_cols,
    ordinal_cols = ordinal_cols,
    numeric_cols = numeric_cols,
    ordinal_numeric_maps = ordinal_numeric_maps,
    design_colnames = design_colnames
  )

  p_real <- as.numeric(predict(xgb_fit, newdata = xgb.DMatrix(mm)))
  p_real <- clamp(p_real)

  min_hamming_real <- hamming_min_dist_one(
    rec_df = rec_df,
    ref_mat_char = real_ref_mat_char
  )

  if (is.null(syn_ref_mat_char) || nrow(syn_ref_mat_char) == 0L) {
    min_hamming_syn <- 1
  } else {
    min_hamming_syn <- hamming_min_dist_one(
      rec_df = rec_df,
      ref_mat_char = syn_ref_mat_char
    )
  }

  h1 <- 1 - p_real

  h2 <- compute_soft_diversity_energy(
    min_hamming_syn = min_hamming_syn,
    tau_diversity = tau_diversity
  )

  h3 <- compute_soft_privacy_energy(
    min_hamming_real = min_hamming_real,
    tau_privacy = tau_privacy
  )

  h4 <- 0
  if (
    lambda_gen_marginal > 0 &&
    !is.null(gen_real_marginals) &&
    !is.null(gen_marginal_counts) &&
    !is.null(gen_marginal_levels) &&
    !is.null(gen_marginal_cols)
  ) {
    h4 <- compute_generation_marginal_energy(
      rec_df = rec_df,
      marginal_counts = gen_marginal_counts,
      real_marginals = gen_real_marginals,
      marginal_levels = gen_marginal_levels,
      n_collected = n_collected_current,
      cols = gen_marginal_cols,
      weights = gen_marginal_weights,
      marginal_eps = marginal_eps
    )
  }

  h5 <- 0
  if (
    lambda_gen_conditional > 0 &&
    !is.null(gen_conditional_base_col) &&
    !is.null(gen_conditional_cols) &&
    !is.null(gen_conditional_marginals) &&
    !is.null(gen_conditional_counts) &&
    !is.null(gen_conditional_levels)
  ) {
    h5 <- compute_conditional_marginal_energy(
      rec_df = rec_df,
      conditional_counts = gen_conditional_counts,
      conditional_marginals = gen_conditional_marginals,
      conditional_levels = gen_conditional_levels,
      base_col = gen_conditional_base_col,
      cols = gen_conditional_cols,
      weights = gen_conditional_weights
    )
  }

  h6 <- 0
  if (
    lambda_gen_pairwise > 0 &&
    !is.null(gen_pairwise_marginals) &&
    !is.null(gen_pairwise_counts) &&
    !is.null(gen_pairwise_levels)
  ) {
    h6 <- compute_pairwise_energy(
      rec_df = rec_df,
      pairwise_counts = gen_pairwise_counts,
      pairwise_marginals = gen_pairwise_marginals,
      pairwise_levels = gen_pairwise_levels,
      weights = gen_pairwise_weights
    )
  }

  H <- lambda_plausibility * h1 +
    lambda_diversity * h2 +
    lambda_privacy * h3 +
    lambda_gen_marginal * h4 +
    lambda_gen_conditional * h5 +
    lambda_gen_pairwise * h6

  list(
    p_real = p_real,
    h1_plausibility = h1,
    h2_diversity = h2,
    h3_privacy = h3,
    h4_marginal = h4,
    h5_conditional = h5,
    h6_pairwise = h6,
    H = H,
    exact_real = FALSE,
    exact_syn = FALSE,
    min_hamming_real = min_hamming_real,
    min_hamming_syn = min_hamming_syn
  )
}

run_mh_chain_bn <- function(
  chain_id,
  n_target_chain,
  mh_max_iter,
  mh_burnin,
  mh_thin,
  fit_bn,
  factor_schema,
  xgb_fit,
  municipality_col,
  nominal_cols,
  ordinal_cols,
  numeric_cols,
  ordinal_numeric_maps,
  design_colnames,
  real_keys_unique,
  real_ref_mat_char,
  seed,
  lambda_plausibility = 1.0,
  lambda_diversity = 0.70,
  lambda_privacy = 1.00,
  tau_diversity = 0.25,
  tau_privacy = 0.25,
  lambda_gen_marginal = 10.0,
  gen_real_marginals = NULL,
  gen_marginal_levels = NULL,
  gen_marginal_cols = NULL,
  gen_marginal_weights = NULL,
  lambda_gen_conditional = 0.0,
  gen_conditional_base_col = NULL,
  gen_conditional_cols = NULL,
  gen_conditional_marginals = NULL,
  gen_conditional_levels = NULL,
  gen_conditional_weights = NULL,
  lambda_gen_pairwise = 0.0,
  gen_pairwise_marginals = NULL,
  gen_pairwise_levels = NULL,
  gen_pairwise_weights = NULL,
  marginal_eps = 1e-8
) {

  withr::local_seed(seed)

  accepted_syn <- NULL
  accepted_syn_keys <- character(0)
  accepted_syn_mat_char <- NULL

  gen_marginal_counts <- init_generation_marginal_counts(
    gen_marginal_levels = gen_marginal_levels,
    gen_marginal_cols = gen_marginal_cols
  )

  gen_conditional_counts <- init_conditional_marginal_counts(
    conditional_levels = gen_conditional_levels,
    cols = gen_conditional_cols
  )

  gen_pairwise_counts <- init_pairwise_counts(
    pairwise_levels = gen_pairwise_levels
  )

  n_accept <- 0L
  n_reject <- 0L
  n_collected <- 0L
  final_iter <- mh_max_iter

  current_rec <- NULL
  current_sc <- NULL
  current_log_q <- NA_real_

  init_ok <- FALSE
  init_iter <- 0L

  while (!init_ok && init_iter < 10000L) {

    init_iter <- init_iter + 1L

    cand <- propose_record_bn(
      fit_bn = fit_bn,
      factor_schema = factor_schema
    )

    cand_sc <- score_candidate_energy(
      rec_df = cand,
      xgb_fit = xgb_fit,
      factor_schema = factor_schema,
      municipality_col = municipality_col,
      nominal_cols = nominal_cols,
      ordinal_cols = ordinal_cols,
      numeric_cols = numeric_cols,
      ordinal_numeric_maps = ordinal_numeric_maps,
      design_colnames = design_colnames,
      real_keys_unique = real_keys_unique,
      real_ref_mat_char = real_ref_mat_char,
      syn_keys_existing = character(0),
      syn_ref_mat_char = NULL,
      lambda_plausibility = lambda_plausibility,
      lambda_diversity = lambda_diversity,
      lambda_privacy = lambda_privacy,
      tau_diversity = tau_diversity,
      tau_privacy = tau_privacy,
      lambda_gen_marginal = lambda_gen_marginal,
      gen_real_marginals = gen_real_marginals,
      gen_marginal_counts = gen_marginal_counts,
      gen_marginal_levels = gen_marginal_levels,
      gen_marginal_cols = gen_marginal_cols,
      gen_marginal_weights = gen_marginal_weights,
      lambda_gen_conditional = lambda_gen_conditional,
      gen_conditional_base_col = gen_conditional_base_col,
      gen_conditional_cols = gen_conditional_cols,
      gen_conditional_marginals = gen_conditional_marginals,
      gen_conditional_counts = gen_conditional_counts,
      gen_conditional_levels = gen_conditional_levels,
      gen_conditional_weights = gen_conditional_weights,
      lambda_gen_pairwise = lambda_gen_pairwise,
      gen_pairwise_marginals = gen_pairwise_marginals,
      gen_pairwise_counts = gen_pairwise_counts,
      gen_pairwise_levels = gen_pairwise_levels,
      gen_pairwise_weights = gen_pairwise_weights,
      n_collected_current = 0L,
      marginal_eps = marginal_eps
    )

    cand_log_q <- log_q_bn_record(
      rec_df = cand,
      fit_bn = fit_bn
    )

    if (is.finite(cand_sc$H) && is.finite(cand_log_q)) {
      current_rec <- cand
      current_sc <- cand_sc
      current_log_q <- cand_log_q
      init_ok <- TRUE
    }
  }

  if (!init_ok) {
    stop("Impossibile inizializzare la catena BN con un record valido.")
  }

  mh_trace <- vector("list", mh_max_iter)

  for (iter in seq_len(mh_max_iter)) {

    prop_rec <- propose_record_bn(
      fit_bn = fit_bn,
      factor_schema = factor_schema
    )

    prop_log_q <- log_q_bn_record(
      rec_df = prop_rec,
      fit_bn = fit_bn
    )

    prop_sc <- score_candidate_energy(
      rec_df = prop_rec,
      xgb_fit = xgb_fit,
      factor_schema = factor_schema,
      municipality_col = municipality_col,
      nominal_cols = nominal_cols,
      ordinal_cols = ordinal_cols,
      numeric_cols = numeric_cols,
      ordinal_numeric_maps = ordinal_numeric_maps,
      design_colnames = design_colnames,
      real_keys_unique = real_keys_unique,
      real_ref_mat_char = real_ref_mat_char,
      syn_keys_existing = accepted_syn_keys,
      syn_ref_mat_char = accepted_syn_mat_char,
      lambda_plausibility = lambda_plausibility,
      lambda_diversity = lambda_diversity,
      lambda_privacy = lambda_privacy,
      tau_diversity = tau_diversity,
      tau_privacy = tau_privacy,
      lambda_gen_marginal = lambda_gen_marginal,
      gen_real_marginals = gen_real_marginals,
      gen_marginal_counts = gen_marginal_counts,
      gen_marginal_levels = gen_marginal_levels,
      gen_marginal_cols = gen_marginal_cols,
      gen_marginal_weights = gen_marginal_weights,
      lambda_gen_conditional = lambda_gen_conditional,
      gen_conditional_base_col = gen_conditional_base_col,
      gen_conditional_cols = gen_conditional_cols,
      gen_conditional_marginals = gen_conditional_marginals,
      gen_conditional_counts = gen_conditional_counts,
      gen_conditional_levels = gen_conditional_levels,
      gen_conditional_weights = gen_conditional_weights,
      lambda_gen_pairwise = lambda_gen_pairwise,
      gen_pairwise_marginals = gen_pairwise_marginals,
      gen_pairwise_counts = gen_pairwise_counts,
      gen_pairwise_levels = gen_pairwise_levels,
      gen_pairwise_weights = gen_pairwise_weights,
      n_collected_current = n_collected,
      marginal_eps = marginal_eps
    )

    accept <- FALSE
    log_alpha <- -Inf

    if (
      is.finite(prop_sc$H) &&
      is.finite(current_sc$H) &&
      is.finite(prop_log_q) &&
      is.finite(current_log_q)
    ) {

      log_alpha <-
        (current_sc$H - prop_sc$H) +
        (current_log_q - prop_log_q)

      accept <- log(runif(1)) < min(0, log_alpha)
    }

    if (accept) {
      current_rec <- prop_rec
      current_sc <- prop_sc
      current_log_q <- prop_log_q
      n_accept <- n_accept + 1L
    } else {
      n_reject <- n_reject + 1L
    }

    collect_now <- (iter > mh_burnin) &&
      (((iter - mh_burnin) %% mh_thin) == 0L)

    if (collect_now && is.finite(current_sc$H)) {

      current_key <- record_key_one(current_rec)

      if (!(current_key %in% accepted_syn_keys) &&
          !(current_key %in% real_keys_unique)) {

        accepted_syn_keys <- c(accepted_syn_keys, current_key)

        if (is.null(accepted_syn)) {
          accepted_syn <- current_rec
        } else {
          accepted_syn <- rbind(accepted_syn, current_rec)
        }

        current_char <- df_to_char_matrix(current_rec)

        if (is.null(accepted_syn_mat_char)) {
          accepted_syn_mat_char <- current_char
        } else {
          accepted_syn_mat_char <- rbind(
            accepted_syn_mat_char,
            current_char
          )
        }

        n_collected <- n_collected + 1L

        gen_marginal_counts <- update_generation_marginal_counts(
          marginal_counts = gen_marginal_counts,
          rec_df = current_rec,
          marginal_levels = gen_marginal_levels,
          cols = gen_marginal_cols
        )

        gen_conditional_counts <- update_conditional_marginal_counts(
          conditional_counts = gen_conditional_counts,
          rec_df = current_rec,
          conditional_levels = gen_conditional_levels,
          base_col = gen_conditional_base_col,
          cols = gen_conditional_cols
        )

        gen_pairwise_counts <- update_pairwise_counts(
          pairwise_counts = gen_pairwise_counts,
          rec_df = current_rec,
          pairwise_levels = gen_pairwise_levels
        )
      }
    }

    mh_trace[[iter]] <- data.frame(
      chain_id = chain_id,
      iter = iter,
      accepted_move = accept,
      collected_total = n_collected,
      current_p_real = current_sc$p_real,
      current_H = current_sc$H,
      current_h1_plausibility = current_sc$h1_plausibility,
      current_h2_diversity = current_sc$h2_diversity,
      current_h3_privacy = current_sc$h3_privacy,
      current_h4_marginal = current_sc$h4_marginal,
      current_h5_conditional = current_sc$h5_conditional,
      current_h6_pairwise = current_sc$h6_pairwise,
      current_min_hamming_real = current_sc$min_hamming_real,
      current_min_hamming_syn = current_sc$min_hamming_syn,
      current_log_q = current_log_q,
      proposal_log_q = prop_log_q,
      log_alpha = log_alpha,
      stringsAsFactors = FALSE
    )

    if (n_collected >= n_target_chain) {
      final_iter <- iter
      mh_trace <- mh_trace[seq_len(iter)]
      break
    }
  }

  list(
    chain_id = chain_id,
    accepted_syn = accepted_syn,
    accepted_syn_keys = accepted_syn_keys,
    mh_trace_df = do.call(rbind, mh_trace),
    n_collected = if (is.null(accepted_syn)) 0L else nrow(accepted_syn),
    n_accept = n_accept,
    n_reject = n_reject,
    final_iter = final_iter
  )
}

build_real_marginals <- function(X_real, cols) {
  out <- vector("list", length(cols))
  names(out) <- cols

  for (cc in cols) {
    out[[cc]] <- prop.table(table(as.character(X_real[[cc]])))
  }

  out
}

compute_marginal_drift_from_reference <- function(real_marginals, X_syn, cols) {
  out_list <- vector("list", length(cols))
  names(out_list) <- cols

  for (cc in cols) {
    p_real_tab <- real_marginals[[cc]]
    p_syn_tab <- prop.table(table(as.character(X_syn[[cc]])))

    levs <- union(names(p_real_tab), names(p_syn_tab))

    p_real <- as.numeric(p_real_tab[levs])
    p_syn <- as.numeric(p_syn_tab[levs])

    p_real[is.na(p_real)] <- 0
    p_syn[is.na(p_syn)] <- 0

    out_list[[cc]] <- data.frame(
      variable = cc,
      level = levs,
      p_real = p_real,
      p_syn = p_syn,
      abs_diff = abs(p_real - p_syn),
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, out_list)
}

compute_drift_summary_by_variable <- function(drift_df) {
  out <- aggregate(
    abs_diff ~ variable,
    data = drift_df,
    FUN = mean,
    na.rm = TRUE
  )

  names(out) <- c("variable", "distance_mean_abs_diff")
  out <- out[order(out$distance_mean_abs_diff, decreasing = TRUE), ]
  row.names(out) <- NULL
  out
}

compute_drift_summary <- function(drift_df) {
  by_var <- compute_drift_summary_by_variable(drift_df)

  data.frame(
    mean_distance_across_variables =
      mean(by_var$distance_mean_abs_diff, na.rm = TRUE),
    max_distance_across_variables =
      max(by_var$distance_mean_abs_diff, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
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
    H_rf <- 0
    acc_gap <- NA_real_
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
  L = 75000L,
  burnin = 750L,
  T0 = 0.05,
  Tf = 1e-4,
  alpha = 0.9995,
  objective_mode = "energy",
  swap_n = 20L,
  seed = 123L,
  lambda_max = 1.0,
  lambda_mean = 0.5,
  lambda_hot = 5.0,
  tau_drift = 0.08,
  lambda_rf_acc = 0.0
) {
  withr::local_seed(seed)

  n_pool <- nrow(X_pool)
  target_n <- as.integer(target_n)

  if (n_pool <= target_n) {
    idx <- seq_len(n_pool)

    counts <- build_subset_counts(
      idx = idx,
      pool_code_obj = pool_code_obj,
      cols = cols
    )

    rf_hit_sum <- sum(rf_hit_pool[idx])

    ob_obj <- compute_objective_from_counts(
      counts = counts,
      p_real_list = pool_code_obj$p_real_list,
      target_n = length(idx),
      cols = cols,
      mode = objective_mode,
      lambda_max = lambda_max,
      lambda_mean = lambda_mean,
      lambda_hot = lambda_hot,
      tau_drift = tau_drift,
      rf_hit_sum = rf_hit_sum,
      acc_real = acc_real,
      lambda_rf_acc = lambda_rf_acc
    )

    diag <- data.frame(
      iter = 0L,
      burnin = FALSE,
      accepted = TRUE,
      n_accept_post_burnin = 0L,
      T = T0,
      swap_n = 0L,
      objective_current = ob_obj$objective,
      objective_core_current = ob_obj$objective_core,
      H_rf_current = ob_obj$H_rf,
      acc_syn_current = ob_obj$acc_syn,
      acc_gap_current = ob_obj$acc_gap,
      objective_proposal = ob_obj$objective,
      objective_best = ob_obj$objective,
      H_max_current = ob_obj$H_max,
      H_mean_current = ob_obj$H_mean,
      H_hot_current = ob_obj$H_hot,
      H_max_best = ob_obj$H_max,
      H_mean_best = ob_obj$H_mean,
      H_hot_best = ob_obj$H_hot,
      H_rf_best = ob_obj$H_rf,
      acc_syn_best = ob_obj$acc_syn,
      acc_gap_best = ob_obj$acc_gap,
      alpha_accept = 1,
      stringsAsFactors = FALSE
    )

    attr(idx, "sa_diag") <- diag
    attr(idx, "sa_best_dist_by_var") <- ob_obj$dist_by_var
    attr(idx, "sa_best_energy") <- ob_obj$objective
    attr(idx, "sa_best_H_max") <- ob_obj$H_max
    attr(idx, "sa_best_H_mean") <- ob_obj$H_mean
    attr(idx, "sa_best_H_hot") <- ob_obj$H_hot
    attr(idx, "sa_best_H_rf") <- ob_obj$H_rf
    attr(idx, "sa_best_acc_syn") <- ob_obj$acc_syn
    attr(idx, "sa_best_acc_gap") <- ob_obj$acc_gap

    return(idx)
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
  dist_current <- obj_current_list$dist_by_var
  H_max_current <- obj_current_list$H_max
  H_mean_current <- obj_current_list$H_mean
  H_hot_current <- obj_current_list$H_hot
  H_rf_current <- obj_current_list$H_rf
  acc_syn_current <- obj_current_list$acc_syn
  acc_gap_current <- obj_current_list$acc_gap

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

  diag_list <- vector("list", L)

  for (tt in seq_len(L)) {
    is_burnin <- tt <= burnin

    idx_out_pos <- sample(
      seq_along(idx_current),
      size = swap_n,
      replace = FALSE
    )

    idx_out <- idx_current[idx_out_pos]

    idx_not_current <- which(!selected)

    idx_in <- sample(
      idx_not_current,
      size = swap_n,
      replace = FALSE
    )

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
    alpha_accept <- exp(log_alpha_capped)

    accept <- log(runif(1)) < log_alpha_capped

    if (accept) {
      selected[idx_out] <- FALSE
      selected[idx_in] <- TRUE

      idx_current <- idx_prop
      counts_current <- counts_prop
      rf_hit_sum_current <- rf_hit_sum_prop

      obj_current <- obj_prop
      obj_core_current <- obj_prop_list$objective_core
      dist_current <- obj_prop_list$dist_by_var
      H_max_current <- obj_prop_list$H_max
      H_mean_current <- obj_prop_list$H_mean
      H_hot_current <- obj_prop_list$H_hot
      H_rf_current <- obj_prop_list$H_rf
      acc_syn_current <- obj_prop_list$acc_syn
      acc_gap_current <- obj_prop_list$acc_gap

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

    diag_list[[tt]] <- data.frame(
      iter = tt,
      burnin = is_burnin,
      accepted = accept,
      n_accept_post_burnin = n_accept_post_burnin,
      T = T,
      swap_n = swap_n,
      objective_current = obj_current,
      objective_core_current = obj_core_current,
      H_rf_current = H_rf_current,
      acc_syn_current = acc_syn_current,
      acc_gap_current = acc_gap_current,
      objective_proposal = obj_prop,
      objective_best = obj_best,
      H_max_current = H_max_current,
      H_mean_current = H_mean_current,
      H_hot_current = H_hot_current,
      H_max_best = H_max_best,
      H_mean_best = H_mean_best,
      H_hot_best = H_hot_best,
      H_rf_best = H_rf_best,
      acc_syn_best = acc_syn_best,
      acc_gap_best = acc_gap_best,
      alpha_accept = alpha_accept,
      stringsAsFactors = FALSE
    )

    if (tt %% 1000L == 0L) {
      cat(
        "[STEP 8ter] iter =", tt,
        "| H_current =", signif(obj_current, 6),
        "| H_best =", signif(obj_best, 6),
        "| acc_syn_current =", signif(acc_syn_current, 6),
        "| acc_gap_current =", signif(acc_gap_current, 6),
        "| H_rf_current =", signif(H_rf_current, 6),
        "| max_drift_current =", signif(H_max_current, 6),
        "| max_drift_best =", signif(H_max_best, 6),
        "| accepted_post_burnin =", n_accept_post_burnin,
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

  diag <- do.call(rbind, diag_list)

  attr(idx_best, "sa_diag") <- diag
  attr(idx_best, "sa_best_dist_by_var") <- dist_best
  attr(idx_best, "sa_best_energy") <- obj_best
  attr(idx_best, "sa_best_H_max") <- H_max_best
  attr(idx_best, "sa_best_H_mean") <- H_mean_best
  attr(idx_best, "sa_best_H_hot") <- H_hot_best
  attr(idx_best, "sa_best_H_rf") <- H_rf_best
  attr(idx_best, "sa_best_acc_syn") <- acc_syn_best
  attr(idx_best, "sa_best_acc_gap") <- acc_gap_best

  idx_best
}

sample_uniform_from_discretizer <- function(x_bin,
                                            discretizer,
                                            integer_output = TRUE) {

  x_bin <- as.character(x_bin)
  out <- rep(NA_real_, length(x_bin))

  if (is.null(discretizer)) {
    stop("[STEP 10] ERRORE: discretizer nullo.")
  }

  if (identical(discretizer$method, "missing_only")) {
    out[] <- NA_real_
    return(out)
  }

  if (identical(discretizer$method, "constant")) {
    out[x_bin != "__MISSING__"] <- discretizer$constant_value
    return(out)
  }

  levs <- as.character(discretizer$levels)
  levs_nomiss <- levs[levs != "__MISSING__"]

  brks <- discretizer$breaks

  if (is.null(brks)) {
    stop("[STEP 10] ERRORE: breaks assenti nel discretizer.")
  }

  if (length(brks) != length(levs_nomiss) + 1L) {
    stop("[STEP 10] ERRORE: incoerenza tra breaks e livelli bin.")
  }

  idx <- match(x_bin, levs_nomiss)

  ok <- !is.na(idx) & x_bin != "__MISSING__"

  lower <- brks[idx[ok]]
  upper <- brks[idx[ok] + 1L]

  out[ok] <- stats::runif(
    n = sum(ok),
    min = lower,
    max = upper
  )

  if (integer_output) {

    out[ok] <- floor(out[ok])

    real_min <- ceiling(min(brks, na.rm = TRUE))
    real_max <- floor(max(brks, na.rm = TRUE))

    out[ok] <- pmax(out[ok], real_min)
    out[ok] <- pmin(out[ok], real_max)
  }

  out
}

compare_marginals <- function(df_real_cat, df_syn_cat) {

  out <- lapply(names(df_real_cat), function(cc) {

    tr <- prop.table(table(as.character(df_real_cat[[cc]])))
    ts <- prop.table(table(as.character(df_syn_cat[[cc]])))

    levs <- union(names(tr), names(ts))

    p_real <- as.numeric(tr[levs])
    p_syn  <- as.numeric(ts[levs])

    p_real[is.na(p_real)] <- 0
    p_syn[is.na(p_syn)] <- 0

    data.frame(
      variable = cc,
      level = levs,
      p_real = p_real,
      p_syn = p_syn,
      abs_diff = abs(p_real - p_syn),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, out)
}

compare_pairwise_joint <- function(df_real_cat, df_syn_cat) {

  vars <- names(df_real_cat)
  pairs <- combn(vars, 2, simplify = FALSE)

  out <- lapply(pairs, function(pp) {

    v1 <- pp[1]
    v2 <- pp[2]

    tr <- prop.table(table(
      as.character(df_real_cat[[v1]]),
      as.character(df_real_cat[[v2]])
    ))

    ts <- prop.table(table(
      as.character(df_syn_cat[[v1]]),
      as.character(df_syn_cat[[v2]])
    ))

    rn <- union(rownames(tr), rownames(ts))
    cn <- union(colnames(tr), colnames(ts))

    tr_full <- matrix(
      0,
      nrow = length(rn),
      ncol = length(cn),
      dimnames = list(rn, cn)
    )

    ts_full <- matrix(
      0,
      nrow = length(rn),
      ncol = length(cn),
      dimnames = list(rn, cn)
    )

    tr_full[rownames(tr), colnames(tr)] <- tr
    ts_full[rownames(ts), colnames(ts)] <- ts

    d <- abs(tr_full - ts_full)

    data.frame(
      var1 = v1,
      var2 = v2,
      mean_abs_diff = mean(d),
      max_abs_diff = max(d),
      l1_distance = sum(d),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, out)
}

summarise_metric <- function(x, prefix) {

  data.frame(
    metric = paste0(
      prefix,
      c("_min", "_q1", "_median", "_mean", "_q3", "_max")
    ),
    value = c(
      min(x, na.rm = TRUE),
      as.numeric(quantile(x, 0.25, na.rm = TRUE)),
      median(x, na.rm = TRUE),
      mean(x, na.rm = TRUE),
      as.numeric(quantile(x, 0.75, na.rm = TRUE)),
      max(x, na.rm = TRUE)
    ),
    stringsAsFactors = FALSE
  )
}

df_to_char_matrix <- function(df) {

  as.matrix(
    data.frame(
      lapply(df, as.character),
      stringsAsFactors = FALSE
    )
  )
}

row_key <- function(df) {

  apply(
    data.frame(
      lapply(df, as.character),
      stringsAsFactors = FALSE
    ),
    1L,
    paste,
    collapse = "||"
  )
}

min_hamming_within_set <- function(df_a) {

  A <- df_to_char_matrix(df_a)
  nA <- nrow(A)

  out <- rep(NA_real_, nA)

  if (nA <= 1L) {
    out[] <- NA_real_
    return(out)
  }

  for (i in seq_len(nA)) {

    B <- A[-i, , drop = FALSE]

    cmp <- B != matrix(
      A[i, ],
      nrow = nrow(B),
      ncol = ncol(B),
      byrow = TRUE
    )

    out[i] <- min(rowMeans(cmp))
  }

  out
}

environment()
})
