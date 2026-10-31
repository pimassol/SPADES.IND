# Extracted from privacy_diagnostics.R. Original algorithms are unchanged.
# Never source the case-study driver when loading the package.
.spades_privacy <- local({

row_key <- function(df) {
  apply(as.data.frame(df), 1L, paste, collapse = "||")
}

to_char_df <- function(df) {
  df <- as.data.frame(df)
  df[] <- lapply(df, function(x) {
    x <- as.character(x)
    x[is.na(x)] <- "<NA>"
    x
  })
  df
}

align_common_cols <- function(real, syn, holdout = NULL) {

  real <- as.data.table(real)
  syn <- as.data.table(syn)

  if (!is.null(holdout)) {
    holdout <- as.data.table(holdout)
  }

  common_cols <- intersect(names(real), names(syn))

  if (!is.null(holdout)) {
    common_cols <- intersect(common_cols, names(holdout))
  }

  if (length(common_cols) == 0L) {
    stop("Nessuna colonna comune tra real, syn e holdout.")
  }

  real <- real[, ..common_cols]
  syn <- syn[, ..common_cols]

  if (!is.null(holdout)) {
    holdout <- holdout[, ..common_cols]
  }

  list(
    real = real,
    syn = syn,
    holdout = holdout,
    cols = common_cols
  )
}

min_hamming_features <- function(query_df,
                                 ref_df,
                                 thresholds = c(0, 1, 2, 3),
                                 chunk_size = 250L) {

  query_df <- to_char_df(query_df)
  ref_df <- to_char_df(ref_df)

  q_mat <- as.data.frame(query_df, stringsAsFactors = FALSE)
  r_mat <- as.data.frame(ref_df, stringsAsFactors = FALSE)

  if (ncol(q_mat) != ncol(r_mat)) {
    stop(
      "query_df e ref_df hanno un numero diverso di colonne: ",
      ncol(q_mat), " vs ", ncol(r_mat),
      ". Riallineare le colonne prima del calcolo Hamming."
    )
  }

  if (!identical(names(q_mat), names(r_mat))) {
    stop("query_df e ref_df hanno nomi di colonne diversi.")
  }

  p <- ncol(q_mat)
  n_q <- nrow(q_mat)
  n_r <- nrow(r_mat)

  out <- data.table(
    min_dist_abs = rep(NA_real_, n_q),
    min_dist_norm = rep(NA_real_, n_q)
  )

  for (thr in thresholds) {
    out[[paste0("n_within_", thr)]] <- 0L
  }

  for (start in seq(1L, n_q, by = chunk_size)) {

    end <- min(start + chunk_size - 1L, n_q)
    idx <- start:end
    m <- length(idx)

    dmat <- matrix(0L, nrow = m, ncol = n_r)

    for (j in seq_len(p)) {
      dmat <- dmat + outer(q_mat[[j]][idx], r_mat[[j]], FUN = "!=")
    }

    min_abs <- apply(dmat, 1L, min)

    out$min_dist_abs[idx] <- min_abs
    out$min_dist_norm[idx] <- min_abs / p

    for (thr in thresholds) {
      out[[paste0("n_within_", thr)]][idx] <- rowSums(dmat <= thr)
    }

    rm(dmat)
    invisible(gc())
  }

  out
}

sample_pairwise_hamming <- function(df, n_pairs = 50000L) {

  df <- to_char_df(df)
  n <- nrow(df)
  p <- ncol(df)

  i <- sample.int(n, n_pairs, replace = TRUE)
  j <- sample.int(n, n_pairs, replace = TRUE)

  d <- rep(0L, n_pairs)

  for (k in seq_len(p)) {
    d <- d + as.integer(df[[k]][i] != df[[k]][j])
  }

  data.table(
    dist_abs = d,
    dist_norm = d / p
  )
}

marginal_drift <- function(real, syn) {

  cols <- names(real)

  res <- rbindlist(lapply(cols, function(v) {

    tab_r <- prop.table(table(real[[v]]))
    tab_s <- prop.table(table(syn[[v]]))

    lev <- union(names(tab_r), names(tab_s))

    pr <- as.numeric(tab_r[lev])
    ps <- as.numeric(tab_s[lev])

    pr[is.na(pr)] <- 0
    ps[is.na(ps)] <- 0

    data.table(
      variable = v,
      level = lev,
      p_real = pr,
      p_syn = ps,
      abs_diff = abs(pr - ps)
    )
  }))

  summary <- res[
    ,
    .(
      mean_abs_diff = mean(abs_diff),
      max_abs_diff = max(abs_diff),
      total_variation = 0.5 * sum(abs_diff)
    ),
    by = variable
  ]

  list(detail = res, summary = summary)
}

auc_manual <- function(y, score) {
  pos <- score[y == 1]
  neg <- score[y == 0]
  if (length(pos) == 0 || length(neg) == 0) return(NA_real_)
  mean(outer(pos, neg, FUN = ">")) +
    0.5 * mean(outer(pos, neg, FUN = "=="))
}

environment()
})
