# Paper equations 23-26 use uniform component-internal weights.
# RF importances select pairs; they are not converted into energy weights.
spades_structure_auto <- function(prepared, seed = 123L, threads = 1L,
                                  high_cardinality = NULL) {
  if (!inherits(prepared, "spades_prepared")) stop("Use spades_prepare first.")
  .spades_check_prepared(prepared)
  x <- prepared$data
  if (anyNA(x) || nrow(x) < 10L || ncol(x) < 2L ||
      !all(vapply(x, is.factor, logical(1)))) stop("Invalid prepared reference.")
  if (anyDuplicated(names(x)) || any(!nzchar(names(x)))) stop("Invalid column names.")
  integer_arg <- function(v, name, lower) {
    if (length(v) != 1L || !is.numeric(v) || !is.finite(v) ||
        v < lower || v > .Machine$integer.max || v != floor(v)) stop("Invalid ", name)
    as.integer(v)
  }
  seed <- integer_arg(seed, "seed", 0)
  threads <- integer_arg(threads, "threads", 1)
  # Engineering default: the paper does not specify a numeric cutoff.
  if (is.null(high_cardinality)) high_cardinality <- max(50L, ceiling(sqrt(nrow(x))))
  cutoff <- integer_arg(high_cardinality, "high_cardinality", 2)
  withr::local_preserve_seed()
  cardinality <- vapply(x, function(z) length(unique(as.character(z))), integer(1))
  
  excluded <- names(x)[cardinality > cutoff]
  # Highest cardinality; column order resolves ties reproducibly.
  base <- names(cardinality)[which.max(cardinality)]
  eligible <- setdiff(names(x)[cardinality >= 2L], excluded)
  if (length(eligible) < 2L) return(structure(list(pairs=list(),conditional_on=base,
    reference_hash=.spades_flight_hash(prepared),excluded=excluded),class="spades_structure"))
  rankings <- vector("list", length(eligible))
  for (i in seq_along(eligible)) {
    target <- eligible[i]
    predictors <- setdiff(eligible, target)
    d <- x[, c(target, predictors), drop = FALSE]
    d[] <- lapply(d, function(z) factor(as.character(z), levels = unique(as.character(z))))
    # Formula-free call supports arbitrary column names.
    fit <- ranger::ranger(x = d[, predictors, drop = FALSE], y = d[[target]],
      num.trees = 100L, mtry = max(1L, floor(sqrt(length(predictors)))),
      min.node.size = 10L, importance = "impurity", probability = FALSE,
      respect.unordered.factors = "order", sample.fraction = 0.5,
      replace = FALSE, num.threads = threads, verbose = FALSE, seed = seed)
    imp <- fit$variable.importance[predictors]
    if (any(!is.finite(imp))) stop("Non-finite RF importance for ", target)
    order_idx <- order(-imp, seq_along(imp))
    rankings[[i]] <- data.frame(target = target, predictor = predictors[order_idx],
      importance = unname(imp[order_idx]), rank = seq_along(order_idx),
      stringsAsFactors = FALSE)
  }
  rankings <- do.call(rbind, rankings)
  rownames(rankings) <- NULL
  top <- rankings[rankings$rank == 1L, , drop = FALSE]
  # B is a set of unordered pairs: reciprocal selections occur once.
  indexes <- lapply(seq_len(nrow(top)), function(i)
    sort(match(c(top$target[i], top$predictor[i]), names(x))))
  keys <- vapply(indexes, paste, collapse = ":", FUN.VALUE = character(1))
  indexes <- indexes[!duplicated(keys)]
  pairs <- lapply(indexes, function(i) names(x)[i])
  pair_names <- vapply(pairs, paste, collapse = "__", FUN.VALUE = character(1))
  if (anyDuplicated(pair_names)) stop("Ambiguous pair names containing separators.")
  one <- function(nms) stats::setNames(rep(1, length(nms)), nms)
  structure(list(pairs = pairs, conditional_on = base,
    weights = list(marginal = one(names(x)),
      conditional = one(if (is.null(base)) character() else setdiff(names(x), base)),
      pairwise = one(pair_names)),
    top = top, importance = rankings, excluded = excluded,
    controls = list(seed = seed, threads = threads, high_cardinality = cutoff,
      weighting = "Uniform: paper equations 23-26", selection = "RF top-1; unordered pair union"),
    reference_hash = .spades_flight_hash(prepared),
    ranger_version = as.character(utils::packageVersion("ranger"))),
    class = "spades_structure")
}

