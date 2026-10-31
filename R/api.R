# Public development API. No source script is executed at package load.
.spades_module_names <- c("core", "fidelity", "privacy", "conformal",
                           "stability", "sensitivity", "target_size")
.spades_dependencies <- list(
  core = c("xgboost", "Matrix", "doParallel", "foreach", "parallel", "ranger", "bnlearn"),
  fidelity = c("data.table", "ranger"),
  privacy = c("data.table", "ranger"),
  conformal = c("data.table", "ggplot2", "keras", "tensorflow"),
  stability = "ranger", sensitivity = "ranger", target_size = "ranger"
)

spades_modules <- function() {
  data.frame(module = .spades_module_names,
    functions = vapply(.spades_module_names, function(m) {
      env <- get(paste0(".spades_", m), envir = asNamespace("SPADES.IND"))
      length(ls(env, all.names = TRUE))
    }, integer(1)), row.names = NULL)
}

spades_check_dependencies <- function(module = "core") {
  module <- match.arg(module, .spades_module_names)
  pkgs <- .spades_dependencies[[module]]
  data.frame(package = pkgs, installed = vapply(pkgs, function(p) {
    nzchar(system.file(package = p))
  }, logical(1)), row.names = NULL)
}

spades_functions <- function(module = "core", state = list(),
                             load_dependencies = FALSE) {
  module <- match.arg(module, .spades_module_names)
  if (!is.list(state) || (length(state) &&
      (is.null(names(state)) || anyNA(names(state)) ||
       any(!nzchar(names(state))) || anyDuplicated(names(state))))) {
    stop("state must be a uniquely named list.")
  }
  original <- get(paste0(".spades_", module), envir = asNamespace("SPADES.IND"))
  fn_names <- ls(original, all.names = TRUE)
  if (any(names(state) %in% fn_names)) {
    stop("state must not replace extracted functions.")
  }
  # Imported bindings live only in a private parent environment.
  imports <- new.env(parent = asNamespace("SPADES.IND"))
  if (isTRUE(load_dependencies)) {
    for (pkg in .spades_dependencies[[module]]) {
      if (!requireNamespace(pkg, quietly = TRUE)) {
        stop("Install the optional dependency: ", pkg)
      }
      for (nm in getNamespaceExports(pkg)) {
        assign(nm, getExportedValue(pkg, nm), envir = imports)
      }
    }
  }
  env <- list2env(state, parent = imports)
  for (nm in fn_names) {
    fn <- get(nm, envir = original, inherits = FALSE)
    environment(fn) <- env
    assign(nm, fn, envir = env)
  }
  env
}

spades_function_index <- function() {
 do.call(rbind,lapply(.spades_module_names,function(m) {
  nms<-ls(get(paste0('.spades_',m),envir=asNamespace('SPADES.IND')),all.names=TRUE)
  data.frame(module=m, function_name=nms)
 }))
}

spades_prepare <- function(data, nominal = NULL, ordinal = NULL, numeric = NULL,
                           n_bins = 20L, method = c("quantile", "equal_width"),
                           missing_tokens = "NA", verbose = TRUE) {
  method <- match.arg(method)
  if (!is.data.frame(data) || nrow(data) < 1L || ncol(data) < 1L) {
    stop("data must be a nonempty data.frame.")
  }
  if (anyNA(names(data)) || any(!nzchar(names(data))) || anyDuplicated(names(data))) {
    stop("Column names must be unique and nonempty.")
  }
  if (any(!vapply(data, function(x) is.factor(x) || is.character(x) ||
      is.logical(x) || (is.numeric(x) && !is.object(x)), logical(1)))) {
    stop("Supported columns: factors, characters, logicals, integers and doubles.")
  }
  if (is.null(ordinal)) ordinal <- names(data)[vapply(data, is.ordered, logical(1))]
  if (is.null(numeric)) numeric <- names(data)[vapply(data, is.numeric, logical(1))]
  if (is.null(nominal)) nominal <- setdiff(names(data), c(ordinal, numeric))
  groups <- c(nominal, ordinal, numeric)
  if (!is.character(groups) || anyNA(groups) || anyDuplicated(groups) ||
      !setequal(groups, names(data))) {
    stop("nominal, ordinal and numeric must partition all columns exactly once.")
  }
  if (length(n_bins) != 1L || !is.finite(n_bins) || n_bins < 2 ||
      n_bins != as.integer(n_bins)) stop("n_bins must be an integer >= 2.")
  if (any(!vapply(data[numeric], is.numeric, logical(1)))) {
    stop("Columns declared numeric must contain numeric values.")
  }
  tokens <- .spades_missing_tokens(missing_tokens)
  bad <- .spades_missing_rows(data,tokens)
  filtering <- data.frame(input=nrow(data),excluded=sum(bad),used=sum(!bad))
  if(isTRUE(verbose)) {cat(sprintf('INPUT | records: %d | excluded: %d | used: %d\n',
    filtering$input,filtering$excluded,filtering$used));flush.console()}
  if(all(bad))stop('No complete records remain after missing-value filtering.')
  retained_rows <- which(!bad)
  data <- droplevels(data[retained_rows,,drop=FALSE])
  if (any(vapply(data[numeric], function(x) any(is.infinite(x)), logical(1)))) {
    stop("Infinite numeric values are not supported.")
  }
  f <- spades_functions("core")
  out <- f$make_all_categorical(data, municipality_col = character(),
    nominal_cols = nominal, ordinal_cols = ordinal, numeric_cols = numeric,
    n_bins_numeric = as.integer(n_bins), numeric_method = method)
  # Respect the order explicitly supplied as ordered-factor metadata.
  for (nm in ordinal) {
    if (is.ordered(data[[nm]])) {
      lev <- levels(data[[nm]])
      if (anyNA(data[[nm]])) lev <- c(lev, "__MISSING__")
      out[[nm]] <- ordered(as.character(out[[nm]]), levels = lev)
    }
  }
  discretizers <- attr(out, "numeric_discretizers")
  out <- out[, names(data), drop = FALSE]
  structure(list(data = out, original = data, filtering=filtering, retained_rows=retained_rows,
    missing_policy=list(tokens=tokens,match="case-sensitive after trimming whitespace",action="exclude whole row"), integer_columns = names(data)[vapply(data, function(z) is.numeric(z) && all(is.na(z) | z == floor(z)), logical(1))], numeric_discretizers = discretizers,
    schema = list(columns = names(data), nominal = nominal, ordinal = ordinal,
                  numeric = numeric), controls = list(n_bins = n_bins, method = method)),
    class = "spades_prepared")
}

# Keep the caller's random state unchanged, including a previously absent seed.
.spades_with_seed <- function(seed, code) {
  if (length(seed) != 1L || !is.finite(seed) || seed < 0 ||
      seed > .Machine$integer.max || seed != floor(seed)) stop("Invalid seed.")
  withr::with_seed(as.integer(seed), force(code))
}

# Reset RNG kind before the scoped seed guard restores/removes the caller seed.
.spades_with_rng_kind <- function(kind, code) {
  old_kind <- RNGkind()
  withr::with_preserve_seed({
    tryCatch({do.call(RNGkind,as.list(kind));force(code)},
      finally=do.call(RNGkind,as.list(old_kind)))
  })
}
