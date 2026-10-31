# Extracted from synthetic_data_generator_Conformal_prediction.R. Original algorithms are unchanged.
# Never source the case-study driver when loading the package.
.spades_conformal <- local({

safe_char <- function(x) {
  x <- as.character(x)
  x[is.na(x) | trimws(x) == ""] <- "MISSING"
  x
}

accuracy_score <- function(y_true, y_pred) {
  mean(as.character(y_true) == as.character(y_pred))
}

balanced_accuracy_score <- function(y_true, y_pred) {

  y_true <- factor(y_true)
  y_pred <- factor(y_pred, levels = levels(y_true))

  tab <- table(y_true, y_pred)
  recalls <- diag(tab) / rowSums(tab)
  recalls[is.na(recalls)] <- 0

  mean(recalls)
}

multiclass_logloss <- function(P, y, class_levels, eps = 1e-15) {

  P <- pmin(pmax(P, eps), 1 - eps)

  idx <- match(as.character(y), class_levels)

  if (any(is.na(idx))) {
    stop("Alcuni livelli del target non sono presenti in class_levels.")
  }

  -mean(log(P[cbind(seq_along(idx), idx)]))
}

compute_gini_row <- function(P) {
  1 - rowSums(P^2)
}

build_freq_encoder <- function(x) {
  x <- safe_char(x)
  prop.table(table(x))
}

apply_freq_encoder <- function(x, freq_map) {
  x <- safe_char(x)
  out <- as.numeric(freq_map[x])
  out[is.na(out)] <- 0
  out
}

scale_numeric_train <- function(x) {
  mu <- mean(x, na.rm = TRUE)
  sig <- stats::sd(x, na.rm = TRUE)

  if (is.na(mu)) {
    mu <- 0
  }

  if (is.na(sig) || sig <= 0) {
    sig <- 1
  }

  list(mean = mu, sd = sig)
}

apply_numeric_scaler <- function(x, scaler) {
  x <- as.numeric(x)
  x[is.na(x)] <- scaler$mean
  (x - scaler$mean) / scaler$sd
}

make_internal_cp_split <- function(y,
                                   train_prop = 0.80,
                                   cal_prop_within_holdout = 0.80,
                                   seed = 123) {

  withr::local_seed(seed)

  y_chr <- as.character(y)

  idx_train <- integer()
  idx_holdout <- integer()

  for (lev in unique(y_chr)) {

    idx_lev <- which(y_chr == lev)
    idx_lev <- sample(idx_lev)

    n <- length(idx_lev)
    n_train <- max(1L, floor(n * train_prop))

    idx_train <- c(idx_train, idx_lev[seq_len(n_train)])

    if (n_train < n) {
      idx_holdout <- c(idx_holdout, idx_lev[(n_train + 1L):n])
    }
  }

  idx_cal <- integer()
  idx_test <- integer()

  y_hold <- y_chr[idx_holdout]

  for (lev in unique(y_hold)) {

    idx_lev_local <- which(y_hold == lev)
    idx_lev_local <- sample(idx_lev_local)

    n <- length(idx_lev_local)
    n_cal <- max(1L, floor(n * cal_prop_within_holdout))

    idx_cal <- c(
      idx_cal,
      idx_holdout[idx_lev_local[seq_len(n_cal)]]
    )

    if (n_cal < n) {
      idx_test <- c(
        idx_test,
        idx_holdout[idx_lev_local[(n_cal + 1L):n]]
      )
    }
  }

  list(
    train = sort(idx_train),
    cal = sort(idx_cal),
    test = sort(idx_test)
  )
}

prepare_internal_data <- function(train_df,
                                  cal_df,
                                  test_df,
                                  target_col,
                                  predictor_cols,
                                  municipality_col,
                                  class_levels) {

  train_df <- as.data.frame(
    train_df[, c(target_col, predictor_cols), drop = FALSE]
  )

  cal_df <- as.data.frame(
    cal_df[, c(target_col, predictor_cols), drop = FALSE]
  )

  test_df <- as.data.frame(
    test_df[, c(target_col, predictor_cols), drop = FALSE]
  )

  freq_col <- paste0(municipality_col, "_freq")

  freq_map <- build_freq_encoder(train_df[[municipality_col]])

  train_df[[freq_col]] <- apply_freq_encoder(
    train_df[[municipality_col]],
    freq_map
  )

  cal_df[[freq_col]] <- apply_freq_encoder(
    cal_df[[municipality_col]],
    freq_map
  )

  test_df[[freq_col]] <- apply_freq_encoder(
    test_df[[municipality_col]],
    freq_map
  )

  train_df[[municipality_col]] <- NULL
  cal_df[[municipality_col]] <- NULL
  test_df[[municipality_col]] <- NULL

  predictors_final <- c(
    setdiff(predictor_cols, municipality_col),
    freq_col
  )

  categorical_final <- setdiff(
    predictors_final,
    c("age", freq_col)
  )

  for (cc in categorical_final) {

    levs <- sort(unique(c(
      safe_char(train_df[[cc]]),
      safe_char(cal_df[[cc]]),
      safe_char(test_df[[cc]])
    )))

    train_df[[cc]] <- factor(
      safe_char(train_df[[cc]]),
      levels = levs
    )

    cal_df[[cc]] <- factor(
      safe_char(cal_df[[cc]]),
      levels = levs
    )

    test_df[[cc]] <- factor(
      safe_char(test_df[[cc]]),
      levels = levs
    )
  }

  train_df[["age"]] <- suppressWarnings(as.numeric(train_df[["age"]]))
  cal_df[["age"]] <- suppressWarnings(as.numeric(cal_df[["age"]]))
  test_df[["age"]] <- suppressWarnings(as.numeric(test_df[["age"]]))

  train_df[[freq_col]] <- suppressWarnings(as.numeric(train_df[[freq_col]]))
  cal_df[[freq_col]] <- suppressWarnings(as.numeric(cal_df[[freq_col]]))
  test_df[[freq_col]] <- suppressWarnings(as.numeric(test_df[[freq_col]]))

  train_df[[target_col]] <- factor(
    safe_char(train_df[[target_col]]),
    levels = class_levels
  )

  cal_df[[target_col]] <- factor(
    safe_char(cal_df[[target_col]]),
    levels = class_levels
  )

  test_df[[target_col]] <- factor(
    safe_char(test_df[[target_col]]),
    levels = class_levels
  )

  list(
    train = train_df,
    cal = cal_df,
    test = test_df,
    predictors_final = predictors_final,
    categorical_final = categorical_final,
    freq_col = freq_col,
    freq_map = freq_map
  )
}

build_mlp_design <- function(train_df,
                             cal_df,
                             test_df,
                             target_col,
                             predictors_final,
                             categorical_final,
                             class_levels) {

  train_x_df <- train_df[, predictors_final, drop = FALSE]
  cal_x_df <- cal_df[, predictors_final, drop = FALSE]
  test_x_df <- test_df[, predictors_final, drop = FALSE]

  all_x_df <- rbind(
    train_x_df,
    cal_x_df,
    test_x_df
  )

  for (cc in categorical_final) {
    all_x_df[[cc]] <- factor(safe_char(all_x_df[[cc]]))
  }

  mm_all <- model.matrix(
    ~ . - 1,
    data = all_x_df
  )

  n_train <- nrow(train_x_df)
  n_cal <- nrow(cal_x_df)
  n_test <- nrow(test_x_df)

  X_train <- mm_all[seq_len(n_train), , drop = FALSE]

  X_cal <- mm_all[
    (n_train + 1L):(n_train + n_cal),
    ,
    drop = FALSE
  ]

  X_test <- mm_all[
    (n_train + n_cal + 1L):(n_train + n_cal + n_test),
    ,
    drop = FALSE
  ]

  num_cols <- colnames(X_train)

  scalers <- lapply(seq_along(num_cols), function(j) {
    scale_numeric_train(X_train[, j])
  })

  for (j in seq_along(num_cols)) {
    X_train[, j] <- apply_numeric_scaler(X_train[, j], scalers[[j]])
    X_cal[, j] <- apply_numeric_scaler(X_cal[, j], scalers[[j]])
    X_test[, j] <- apply_numeric_scaler(X_test[, j], scalers[[j]])
  }

  y_train_idx <- match(
    as.character(train_df[[target_col]]),
    class_levels
  ) - 1L

  y_cal_idx <- match(
    as.character(cal_df[[target_col]]),
    class_levels
  ) - 1L

  y_test_idx <- match(
    as.character(test_df[[target_col]]),
    class_levels
  ) - 1L

  list(
    X_train = X_train,
    X_cal = X_cal,
    X_test = X_test,
    y_train_idx = y_train_idx,
    y_cal_idx = y_cal_idx,
    y_test_idx = y_test_idx,
    feature_names = colnames(X_train),
    scalers = scalers
  )
}

fit_mlp_prob <- function(X_train,
                         y_train_idx,
                         class_levels,
                         hidden_units = c(64L, 32L),
                         dropout = c(0.20, 0.10),
                         epochs = 100L,
                         batch_size = 256L,
                         validation_split = 0.15,
                         patience = 10L,
                         learning_rate = 1e-3,
                         seed = 123) {

  withr::local_seed(seed)
  tensorflow::tf$random$set_seed(seed)

  n_features <- ncol(X_train)
  n_classes <- length(class_levels)

  y_train_cat <- keras::to_categorical(
    y_train_idx,
    num_classes = n_classes
  )

  model <- keras_model_sequential()

  model |>
    layer_dense(
      units = hidden_units[1],
      activation = "relu",
      input_shape = n_features
    ) |>
    layer_dropout(rate = dropout[1]) |>
    layer_dense(
      units = hidden_units[2],
      activation = "relu"
    ) |>
    layer_dropout(rate = dropout[2]) |>
    layer_dense(
      units = n_classes,
      activation = "softmax"
    )

  model |>
    compile(
      optimizer = optimizer_adam(learning_rate = learning_rate),
      loss = "categorical_crossentropy",
      metrics = "accuracy"
    )

  cb <- list(
    callback_early_stopping(
      monitor = "val_loss",
      patience = patience,
      restore_best_weights = TRUE
    )
  )

  history <- model |>
    fit(
      x = X_train,
      y = y_train_cat,
      epochs = epochs,
      batch_size = batch_size,
      validation_split = validation_split,
      callbacks = cb,
      verbose = 0
    )

  list(
    model = model,
    history = history
  )
}

predict_mlp_prob <- function(fit,
                             X,
                             class_levels) {

  P <- as.matrix(
    predict(
      fit$model,
      X,
      verbose = 0
    )
  )

  colnames(P) <- class_levels

  P <- pmin(pmax(P, 1e-15), 1 - 1e-15)
  P <- P / rowSums(P)

  P
}

split_cp_scores <- function(P, y, class_levels) {

  idx <- match(as.character(y), class_levels)

  if (any(is.na(idx))) {
    stop("Alcuni livelli del target non sono presenti in class_levels.")
  }

  1 - P[cbind(seq_along(idx), idx)]
}

split_cp_prediction_sets <- function(P, qhat, class_levels) {

  threshold <- 1 - qhat

  sets <- vector("list", nrow(P))

  for (i in seq_len(nrow(P))) {

    idx <- which(P[i, ] >= threshold)

    if (length(idx) == 0L) {
      idx <- which.max(P[i, ])
    }

    sets[[i]] <- class_levels[idx]
  }

  sets
}

run_internal_split_cp_mlp <- function(scenario_name,
                                      df,
                                      target_col,
                                      predictor_cols,
                                      municipality_col,
                                      class_levels,
                                      train_prop = 0.80,
                                      cal_prop_within_holdout = 0.80,
                                      alpha = 0.05,
                                      seed = 123) {

  split_idx <- make_internal_cp_split(
    y = df[[target_col]],
    train_prop = train_prop,
    cal_prop_within_holdout = cal_prop_within_holdout,
    seed = seed
  )

  train_df <- df[split_idx$train, , drop = FALSE]
  cal_df <- df[split_idx$cal, , drop = FALSE]
  test_df <- df[split_idx$test, , drop = FALSE]

  prep <- prepare_internal_data(
    train_df = train_df,
    cal_df = cal_df,
    test_df = test_df,
    target_col = target_col,
    predictor_cols = predictor_cols,
    municipality_col = municipality_col,
    class_levels = class_levels
  )

  design <- build_mlp_design(
    train_df = prep$train,
    cal_df = prep$cal,
    test_df = prep$test,
    target_col = target_col,
    predictors_final = prep$predictors_final,
    categorical_final = prep$categorical_final,
    class_levels = class_levels
  )

  mlp_fit <- fit_mlp_prob(
    X_train = design$X_train,
    y_train_idx = design$y_train_idx,
    class_levels = class_levels,
    hidden_units = mlp_hidden_units,
    dropout = mlp_dropout,
    epochs = mlp_epochs,
    batch_size = mlp_batch_size,
    validation_split = mlp_validation_split,
    patience = mlp_patience,
    learning_rate = mlp_learning_rate,
    seed = seed + 1000L
  )

  P_cal <- predict_mlp_prob(
    fit = mlp_fit,
    X = design$X_cal,
    class_levels = class_levels
  )

  P_test <- predict_mlp_prob(
    fit = mlp_fit,
    X = design$X_test,
    class_levels = class_levels
  )

  y_cal <- prep$cal[[target_col]]
  y_test <- prep$test[[target_col]]

  R_cal <- split_cp_scores(
    P = P_cal,
    y = y_cal,
    class_levels = class_levels
  )

  n_cal <- length(R_cal)

  q_level <- ceiling((n_cal + 1) * (1 - alpha)) / n_cal
  q_level <- min(q_level, 1)

  qhat <- as.numeric(
    quantile(
      R_cal,
      probs = q_level,
      type = 1,
      names = FALSE
    )
  )

  pred_sets <- split_cp_prediction_sets(
    P = P_test,
    qhat = qhat,
    class_levels = class_levels
  )

  covered <- mapply(
    function(s, yy) as.character(yy) %in% s,
    pred_sets,
    y_test
  )

  set_size <- lengths(pred_sets)

  y_pred <- factor(
    class_levels[max.col(P_test, ties.method = "first")],
    levels = class_levels
  )

  max_prob <- apply(P_test, 1, max)
  gini <- compute_gini_row(P_test)

  class_coverage <- data.frame(
    scenario = scenario_name,
    class = class_levels,
    n_test_class = NA_integer_,
    class_coverage = NA_real_,
    class_mean_set_size = NA_real_,
    class_accuracy = NA_real_,
    stringsAsFactors = FALSE
  )

  for (cl in class_levels) {

    idx <- which(as.character(y_test) == cl)

    if (length(idx) > 0L) {

      class_coverage$n_test_class[
        class_coverage$class == cl
      ] <- length(idx)

      class_coverage$class_coverage[
        class_coverage$class == cl
      ] <- mean(covered[idx])

      class_coverage$class_mean_set_size[
        class_coverage$class == cl
      ] <- mean(set_size[idx])

      class_coverage$class_accuracy[
        class_coverage$class == cl
      ] <- mean(as.character(y_pred[idx]) == as.character(y_test[idx]))
    }
  }

  set_size_distribution <- as.data.frame(table(set_size))
  names(set_size_distribution) <- c("set_size", "n")
  set_size_distribution$set_size <- as.integer(as.character(
    set_size_distribution$set_size
  ))
  set_size_distribution$prop <- (
    set_size_distribution$n / sum(set_size_distribution$n)
  )
  set_size_distribution$scenario <- scenario_name

  adaptivity_by_size <- aggregate(
    cbind(max_prob, gini, covered) ~ set_size,
    data = data.frame(
      set_size = set_size,
      max_prob = max_prob,
      gini = gini,
      covered = as.numeric(covered)
    ),
    FUN = mean
  )

  adaptivity_by_size$scenario <- scenario_name

  metrics <- data.frame(
    scenario = scenario_name,
    n_total = nrow(df),
    n_train = nrow(prep$train),
    n_cal = nrow(prep$cal),
    n_test = nrow(prep$test),
    train_prop = train_prop,
    cal_prop_within_holdout = cal_prop_within_holdout,
    alpha = alpha,
    nominal_coverage = 1 - alpha,
    qhat = qhat,
    prob_threshold = 1 - qhat,
    coverage = mean(covered),
    mean_set_size = mean(set_size),
    median_set_size = median(set_size),
    min_set_size = min(set_size),
    max_set_size = max(set_size),
    p_set_size_1 = mean(set_size == 1L),
    p_set_size_2 = mean(set_size == 2L),
    p_set_size_3 = mean(set_size == 3L),
    p_set_size_4 = mean(set_size == 4L),
    accuracy = accuracy_score(y_test, y_pred),
    balanced_accuracy = balanced_accuracy_score(y_test, y_pred),
    logloss = multiclass_logloss(P_test, y_test, class_levels),
    mean_max_prob = mean(max_prob),
    median_max_prob = median(max_prob),
    mean_gini = mean(gini),
    cor_set_size_max_prob = suppressWarnings(cor(set_size, max_prob)),
    cor_set_size_gini = suppressWarnings(cor(set_size, gini)),
    stringsAsFactors = FALSE
  )

  list(
    scenario = scenario_name,
    metrics = metrics,
    class_coverage = class_coverage,
    set_size_distribution = set_size_distribution,
    adaptivity_by_size = adaptivity_by_size,
    pred_sets = pred_sets,
    set_size = set_size,
    covered = covered,
    y_test = y_test,
    y_pred = y_pred,
    P_test = P_test,
    max_prob = max_prob,
    gini = gini,
    qhat = qhat,
    R_cal = R_cal,
    mlp_fit = mlp_fit,
    prep = prep,
    design = design,
    split_idx = split_idx
  )
}

environment()
})
