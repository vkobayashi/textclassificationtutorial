#' F-measure
#'
#' @param precision Numeric precision.
#' @param recall Numeric recall.
#' @param beta Relative weight assigned to recall.
#'
#' @return Numeric F-measure.
#' @export
f_measure <- function(precision, recall, beta = 1) {
  if (any(c(precision, recall) < 0, na.rm = TRUE) ||
      any(c(precision, recall) > 1, na.rm = TRUE)) {
    stop("`precision` and `recall` must be between 0 and 1.",
         call. = FALSE)
  }
  if (length(beta) != 1L || is.na(beta) || beta <= 0) {
    stop("`beta` must be positive.", call. = FALSE)
  }
  denominator <- beta^2 * precision + recall
  result <- (1 + beta^2) * precision * recall / denominator
  result[!is.na(denominator) & denominator == 0] <- 0
  result
}

#' Calculate binary classification metrics
#'
#' @param truth Vector of true classes.
#' @param estimate Vector of predicted classes.
#' @param positive Value identifying the positive class.
#'
#' @return A one-row data frame containing confusion counts, accuracy,
#'   balanced accuracy, precision, recall, specificity, and F1.
#' @export
#'
#' @examples
#' classification_metrics(c("task", "task", "other"), c("task", "other", "other"),
#'                        positive = "task")
classification_metrics <- function(truth, estimate, positive) {
  if (length(truth) != length(estimate) || !length(truth)) {
    stop("`truth` and `estimate` must have the same nonzero length.",
         call. = FALSE)
  }
  if (length(positive) != 1L || is.na(positive)) {
    stop("`positive` must identify one class.", call. = FALSE)
  }
  complete <- !is.na(truth) & !is.na(estimate)
  truth <- truth[complete]
  estimate <- estimate[complete]
  actual_positive <- truth == positive
  predicted_positive <- estimate == positive
  tp <- sum(actual_positive & predicted_positive)
  fn <- sum(actual_positive & !predicted_positive)
  fp <- sum(!actual_positive & predicted_positive)
  tn <- sum(!actual_positive & !predicted_positive)
  safe_ratio <- function(a, b) if (b == 0) NA_real_ else a / b
  precision <- safe_ratio(tp, tp + fp)
  recall <- safe_ratio(tp, tp + fn)
  specificity <- safe_ratio(tn, tn + fp)
  data.frame(
    n = length(truth), true_positive = tp, false_positive = fp,
    true_negative = tn, false_negative = fn,
    accuracy = safe_ratio(tp + tn, length(truth)),
    balanced_accuracy = mean(c(recall, specificity), na.rm = TRUE),
    precision = precision, recall = recall, specificity = specificity,
    f1 = if (is.na(precision) || is.na(recall)) NA_real_ else
      f_measure(precision, recall)
  )
}

#' Create stratified cross-validation folds
#'
#' @param y Class labels.
#' @param k Number of folds.
#' @param repeats Number of repeated fold sets.
#' @param seed Optional random seed. The caller's random-number state is
#'   restored.
#'
#' @return A list of integer test-set indices with class `text_folds`.
#' @export
stratified_folds <- function(y, k = 5L, repeats = 1L, seed = NULL) {
  if (length(y) < 2L || anyNA(y)) {
    stop("`y` must contain at least two non-missing labels.", call. = FALSE)
  }
  counts <- table(y)
  if (length(counts) < 2L) stop("`y` must contain at least two classes.",
                                call. = FALSE)
  if (length(k) != 1L || is.na(k) || k < 2 || k > min(counts) ||
      k != as.integer(k)) {
    stop("`k` must be between 2 and the smallest class size.",
         call. = FALSE)
  }
  if (length(repeats) != 1L || is.na(repeats) || repeats < 1 ||
      repeats != as.integer(repeats)) {
    stop("`repeats` must be at least 1.", call. = FALSE)
  }
  k <- as.integer(k)
  repeats <- as.integer(repeats)
  with_preserved_seed(seed, {
    folds <- vector("list", k * repeats)
    position <- 1L
    for (repeat_id in seq_len(repeats)) {
      assignments <- integer(length(y))
      labels <- as.character(y)
      for (class in unique(labels)) {
        index <- which(labels == class)
        assignments[index] <- sample(rep(seq_len(k), length.out = length(index)))
      }
      for (fold_id in seq_len(k)) {
        folds[[position]] <- which(assignments == fold_id)
        position <- position + 1L
      }
    }
    names(folds) <- sprintf("Repeat%02d_Fold%02d",
                            rep(seq_len(repeats), each = k),
                            rep(seq_len(k), repeats))
    structure(folds, class = c("text_folds", "list"), k = k,
              repeats = repeats)
  })
}

with_preserved_seed <- function(seed, code) {
  if (is.null(seed)) return(force(code))
  existed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  if (existed) old_seed <- get(".Random.seed", envir = .GlobalEnv)
  on.exit({
    if (existed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)
  set.seed(seed)
  force(code)
}
