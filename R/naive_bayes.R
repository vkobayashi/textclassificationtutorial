#' Fit a multinomial Naive Bayes text classifier
#'
#' @param x Nonnegative numeric document-term matrix.
#' @param y Class labels with one value per row of `x`.
#' @param laplace Nonnegative additive smoothing parameter.
#' @param prior Optional named class probabilities.
#'
#' @return An object of class `text_nb`.
#' @export
#'
#' @examples
#' x <- rbind(c(3, 0), c(2, 0), c(0, 3), c(0, 2))
#' colnames(x) <- c("analysis", "care")
#' model <- fit_naive_bayes(x, c("data", "data", "health", "health"))
#' predict(model, x)
fit_naive_bayes <- function(x, y, laplace = 1, prior = NULL) {
  validate_dtm(x)
  if (nrow(x) != length(y) || anyNA(y)) {
    stop("`y` must contain one non-missing label per document.",
         call. = FALSE)
  }
  if (any(x < 0) || any(!is.finite(x))) {
    stop("`x` must contain finite nonnegative values.", call. = FALSE)
  }
  if (length(laplace) != 1L || is.na(laplace) || laplace < 0) {
    stop("`laplace` must be nonnegative.", call. = FALSE)
  }
  classes <- unique(as.character(y))
  if (length(classes) < 2L) stop("At least two classes are required.",
                                 call. = FALSE)
  y <- factor(y, levels = classes)
  if (is.null(prior)) {
    class_prior <- prop.table(table(y))
  } else {
    if (is.null(names(prior)) || !setequal(names(prior), classes) ||
        any(prior < 0) || abs(sum(prior) - 1) > sqrt(.Machine$double.eps)) {
      stop("`prior` must be named probabilities summing to 1 for every class.",
           call. = FALSE)
    }
    class_prior <- prior[classes]
  }
  term_counts <- vapply(
    classes,
    function(class) colSums(x[y == class, , drop = FALSE]),
    numeric(ncol(x))
  )
  term_probability <- sweep(
    term_counts + laplace,
    2,
    colSums(term_counts) + laplace * nrow(term_counts),
    `/`
  )
  structure(
    list(
      classes = classes,
      terms = colnames(x),
      prior = as.numeric(class_prior[classes]),
      term_probability = term_probability,
      laplace = laplace
    ),
    class = "text_nb"
  )
}

#' @export
predict.text_nb <- function(object, newdata, type = c("class", "prob"), ...) {
  type <- match.arg(type)
  validate_dtm(newdata)
  missing_terms <- setdiff(object$terms, colnames(newdata))
  if (length(missing_terms)) {
    padding <- matrix(0, nrow(newdata), length(missing_terms),
                      dimnames = list(rownames(newdata), missing_terms))
    newdata <- cbind(newdata, padding)
  }
  newdata <- newdata[, object$terms, drop = FALSE]
  log_scores <- newdata %*% log(object$term_probability)
  log_scores <- sweep(log_scores, 2, log(object$prior), `+`)
  row_max <- apply(log_scores, 1, max)
  probabilities <- exp(log_scores - row_max)
  probabilities <- probabilities / rowSums(probabilities)
  colnames(probabilities) <- object$classes
  rownames(probabilities) <- rownames(newdata)
  if (type == "prob") return(probabilities)
  factor(object$classes[max.col(probabilities, ties.method = "first")],
         levels = object$classes)
}

#' @export
print.text_nb <- function(x, ...) {
  cat("<text_nb> Multinomial Naive Bayes\n")
  cat("Classes: ", paste(x$classes, collapse = ", "), "\n", sep = "")
  cat("Terms: ", length(x$terms), "\n", sep = "")
  invisible(x)
}
