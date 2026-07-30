#' Compute cosine similarity
#'
#' @param x Numeric matrix whose rows are observations.
#' @param y Optional numeric matrix with the same columns as `x`. When omitted,
#'   computes all pairwise similarities among rows of `x`.
#'
#' @return A numeric similarity matrix. Similarities involving zero vectors
#'   are returned as `NA`.
#' @export
#'
#' @examples
#' x <- rbind(a = c(1, 1, 0), b = c(1, 0, 0), c = c(0, 0, 1))
#' cosine_similarity(x)
cosine_similarity <- function(x, y = NULL) {
  if (!is.matrix(x) || !is.numeric(x)) {
    stop("`x` must be a numeric matrix.", call. = FALSE)
  }
  if (is.null(y)) y <- x
  if (!is.matrix(y) || !is.numeric(y) || ncol(y) != ncol(x)) {
    stop("`y` must be a numeric matrix with the same columns as `x`.",
         call. = FALSE)
  }
  denominator <- outer(sqrt(rowSums(x^2)), sqrt(rowSums(y^2)))
  result <- tcrossprod(x, y) / denominator
  result[denominator == 0] <- NA_real_
  dimnames(result) <- list(rownames(x), rownames(y))
  result
}
