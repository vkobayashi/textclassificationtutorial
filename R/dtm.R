#' Construct a document-term matrix
#'
#' @param text Character vector containing one preprocessed document per item.
#' @param document_ids Optional unique document identifiers.
#' @param binary Logical; store term presence instead of term frequency?
#' @param min_doc_freq Minimum number of documents in which a term must occur.
#' @param max_doc_prop Maximum proportion of documents in which a term may
#'   occur.
#'
#' @return A numeric matrix with class `text_dtm`.
#' @export
#'
#' @examples
#' docs <- preprocess_text(c("data science", "data analysis", "science"))
#' document_term_matrix(docs)
document_term_matrix <- function(text, document_ids = NULL, binary = FALSE,
                                 min_doc_freq = 1L, max_doc_prop = 1) {
  if (!is.character(text)) stop("`text` must be character.", call. = FALSE)
  n <- length(text)
  if (!n) stop("`text` must contain at least one document.", call. = FALSE)
  if (is.null(document_ids)) document_ids <- paste0("doc", seq_len(n))
  if (length(document_ids) != n || anyNA(document_ids) ||
      anyDuplicated(document_ids)) {
    stop("`document_ids` must be unique and match the number of documents.",
         call. = FALSE)
  }
  if (length(min_doc_freq) != 1L || is.na(min_doc_freq) ||
      min_doc_freq < 1) {
    stop("`min_doc_freq` must be at least 1.", call. = FALSE)
  }
  if (length(max_doc_prop) != 1L || is.na(max_doc_prop) ||
      max_doc_prop <= 0 || max_doc_prop > 1) {
    stop("`max_doc_prop` must be in (0, 1].", call. = FALSE)
  }

  tokens <- strsplit(trimws(text), "\\s+", perl = TRUE)
  tokens <- lapply(tokens, function(x) x[nzchar(x)])
  vocabulary <- sort(unique(unlist(tokens, use.names = FALSE)))
  matrix <- matrix(0, nrow = n, ncol = length(vocabulary),
                   dimnames = list(as.character(document_ids), vocabulary))
  if (length(vocabulary)) {
    for (i in seq_along(tokens)) {
      counts <- table(tokens[[i]])
      matrix[i, names(counts)] <- as.numeric(counts)
    }
    doc_frequency <- colSums(matrix > 0)
    keep <- doc_frequency >= min_doc_freq &
      doc_frequency / n <= max_doc_prop
    matrix <- matrix[, keep, drop = FALSE]
  }
  if (binary) matrix[] <- as.numeric(matrix > 0)
  class(matrix) <- c("text_dtm", "matrix", "array")
  attr(matrix, "binary") <- binary
  matrix
}

#' @export
print.text_dtm <- function(x, ...) {
  cat("<text_dtm> ", nrow(x), " documents x ", ncol(x), " terms\n", sep = "")
  print(unclass(x), ...)
  invisible(x)
}

#' Calculate TF-IDF weights
#'
#' @param dtm Numeric document-term matrix.
#' @param normalize Term-frequency normalization: document length, maximum
#'   frequency, or none.
#' @param smooth_idf Logical; use smoothed inverse document frequency?
#'
#' @return A numeric matrix of TF-IDF weights.
#' @export
tf_idf <- function(dtm, normalize = c("length", "max", "none"),
                   smooth_idf = TRUE) {
  normalize <- match.arg(normalize)
  validate_dtm(dtm)
  tf <- unclass(as.matrix(dtm))
  if (normalize == "length") {
    denominator <- rowSums(tf)
    denominator[denominator == 0] <- 1
    tf <- tf / denominator
  } else if (normalize == "max") {
    denominator <- apply(tf, 1, max)
    denominator[denominator == 0] <- 1
    tf <- tf / denominator
  }
  document_frequency <- colSums(dtm > 0)
  idf <- if (smooth_idf) {
    log((1 + nrow(dtm)) / (1 + document_frequency)) + 1
  } else {
    result <- log(nrow(dtm) / document_frequency)
    result[!is.finite(result)] <- 0
    result
  }
  sweep(tf, 2, idf, `*`)
}

#' Extract top TF-IDF keywords
#'
#' @param x A document-term matrix or TF-IDF matrix.
#' @param n Number of keywords per document.
#' @param already_tfidf Logical; is `x` already weighted?
#'
#' @return A data frame with document, rank, term, and weight.
#' @export
extract_keywords <- function(x, n = 1L, already_tfidf = FALSE) {
  validate_dtm(x)
  if (length(n) != 1L || is.na(n) || n < 1) {
    stop("`n` must be at least 1.", call. = FALSE)
  }
  weights <- if (already_tfidf) as.matrix(x) else tf_idf(x)
  output <- lapply(seq_len(nrow(weights)), function(i) {
    values <- weights[i, ]
    order_index <- order(values, decreasing = TRUE)
    order_index <- order_index[values[order_index] > 0]
    order_index <- utils::head(order_index, n)
    if (!length(order_index)) {
      return(data.frame(
        document = character(), rank = integer(), term = character(),
        weight = numeric(), stringsAsFactors = FALSE
      ))
    }
    data.frame(
      document = rownames(weights)[i] %||% as.character(i),
      rank = seq_along(order_index),
      term = colnames(weights)[order_index],
      weight = unname(values[order_index]),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, output)
}

validate_dtm <- function(x) {
  if (!is.matrix(x) || !is.numeric(x) || is.null(colnames(x))) {
    stop("`x` must be a numeric matrix with term column names.",
         call. = FALSE)
  }
  invisible(TRUE)
}

`%||%` <- function(x, y) if (is.null(x)) y else x
