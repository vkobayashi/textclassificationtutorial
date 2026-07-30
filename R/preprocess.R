#' Split text into sentences
#'
#' A lightweight sentence segmenter suitable for tutorials and clean prose.
#' It splits at terminal punctuation followed by whitespace, and optionally at
#' line breaks and vertical bars. For production multilingual segmentation,
#' use a dedicated NLP tokenizer.
#'
#' @param text Character vector.
#' @param split_lines Logical; treat line breaks and `|` as boundaries?
#' @param keep_punctuation Logical; retain terminal punctuation?
#' @param drop_empty Logical; remove empty results?
#'
#' @return A character vector of sentences.
#' @export
#'
#' @examples
#' split_sentences("Analyze data. Present results! Work with teams?")
split_sentences <- function(text, split_lines = TRUE, keep_punctuation = TRUE,
                            drop_empty = TRUE) {
  if (!is.character(text)) stop("`text` must be character.", call. = FALSE)
  text[is.na(text)] <- ""
  boundary <- if (keep_punctuation) {
    "(?<=[.!?])\\s+"
  } else {
    "[.!?]+\\s*"
  }
  if (split_lines) {
    text <- gsub("[\r\n|]+", "\n", text)
    boundary <- paste0(boundary, "|\\n+")
  }
  pieces <- unlist(strsplit(text, boundary, perl = TRUE), use.names = FALSE)
  pieces <- trimws(pieces)
  if (drop_empty) pieces[nzchar(pieces)] else pieces
}

#' Normalize text for document-term analysis
#'
#' @param text Character vector.
#' @param lowercase Logical; convert text to lowercase?
#' @param remove_punctuation Logical; replace punctuation with spaces?
#' @param remove_numbers Logical; replace digits with spaces?
#' @param stopwords Optional character vector of words to remove.
#' @param min_token_length Minimum number of characters per token.
#'
#' @return A character vector of normalized documents.
#' @export
#'
#' @examples
#' preprocess_text(
#'   c("Analyze the data!", "Present 2 reports."),
#'   stopwords = c("the")
#' )
preprocess_text <- function(text, lowercase = TRUE, remove_punctuation = TRUE,
                            remove_numbers = TRUE, stopwords = character(),
                            min_token_length = 1L) {
  if (!is.character(text)) stop("`text` must be character.", call. = FALSE)
  if (!is.character(stopwords)) stop("`stopwords` must be character.",
                                     call. = FALSE)
  if (length(min_token_length) != 1L || is.na(min_token_length) ||
      min_token_length < 1) {
    stop("`min_token_length` must be at least 1.", call. = FALSE)
  }
  text[is.na(text)] <- ""
  if (lowercase) {
    text <- tolower(text)
    stopwords <- tolower(stopwords)
  }
  if (remove_punctuation) text <- gsub("[[:punct:]]+", " ", text)
  if (remove_numbers) text <- gsub("[[:digit:]]+", " ", text)
  text <- gsub("[[:space:]]+", " ", text)
  text <- trimws(text)

  clean_one <- function(document) {
    tokens <- strsplit(document, "\\s+", perl = TRUE)[[1]]
    tokens <- tokens[nzchar(tokens)]
    tokens <- tokens[nchar(tokens) >= min_token_length]
    tokens <- tokens[!tokens %in% stopwords]
    paste(tokens, collapse = " ")
  }
  vapply(text, clean_one, character(1), USE.NAMES = FALSE)
}

#' Read a one-label-per-line file
#'
#' @param path Path to a text file.
#' @param type Return labels as character, integer, numeric, or factor.
#'
#' @return A vector of labels.
#' @export
read_label_file <- function(path,
                            type = c("character", "integer", "numeric",
                                     "factor")) {
  type <- match.arg(type)
  if (!file.exists(path)) stop("File does not exist: ", path, call. = FALSE)
  labels <- trimws(readLines(path, warn = FALSE, encoding = "UTF-8"))
  labels <- labels[nzchar(labels)]
  switch(
    type,
    character = labels,
    integer = as.integer(labels),
    numeric = as.numeric(labels),
    factor = factor(labels)
  )
}
