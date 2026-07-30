#' Extract readable text from HTML
#'
#' Extracts text from an HTML file or character string. When the suggested
#' `xml2` package is installed, simple CSS selectors (`tag`, `.class`, `#id`,
#' or `tag.class`) or XPath can target a specific part of the page. A
#' dependency-free fallback strips markup from the full document.
#'
#' @param x Path to an HTML file or a length-one HTML character string.
#' @param selector Optional CSS selector.
#' @param xpath Optional XPath expression. Supply at most one of `selector`
#'   and `xpath`.
#' @param collapse Character used to join matched nodes.
#' @param trim Logical; normalize whitespace and trim the result?
#'
#' @return A length-one character vector containing extracted text.
#' @export
#'
#' @examples
#' html <- "<html><body><h1>Analyst</h1><p>Analyze data.</p></body></html>"
#' extract_html_text(html)
extract_html_text <- function(x, selector = NULL, xpath = NULL,
                              collapse = "\n", trim = TRUE) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop("`x` must be one HTML string or file path.", call. = FALSE)
  }
  if (!is.null(selector) && !is.null(xpath)) {
    stop("Supply only one of `selector` and `xpath`.", call. = FALSE)
  }
  is_file <- file.exists(x)
  html <- if (is_file) {
    paste(readLines(x, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  } else {
    x
  }

  if (requireNamespace("xml2", quietly = TRUE)) {
    document <- if (is_file) xml2::read_html(x) else xml2::read_html(html)
    unwanted <- xml2::xml_find_all(document, ".//script|.//style")
    if (length(unwanted)) xml2::xml_remove(unwanted)
    nodes <- if (!is.null(selector)) {
      xml2::xml_find_all(document, css_to_xpath(selector))
    } else if (!is.null(xpath)) {
      xml2::xml_find_all(document, xpath)
    } else {
      xml2::xml_find_all(document, ".//body")
    }
    if (!length(nodes)) return(character())
    text <- paste(xml2::xml_text(nodes, trim = trim), collapse = collapse)
  } else {
    if (!is.null(selector) || !is.null(xpath)) {
      stop("Install `xml2` to use CSS or XPath selection.", call. = FALSE)
    }
    html <- gsub("(?is)<(script|style)[^>]*>.*?</\\1>", " ", html,
                 perl = TRUE)
    html <- gsub("(?i)<br\\s*/?>|</p>|</li>|</h[1-6]>", "\n", html,
                 perl = TRUE)
    text <- gsub("(?s)<[^>]+>", " ", html, perl = TRUE)
    text <- decode_html_entities(text)
  }
  if (trim) normalize_text_whitespace(text) else text
}

#' Extract text from a directory of HTML files
#'
#' @param path Directory containing HTML files.
#' @param pattern File-name regular expression.
#' @param recursive Logical; search recursively?
#' @param ... Passed to [extract_html_text()].
#'
#' @return A data frame with `document_id`, `path`, and `text`.
#' @export
extract_html_dir <- function(path, pattern = "\\.html?$", recursive = FALSE,
                             ...) {
  if (!dir.exists(path)) stop("Directory does not exist: ", path,
                              call. = FALSE)
  files <- list.files(path, pattern = pattern, full.names = TRUE,
                      recursive = recursive, ignore.case = TRUE)
  files <- sort(files)
  if (!length(files)) {
    return(data.frame(document_id = character(), path = character(),
                      text = character()))
  }
  data.frame(
    document_id = tools::file_path_sans_ext(basename(files)),
    path = normalizePath(files, winslash = "/", mustWork = TRUE),
    text = vapply(files, extract_html_text, character(1), ...),
    stringsAsFactors = FALSE
  )
}

decode_html_entities <- function(x) {
  replacements <- c(
    "&nbsp;" = " ", "&amp;" = "&", "&lt;" = "<", "&gt;" = ">",
    "&quot;" = "\"", "&#39;" = "'"
  )
  for (entity in names(replacements)) {
    x <- gsub(entity, replacements[[entity]], x, fixed = TRUE)
  }
  numeric_entity <- "&#([0-9]+);"
  while (grepl(numeric_entity, x, perl = TRUE)) {
    value <- as.integer(sub(paste0(".*?", numeric_entity, ".*"), "\\1", x,
                            perl = TRUE))
    x <- sub(numeric_entity, intToUtf8(value), x, perl = TRUE)
  }
  x
}

normalize_text_whitespace <- function(x) {
  x <- gsub("[\\t\\r ]+", " ", x)
  x <- gsub(" *\\n+ *", "\n", x)
  trimws(x)
}

css_to_xpath <- function(selector) {
  if (!is.character(selector) || length(selector) != 1L ||
      is.na(selector) || !nzchar(selector)) {
    stop("`selector` must be one non-empty CSS selector.", call. = FALSE)
  }
  selector <- trimws(selector)
  if (grepl("^#[A-Za-z][A-Za-z0-9_-]*$", selector)) {
    return(paste0(".//*[@id='", substring(selector, 2), "']"))
  }
  if (grepl("^\\.[A-Za-z][A-Za-z0-9_-]*$", selector)) {
    class <- substring(selector, 2)
    return(paste0(
      ".//*[contains(concat(' ', normalize-space(@class), ' '), ' ",
      class, " ')]"
    ))
  }
  if (grepl("^[A-Za-z][A-Za-z0-9_-]*\\.[A-Za-z][A-Za-z0-9_-]*$",
            selector)) {
    parts <- strsplit(selector, ".", fixed = TRUE)[[1]]
    return(paste0(
      ".//", parts[1],
      "[contains(concat(' ', normalize-space(@class), ' '), ' ",
      parts[2], " ')]"
    ))
  }
  if (grepl("^[A-Za-z][A-Za-z0-9_-]*$", selector)) {
    return(paste0(".//", selector))
  }
  stop(
    "Supported CSS selectors are tag, .class, #id, and tag.class. ",
    "Use `xpath` for complex selections.",
    call. = FALSE
  )
}
