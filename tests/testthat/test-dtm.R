test_that("DTM contains expected counts", {
  dtm <- document_term_matrix(
    c("data science data", "science model"),
    document_ids = c("a", "b")
  )
  expect_s3_class(dtm, "text_dtm")
  expect_equal(unname(dtm["a", "data"]), 2)
  expect_equal(unname(dtm["b", "model"]), 1)
  expect_equal(rownames(dtm), c("a", "b"))
})

test_that("document frequency filters work", {
  dtm <- document_term_matrix(c("a b", "a c", "a d"), min_doc_freq = 2)
  expect_equal(colnames(dtm), "a")
  dtm2 <- document_term_matrix(c("a b", "a c", "a d"), max_doc_prop = 0.9)
  expect_false("a" %in% colnames(dtm2))
})

test_that("TF-IDF and keyword extraction are well formed", {
  dtm <- document_term_matrix(c("data data model", "care patient"))
  weighted <- tf_idf(dtm)
  expect_equal(dim(weighted), dim(dtm))
  expect_true(all(is.finite(weighted)))
  keywords <- extract_keywords(dtm, n = 2)
  expect_named(keywords, c("document", "rank", "term", "weight"))
  expect_true(all(keywords$weight > 0))
})

test_that("invalid DTM inputs fail clearly", {
  expect_error(document_term_matrix(character()), "at least one")
  expect_error(document_term_matrix(c("a", "b"), document_ids = c("x", "x")),
               "unique")
})
