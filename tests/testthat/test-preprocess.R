test_that("sentences split at punctuation and lines", {
  result <- split_sentences("First sentence. Second one!\nThird?")
  expect_equal(result, c("First sentence.", "Second one!", "Third?"))
})

test_that("preprocessing applies requested transformations", {
  result <- preprocess_text(
    "The 2 QUICK, foxes!",
    stopwords = "the",
    min_token_length = 4
  )
  expect_equal(result, "quick foxes")
})

test_that("missing text becomes an empty document", {
  expect_equal(preprocess_text(c(NA, "Data")), c("", "data"))
})

test_that("label files support explicit types", {
  path <- tempfile(fileext = ".txt")
  writeLines(c("0", "1", "", "1"), path)
  expect_equal(read_label_file(path, "integer"), c(0L, 1L, 1L))
})
