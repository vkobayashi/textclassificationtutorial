test_that("cosine similarity has known values", {
  x <- rbind(a = c(1, 0), b = c(1, 1), c = c(0, 0))
  result <- cosine_similarity(x)
  expect_equal(result["a", "a"], 1)
  expect_equal(result["a", "b"], 1 / sqrt(2))
  expect_true(is.na(result["a", "c"]))
})

test_that("cosine similarity validates dimensions", {
  expect_error(cosine_similarity(data.frame(a = 1)), "matrix")
  expect_error(cosine_similarity(matrix(1, 1, 2), matrix(1, 1, 3)),
               "same columns")
})
