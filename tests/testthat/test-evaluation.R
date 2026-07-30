test_that("classification metrics use correct confusion counts", {
  truth <- c("yes", "yes", "no", "no")
  estimate <- c("yes", "no", "yes", "no")
  result <- classification_metrics(truth, estimate, positive = "yes")
  expect_equal(result$true_positive, 1)
  expect_equal(result$false_positive, 1)
  expect_equal(result$true_negative, 1)
  expect_equal(result$false_negative, 1)
  expect_equal(result$balanced_accuracy, 0.5)
  expect_equal(result$f1, 0.5)
})

test_that("F-measure handles zero precision and recall", {
  expect_equal(f_measure(0, 0), 0)
  expect_equal(f_measure(1, 1), 1)
  expect_error(f_measure(1.2, 1), "between 0 and 1")
})

test_that("folds are stratified, complete, and reproducible", {
  y <- rep(c("a", "b"), each = 6)
  first <- stratified_folds(y, k = 3, repeats = 2, seed = 42)
  second <- stratified_folds(y, k = 3, repeats = 2, seed = 42)
  expect_equal(first, second)
  expect_length(first, 6)
  for (fold in first) expect_setequal(unique(y[fold]), c("a", "b"))
  expect_setequal(unlist(first[1:3]), seq_along(y))
})
