training_dtm <- structure(
  rbind(
    c(3, 0), c(2, 0), c(0, 3), c(0, 2)
  ),
  dimnames = list(NULL, c("analysis", "care"))
)
training_labels <- c("data", "data", "health", "health")

test_that("Naive Bayes learns and predicts separable documents", {
  model <- fit_naive_bayes(training_dtm, training_labels)
  expect_s3_class(model, "text_nb")
  predictions <- predict(model, training_dtm)
  expect_equal(as.character(predictions), training_labels)
  probabilities <- predict(model, training_dtm, type = "prob")
  expect_equal(rowSums(probabilities), rep(1, 4), tolerance = 1e-12)
})

test_that("prediction handles absent and extra terms", {
  model <- fit_naive_bayes(training_dtm, training_labels)
  newdata <- matrix(c(2, 99), nrow = 1,
                    dimnames = list("new", c("analysis", "extra")))
  prediction <- predict(model, newdata)
  expect_equal(as.character(prediction), "data")
})

test_that("Naive Bayes validates model inputs", {
  expect_error(fit_naive_bayes(training_dtm, training_labels[1:3]),
               "one non-missing")
  expect_error(fit_naive_bayes(training_dtm, rep("data", 4)),
               "two classes")
  expect_error(fit_naive_bayes(training_dtm, training_labels, laplace = -1),
               "nonnegative")
})
