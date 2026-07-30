test_that("HTML text is extracted without markup", {
  html <- paste0(
    "<html><head><style>hidden</style></head><body>",
    "<h1>Data Scientist</h1><p>Analyze &amp; model data.</p>",
    "<script>ignored()</script></body></html>"
  )
  result <- extract_html_text(html)
  expect_match(result, "Data Scientist")
  expect_match(result, "Analyze & model data")
  expect_false(grepl("<[^>]+>", result))
  expect_false(grepl("ignored", result))
})

test_that("directory extraction is ordered and identified", {
  path <- tempfile("html-files-")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  writeLines("<body>Second</body>", file.path(path, "b.html"))
  writeLines("<body>First</body>", file.path(path, "a.html"))
  result <- extract_html_dir(path)
  expect_equal(result$document_id, c("a", "b"))
  expect_equal(result$text, c("First", "Second"))
})

test_that("selectors are validated", {
  expect_error(
    extract_html_text("<p>x</p>", selector = "p", xpath = "//p"),
    "only one"
  )
})
