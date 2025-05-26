test_that("exportResults saves files correctly", {
  tmpdir <- tempdir()

  # Test table export
  test_data <- data.frame(Gene = c("A", "B"), Value = c(1, 2))
  filename_table <- "test_output.csv"
  exportResults(test_data, filename = filename_table, path = tmpdir, type = "table")
  fullpath_table <- file.path(tmpdir, filename_table)
  expect_true(file.exists(fullpath_table))

  # Test plot export
  library(ggplot2)
  p <- ggplot(test_data, aes(x = Gene, y = Value)) + geom_bar(stat = "identity")
  filename_plot <- "test_plot.png"
  exportResults(p, filename = filename_plot, path = tmpdir, type = "plot")
  fullpath_plot <- file.path(tmpdir, filename_plot)
  expect_true(file.exists(fullpath_plot))

  # Test error on invalid type
  expect_error(
    exportResults(test_data, filename = "fail.txt", path = tmpdir, type = "invalid"),
    "should be one of"
  )
})
