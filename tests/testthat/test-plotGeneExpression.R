test_that("plotGeneExpression returns a ggplot object", {
  expr_data <- matrix(c(10, 30, 5, 15), nrow = 2,
                      dimnames = list(c("geneA", "geneB"), c("sample1", "sample2")))

  # Test geneID
  p1 <- plotGeneExpression(expr_data, geneID = "geneA", normalized = TRUE)
  expect_s3_class(p1, "ggplot")

  # Test sampleID
  p2 <- plotGeneExpression(expr_data, sampleID = "sample1", normalized = TRUE)
  expect_s3_class(p2, "ggplot")

  # Test error when both geneID and sampleID are given
  expect_error(
    plotGeneExpression(expr_data, geneID = "geneA", sampleID = "sample1"),
    "Geef óf geneID óf sampleID op"
  )

  # Test error when invalid geneID
  expect_error(
    plotGeneExpression(expr_data, geneID = "geneX"),
    "geneID niet gevonden"
  )
})
