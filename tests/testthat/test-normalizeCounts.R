test_that("normalizeCounts returns a normalized matrix", {
  raw_counts <- matrix(c(100, 200, 300, 400), nrow=2,
                       dimnames = list(c("gene1", "gene2"), c("sample1", "sample2")))

  norm_counts <- normalizeCounts(raw_counts, method = "CPM")

  expect_true(all(dim(norm_counts) == dim(raw_counts)))
  expect_true(all(norm_counts > 0))
})
