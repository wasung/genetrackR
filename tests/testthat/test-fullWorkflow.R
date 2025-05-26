test_that("Full workflow from countReads to exportResults works", {
  skip_if_not_installed("airway")
  skip_if_not_installed("GenomicFeatures")
  skip_if_not_installed("R.utils")

  library(airway)
  library(GenomicFeatures)
  library(R.utils)

  # Stap 1: BAM-bestand (subset van airway)
  bam_file <- system.file("extdata", "SRR1039508_subset.bam", package = "airway")

  # Stap 2: GTF-bestand van jouw package uitpakken
  gtf_gz <- system.file("extdata", "Homo_sapiens.GRCh38.114.gtf.gz", package = "genetrackR")
  gtf_file <- file.path(tempdir(), "annotation.gtf")
  if (!file.exists(gtf_file)) {
    R.utils::gunzip(gtf_gz, destname = gtf_file, remove = FALSE)
  }

  # Stap 3: TxDb aanmaken
  txdb <- GenomicFeatures::makeTxDbFromGFF(gtf_file, format = "gtf")

  # Stap 4: Reads tellen
  se <- countReads(bam_files = bam_file, txdb = txdb)
  expect_s4_class(se, "RangedSummarizedExperiment")

  # Stap 5: Normaliseren
  counts <- assay(se, "counts")
  tpm <- normalizeCounts(counts, txdb = txdb, method = "TPM")
  expect_true(is.matrix(tpm))

  # Stap 6: Plotten
  gene <- rownames(tpm)[1]
  sample <- colnames(tpm)[1]

  p1 <- plotGeneExpression(tpm, geneID = gene)
  p2 <- plotGeneExpression(tpm, sampleID = sample)
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")

  # Stap 7: Exporteren
  exportResults(tpm, filename = "full_test_output.csv", path = tempdir(), type = "table")
  exportResults(p1, filename = "full_test_plot.png", path = tempdir(), type = "plot")

  expect_true(file.exists(file.path(tempdir(), "full_test_output.csv")))
  expect_true(file.exists(file.path(tempdir(), "full_test_plot.png")))
})
