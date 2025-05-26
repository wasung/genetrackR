test_that("countReads returns a RangedSummarizedExperiment", {
  skip_if_not_installed("airway")
  skip_if_not_installed("GenomicFeatures")
  skip_if_not_installed("R.utils")

  library(airway)
  library(GenomicFeatures)
  library(R.utils)

  # BAM-bestand uit airway
  bam_file <- system.file("extdata", "SRR1039508_subset.bam", package = "airway")

  # Pad naar het GTF-bestand in jouw eigen package
  gtf_gz <- system.file("extdata", "Homo_sapiens.GRCh38.114.gtf.gz", package = "genetrackR")
  gtf_file <- file.path(tempdir(), "annotation.gtf")

  # Uitpakken als dat nog niet gebeurd is
  if (!file.exists(gtf_file)) {
    R.utils::gunzip(gtf_gz, destname = gtf_file, remove = FALSE)
  }

  # Maak een TxDb object
  txdb <- GenomicFeatures::makeTxDbFromGFF(gtf_file, format = "gtf")

  # Run je functie
  se <- countReads(bam_files = bam_file, txdb = txdb)

  # Check output
  expect_s4_class(se, "RangedSummarizedExperiment")
  expect_true("counts" %in% assayNames(se))
})
