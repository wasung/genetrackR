#' Normalizes a count matrix via CPM or TPM (optimized).
#'
#' @param counts Raw count matrix (genes x samples) or SummarizedExperiment with assay 'counts'.
#' @param txdb Optional TxDb object for TPM; required if method = "TPM".
#' @param method Normalization method: "CPM" (Counts Per Million) or "TPM" (Transcripts Per Million).
#' @return Normalized expression matrix.
#' @export
normalizeCounts <- function(counts, txdb = NULL, method = c("CPM", "TPM")) {
  method <- match.arg(method)

  if (inherits(counts, "SummarizedExperiment")) {
    counts <- SummarizedExperiment::assay(counts, "counts")
  }
  counts_mat <- as.matrix(counts)

  if (method == "CPM") {
    libSizes <- colSums(counts_mat)
    if (any(libSizes == 0)) stop("One or more samples have 0 total counts.")
    cpm <- t(t(counts_mat) / libSizes) * 1e6
    return(cpm)
  }

  if (method == "TPM") {
    if (is.null(txdb)) stop("TxDb object is required for TPM normalization.")

    genes <- rownames(counts_mat)
    exons_list <- GenomicFeatures::exonsBy(txdb, by = "gene")
    exons_list <- exons_list[genes]  # filter only relevant genes
    exons_reduced <- lapply(exons_list, GenomicRanges::reduce)
    gene_length <- sapply(exons_reduced, function(x) sum(GenomicRanges::width(x)))

    if (!all(genes %in% names(gene_length))) {
      missing_genes <- genes[!genes %in% names(gene_length)]
      stop("Some genes in count matrix are missing in TxDb: ", paste(head(missing_genes), collapse = ", "))
    }

    lengths_kb <- gene_length[genes] / 1000
    RPK <- counts_mat / lengths_kb
    per_million <- colSums(RPK) / 1e6
    tpm <- t(t(RPK) / per_million)
    return(tpm)
  }
}
