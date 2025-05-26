genetrackR is an R package for RNA-seq data analysis (FASTQ/BAM). It counts reads per gene from BAM files, performs normalization (CPM/TPM), and visualizes gene expression. By leveraging Bioconductor packages, it enables efficient and reproducible processing of large datasets.

##Installation
First, install the required Bioconductor packages (e.g., GenomicFeatures, GenomicAlignments, etc.). Then you can install this package from GitHub:
# In R:
if (!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("your_username/genetrackR")
