## Helper functions for exploring and plotting ChIP-seq data

### Prerequisites

* Bioconductor

```
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install(version = "3.11")

BiocManager::install(c("GenomicRanges","IRanges","HilbertVis"))
```

* tsTools

```
install.packages("devtools")

library(devtools)
install_github("musikutiv/tsTools")
```

* Vennerable

```
BiocManager::install(c("RBGL","graph"))
install_github("js229/Vennerable")
```

### Installation

```
library(devtools)
install_github("tschauer/HelpersforChIPSeq")
```

### Documentation

[**Site Centered Composite Plots**](https://htmlpreview.github.io/?https://github.com/tschauer/HelpersforChIPSeq/blob/master/doc/ChIPseq_CompositePlot.html)

[**Site Centered Heatmaps**](https://htmlpreview.github.io/?https://github.com/tschauer/HelpersforChIPSeq/blob/master/doc/ChIPseq_Heatmaps.html)

[**GeneBody Scaled Composite Plots**](https://htmlpreview.github.io/?https://github.com/tschauer/HelpersforChIPSeq/blob/master/doc/ChIPseq_GeneBody_Scaled.html)

[**Venn Diagram of Peak Overlaps**](https://htmlpreview.github.io/?https://github.com/tschauer/HelpersforChIPSeq/blob/master/doc/ChIPseq_Peak_Overlaps.html)

### Tutorials

[**Working with Centered Coverage Matrices**](https://htmlpreview.github.io/?https://github.com/tschauer/HelpersforChIPSeq/blob/master/doc/Coverage_Matrices.html)

[**Peak Overlaps Tutorial**](https://htmlpreview.github.io/?https://github.com/tschauer/HelpersforChIPSeq/blob/master/doc/ChIPseq_Peak_Overlaps_Tutorial.html)


### Note

The package is highly preliminary. Use it with caution ;)
