% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coverageGeneBodyScaled.R
\name{coverageGeneBodyScaled}
\alias{coverageGeneBodyScaled}
\title{coverageGeneBodyScaled}
\usage{
coverageGeneBodyScaled(
  my_coverage,
  my_coordinates,
  margin_outer = 500,
  margin_inner = 500,
  genebody_scaler = 1000
)
}
\arguments{
\item{my_coverage}{coverage vector (\code{\link[IRanges]{RleList}})}

\item{my_coordinates}{\code{\link[base]{data.frame }} of genomic coordinates with columns named as "chr", "start", "end","strand".}

\item{margin_outer}{integer of outer window in bp, default 500.}

\item{margin_inner}{integer of inner window in bp, default 500.}

\item{genebody_scaler}{integer of gene body length to be scaled to, default 1000.}
}
\value{
output matrix with each row corresponding a gene and each column a genomic position.
}
\description{
creates a matrix from a coverage vector \code{\link[IRanges]{RleList}}. The matrix is aligned to start and end genomic coordinates
 and  values in between (e.g. gene body) are scaled (re-sized) to a fixed length.
}
\examples{

my_coverage <- RleList("chrI"=  rpois(100000, 10),
                      "chrII"= rpois(150000, 10))
my_coverage

my_coordinates <- data.frame(chr = c("chrI", "chrII", "chrIII"),
                            start = c(1000, 4000, 200),
                            end = c(5000, 14000, 2000),
                            strand = c("+", "-", "-"),
                            row.names = c("gene1", "gene2", "gene3"))
my_coordinates

my_mat <- coverageGeneBodyScaled(my_coverage = my_coverage,
                                my_coordinates = my_coordinates)
my_mat[,1:5]

}
\seealso{
\code{\link[tsTools]{coverageWindowsCenteredStranded}}
}
