#' coverageGeneBodyScaled
#'
#' creates a matrix from a coverage vector \code{\link[IRanges]{RleList}}. The matrix is aligned to start and end genomic coordinates
#'  and  values in between (e.g. gene body) are scaled (re-sized) to a fixed length.
#'
#' @param my_coverage coverage vector (\code{\link[IRanges]{RleList}})
#' @param my_coordinates \code{\link[base]{data.frame }} of genomic coordinates with columns named as "chr", "start", "end","strand".
#' @param margin_outer integer of outer window in bp, default 500.
#' @param margin_inner integer of inner window in bp, default 500.
#' @param genebody_scaler integer of gene body length to be scaled to, default 1000.
#'
#' @return output matrix with each row corresponding a gene and each column a genomic position.
#'
#'@seealso  \code{\link[tsTools]{coverageWindowsCenteredStranded}}
#'
#' @export
#'
#' @examples
#'
#'my_coverage <- RleList("chrI"=  rpois(100000, 10),
#'                       "chrII"= rpois(150000, 10))
#'my_coverage
#'
#'my_coordinates <- data.frame(chr = c("chrI", "chrII", "chrIII"),
#'                             start = c(1000, 4000, 200),
#'                             end = c(5000, 14000, 2000),
#'                             strand = c("+", "-", "-"),
#'                             row.names = c("gene1", "gene2", "gene3"))
#'my_coordinates
#'
#'my_mat <- coverageGeneBodyScaled(my_coverage = my_coverage,
#'                                 my_coordinates = my_coordinates)
#'my_mat[,1:5]
#'






coverageGeneBodyScaled <- function(my_coverage,
                                   my_coordinates,
                                   margin_outer = 500,
                                   margin_inner = 500,
                                   genebody_scaler = 1000)
{
        my_coordinates <- my_coordinates[my_coordinates$chr %in% names(my_coverage), ]

        my_coverage <- my_coverage[names(my_coverage) %in% my_coordinates$chr]


        my_list <- lapply(names(my_coverage), function(x) {

                my_cov <- my_coverage[[x]]

                my_coords <- my_coordinates[my_coordinates$chr == x, ]

                mw.views.up <-   IRanges::Views(subject = my_cov,
                                                start = my_coords$start - (margin_outer),
                                                end =   my_coords$start + (margin_inner-1),
                                                names = rownames(my_coords))

                mw.views.body <- IRanges::Views(subject = my_cov,
                                                start = my_coords$start + (margin_inner),
                                                end =   my_coords$end  -  (margin_inner+1),
                                                names = rownames(my_coords))

                mw.views.down <- IRanges::Views(subject = my_cov,
                                                start = my_coords$end - (margin_inner),
                                                end =   my_coords$end + (margin_outer-1),
                                                names = rownames(my_coords))

                my_filter <- start(mw.views.up) > 0 & end(mw.views.down) < length(my_cov)

                mw.views.up <-   mw.views.up[my_filter, ]
                mw.views.body <- mw.views.body[my_filter, ]
                mw.views.down <- mw.views.down[my_filter, ]

                stopifnot(exprs = {
                        identical(rownames(mw.views.up), rownames(mw.views.body))
                        identical(rownames(mw.views.up), rownames(mw.views.down))
                })

                my_mat <- cbind(as.matrix(trim(mw.views.up)),
                                t(apply(as.matrix(trim(mw.views.body)), 1, function(x){resizeVector(x, genebody_scaler)})),
                                as.matrix(trim(mw.views.down)))


                if(nrow(my_mat) > 0) {

                        return(my_mat)
                }
                else {
                        return(NULL)
                }
        })

        my_mat <- Reduce(rbind, my_list)

        my_selection <- rownames(my_mat) %in% rownames(my_coordinates)[my_coordinates$strand == "-"]

        my_mat[my_selection,] <- t(apply(my_mat[my_selection,,drop=F], 1, rev))

        my_mat[is.na(my_mat)] <- 0
        my_mat[my_mat < 0] <- 0

        return(my_mat)
}



#' resizeVector
#'
#' resizes a vector \code{x} to a length of \code{out_length} using spline interpolation.
#' @export

resizeVector <- function(x, out_length){

        y <- spline(x = 1:length(x),
                    y = x,
                    n = out_length)$y

        return(y)
}



#' axisGeneBodyScaled
#'
#' ass x-axis to composite plot of gene body scaled matrix
#' @export

axisGeneBodyScaled <- function(margin_outer = 500,
                               margin_inner = 500,
                               genebody_scaler = 1000,
                               add_line = TRUE)
{

        matrix_width <- 2*margin_outer+2*margin_inner+genebody_scaler

        if(margin_inner > 0){
                my_labels <- c(-margin_outer,paste0("+",margin_inner),-margin_inner, paste0("+",margin_outer))
        } else {
                my_labels <- c(-margin_outer,"","",paste0("+",margin_outer))

        }

        axis(side = 1,
             at = c(1, margin_outer+margin_inner,
                    margin_outer+margin_inner+genebody_scaler,
                    matrix_width),
             labels = my_labels)

        axis(side = 1, line = 1, lwd = 0, lwd.ticks = 1,
             at = c(margin_outer,matrix_width-margin_outer),
             labels = c("TSS","TTS"))

        if(add_line){
                abline(v = c(margin_outer,
                             matrix_width-margin_outer), lty = 2)

                if(margin_inner > 0){
                        abline(v = c(1,
                                     margin_outer+margin_inner,
                                     margin_outer+margin_inner+genebody_scaler,
                                     matrix_width), lty = 3)
                }
        }

}


