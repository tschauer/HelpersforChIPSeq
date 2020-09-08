#' binMeansMatrix
#'
#' calculate the mean of non-overlapping bins for each row in a matrix.
#'
#' @param my_mat input matrix.
#' @param my_binning integer of bin size.
#'
#' @return output matrix with original number of rows and \code{my_binning} times less number of columns.
#'
#'@seealso  \code{\link[matrixStats]{binMeans}}
#'
#' @export
#'
#' @examples
#'m <- matrix(1:100, ncol=10)
#'binMeansMatrix(m, 2)

binMeansMatrix <- function(my_mat,
                           my_binning = 10){

        t(apply(my_mat, 1, function(y){

                matrixStats::binMeans(y = y,
                                      x = 1:length(y),
                                      bx = seq(1, length(y)+1, my_binning))
        }))

}
