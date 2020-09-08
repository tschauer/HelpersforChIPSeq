#' orderMats
#'
#' order or filter matrices by a vector
#'
#' @param my_mats Character vector of matrix names.
#' @param my_order Integer vector of the matrix row order.
#'
#' @export


orderMats <- function(my_mats,
                      my_order = 1:nrow(get(my_mats[1]))){

        stopifnot(
                all(sapply(seq_along(my_mats), function(i){(identical(rownames(get(my_mats[length(my_mats)])), rownames(get(my_mats[i]))))}))
        )

        for(j in seq_along(my_mats)){

                my_mat_name <- my_mats[j]
                my_mat <- get(my_mat_name)

                my_mat <- my_mat[my_order,]

                assign(my_mat_name, my_mat, envir = .GlobalEnv)
        }

        stopifnot(
                all(sapply(seq_along(my_mats), function(i){(identical(rownames(get(my_mats[length(my_mats)])), rownames(get(my_mats[i]))))}))
        )
}
