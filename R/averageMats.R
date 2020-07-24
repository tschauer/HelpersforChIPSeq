#' averageMats
#'
#' average matrices
#' @export



averageMats <- function(my_mats){


        my_mat <- matrix(0, nrow = nrow(get(my_mats[1])), ncol = ncol(get(my_mats[1])))
        rownames(my_mat) <- rownames(get(my_mats[1]))

        for(i in seq_along(my_mats)){

                stopifnot(identical(rownames(my_mat), rownames(get(my_mats[i]))))

                my_mat <- my_mat + get(my_mats[i])
        }

        my_mat <- my_mat/(length(my_mats))

        return(my_mat)
}
