#' plotHeatmap
#'
#' plot heatmap signal
#' @export


plotHeatmap <- function(my_sample_mats,
                        my_sample_names = "",
                        my_site_name = "0",
                        font_size = 0.75,
                        my_colors = colorRampPalette(brewer.pal(9, "Blues"))(100),
                        min_value = 1,
                        max_value = 5,
                        my_binning = 10,
                        smoother = 1){

        hidx <- seq(0, 1, length.out = length(my_sample_mats)+1)


        for(i in seq_along(my_sample_mats)){

                my_sample <- my_sample_mats[i]

                my_mat <- get(my_sample)
                x_range <- ncol(my_mat)

                ###########################################

                my_mat[my_mat < min_value] <- min_value
                my_mat[my_mat > max_value] <- max_value

                my_breaks <- seq(min_value, max_value, length.out = 101)

                ###########################################

                par(mar=c(3,1,4,1), cex = font_size)
                par(fig=c(hidx[i],hidx[i+1],0.075,1), new=TRUE)

                if(smoother > 1){
                        my_mat <- t(apply(my_mat, 1, function(x){
                                zoo::rollmean(c(rep(x[1], (smoother-1)/2), x, rep(x[x_range], (smoother-1)/2)),
                                              smoother)}))
                }


                image(t(my_mat)[,nrow(my_mat):1],
                      col = my_colors,
                      breaks =  my_breaks,
                      axes=FALSE,
                      useRaster = TRUE)


                ###########################################

                my_title <- my_sample_names[i]

                title(main = my_title, line = 1)

                axis(side = 1, at = c(0,0.5,1), labels = c("", gsub(".*\\.","",my_site_name), ""))
                axis(side = 1, at = c(0.1,0.9),
                     labels = c(paste("-",round((x_range/2)*my_binning/1000), "kb", sep=""),
                                paste("+",round((x_range/2)*my_binning/1000), "kb", sep="")),
                     col.ticks = NA, col = NA)

                ###########################################

                par(fig=c(hidx[i],hidx[i+1],0,0.12), new=TRUE, mar=c(3,1,2,1))

                image(matrix(seq_along(my_colors)), col=my_colors, axes=FALSE, useRaster = TRUE)
                axis(side = 1, at = c(0,0.5,1), labels = round(my_breaks[c(1,51,101)],1))

        }

}
