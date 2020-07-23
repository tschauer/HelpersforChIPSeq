#' plotComposite
#'
#' plot composite signal
#' @export





plotComposite <- function(my_sample_mats,
                          my_sub_range = 1:400,
                          ylims = c(1,5),
                          my_binning = 10,
                          my_colors_composite,
                          my_title = "",
                          yaxt ="s",
                          site_label = 0,
                          add_line = FALSE,
                          line_lwd = 2,
                          smoother = 11,
                          log_scale = FALSE,
                          show_error = TRUE){


        x_range <- ncol(get(my_sample_mats[1], envir = .GlobalEnv)[,my_sub_range])
        y_data <- colMeans(get(my_sample_mats[1], envir = .GlobalEnv), na.rm = TRUE)[my_sub_range]


        plot(1:x_range, y_data, xaxt = "n", yaxt = yaxt,
             main = my_title, xlab = "", ylab = "",
             type="n", ylim = ylims)

        #abline(h=1)


        axis(side = 1, at = seq(1, x_range , length.out = 3),
             labels =  c( paste("-",round((x_range/2)*my_binning/1000), "kb", sep="") ,
                          site_label,
                          paste("+",round((x_range/2)*my_binning/1000), "kb", sep=""))
        )

        # axis(side = 1, at = c(250, 1000, 1750), labels = c("-750", "0", "+750"))



        for(i in seq_along(my_sample_mats)){

                my_sample_mat <- get(my_sample_mats[i])[,my_sub_range]

                if(log_scale){
                        my_sample_mat <- log2(my_sample_mat+0.001)
                }

                x_range <- ncol(my_sample_mat)
                y_data <- colMeans(my_sample_mat)

                my_yerror <- apply(my_sample_mat, 2, function(x){ qt(0.975, df = length(x)-1)*sd(x, na.rm = TRUE)/sqrt(length(x)) })

                y_data <- zoo::rollmean(y_data, smoother)
                my_yerror <- zoo::rollmean(my_yerror, smoother)


                xx <- c(((smoother-1)/2+1):(x_range-((smoother-1)/2)), (x_range-((smoother-1)/2)):((smoother-1)/2+1))
                yy <- c(y_data-my_yerror, rev(y_data+my_yerror))

                if(show_error){
                        polygon(xx, yy, border = NA, col = paste(my_colors_composite[i], "55",sep=""))
                }


                lines(((smoother-1)/2+1):(x_range-((smoother-1)/2)), y_data, col = my_colors_composite[i], lwd=line_lwd)


        }

        if(add_line){
                abline(h = c(min(y_data) , max(y_data)), lty=2)
        }

}
