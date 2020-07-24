#' plotLegendtoPosition
#'
#' plot legend to positon of x and y coordinates on figure
#' @export

plotLegendtoPosition <- function(conditions,
                                 legend_colors,
                                 legend_size = 0.8,
                                 horizontal = T,
                                 position = c(0,0.25)){


        par(fig=c(0,1,0,1), mar=c(0,0,0,0), oma=c(0,0,0,0), new=TRUE)

        plot.new()

        legend(x = position[1],y = position[2],
               legend = levels(conditions),
               horiz = horizontal,
               bty = "n",
               cex = legend_size,
               pt.cex = legend_size*1.2,
               pch = 19,
               col =  legend_colors[seq_along(levels(conditions))])


}
