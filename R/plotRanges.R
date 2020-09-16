#' plotRanges
#'
#' plots rectangles representing genomic ranges (e.g. peaks)
#'
#' @export




plotRanges <- function(my_ranges,
                       my_region,
                       my_color,
                       my_border = my_color,
                       my_ylims = c(0,1),
                       my_title = "Track" ){

        plot(c(1, end(my_region) - start(my_region)),
             c(1,1), type="n", ylim = my_ylims, bty = "n",
             xaxt = "n", yaxt = "n", xlab ="", ylab = "", main = "")

        if(my_color == "white"){
                title(main = my_title, line = -2, adj = 0, col.main = my_border, cex.main = 2)
        } else {
                title(main = my_title, line = -2, adj = 0, col.main = my_color, cex.main = 2)
        }

        my_ranges_subset <- subsetByOverlaps(my_ranges, my_region)

        if(length(my_ranges_subset) != 0){

                my_starts <- start(my_ranges_subset) - start(my_region)
                my_ends   <- end(my_ranges_subset)   - start(my_region)

                par(xpd = NA)
                rect(xleft = my_starts, xright = my_ends,
                     ytop = my_ylims[1] + 0.2,
                     ybottom = my_ylims[2] -0.2,
                     border = my_border, col = my_color, lwd=3)
                par(xpd = FALSE)

        }


}
