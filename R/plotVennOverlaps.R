#' plotVennOverlaps
#'
#' plots Venn diagram of overlapping genomic ranges (e.g. peaks)
#'
#' @param my_granges_names character vector of granges object names.
#' @param my_labels character vector of label names.
#' @param my_venn_colors character vector of colors.
#' @param line_width integer of line thickness.
#'
#' @export



plotVennOverlaps <- function(my_granges_names,
                             my_labels = NULL,
                             my_venn_colors = NULL,
                             line_width = 5){

        my_pooled_ranges <- GenomicRanges::GRanges()

        for(i in seq_along(my_granges_names)){
                my_pooled_ranges <- append(my_pooled_ranges, get(my_granges_names[i]))
        }

        my_pooled_ranges <- GenomicRanges::reduce(my_pooled_ranges,
                                                  min.gapwidth = 0L,
                                                  ignore.strand = TRUE)

        my_pooled_ranges$range_id <- paste("range", seq_along(my_pooled_ranges), sep="_")

        my_venn_list <- list()

        for(i in seq_along(my_granges_names)){

                my_overlaps <- !(is.na(GenomicRanges::findOverlaps(my_pooled_ranges, get(my_granges_names[i]), select="arbitrary")))

                my_venn_list[[i]] <- my_pooled_ranges$range_id[my_overlaps]

        }

        if(!is.null(my_labels)){
                names(my_venn_list) <- my_labels

        }

        my_venn_plot <- Vennerable::Venn(my_venn_list)

        my_venn_plot <- tryCatch({
                Vennerable::compute.Venn(my_venn_plot,doWeights=TRUE)
        }, error = function(error_condition) {
                Vennerable::compute.Venn(my_venn_plot,doWeights=FALSE)
        })

        gp <- VennThemes(my_venn_plot)

        for(i in seq_along(gp$Set)){

                gp$Set[[i]]$lwd <- line_width

                if(!is.null(my_venn_colors)){
                        gp$Set[[i]]$col <- my_venn_colors[i]
                        gp$SetText[[i]]$col <- my_venn_colors[i]
                }
        }


        grid::grid.newpage()
        Vennerable::plot(my_venn_plot, show = list(Faces = FALSE, Universe=FALSE), gp = gp)



}
