#' countReadsOverGrangesInBAM
#'
#' count the number of reads over genomic ranges in a BAM file
#'
#' @export



countReadsOverGrangesInBAM <- function(my_ranges,
                                       bam_file,
                                       bam_type = c("SINGLE","PAIRED"),
                                       single_read.unique = FALSE,
                                       frag.min = 0,
                                       frag.max = 500,
                                       frag.length = 150,
                                       frag.unique = c(NULL, "oneEND", "bothEND"),
                                       subsampling = c(NULL, 1e6)){

        if(bam_type == "SINGLE"){

                my_bam <- GenomicAlignments::readGAlignments(bam_file)
                grs <- GenomicRanges::granges(my_bam)

                if(single_read.unique){
                        grs <- unique(grs)
                }

                if(!(is.null(subsampling))){
                        set.seed(111)
                        grs <- grs[sample(length(grs), subsampling)]
                }

                grsr <- GenomicRanges::resize(grs,  width = frag.length, fix = "start")
                grsr <- GenomicRanges::resize(grsr, width = 1, fix = "center")

                bam_counts <- GenomicRanges::countOverlaps(my_ranges, subject = grsr, ignore.strand = TRUE)

        } else if(bam_type == "PAIRED"){

                my_bam <- GenomicAlignments::readGAlignmentPairs(bam_file)
                grs <- GenomicRanges::granges(my_bam, on.discordant.seqnames="drop")

                if(!(is.null(frag.unique))){

                        if(frag.unique == "oneEND"){
                                grs <- unique(grs)
                        }

                        if(frag.unique == "bothEND"){

                                grs$frag_start <- paste(seqnames(grs), start(grs), sep="_")
                                grs$frag_end <-   paste(seqnames(grs), end(grs),   sep="_")

                                grs <- grs[!(duplicated(grs$frag_start)) & !(duplicated(grs$frag_end))]
                        }
                }

                grs <- grs[GenomicRanges::width(grs) > frag.min & GenomicRanges::width(grs) < frag.max]


                if(!(is.null(subsampling))){
                        set.seed(111)
                        grs <- grs[sample(length(grs), subsampling)]
                }

                grsr <- GenomicRanges::resize(grs, 1, fix = "center")

                bam_counts <- GenomicRanges::countOverlaps(my_ranges, subject = grsr, ignore.strand = TRUE)

        }

        return(bam_counts)

}
