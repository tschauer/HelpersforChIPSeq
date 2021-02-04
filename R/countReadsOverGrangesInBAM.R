#' countReadsOverGrangesInBAM
#'
#' count the number of reads over genomic ranges in a BAM file
#'
#' @export



countReadsOverGrangesInBAM <- function(my_ranges,
                                       bam_file,
                                       bam_type = c("SINGLE","PAIRED"),
                                       subsampling = NULL,
                                       frag.min = 0,
                                       frag.max = 500,
                                       frag.length = 150){

        if(bam_type == "SINGLE"){

                my_bam <- GenomicAlignments::readGAlignments(bam_file)
                grs <- GenomicRanges::granges(my_bam)

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
