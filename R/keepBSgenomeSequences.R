#' keepBSgenomeSequences
#'
#' subset BS genome by chromosome(s)
#'
#' Source: http://supportupgrade.bioconductor.org/p/83588/#83594
#' @export


keepBSgenomeSequences <- function(genome, seqnames)
{
        stopifnot(all(seqnames %in% seqnames(genome)))
        genome@user_seqnames <- setNames(seqnames, seqnames)
        genome@seqinfo <- genome@seqinfo[seqnames]
        genome
}
