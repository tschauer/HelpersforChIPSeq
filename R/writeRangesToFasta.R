#' writeRangesToFasta
#'
#' writes sequences of genomic ranges into fasta file and ranges into bed file.
#'
#' @param my_genome BSgenome object.
#' @param my_ranges genomic ranges object.
#' @param my_file_path output directory.
#'
#' @export

writeRangesToFasta <- function(my_genome, my_ranges, my_file_path){

        GenomeInfoDb::seqlevelsStyle(my_ranges) <- "UCSC"
        GenomeInfoDb::seqlevelsStyle(my_genome) <- "UCSC"

        my_seqs <- BSgenome::getSeq(my_genome, my_ranges)
        names(my_seqs) <- paste(seqnames(my_ranges), start(my_ranges), end(my_ranges), sep="_")

        Biostrings::writeXStringSet(my_seqs, paste0(my_file_path, ".fasta"))

        rtracklayer::export.bed(my_ranges, paste0(my_file_path, ".bed"))
}
