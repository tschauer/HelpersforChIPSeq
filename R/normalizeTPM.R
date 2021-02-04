#' normalizeTPM
#'
#' normalize read counts by TPM
#'
#' @export


normalizeTPM <- function(my_counts, effective_length, scaler = 1e6)
{
        rate <- log(my_counts) - log(effective_length)
        denom <- log(sum(exp(rate)))
        tpm <- exp(rate - denom + log(scaler))

        return(tpm)
}
