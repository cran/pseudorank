################################################################################
### File: pseudoranks.R
### Description: Function to calculate mid-pseudo-ranks
###
################################################################################


#globalVariables("_pseudoranks_psrank")

#' Calculation of Pseudo-Ranks
#'
#' @description Calculation of (mid) pseudo-ranks of a sample. In case of ties (i.e. equal values), the average of min pseudo-rank and max-pseudor-rank are taken (similar to rank with ties.method="average").
#' @param data numerical vector
#' @param group vector coding for the groups
#' @return Returns a numerical vector containing the pseudo-ranks
#' @seealso \code{\link{rank}}.
#' @keywords internal
recursiveCalculation <- function(data, group) {

  stopifnot(is.numeric(data), is.factor(group))
  n <- as.numeric(as.matrix(table(group)))

  if( identical(n,rep(n[1],length(n)))  ) {
    return(rank(data, ties.method = "average"))
  } else {
    id <- 1:length(data)
    df <- matrix(c(data = data, group = group, id = id), ncol=3)
    df <- df[order(df[, 1]),]
    prank <- .Call(`_pseudorank_psrank`, df[, 1], df[, 2], n)
    sortback <- match(id, df[, 3])
    return(prank[sortback])
  }
}