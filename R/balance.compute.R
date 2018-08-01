#' Transform Samples with the ilr of a Balance
#'
#' @param x A matrix with rows as samples (N) and columns as components (D).
#' @param contrast A vector. One column of a serial binary partition matrix
#'  with values [-1, 0, 1] describing D components.
#'
#' @return A transformation of samples for the balance provided.
#'
#' @export
balance.compute <- function(x, contrast){

  if(length(contrast) != ncol(x)) stop("Contrast must have length ncol(x) = D.")
  if(any(!contrast %in% c(-1, 0, 1))) stop("Contrast must contain [-1, 0, 1] only.")

  lpos <- sum(contrast == 1)
  lneg <- sum(contrast == -1)
  const <- sqrt((lpos*lneg)/(lpos+lneg))

  apply(x, 1, function(sample){
    ipos <- sample[contrast == 1]
    ineg <- sample[contrast == -1]
    geomean <- function(i) prod(i)^(1/length(i))
    const * log(geomean(ipos) / geomean(ineg))
  })
}

#' Compute Balances from an SBP Matrix
#'
#' @param x A matrix with rows as samples (N) and columns as components (D).
#' @param y A serial binary partition matrix with rows as components (D) and
#'  columns as balances (D-1).
#'
#' @return A transformation of samples for each balance in the SBP matrix.
#'
#' @export
balances <- function(x, y){

  res <- apply(y, 2, function(z) balance.compute(x, z))
  rownames(res) <- as.character(1:nrow(res))
  return(res)
}
