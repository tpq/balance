#' Transform Samples with the ilr of a Balance
#'
#' @param x A matrix with rows as samples (N) and columns as components (D).
#' @param contrast A vector. One column of a serial binary partition matrix
#'  with values [-1, 0, 1] describing D components.
#'
#' @return A transformation of samples for the balance provided.
#'
#' @author Thom Quinn
#'
#' @examples
#' library(balance)
#' data(iris)
#' x <- iris[,1:4]
#' sbp <- sbp.fromPBA(x)
#' balance.fromContrast(x, sbp[,1])
#'
#' @export
balance.fromContrast <- function(x, contrast){

  if(length(contrast) != ncol(x)) stop("Contrast must have length ncol(x) = D.")
  if(any(!contrast %in% c(-1, 0, 1))) stop("Contrast must contain [-1, 0, 1] only.")

  lpos <- sum(contrast == 1)
  lneg <- sum(contrast == -1)
  const <- sqrt((lpos*lneg)/(lpos+lneg))

  logX <- log(x)
  ipos <- rowMeans(logX[, contrast == 1, drop = FALSE])
  ineg <- rowMeans(logX[, contrast == -1, drop = FALSE])

  const * log(exp(ipos) / exp(ineg))
}

#' Compute Balances from an SBP Matrix
#'
#' @param x A matrix with rows as samples (N) and columns as components (D).
#' @param y A serial binary partition matrix with rows as components (D) and
#'  columns as balances (D-1).
#'
#' @return A transformation of samples for each balance in the SBP matrix.
#'
#' @author Thom Quinn
#'
#' @examples
#' library(balance)
#' data(iris)
#' x <- iris[,1:4]
#' sbp <- sbp.fromPBA(x)
#' balance.fromSBP(x, sbp)
#'
#' @export
balance.fromSBP <- function(x, y){

  x <- as.matrix(x)

  if(any(x == 0)){

    message("Alert: Replacing 0s with next smallest value to calculate balances.")
    zeros <- x == 0
    x[zeros] <- min(x[!zeros])
  }

  res <- apply(y, 2, function(z) balance.fromContrast(x, z))
  rownames(res) <- as.character(1:nrow(res))
  return(res)
}
