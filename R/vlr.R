#' Calculate Log-ratio Variance
#'
#' This function calculates the log-ratio variance
#'  for all components in a matrix.
#'
#' @param x A matrix with rows as samples (N) and columns as components (D).
#' @param alpha A double. Defines a hyper-parameter used
#'  by the Box-Cox transformation to approximate log-ratio
#'  variance in the presence of zeros. Skip with NA.
#'
#' @return A VLR matrix.
#'
#' @author Thom Quinn
#'
#' @examples
#' library(balance)
#' data(iris)
#' x <- iris[,1:4]
#' vlr(x)
#'
#' @export
vlr <- function(x, alpha = NA){

  x <- as.matrix(x)

  if(is.na(alpha)){

    if(any(x == 0)){

      message("Alert: Replacing 0s with next smallest value to calculate log-ratio variances.")
      zeros <- x == 0
      x[zeros] <- min(x[!zeros])
    }

    logX <- log(x)

  }else{

    logX <- (x^alpha - 1)/alpha
  }

  # Calculate D and N from logX
  id <- rep(1, ncol(logX))
  N <- nrow(logX)

  # Calculate A %*% B
  ab <- t(logX) %*% logX
  dots <- diag(ab)
  dot.join <- dots %*% t(id) + id %*% t(dots)

  # Calculate sums
  sums <- colSums(logX)
  sum.diff <- sums %*% t(id) - id %*% t(sums)

  # Calculate VLR
  out <- ((dot.join - 2 * ab) -
            (sum.diff)^2 / (N)) / (N - 1)

  rownames(out) <- colnames(out)
  out
}
