#' Build SBP Matrix from hclust Object
#'
#' This function builds an SBP matrix from an \code{hclust} object
#'  as produced by the \code{hclust} function.
#'
#' @param hclust An \code{hclust} object.
#'
#' @return An SBP matrix.
#'
#' @author Thom Quinn
#'
#' @examples
#' library(balance)
#' data(cars)
#' h <- hclust(dist(cars))
#' sbp.fromHclust(h)
#'
#' @export
sbp.fromHclust <- function(hclust){

  if(class(hclust) != "hclust"){

    stop("This function expects an 'hclust' object.")
  }

  labels <- hclust$labels
  merge <- hclust$merge

  out <- matrix(0, nrow(merge), nrow(merge) + 1)
  if(is.null(labels)){
    colnames(out) <- 1:ncol(out)
  }else{
    colnames(out) <- labels
  }

  branches <- vector("list", nrow(merge) - 1)
  for(i in 1:nrow(merge)){

    # Assign +1 to branch 1
    branch1 <- merge[i,1]
    if(branch1 < 0){
      include1 <- -1 * branch1
    }else{
      include1 <- branches[[branch1]]
    }
    out[i, include1] <- 1

    # Assign -1 to branch 2
    branch2 <- merge[i,2]
    if(branch2 < 0){
      include2 <- -1 * branch2
    }else{
      include2 <- branches[[branch2]]
    }
    out[i, include2] <- -1

    # Track base of branch
    branches[[i]] <- c(include1, include2)
  }

  # Sort balances by tree height
  sbp <- t(out[nrow(out):1,])
  colnames(sbp) <- paste0("z", 1:ncol(sbp))
  sbp
}

#' Build SBP Matrix of Principal Balances
#'
#' This function builds an SBP of principal balances using the
#'  hierarchical clustering of components method described
#'  by Pawlowsky-Glahn et al. in "Principal balances"
#'  from the CoDaWork 2011 proceedings.
#'
#' @inheritParams vlr
#'
#' @return An SBP matrix.
#'
#' @author Thom Quinn
#'
#' @examples
#' library(balance)
#' data(iris)
#' x <- iris[,1:4]
#' sbp.fromPBA(x)
#'
#' @export
sbp.fromPBA <- function(x, alpha = NA){

  vlr <- vlr(x, alpha)
  h <- stats::hclust(stats::as.dist(vlr), method = "ward.D2")
  sbp.fromHclust(h)
}

#' Sort SBP Matrix
#'
#' @param sbp An SBP matrix.
#'
#' @return An SBP matrix.
#'
#' @author Thom Quinn
#'
#' @examples
#' library(balance)
#' data(iris)
#' x <- iris[,1:4]
#' sbp <- sbp.fromPBA(x)
#' sbp.sort(sbp)
#'
#' @export
sbp.sort <- function(sbp){

  # Order Serial Binary Partition [order balance (D-1)]
  b.weight <- apply(sbp, 2, function(i) sum(abs(i)))
  b.order <- order(b.weight, decreasing = TRUE)

  # Order Serial Binary Partition [order components (D)]
  d.weight <- apply(sbp[,b.order], 1, function(i) sum(i * 1/1:length(i)))
  d.order <- order(d.weight, decreasing = TRUE)

  sbp <- sbp[d.order, b.order]

  # Resolve some ties
  b.weight <- apply(sbp, 2, function(i) sum(i * 1/1:length(i)))
  b.order <- order(b.weight, decreasing = TRUE)

  sbp[, b.order]
}
