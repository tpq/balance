#' Build SBP Matrix from Random Tree
#'
#' This function builds an SBP from a random tree.
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
#' sbp.fromRandom(x)
#'
#' @export
sbp.fromRandom <- function(x){

  # Initialize SBP called Z
  D <- ncol(x)
  firstSeed <- sample(1:D, 2)
  Z <- matrix(0, D, D-1)
  Z[firstSeed,1] <- c(-1, 1)

  # Initialize Node Family index
  FAMILY <- matrix(FALSE, D, D)
  diag(FAMILY) <- TRUE
  FAMILY[firstSeed,firstSeed] <- TRUE

  for(z in 2:ncol(Z)){

    # Sample a Node (or Node Family)
    nodeCurrent <- sample(1:D, 1, prob = 1/colSums(FAMILY))
    nodeFamily <- (1:D)[FAMILY[,nodeCurrent]]

    # Join with another Node (or Node Family)
    outsiders <- setdiff(1:D, nodeFamily)
    if(length(outsiders) == 1){
      joinCurrent <- outsiders
      joinFamily <- (1:D)[FAMILY[,joinCurrent]]
    }else{
      joinCurrent <- sample(outsiders, 1, prob = 1/colSums(FAMILY[,outsiders]))
      joinFamily <- (1:D)[FAMILY[,joinCurrent]]
    }

    # Update Z
    Z[nodeFamily,z] <- -1
    Z[joinFamily,z] <- 1

    # Update Node Family index
    FAMILY[c(nodeFamily, joinFamily),c(nodeFamily, joinFamily)] <- TRUE
  }

  Z <- Z[,ncol(Z):1]
  rownames(Z) <- colnames(x)
  colnames(Z) <- paste0("z", 1:ncol(Z))
  Z
}

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

#' Build SBP Matrix of "Anti-Principal Balances"
#'
#' This function builds an SBP of "anti-principal balances"
#'  by clustering the difference of the log-ratio variance from
#'  the maximum log-ratio variance. Unlike principal balances,
#'  where the first balances explain the most variance,
#'  this function selects "anti-principal balances" so that
#'  the last balances explain relatively more variance.
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
#' sbp.fromABA(x)
#'
#' @export
sbp.fromABA <- function(x, alpha = NA){

  vlr <- vlr(x, alpha)
  h <- stats::hclust(stats::as.dist(max(vlr) - vlr), method = "ward.D2")
  sbp.fromHclust(h)
}

#' Build SBP Matrix of "Principal Discriminant Balances"
#'
#' This function builds an SBP of "discriminant balances"
#'  by clustering a matrix of the pair-wise total within-group
#'  variance, adjusted by the pair-wise total variance
#'  (inverted by subtracting this value from 1).
#'  The method is intended to make the largest balances
#'  most discriminative.
#'
#' @inheritParams vlr
#' @param group A character vector. Group or sub-group membership.
#'  Argument passed to \code{propr::propd}.
#' @param ... Other arguments passed to \code{propr::propd}.
#'
#' @return An SBP matrix.
#'
#' @author Thom Quinn
#'
#' @examples
#' \dontrun{
#' library(balance)
#' data(iris)
#' x <- iris[1:100,1:4]
#' y <- iris[1:100,5]
#' sbp.fromPDBA(x, y)
#' }
#'
#' @export
sbp.fromPDBA <- function(x, group, ...){

  packageCheck("propr")

  pd <- suppressMessages(propr::propd(x, group, ...))
  theta <- propr::getMatrix(pd)
  h <- stats::hclust(stats::as.dist(1 - theta))
  sbp.fromHclust(h)
}

#' Build SBP Matrix of "Anti-Principal Discriminant Balances"
#'
#' This function builds an SBP of "discriminant balances"
#'  by clustering a matrix of the pair-wise total within-group
#'  variance, adjusted by the pair-wise total variance.
#'  The method is intended to make the smallest balances
#'  most discriminative.
#'
#' @inheritParams sbp.fromPDBA
#'
#' @return An SBP matrix.
#'
#' @author Thom Quinn
#'
#' @examples
#' \dontrun{
#' library(balance)
#' data(iris)
#' x <- iris[1:100,1:4]
#' y <- iris[1:100,5]
#' sbp.fromADBA(x, y)
#' }
#'
#' @export
sbp.fromADBA <- function(x, group, ...){

  packageCheck("propr")

  pd <- suppressMessages(propr::propd(x, group, ...))
  theta <- propr::getMatrix(pd)
  h <- stats::hclust(stats::as.dist(theta))
  sbp.fromHclust(h)
}

#' Build SBP Matrix of "Anti-Principal Discriminant Balances"
#'
#' A wrapper for \code{\link{sbp.fromADBA}}.
#'  See also \code{\link{sbp.fromPDBA}}.
#'
#' @inheritParams sbp.fromPDBA
#'
#' @return An SBP matrix.
#'
#' @author Thom Quinn
#'
#' @examples
#' \dontrun{
#' library(balance)
#' data(iris)
#' x <- iris[1:100,1:4]
#' y <- iris[1:100,5]
#' sbp.fromPropd(x, y)
#' }
#'
#' @export
sbp.fromPropd <- function(x, group, ...){

  sbp.fromADBA(x, group, ...)
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

#' Subset SBP Matrix
#'
#' @param sbp An SBP matrix.
#' @param ternary A boolean. Toggles whether to return
#'  balances representing three components.
#' @param ratios A boolean. Toggles whether to return
#'  balances representing two components.
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
#' sbp.subset(sbp)
#'
#' @export
sbp.subset <- function(sbp, ternary = TRUE, ratios = TRUE){

  if(!ternary & !ratios){

    message("Alert: Skipping balance subset.")
    return(sbp)
  }

  if(ternary & !ratios){

    message("Alert: Using 'ternary' enables 'ratios' too.")
    ratios <- TRUE
  }

  b.size <- apply(sbp, 2, function(x) sum(abs(x)))
  keep <- rep(FALSE, ncol(sbp))
  if(ternary) keep <- keep | (b.size == 3)
  if(ratios) keep <- keep | (b.size == 2)
  sbp <- sbp[, keep, drop = FALSE]

  # Name balances from involved features
  joinNames <- function(x) make.names(paste0(sort(x), collapse = "_and_"))
  colnames(sbp) <- apply(sbp, 2, function(x) joinNames(rownames(sbp)[x != 0]))
  sbp
}
