#' A pba model S4 class
#'
#' @slot data A matrix. The original data.
#' @slot sbp A matrix. The SBP matrix.
#' @slot pba A matrix. The balances.
#' @slot totvar A numeric vector. The total variance per balance.
#' @slot subvar A numeric vector. The fractional variance per balance.
#'
#' @export
setClass("pba",
         slots = c(
           data = "matrix",
           sbp = "matrix",
           pba = "matrix",
           totvar = "numeric",
           subvar = "numeric"
         )
)

#' Principal Balance Analysis
#'
#' This function performs a principal balance analysis using the
#'  hierarchical clustering of components method described
#'  by Pawlowsky-Glahn et al. in "Principal balances"
#'  from the CoDaWork 2011 proceedings.
#'
#' This resultant object contains the original data, the serial
#'  binary partition, the principal balances, and the fractional
#'  variances per balance. Use \code{predict} to deploy the
#'  \code{pba} model on new data.
#'
#' @inheritParams vlr
#'
#' @return A \code{pba} object.
#'
#' @export
pba <- function(x, alpha = NA){

  object <- methods::new("pba")
  object@data <- as.matrix(x)
  object@sbp <- sbp.fromPBA(x, alpha)
  object@pba <- balance.fromSBP(x, object@sbp)

  object@totvar <- apply(object@pba, 2, stats::var)
  object@subvar <- object@totvar/sum(object@totvar)

  object
}

#' @describeIn pba Method to deploy \code{pba} model.
#'
#' @param object A \code{pba} object.
#' @param newdata A matrix.
#'
#' @docType methods
#'
#' @export
setMethod("predict", "pba",
          function(object, newdata){

            balance.fromSBP(newdata, object@sbp)
          }
)
