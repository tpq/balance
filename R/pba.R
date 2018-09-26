#' A pba model S4 class
#'
#' @param object,x A \code{pba} object.
#' @param y A matrix on which to deploy the \code{pba} model.
#' @param group A character vector. Group labels used to color points.
#' @param pb1,pb2 An integer. Sets principal balances to plot.
#' @param size.text An integer. Sets legend text size.
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

#' @describeIn pba Method to show \code{pba} model.
#' @export
setMethod("show", "pba",
          function(object){

            cat("###PBA object\n")

            cat("@data summary:",
                nrow(object@data), "samples by", ncol(object@data), "components\n")

            cat("@sbp summary:",
                nrow(object@sbp), "components by", ncol(object@sbp), "balances\n")

            cat("@pba summary:",
                nrow(object@pba), "samples by", ncol(object@pba), "balances\n")

            cat("-z1:",
                paste0(round(object@subvar[1]*100, 2), "%"), "variance explained\n")
            if(ncol(object@sbp) > 1){
              cat("-z2:",
                  paste0(round(object@subvar[2]*100, 2), "%"), "variance explained\n")
            }
            if(ncol(object@sbp) > 2){
              cat("-z3:",
                  paste0(round(object@subvar[3]*100, 2), "%"), "variance explained\n")
            }
          }
)

#' @describeIn pba Method to deploy \code{pba} model.
#' @export
setMethod("predict", "pba",
          function(object, y){

            balance.fromSBP(y, object@sbp)
          }
)

#' @describeIn pba Method to plot \code{pba} model.
#' @export
setMethod("plot", signature(x = "pba", y = "missing"),
          function(x, y, group, pb1 = 1, pb2 = 2, size.text = 18){

            if(missing(group)) group <- "Unknown"
            df <- cbind(x@pba[,c(pb1, pb2)], data.frame(group))
            names <- colnames(df)
            colnames(df) <- c("First", "Second", "group")
            ggplot2::ggplot(df, ggplot2::aes_string(x = "First", y = "Second", col = "group")) +
              ggplot2::geom_point() +
              ggplot2::xlab(paste0(names[1], " [x var explained: ",
                                   round(x@subvar[pb1]*100, 2), "%]")) +
              ggplot2::ylab(paste0(names[2], " [x var explained: ",
                                   round(x@subvar[pb2]*100, 2), "%]")) +
              ggplot2::scale_color_brewer(palette = "Set2") +
              ggplot2::labs(col = "Sample Group") +
              ggplot2::theme_bw() +
              ggplot2::theme(text = ggplot2::element_text(size = size.text)) +
              ggplot2::theme(legend.position = "top")
          }
)

#' @describeIn pba Method to plot \code{pba} model.
#' @export
setMethod("plot", signature(x = "pba", y = "matrix"),
          function(x, y, group, pb1 = 1, pb2 = 2, size.text = 18){

            if(missing(group)) group <- "Unknown"
            df <- cbind(predict(x, y)[,c(pb1, pb2)], data.frame(group))
            names <- colnames(df)
            colnames(df) <- c("First", "Second", "group")
            ggplot2::ggplot(df, ggplot2::aes_string(x = "First", y = "Second", col = "group")) +
              ggplot2::geom_point() +
              ggplot2::xlab(paste0(names[1], " [x var explained: ",
                                   round(x@subvar[pb1]*100, 2), "%]")) +
              ggplot2::ylab(paste0(names[2], " [x var explained: ",
                                   round(x@subvar[pb2]*100, 2), "%]")) +
              ggplot2::scale_color_brewer(palette = "Set2") +
              ggplot2::labs(col = "Sample Group") +
              ggplot2::theme_bw() +
              ggplot2::theme(text = ggplot2::element_text(size = size.text)) +
              ggplot2::theme(legend.position = "top")
          }
)

#' @describeIn pba Method to plot \code{pba} model.
#' @export
setMethod("plot", signature(x = "pba", y = "data.frame"),
          function(x, y, group, pb1 = 1, pb2 = 2, size.text = 18){

            plot(x, as.matrix(y), group, pb1, pb2, size.text)
          }
)