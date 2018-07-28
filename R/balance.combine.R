#' Combine Two Sub-Plots
#'
#' This function combines the "partition" sub-plot with the
#'  "distribution" sub-plot, preserving scale.
#'
#' @param balance.partition A \code{ggplot} object. The "partition" sub-plot.
#' @param balance.distribution A \code{ggplot} object. The "distribution" sub-plot.
#' @param size A string. Toggles whether to size final figure based on
#'  "first" (partition) or "last" (distribution) figure provided.
#'
#' @author Thom Quinn
#'
#' @export
balance.combine <- function(balance.partition, balance.distribution, size = "first"){

  grid::grid.newpage()
  grob <- cbind(ggplot2::ggplotGrob(balance.partition),
                ggplot2::ggplotGrob(balance.distribution),
                size = size)
  grid::grid.draw(grob)
}
