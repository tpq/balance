#' A pba model S4 class
#'
#' @param object,x A \code{bplot} object.
#' @param i An integer. Used to index the \code{bplot} object.
#'
#' @slot balance.partition A \code{ggplot} object. The "partition" sub-plot.
#' @slot balance.distribution A \code{ggplot} object. The "distribution" sub-plot.
#' @slot balances The results of \code{balance.fromSBP}.
#'
#' @author Thom Quinn
#'
#' @examples
#' library(balance)
#' data(iris)
#' x <- iris[,1:4]
#' sbp <- sbp.fromPBA(x)
#' balance(x, sbp)
#'
#' @export
setClass("bplot",
         slots = c(
           balance.partition = "ANY",
           balance.distribution = "ANY",
           balances = "ANY"
         )
)

#' @describeIn bplot Method to show \code{bplot} object.
#' @export
setMethod("show", "bplot",
          function(object){

            cat("@balance.partition: access with x[[1]]\n")
            cat("@balance.distribution: access with x[[2]]\n")
            cat("@balances: access with x[[3]]\n")
          }
)

#' @describeIn bplot Method to subset \code{bplot} object.
#' @export
setMethod("[[", "bplot",
          function(x, i){

            if(i == 1){
              x@balance.partition
            }else if(i == 2){
              x@balance.distribution
            }else if(i == 3){
              x@balances
            }else{
              stop("subscript out of bounds")
            }
          }
)

#' Calculate and Visualize Balances
#'
#' This function calculates balances based on the compositional data
#'  set and serial binary partition (SBP) matrix provided, then generates a
#'  figure from the results.
#'
#' @param x A matrix with rows as samples (N) and columns as components (D).
#' @param y A serial binary partition matrix with rows as components (D) and
#'  columns as balances (D-1).
#' @param d.group A vector of group labels for components. Optional.
#'  If provided, used to color component points.
#' @param n.group A vector of group labels for samples. Optional.
#'  If provided, used to color sample points.
#' @param boxplot.split A boolean. Toggles whether to split the boxplot
#'  by \code{n.group}. TRUE better resembles balance dendrogram style.
#' @param weigh.var A boolean. Toggles whether to weigh line width
#'  by the proportion of explained variance. Only do this if balances
#'  come from an SBP that decomposes variance.
#' @param size.text An integer. Sets legend text size.
#' @param size.pt An integer. Sets point size.
#'
#' @return A list of the "partition" \code{ggplot} object, the "distribution"
#'  \code{ggplot} object, and the per-sample balances.
#'
#' @author Thom Quinn
#'
#' @examples
#' library(balance)
#' data(iris)
#' x <- iris[,1:4]
#' sbp <- sbp.fromPBA(x)
#' balance(x, sbp)
#'
#' @export
balance.plot <- function(x, y,
                         d.group,
                         n.group,
                         boxplot.split = TRUE,
                         weigh.var = FALSE,
                         size.text = 20,
                         size.pt = 4){

  cols <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3",
            "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3")

  if(ncol(x) != nrow(y)){
    stop("Please check that ncol(x) = nrow(y) = D.")
  }

  if(any(x == 0)){
    stop("Please remove zeros before analysis.")
  }

  # Force column names
  if(is.null(colnames(x))){
    colnames(x) <- paste0("Component", 1:ncol(x))
  }
  if(is.null(colnames(y))){
    colnames(y) <- paste0("Balance", 1:ncol(y))
  }
  if(is.null(rownames(x))){
    rownames(x) <- as.character(1:nrow(x))
  }

  # Anticipate irregular data
  x <- as.data.frame(x)
  y <- as.data.frame(y)
  rownames(y) <- colnames(x)

  # Order Serial Binary Partition [order balance (D-1)]
  b.weight <- apply(y, 2, function(i) sum(abs(i)))
  b.order <- order(b.weight, decreasing = TRUE)

  # Order Serial Binary Partition [order components (D)]
  d.weight <- apply(y[,b.order], 1, function(i) sum(i * 1/1:length(i)))
  d.order <- order(d.weight, decreasing = TRUE)

  # Prepare balance.partition plot
  pt <- y
  pt <- wide2long(pt)
  pt <- pt[pt$value != 0,]
  colnames(pt) <- c("Part", "BalanceID", "Component")
  pt$Component <- factor(pt$Component, colnames(x)[d.order])
  pt$BalanceID <- factor(pt$BalanceID, rev(colnames(y)[b.order]))
  pt$Part <- factor(pt$Part, levels = c(1, -1))
  pt$Group <- paste0(pt$BalanceID, pt$Part) # for geom_line()

  # Prepare balance.distribution plot
  B <- balance.fromSBP(x, y)
  colnames(B) <- colnames(y)
  rownames(B) <- rownames(x)
  dt <- wide2long(B)
  colnames(dt) <- c("SampleValue", "BalanceID", "Index")
  dt$BalanceID <- factor(dt$BalanceID, rev(colnames(y)[b.order]))

  # Calculate line width from var
  if(weigh.var){
    vars <- apply(B, 2, stats::var)
    vars <- vars/sum(vars)
    linewidth <- data.frame("BalanceID" = colnames(B), "LineWidth" = vars)
    dt <- merge(dt, linewidth)
  }

  if(!all(pt$BalanceID %in% dt$BalanceID)) stop("Unexpected Error: try renaming balances.")
  if(!all(dt$BalanceID %in% pt$BalanceID)) stop("Unexpected Error: try renaming balances.")

  if(!missing(d.group)){
    if(length(unique(d.group)) > 8) stop("Only 8 groups for 'd.group' supported.")
    d.group <- as.character(d.group)
    names(d.group) <- colnames(x)
    pt$d.group <- d.group[as.character(pt$Component)]
    d.cols <- cols[1:length(unique(pt$d.group))]
  }else{
    pt$d.group <- "1"
    d.cols <- "black"
  }

  if(!missing(n.group)){
    if(length(unique(n.group)) > 8) stop("Only 8 groups for 'n.group' supported.")
    n.group <- as.character(n.group)
    names(n.group) <- rownames(x)
    dt$n.group <- n.group[as.character(dt$Index)]
    n.cols <- cols[1:length(unique(dt$n.group))]
  }else{
    dt$n.group <- "1"
    n.cols <- "black"
  }

  if(boxplot.split){
    dt$Group <- paste(dt$BalanceID, dt$n.group)
  }else{
    dt$Group <- dt$BalanceID
  }

  balance.partition <-
    ggplot2::ggplot(pt, ggplot2::aes_string(x = "BalanceID", y = "Component", shape = "Part", group = "Group")) +
    ggplot2::geom_line() + ggplot2::geom_point(ggplot2::aes_string(col = "d.group"), size = size.pt) + # if missing, set to "1"
    ggplot2::scale_colour_manual(values = d.cols) + # if missing, set to "black"
    ggplot2::xlab("Balance ID") + ggplot2::ylab("Component ID") +
    ggplot2::labs(col = "Component Group", shape = "Partition") +
    ggplot2::coord_flip() + ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = .5)) +
    ggplot2::theme(text = ggplot2::element_text(size = size.text)) +
    ggplot2::theme(legend.position = "top")

  balance.distribution <-
    ggplot2::ggplot(dt, ggplot2::aes_string(x = "BalanceID", y = "SampleValue", group = "BalanceID"), col = "black") +
    ggplot2::scale_colour_manual(values = n.cols) + # if missing, set to "black"
    ggplot2::xlab("") + ggplot2::ylab("Sample-wise Distribution of Balance") +
    ggplot2::ylim(-1.1 * max(abs(dt$SampleValue)), 1.1 * max(abs(dt$SampleValue))) +
    ggplot2::labs(col = "Sample Group") +
    ggplot2::coord_flip() + ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = .5)) +
    ggplot2::theme(text = ggplot2::element_text(size = size.text)) +
    ggplot2::theme(legend.position = "top")

  if(weigh.var){
    balance.distribution <- balance.distribution +
      ggplot2::geom_line(ggplot2::aes_string(size = "LineWidth"), alpha = .5) + # keep unchanged to show range...
      ggplot2::guides(size = FALSE)
  }else{
    balance.distribution <- balance.distribution +
      ggplot2::geom_line(alpha = .5) +
      ggplot2::guides(size = FALSE)
  }

  if(boxplot.split){
    balance.distribution <- balance.distribution +
      ggplot2::geom_boxplot(ggplot2::aes_string(group = "Group", col = "n.group"),
                            outlier.size = size.pt,
                            position = ggplot2::position_dodge(0.8),
                            alpha = .5) # if missing, n.group set to "1"
  }else{
    balance.distribution <- balance.distribution +
      ggplot2::geom_point(ggplot2::aes_string(col = "n.group"),
                          size = size.pt,
                          position = ggplot2::position_dodge(0.8),
                          alpha = .5) # if missing, n.group set to "1"
  }

  grid::grid.newpage()
  grob <- cbind(ggplot2::ggplotGrob(balance.partition),
                ggplot2::ggplotGrob(balance.distribution),
                size = "first")
  grid::grid.draw(grob)

  # Return results as bplot object
  res <- methods::new("bplot")
  res@balance.partition <- balance.partition
  res@balance.distribution <- balance.distribution
  res@balances <- B

  return(res)
}

#' Calculate and Visualize Balances
#'
#' This function wraps \code{\link{balance.plot}}.
#'
#' @param ... Arguments to \code{\link{balance.plot}}.
#'
#' @export
balance <- function(...){

  balance.plot(...)
}

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
#' @examples
#' library(balance)
#' data(iris)
#' x <- iris[,1:4]
#' sbp <- sbp.fromPBA(x)
#' res <- balance(x, sbp)
#' custom1 <- res[[1]] + ggplot2::theme_dark()
#' custom2 <- res[[2]] + ggplot2::theme_dark()
#' balance.combine(custom1, custom2)
#'
#' @export
balance.combine <- function(balance.partition, balance.distribution, size = "first"){

  grid::grid.newpage()
  grob <- cbind(ggplot2::ggplotGrob(balance.partition),
                ggplot2::ggplotGrob(balance.distribution),
                size = size)
  grid::grid.draw(grob)
}
