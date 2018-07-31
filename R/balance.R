#' Calculate and Visualize Balances
#'
#' This function uses \code{robCompositions::balances} to calculate the
#'  balances based on the compositional data set and a serial binary partition
#'  matrix, then generates a figure based on the results. For more details,
#'  see \code{?robCompositions::balances}.
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
#' @param size.text An integer. Sets legend text size.
#' @param size.pt An integer. Sets point size.
#'
#' @return A list of the "partition" \code{ggplot} object, the "distribution"
#'  \code{ggplot} object, and the per-sample balances.
#'
#' @author Thom Quinn
#'
#' @export
balance <- function(x, y,
                    d.group,
                    n.group,
                    boxplot.split = FALSE,
                    size.text = 20,
                    size.pt = 4){

  cols <- c("#66C2A5", "#FC8D62", "#8DA0CB", "#E78AC3",
            "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3")

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
  pt$Component <- colnames(x)
  pt <- suppressMessages(reshape2::melt(pt))
  pt <- pt[pt$value != 0,]
  colnames(pt) <- c("Component", "BalanceID", "Part")
  pt$Component <- factor(pt$Component, colnames(x)[d.order])
  pt$BalanceID <- factor(pt$BalanceID, rev(colnames(y)[b.order]))
  pt$Part <- factor(pt$Part, levels = c(1, -1))
  pt$Group <- paste0(pt$BalanceID, pt$Part) # for geom_line()

  # Prepare balance.distribution plot
  B <- robCompositions::balances(x, y)[[1]]
  colnames(B) <- colnames(y)
  dt <- suppressMessages(reshape2::melt(B))
  colnames(dt) <- c("Index", "BalanceID", "SampleValue")
  dt$BalanceID <- factor(dt$BalanceID, rev(colnames(y)[b.order]))

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
    ggplot2::geom_line(ggplot2::aes_string(linetype = "Part")) +
    ggplot2::geom_point(ggplot2::aes_string(col = "d.group"), size = size.pt) + # if missing, set to "1"
    ggplot2::scale_colour_manual(values = d.cols) + # if missing, set to "black"
    ggplot2::xlab("Balance ID") + ggplot2::ylab("Component ID") +
    ggplot2::labs(col = "Component Group", shape = "Partition", linetype = "Partition") +
    ggplot2::coord_flip() + ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = .5)) +
    ggplot2::theme(text = ggplot2::element_text(size = size.text)) +
    ggplot2::theme(legend.position = "top")

  balance.distribution <-
    ggplot2::ggplot(dt, ggplot2::aes_string(x = "BalanceID", y = "SampleValue", group = "Group"), col = "black") +
    ggplot2::geom_boxplot(ggplot2::aes_string(col = "n.group")) + # if missing, set to "1"
    ggplot2::geom_line() + # keep unchanged to show range...
    ggplot2::geom_jitter(ggplot2::aes_string(col = "n.group"), size = size.pt) + # if missing, set to "1"
    ggplot2::scale_colour_manual(values = n.cols) + # if missing, set to "black"
    ggplot2::xlab("") + ggplot2::ylab("Sample-wise Distribution of Balance") +
    ggplot2::ylim(-1.1 * max(abs(dt$SampleValue)), 1.1 * max(abs(dt$SampleValue))) +
    ggplot2::labs(col = "Sample Group") +
    ggplot2::coord_flip() + ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = .5)) +
    ggplot2::theme(text = ggplot2::element_text(size = size.text)) +
    ggplot2::theme(legend.position = "top")

  grid::grid.newpage()
  grob <- cbind(ggplot2::ggplotGrob(balance.partition),
                ggplot2::ggplotGrob(balance.distribution),
                size = "first")
  grid::grid.draw(grob)

  return(
    list(
      balance.partition,
      balance.distribution,
      B
    )
  )
}
