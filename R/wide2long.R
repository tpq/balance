#' Make Long Data from Wide Data
#'
#' @param wide A data set in wide format.
#'
#' @return A data set in long format.
#'
#' @export
wide2long <- function(wide){

  # Force column names
  if(is.null(colnames(wide))){
    colnames(wide) <- paste0("Col", 1:ncol(wide))
  }

  # Force row names
  if(is.null(rownames(wide))){
    rownames(wide) <- as.character(rownames(wide))
  }

  df <- data.frame("value" = as.vector(as.matrix(wide)))
  df$variable <- unlist(lapply(colnames(wide), function(x) rep(x, nrow(wide))))
  df$id <- rownames(wide)
  return(df)
}
