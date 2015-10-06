#' @title write_clr
#' @description Create a colormap for ArcMAP. 
#' @param ids The class ids
#' @param labels The class labels
#' @param color The colors
#' @param ... Not used.
#' @return A color map
#' @export
write_clr <- function(ids, colors, file, ...) {
  x = t(col2rgb(colors))
  clr <- cbind(ids, x)
  write.table(clr, file=file, quote=FALSE, row.names=F, col.names=F)
  invisible(clr)
}
