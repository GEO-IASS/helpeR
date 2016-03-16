#' Title
#'
#' @param x 
#' @param sep 
#' @param collapse 
#' @param collapse_vals
#'
#' @return
#' @export
#'
#' @examples
#' # With a list
#' x <- list(a=c(1:3), b=1)
#' get_nameUval_string(x)
#' get_nameUval_string(x, sep=" = ", collapse=" || ",  collapse_vals=",")
#' 
get_nameUval_string <- function(x, sep=":", collapse="|", 
                                collapse_vals=",") {
  nms <- names(x)
  vls <- sapply(x, function(xi) paste0(xi, collapse=collapse_vals))
  
  paste(nms, vls, sep=sep, collapse=collapse)
}