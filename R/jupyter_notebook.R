#' Activate specific conda environment and call jupyter notebook.
#'
#' @param env The conda environment to be activated
#' @param bdir The directory where to launch jupyter
#' @param wait Changes default behaviour of the argument when running \code{\link{system}}
#' @param invisible Changes default behaviour of the argument when running \code{\link{system}}
#' @param ... Other parameters that can be passed to to \code{\link{system}}
#'
#' @return NULL
#' @export
#'
#' @examples
jupyter_notebook <- function(env="r-oneClass", bdir=getwd(), 
                             wait=FALSE, invisible=FALSE, ...) {
  call = paste0("cd ", bdir, " & activate ", env, 
                " & jupyter notebook")
  call = paste0("activate ", env, " & jupyter notebook")
  wd_bak <- getwd()
  setwd(bdir)
  cat("CALLING from ", getwd(), "\n", call, "\n")
  system(call, wait=wait, invisible=invisible, ...)
  setwd(wd_bak)
  return(NULL)
}