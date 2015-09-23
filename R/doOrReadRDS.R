#' @title doOrReadRDS
#' @description Evaluate an expression and write the result 
#' to disc or load it if the file exists.
#' if it already exists.  
#' @param expr An expression.
#' @param file The file where the result of the expression 
#' should be stored.
#' @param overwrite Should the expression be evaluated again 
#' and re-stored even if the file already exists?
#' @param makedirs Should the directory \code{dirname(file)}
#' be created (recursively) if it does not exist?
#' @examples 
#' \dontrun{
#' x1 <- doOrReadRDS({a <- 1
#'                    b <- 1
#'                    a*b
#'                    }, file="doOrReadRDS_cachefile.rds")
#' x1  # The result is 1.
#' x1 <- doOrReadRDS({a <- 1
#'                    b <- 2   # !!!
#'                    a*b
#'                    }, file="doOrReadRDS_cachefile.rds")
#' x1  # The result is still because the result is loaded.  
#' x1 <- doOrReadRDS({a <- 1
#'                    b <- 2   # !!!
#'                    a*b
#'                    }, file="doOrReadRDS_cachefile.rds", 
#'                    overwrite=TRUE)
#' x1  # The result is now 2 and also stored in the file.  
#' }
#' @return The result of the expression (computed or loaded).
#' @export 
doOrReadRDS <- function(expr, file, overwrite=FALSE, makedirs=TRUE) {
  # Cache results...
  # Evaluate an expression and store the result to "file". If overwrite is not 
  # TRUE and if "file" exists the expression is not evaluated but the result is 
  # loaded from the file.
  if (!file.exists(file) | overwrite) {
    systime <- system.time({
      obj <- expr
    })
    attr(obj, "doOrReadRDS_doTime") <- systime
    if (!file.exists(dirname(file)) & makedirs)
      dir.create(dirname(file), recursive=TRUE)
    
    saveRDS(obj, file)
  } else {
    systime <- system.time({
      obj <- try(readRDS(file))
    })
    if (class(obj)=="try-error") {
      # try again... maybe file is corrupt
      systime <- system.time({
        obj <- expr
      })
      attr(obj, "doOrReadRDS_readTime") <- systime
      if (!file.exists(dirname(file)) & makedirs)
        dir.create(dirname(file), recursive=TRUE)
      
      saveRDS(obj, file)
    }
  }
  return(obj)
}
