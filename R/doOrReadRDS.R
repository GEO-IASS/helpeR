#' @title doOrReadRDS
#' @description Evaluate an expression and write the result to disc or load it 
#' if it already exists.  
#' @return The result of the expression (computed or loaded).  
#' @export 
doOrReadRDS <- function(expr, file, overwrite=FALSE, mkdir=TRUE) {
  # Cache results...
  # Evaluate an expression and store the result to "file". If overwrite is not 
  # TRUE and if "file" exists the expression is not evaluated but the result is 
  # loaded from the file.
  if (!file.exists(file) | overwrite) {
    systime <- system.time({
      obj <- expr
    })
    attr(obj, "doOrReadRDS_doTime") <- systime
    
    dname <- dirname(file)
    if (mkdir & !file.exists(dname))
      dir.create(dname)
    saveRDS(obj, file)
  } else {
    systime <- system.time({
      obj <- readRDS(file)
    })
    attr(obj, "doOrReadRDS_readTime") <- systime
  }
  return(obj)
}
