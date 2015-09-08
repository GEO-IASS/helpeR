#' @title emptyRaster
#' @description Create an empty raster layer given a Raster* object template. 
#' @return A RasterLayer with the same number of rows/columns, crs and extent 
#' as the input Raster* object.  
#' @export 
emptyRaster <- function(x, ...) {
  
  emptyRaster <- raster(nrows=nrow(x), ncols=ncol(x),
                        crs=x@crs, 
                        ext=extent(x), ...)
  
  return(emptyRaster)
}
