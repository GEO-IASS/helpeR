#' @name sp_stratified_random_sampling
#' 
#' @title Random sampling of points in each polygon of a polygon shape. 
#' 
#' @description ...
#' 
#' @param srcfile Path of a shapefile.
#' @param n Number of random points to be sampled per feature.
#' @param dstfile Path of the shapefile to be generated with the random points.
#' @param writeOnly Should the SpatialPointsDataFrame be returned?
#' @export
sp_stratified_random_sampling <- function (srcfile, n, dstfile=NULL, writeOnly=FALSE) {
  
  require(sp)
  require(rgdal)
  
  if (length(n) == 1)
    n <- rep(n, length(features))
  features = readOGR(dirname(srcfile), gsub(".shp", "", basename(srcfile)))
  
  for (i in 1:length(features)) {
    feature = features[i, ]
    at <- data.frame(feature@data, stringsAsFactors=F)
    idx <- sapply(at, is.factor)
    at[idx] <- lapply(at[idx], as.character)
    at <- as.data.frame(t(sapply(rep(1, n[i]), function(i) at[i, ])),
                        stringsAsFactor=F)
    randPoints_i = spsample(feature, n[i], "random")
    randPoints_i <- SpatialPointsDataFrame(randPoints_i, data=at)
    if (i == 1) {
      randPoints <- randPoints_i
    } else {
      randPoints <- rbind(randPoints, randPoints_i)
    }
  }
  if (!is.null(dstfile)) {
    writeOGR(randPoints, dsn=dirname(dstfile), layer=gsub(".shp", "", basename(srcfile)))
  }
  if (!writeOnly) {
    return(randPoints)
  } else {
    return(NULL)  
  }
}
