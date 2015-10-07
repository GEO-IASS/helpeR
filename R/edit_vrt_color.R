#' @name edit_vrt_color
#' 
#' @title Add a color table to a vrt file. 
#' 
#' @description It is assumed that the vrt represents a single band raster
#' having dataType="Byte". All values not given via id will be transparent.
#' @param id The IDs to be displayed with the colors given via \code{col}.
#' @param col The colors corresponding to the \code{id}.
#' @param file The vrt file to be edited.
#' @param backup Backup the vrt. Defaults to \code{TRUE}.
#' @seealso \code{\link{divColsAndBreaks}}, \code{\link{rgb24bit}} 
#' @references #' The function has been inspired by 
#' \url{https://gist.github.com/adammwilson/11052135}.
#'@export
edit_vrt_color <- function(id, col, file, backup=TRUE) {
  
  if (backup)
    file.copy(file, gsub("[.]vrt", "_bak.vrt", file))
  
  # from: https://gist.github.com/adammwilson/11052135
  col_nodata <- "#000000"
  colR <- rep(col_nodata, 256)
  idR <- 0:255
  colR[match(id, idR)] <- col
  c4 <-  rep(255, length(colR))
  c4[!0:255 %in% id] <-  0
  
  
  vrt=scan(file,what="char")                                                                                                                                                                               
  hd=c("<ColorInterp>Palette</ColorInterp>","<ColorTable>")                                                                                                                                                      
  ft="</ColorTable>"                                                                                                                                                                                             
  # colR=colorRampPalette(c("#08306b","#0d57a1","#2878b8","#4997c9","#72b2d7","#a2cbe2","#c7dcef","#deebf7","#f7fbff"))                                                                                            
  cols=data.frame(t(col2rgb(colR)))                                                                                                                                                                         
  ct=paste("<Entry c1=\"",cols$red,"\" c2=\"",cols$green,"\" c3=\"",cols$blue,"\" c4=\"", c4, "\"/>")                                                                                                                 
  cti=grep("ColorInterp",vrt)  # get index of current color table                                                                                                                                                
  vrt2=c(vrt[1:(cti-1)],hd,ct,ft,vrt[(cti+1):length(vrt)])                                                                                                                                                       
  ## update missing data flag following http://lists.osgeo.org/pipermail/gdal-dev/2010-February/023541.html                                                                                                      
  csi=grep("<ComplexSource>",vrt2)  # get index of current color table                                                                                                                                           
  vrt2=c(vrt2[1:csi],"<NODATA>255</NODATA>",vrt2[(csi+1):length(vrt2)])                                                                                                                                          
  write.table(vrt2,file=file,col.names=F,row.names=F,quote=F)
}
