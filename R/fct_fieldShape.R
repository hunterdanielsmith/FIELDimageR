#' fieldShape 
#' 
#' @title Building the plot \code{fieldshape} file
#' 
#' @description The user should select the four experimental field corners and the shape file with plots will be automatcly 
#' built using a grid with the number of ranges and rows. Attention: The base of image should be parallel to axis X, 
#' please use first the function \code{\link{fieldRotate}}.
#' 
#' @param mosaic object of class stack obtained from function \code{\link{fieldRotate}} with the base of image parallel to axis X.
#' @param ncols number of ranges.
#' @param nrows number of rows.
#' @param nPoint number of points necessary to select field boundaries or area to remove (4 >= nPoint <= 50).
#' @param fieldMap matrix with plots ID identified by rows and ranges, please use first the funsction \code{\link{fieldMap}}.
#' @param fieldData data frame with plot ID and all attributes of each plot (Traits as columns and genotypes as rows).
#' @param ID name of plot ID in the fieldData file to combine with fieldShape.
#' @param theta angle of rotation used on function \code{\link{fieldRotate}}. It is an important step to fit the fieldShape on the original image.
#' @param plot if is TRUE the crop image and fieldShape will be plotted.
#' @param fast.plot if TRUE only the grey scale image will be plotted as reference (faster approach).
#' @param extent if is TRUE the entire image area will be the fieldShape (one unique plot).
#' 
#' @importFrom terra rast plotRGB res project crs ext atan2 crop as.polygons mask extract vect
#' @importFrom graphics abline axis lines par plot points locator legend nlyr
#' @importFrom grDevices grey rgb col2rgb
#' @importFrom sp bbox Polygons Polygon SpatialPolygonsDataFrame SpatialPolygons spsample SpatialPointsDataFrame over proj4string elide
#' 
#'
#' @return A list with two element
#' \itemize{
#'   \item \code{mosaic} new mosaic cropped with the drew \code{fieldshape}.
#'   \item \code{fieldShape} The function returns the \code{fieldShape} format 
#'   \code{SpatialPolygonsDataFrame} with plots numbered from left to right and top to bottom, 
#'   and a new reduced image with format stack. The \code{fieldMap} can be used to identify the plot ID.
#' }
#' 
#'
#' @export
fieldShape <- function(mosaic, ncols = 10, nrows = 10, nPoint = 4, fieldMap = NULL, fieldData = NULL,
                       ID = NULL, theta = NULL, plot = TRUE, fast.plot = FALSE, extent = FALSE) {
  num.band<-nlyr(mosaic)
  print(paste(num.band," layers available", sep = ""))
  if(!extent){
  if(nPoint<4|nPoint>50){stop("nPoint must be >= 4 and <= 50")}
    }
  par(mfrow=c(1,2))
  if(fast.plot){
    terra::plot(mosaic[[1]], col=grey(1:100/100), axes=FALSE, box=FALSE, legend=FALSE)}
  if(!fast.plot){
    if(num.band>2){plotRGB(mosaic, r = 1, g = 2, b = 3)}
    if(num.band<3){terra::plot(mosaic, axes=FALSE, box=FALSE)}}
  if(!extent){
  print(paste("Select ",nPoint," points at the corners of field of interest in the plots space.",sep = ""))
  c1<-NULL
  for(i in 1:nPoint){
    c1.1<-locator(type="p",n = 1, col="red",pch=19)
    c1<-rbind(c1,c(c1.1$x,c1.1$y))
  }
  c1<-rbind(c1,c1[1,])
  colnames(c1)<-c("x","y")
  lines(c1, col= "red", type="l", lty=2, lwd=3)}
  if(extent){
    c1<-ext(mosaic)
  }
  p1 <- Polygons(list(Polygon(c1)), "x")
  f1 <- SpatialPolygonsDataFrame( SpatialPolygons(list(p1)), data.frame( z=1, row.names=c("x") ) )
  terra::crs(f1) <- terra::crs(mosaic)
  r <- crop(x = mosaic, y = f1)
  grid <- rast(vect(f1),nrows=nrows, ncols=ncols,crs = crs(r))
  fieldShape <- as.polygons(grid)
  if(plot){
    if(fast.plot){
      terra::plot(r[[1]], col=grey(1:100/100), axes=FALSE, box=FALSE, legend=FALSE)
      sp::plot(fieldShape, add = T)}
    if(!fast.plot){
      if(num.band>2){
        plotRGB(r, r = 1, g = 2, b = 3)
        sp::plot(fieldShape, add = T)
      }
      if(num.band<3){
        terra::plot(r, axes=FALSE, box=FALSE)
        sp::plot(fieldShape, add = T)}}}
  values(fieldShape)<-data.frame(fieldID=as.character(seq(1,nrow(fieldShape))))
  if(!is.null(fieldMap)){values(fieldShape)<-data.frame(PlotName=as.character(c(t(fieldMap))))}
  if(!is.null(fieldData)){
    fieldData<-as.data.frame(fieldData)
    if(is.null(ID)){stop("Choose one ID (column) to combine fieldData with fiedShape")}
    if(length(ID)>1){stop("Choose only one ID")}
    if(is.null(fieldMap)){stop("fieldMap is necessary, please use function fieldMap()")}
    if(!as.character(ID)%in%as.character(colnames(fieldData))){stop(paste("ID: ",ID," is not valid."))}
    fieldData$PlotName<-as.character(fieldData[,colnames(fieldData)==ID])
    values(fieldShape)<-plyr::join(values(fieldShape),fieldData,by="PlotName")
  }
  Out<-list(fieldShape=fieldShape,cropField=r)
  if(!is.null(theta)){
    fieldShape1 <- elide(fieldShape,rotate=theta,center=apply(bbox(fieldShape), 1, mean))
    terra::crs(fieldShape1) <- terra::crs(mosaic)
    Out<-list(fieldShape=fieldShape,fieldShapeGIS=fieldShape1,cropField=r)
  }
  par(mfrow=c(1,1))
  return(Out)
}
