install.packages("devtools") 
devtools::install_github("OpenDroneMap/FIELDimageR") 

setwd("~/FIELDimageR-master.zip") # ~ is the path from where you saved the file.zip 
unzip("FIELDimageR-master.zip")  
file.rename("FIELDimageR-master", "FIELDimageR")  
shell("R CMD build FIELDimageR") # or system("R CMD build FIELDimageR") 
install.packages("FIELDimageR_0.3.2.tar.gz", repos = NULL, type="source") # Make sure to use the right version (e.g. 0.3.2) 

install.packages("BiocManager") 

BiocManager::install("EBImage") 

install.packages ("doParallel") 

setwd("~/FIELDimageR-master.zip") # ~ is the path from where you saved the file.zip 
unzip("FIELDimageR-master.zip")  
file.rename("FIELDimageR-master", "FIELDimageR")  
system("R CMD build FIELDimageR") #only system works on linux 
install.packages("FIELDimageR_0.3.2.tar.gz", repos = NULL, type="source") # Make sure to use the right version (e.g. 0.3.2) 
 

library(FIELDimageR) 
library(raster) 

EX1 <- stack("EX1_RGB.tif") 
plotRGB(EX1, r = 1, g = 2, b = 3) 
 

EX1.Crop <- fieldCrop(mosaic = EX1) # For heavy images (large, high resolution, etc.) please use: fast.plot=T 
 

# Codeline when you don't know the rotation angle "Theta": 
EX1.Rotated <- fieldRotate(mosaic = EX1.Crop, clockwise = F, h=F) # h=horizontal 
 
# Codeline when you know the rotation angle "Theta" (theta = 2.3): 
EX1.Rotated <- fieldRotate(mosaic = EX1.Crop, theta = 2.3) 
 
# Codeline with "extentGIS=TRUE" to fit back the shapefile to the original image GIS. More information at section "5. Building the plot shape file": 
EX1.Rotated<-fieldRotate(mosaic = EX1.Crop, theta = 2.3, extentGIS=TRUE) 
 

EX1.RemSoil <- fieldMask(mosaic = EX1.Rotated, Red = 1, Green = 2, Blue = 3, index = "HUE") 
 

EX1.Shape<-fieldShape(mosaic = EX1.RemSoil,ncols = 14, nrows = 9) 
 

### Field map identification (name for each Plot). 'fieldPlot' argument can be a number or name. 
 
DataTable<-read.csv("DataTable.csv",header = T)   
fieldMap<-fieldMap(fieldPlot=DataTable$Plot, fieldColumn=DataTable$Row, fieldRow=DataTable$Range, decreasing=T) 
fieldMap 
 
# The new column PlotName is identifying the plots: 
EX1.Shape<-fieldShape(mosaic = EX1.RemSoil, ncols = 14, nrows = 9, fieldMap = fieldMap) 

### Joing all information in one "fieldShape" file: 
 
EX1.Shape<-fieldShape(mosaic = EX1.RemSoil, ncols = 14, nrows = 9, fieldMap = fieldMap,  
                      fieldData = DataTable, ID = "Plot") 
                       
# The new column PlotName is identifying the plots:                       
EX1.Shape$fieldShape@data                       

### Different plot dimensions using "fieldShape": 
 
# ncols = 14 and nrows = 9 
EX1.Shape.1Line<-fieldShape(mosaic = EX1.RemSoil, ncols = 14, nrows = 9) 
 
# ncols = 7 and nrows = 9 
EX1.Shape.2lines<-fieldShape(mosaic = EX1.RemSoil, ncols = 7, nrows = 9) 
 
# ncols = 7 and nrows = 3 
EX1.Shape.6lines<-fieldShape(mosaic = EX1.RemSoil, ncols = 7, nrows = 3)                      

### Rotation angle "theta=2.3" from fieldRotate(): 
 
EX1.Shape<-fieldShape(mosaic = EX1.RemSoil, ncols = 14, nrows = 9, fieldMap = fieldMap,  
                      fieldData = DataTable, ID = "Plot", theta = 2.3) 
                       
plotRGB(EX1.RemSoil$newMosaic) 
plot(EX1.Shape$fieldShape,add=T) 
 
plotRGB(EX1) 
plot(EX1.Shape$fieldShapeGIS,add=T)  
 

# Calculating myIndex = "(Red-Blue)/Green" (not avaliable at 'FIELDimageR') 
 
EX1.Indices<- fieldIndex(mosaic = EX1.RemSoil$newMosaic, Red = 1, Green = 2, Blue = 3,  
                          index = c("NGRDI","BGI"),  
                          myIndex = c("(Red-Blue)/Green")) 
                           
# More than one myIndex code: myIndex = c("myIndex1","myIndex2","myIndex3")     
 
EX1.Indices.myIndex<- fieldIndex(mosaic = EX1.RemSoil$newMosaic, Red = 1, Green = 2, Blue = 3,  
                          index = c("NGRDI","BGI"),  
                          myIndex = c("(Red-Blue)/Green","Red/Green","Blue/Green")) 
                           

EX1.Indices.BGI<- fieldIndex(mosaic = EX1.Rotated, index = c("BGI")) 
 
dev.off() 
hist(EX1.Indices.BGI$BGI) # Image segmentation start from 0.7 (soil and plants) 
 
EX1.BGI<- fieldMask(mosaic = EX1.Rotated, Red = 1, Green = 2, Blue = 3,  
                   index = "BGI", cropValue = 0.7, cropAbove = T)  
 
#Check if: cropValue=0.8 or cropValue=0.6 works better. 
                                             

EX1.SC<-fieldCount(mosaic = EX1.RemSoil$mask, fieldShape = EX1.Shape$fieldShape, cex=0.4, col="blue") 
EX1.SC$fieldCount 
 
### Parallel (n.core = 3) 
EX1.SC<-fieldCount(mosaic = EX1.RemSoil$mask, fieldShape = EX1.Shape$fieldShape, n.core = 3, cex=0.4, col="blue") 
EX1.SC$fieldCount 

# Uploading file 
EX.SC<-stack("EX_StandCount.tif") 
plotRGB(EX.SC, r = 1, g = 2, b = 3) 
 
# Removing the soil 
EX.SC.RemSoil<- fieldMask(mosaic = EX.SC, Red = 1, Green = 2, Blue = 3, index = "HUE") 
 
# Building the plot shapefile (ncols = 1 and nrows = 7) 
EX.SC.Shape<-fieldShape(mosaic = EX.SC.RemSoil,ncols = 1, nrows = 7) 

### When all shapes are counted: minSize = 0.00 
 
EX1.SC<-fieldCount(mosaic = EX.SC.RemSoil$mask,  
                   fieldShape = EX.SC.Shape$fieldShape, 
                   minSize = 0.00) 
                    
EX1.SC$objectSel[[4]] # Identifies 14 points, but point 6 and 9 are small artifacts 
EX1.SC$objectReject[[4]] # No shape rejected because minSize = 0.00 

### When all shapes with size greater than 0.04% of plot area are counted: minSize = 0.04 
 
EX1.SC<-fieldCount(mosaic = EX.SC.RemSoil$mask,  
                   fieldShape = EX.SC.Shape$fieldShape, 
                   minSize = 0.04) 
 
EX1.SC$objectSel[[4]] # Identifies 12 points 
EX1.SC$objectReject[[4]] # Shows 2 artifacts that were rejected (6 and 9 from previous example) 

# Uploading image 
EX.P<-stack("EX_Pollen.jpeg") 
 
# Reducing image resolution (fast analysis) 
EX.P<-aggregate(EX.P,fact=4)  
plotRGB(EX.P, r = 1, g = 2, b = 3) 
 
# Shapefile using the entire image (extent = T) 
EX.P.shapeFile<-fieldPolygon(EX.P,extent = T) 
 
# Using index "BIM" to remove background (above 19) 
EX.P.R1<- fieldMask(mosaic = EX.P,index = "BIM", cropValue = 19, cropAbove = T) 
plotRGB(EX.P.R1$newMosaic) 
 
# Counting all pollens above 0.01 size (all sample) 
EX.P.Total<-fieldCount(mosaic = EX.P.R1$mask, fieldShape = EX.P.shapeFile$fieldShape, minSize = 0.01)  
 
# Using index "BIM" to identify germinated pollen grain (removing values above 16) 
EX.P.R2<- fieldMask(mosaic = EX.P, index = "BIM", cropValue = 16, cropAbove = T) 
plotRGB(EX.P.R2$newMosaic) 
 
# Counting all germinated pollen above 0.005  
EX.P.<-fieldCount(mosaic = EX.P.R2$mask, fieldShape = EX.P.shapeFile$fieldShape, minSize = 0.005) 
 

EX1.Canopy<-fieldArea(mosaic = EX1.RemSoil$mask, fieldShape = EX1.Shape$fieldShape) 
EX1.Canopy$areaPorcent 
 
### Parallel (n.core = 3) 
EX1.Canopy<-fieldArea(mosaic = EX1.RemSoil$mask, fieldShape = EX1.Shape$fieldShape, n.core = 3) 
EX1.Canopy$areaPorcent 

EX1.Info<- fieldInfo(mosaic = EX1.Indices,fieldShape = EX1.Shape$fieldShape) 
EX1.Info$fieldShape@data 
 
### Parallel (n.core = 3) 
EX1.Info<- fieldInfo(mosaic = EX1.Indices,fieldShape = EX1.Shape$fieldShape, n.core = 3) 
EX1.Info$fieldShape@data 
 

# Uploading files from soil base (EX_DSM0.tif) and vegetative growth (EX_DSM1.tif): 
DSM0 <- stack("EX_DSM0.tif") 
DSM1 <- stack("EX_DSM1.tif") 
 
# Cropping the image using the previous shape from step 2: 
DSM0.C <- fieldCrop(mosaic = DSM0,fieldShape = EX1.Crop) 
DSM1.C <- fieldCrop(mosaic = DSM1,fieldShape = EX1.Crop) 
 
# Canopy Height Model (CHM): 
DSM0.R <- resample(DSM0.C, DSM1.C) 
CHM <- DSM1.C-DSM0.R 
 
# Rotating the image using the same theta from step 3: 
CHM.R<-fieldRotate(CHM, theta = 2.3) 
 
# Removing the soil using mask from step 4: 
CHM.S <- fieldMask(CHM.R, mask = EX1.RemSoil$mask) 
 
# Extracting the estimate plant height average (EPH): 
EPH <- fieldInfo(CHM.S$newMosaic, fieldShape = EX1.Shape$fieldShape, fun = "mean") 
EPH$plotValue 
 
# Extracting the estimate plant height at 10% and 90% of quantile: 
probs = c(0.1,0.9) 
EPH.Extract<-extract(x = CHM.S$newMosaic, y = EX1.Shape$fieldShape) 
EPH.10.90<-do.call(rbind,lapply(EPH.Extract, quantile, probs = probs, na.rm=TRUE)) 
EPH.10.90 
 
# Data: 
EPH.DataTotal<-data.frame(EPH$fieldShape@data,EPH.10.90) 
colnames(EPH.DataTotal)[c((dim(EPH.DataTotal)[2]-length(probs)):c(dim(EPH.DataTotal)[2]))]<-c("EPH_Mean",paste("EPH_",probs*100,"%",sep="")) 
EPH.DataTotal 
 

# Uploading file (EX_Obj.tif) 
EX.Obj <- stack("EX_Obj.jpg") 
plotRGB(EX.Obj) 
EX.Obj <- aggregate(EX.Obj,4) 
EX.shapeFile<-fieldPolygon(EX.Obj,extent = T) 
 
# Removing the background 
EX.Obj.M<- fieldMask(mosaic = EX.Obj, index = "BGI",cropValue = 0.7,cropAbove = T) 
dev.off() 
 
# Taking measurements (Remove artifacts by changing the parameter *minArea* and observing the values on EX.Obj.D$Dimension$area) 
EX.Obj.D<-fieldObject(mosaic = EX.Obj.M$mask, watershed = T, minArea = 0.01) 
 
# Measurement Output: 
EX.Obj.D$numObjects 
EX.Obj.D$Dimension 
plotRGB(EX.Obj) 
plot(EX.shapeFile$fieldShape, add=T) 
plot(EX.Obj.D$Objects, add=T, border="red") 
plot(EX.Obj.D$Polygons, add=T, border="blue") 
plot(EX.Obj.D$single.obj[[1]], add=T, col="yellow") 
lines(EX.Obj.D$x.position[[1]], col="red", lty=2) 
lines(EX.Obj.D$y.position[[1]], col="red", lty=2) 
EX.Obj.I<- fieldIndex(mosaic = EX.Obj,index = c("SI","BGI","BI")) 
EX.Obj.Data<-fieldInfo(mosaic = EX.Obj.I[[c("SI","BGI","BI")]], fieldShape = EX.Obj.D$Objects, projection = F) 
EX.Obj.Data$fieldShape@data 
 
# Perimeter: 
# install.packages("spatialEco") 
library(spatialEco) 
perimeter<-polyPerimeter(EX.Obj.D$Objects) 
box<-polyPerimeter(EX.Obj.D$Polygons) 
Data.Obj<-cbind(EX.Obj.Data$fieldShape@data,EX.Obj.D$Dimension,perimeter=perimeter,box=box) 
Data.Obj 
 

# Uploading file (EX_RemObj.tif) 
EX.RemObj <- stack("EX_RemObj.tif") 
 
# Selecting the object boundaries to be removed (nPoint = 10) 
EX.RemObj.Crop <- fieldCrop(mosaic = EX.RemObj, remove = T, nPoint = 10) # Selecting the plant in plot 13 
 
# Removing the soil 
EX.RemObj.RemSoil<- fieldMask(mosaic = EX.RemObj.Crop,index = "HUE") 
 
# Building the plot shapefile (ncols = 8 and nrows = 4) 
EX.RemObj.Shape<-fieldShape(mosaic = EX.RemObj.RemSoil,ncols = 8, nrows = 4) 
 
# Building indice (NGRDI) 
EX.RemObj.Indices<- fieldIndex(mosaic = EX.RemObj.RemSoil$newMosaic,index = c("NGRDI")) 
 
# Extracting data (NGRDI) 
EX.RemObj.Info<- fieldInfo(mosaic = EX.RemObj.Indices$NGRDI, 
                      fieldShape = EX.RemObj.Shape$fieldShape, 
                      n.core = 3) 
                       
# Comparing plots values (the plant in plot 13 was removed and its value must be lower than plot 12 and 14)                       
EX.RemObj.Info$plotValue[c(12,13,14),] 
 

# Uploading file (EX_RemObj.tif) 
EX.Dist <- stack("EX_RemObj.tif") 
 
# vegetation indices 
EX.Dist.Ind <- fieldIndex(mosaic = EX.Dist,index = c("NGRDI")) 
 
# Removing the soil 
EX.Dist.RemSoil <- fieldMask(mosaic = EX.Dist.Ind) 
 
# Evaluating distance between plants. Remember to press ESC when finished to draw the line. 
EX.Dist.Draw <- fieldDraw(mosaic = EX.Dist.RemSoil$mask,  
                          dist = T, 
                          value = 1, # Use the number to identify the object to be measured (1 for space and 0 for plants) 
                          distSel = 0.4)  
EX.Dist.Draw$drawDist # Distance between plants 
                           
# Making plots                          
plot(EX.Dist.Ind$NGRDI) 
points(EX.Dist.Draw$drawData$x,EX.Dist.Draw$drawData$y, col="red",pch=16,cex=0.7) 
points(EX.Dist.Draw$drawSegments$x,EX.Dist.Draw$drawSegments$y, col="blue",pch=16,cex=0.7) 
lines(EX.Dist.Draw$drawDist[1,c("x1","x2")],EX.Dist.Draw$drawDist[1,c("y1","y2")], col="green",lwd=5) 
 
# Evaluating an specific layer profile (e.g. NGRDI) 
EX.Dist.Draw.2 <- fieldDraw(mosaic = EX.Dist.Ind, 
                            ndraw = 4, # Making 4 lines (press ESC to conclude each line) 
                            lwd = 5) 
EX.Data<-EX.Dist.Draw.2$Draw1$drawData 
dev.off() 
plot(x = EX.Data$x, y = EX.Data$NGRDI, type="l", col="red",lwd=2,xlab="Distance (m)", ylab="NGRDI") 
abline(h=0.0,col="blue", lty=2, lwd=3) 
 
# Making polygons and extracting data per cell 
EX.Dist.Draw.3 <- fieldDraw(mosaic = EX.Dist.Ind, 
                            line = F, # Making 2 polygons (press ESC to conclude each polygon) 
                            ndraw = 2, 
                            lwd = 5) 
plotRGB(EX.Dist.RemSoil$newMosaic) 
plot(EX.Dist.Draw.3$Draw1$drawObject, col="red",add=T) 
 

### Image and resolution decrease  
 
RES_1<-stack("EX1_RGB_HighResolution.tif") 
RES_2<-aggregate(RES_1, fact=2) 
RES_4<-aggregate(RES_1, fact=4) 
 
res(RES_1) 
res(RES_2) 
res(RES_4) 
 
par(mfrow=c(1,3)) 
plotRGB(RES_1) 
plotRGB(RES_2) 
plotRGB(RES_4) 
 
### Crooping  
 
system.time({RES_1_C <- fieldCrop(mosaic = RES_1,fieldShape = EX1.Crop, plot = T)}) 
system.time({RES_2_C <- fieldCrop(mosaic = RES_2,fieldShape = EX1.Crop, plot = T)}) 
system.time({RES_4_C <- fieldCrop(mosaic = RES_4,fieldShape = EX1.Crop, plot = T)}) 
 
### Rotating  
   
system.time({RES_1_R <- fieldRotate(RES_1_C,theta = 2.3, plot = T)})  
system.time({RES_2_R <- fieldRotate(RES_2_C,theta = 2.3, plot = T)}) 
system.time({RES_4_R <- fieldRotate(RES_4_C,theta = 2.3, plot = T)}) 
   
### Removing Soil  
 
system.time({RES_1_S <- fieldMask(RES_1_R,index="HUE")}) 
system.time({RES_2_S <- fieldMask(RES_2_R,index="HUE")}) 
system.time({RES_4_S <- fieldMask(RES_4_R,index="HUE")}) 
 
### Indices 
   
system.time({RES_1_I <- fieldIndex(RES_1_S$newMosaic,index=c("BGI"))}) 
system.time({RES_2_I <- fieldIndex(RES_2_S$newMosaic,index=c("BGI"))}) 
system.time({RES_4_I <- fieldIndex(RES_4_S$newMosaic,index=c("BGI"))}) 
   
### Get Information (1 Band) 
   
system.time({RES_1_Info <- fieldInfo(RES_1_I$BGI,fieldShape = EX1.Shape$fieldShape,n.core = 3)}) 
system.time({RES_2_Info <- fieldInfo(RES_2_I$BGI,fieldShape = EX1.Shape$fieldShape,n.core = 3)}) 
system.time({RES_4_Info <- fieldInfo(RES_4_I$BGI,fieldShape = EX1.Shape$fieldShape,n.core = 3)}) 
   
### Get Information (3 Bands) 
   
system.time({RES_1_Info2 <- fieldInfo(RES_1_I[[c(1,2,3)]],fieldShape = EX1.Shape$fieldShape,n.core = 3)}) 
system.time({RES_2_Info2 <- fieldInfo(RES_2_I[[c(1,2,3)]],fieldShape = EX1.Shape$fieldShape,n.core = 3)}) 
system.time({RES_4_Info2 <- fieldInfo(RES_4_I[[c(1,2,3)]],fieldShape = EX1.Shape$fieldShape,n.core = 3)}) 
 
### Correlation 
 
DataBGI <- data.frame(R1=RES_1_Info$plotValue$BGI, 
                    R2=RES_2_Info$plotValue$BGI, 
                    R4=RES_4_Info$plotValue$BGI) 
DataBlue <- data.frame(R1=RES_1_Info2$plotValue$Blue, 
                       R2=RES_2_Info2$plotValue$Blue, 
                       R4=RES_4_Info2$plotValue$Blue) 
DataGreen <- data.frame(R1=RES_1_Info2$plotValue$Green, 
                       R2=RES_2_Info2$plotValue$Green, 
                       R4=RES_4_Info2$plotValue$Green) 
DataRed <- data.frame(R1=RES_1_Info2$plotValue$Red, 
                       R2=RES_2_Info2$plotValue$Red, 
                       R4=RES_4_Info2$plotValue$Red) 
cor(DataBGI) 
cor(DataBlue) 
cor(DataGreen) 
cor(DataRed) 
 

# Uploading Flowering (EX2_RGB.tif) and Senescence (EX3_RGB.tif) files: 
EX2 <- stack("EX2_RGB.tif") 
EX3 <- stack("EX3_RGB.tif") 
 
# Cropping the image using the previous shape from step 2: 
 
EX2.Crop <- fieldCrop(mosaic = EX2,fieldShape = EX1.Crop, plot = T) 
EX3.Crop <- fieldCrop(mosaic = EX3,fieldShape = EX1.Crop, plot = T) 
 
# Rotating the image using the same theta from step 3: 
 
EX2.Rotated<-fieldRotate(EX2.Crop,theta = 2.3, plot = T) 
EX3.Rotated<-fieldRotate(EX3.Crop,theta = 2.3, plot = T) 
 
# Removing the soil using index and mask from step 4: 
 
EX2.RemSoil<-fieldMask(EX2.Rotated,index="HUE",cropValue=0,cropAbove=T,plot=T) 
EX3.RS<-fieldMask(EX3.Rotated,index="HUE",cropValue=0,cropAbove=T,plot=T) # Removing soil at senescence stage 
EX3.RemSoil<-fieldMask(EX3.RS$newMosaic,mask = EX2.RemSoil$mask ,cropValue=0,cropAbove=T,plot=T) # Removing weeds from senescence stage with flowering mask  
 
# Building indices 
 
EX2.Indices <- fieldIndex(EX2.RemSoil$newMosaic,Red=1,Green=2,Blue=3, 
                 index = c("NGRDI","BGI"), myIndex = c("(Red-Blue)/Green")) 
EX3.Indices <- fieldIndex(EX3.RemSoil$newMosaic,Red=1,Green=2,Blue=3, 
                 index = c("NGRDI","BGI"), myIndex = c("(Red-Blue)/Green")) 
 
# Extracting data using the same fieldShape file from step 5: 
 
EX2.Info<- fieldInfo(mosaic = EX2.Indices$myIndex,fieldShape = EX1.Shape$fieldShape,n.core = 3) 
EX3.Info<- fieldInfo(mosaic = EX3.Indices$myIndex,fieldShape = EX1.Shape$fieldShape,n.core = 3) 
 
Data.Cycle<-data.frame(EX1=EX1.Info$plotValue$myIndex, 
      EX2=EX2.Info$plotValue$myIndex, 
      EX3=EX3.Info$plotValue$myIndex) 
 
Data.Cycle 

 
##################### 
### Multispectral ### 
##################### 
 
# Uploading multispectral mosaic: 
EX1.5b <- stack("EX1_5Band.tif") 
 
# Cropping the image using the previous shape from step 2: 
EX1.5b.Crop <- fieldCrop(mosaic = EX1.5b,fieldShape = EX1.Crop, plot = T) 
 
# Rotating the image using the same theta from step 3: 
EX1.5b.Rotated<-fieldRotate(EX1.5b.Crop,theta = 2.3, plot = T) 
 
# Removing the soil using index and mask from step 4: 
EX1.5b.RemSoil<-fieldMask(EX1.5b.Rotated,Red=1,Green=2,Blue=3,index="HUE",cropValue=0,cropAbove=T,plot=T) 
 
# Building indices (NDVI and NDRE) 
EX1.5b.Indices <- fieldIndex(EX1.5b.RemSoil$newMosaic,Red=1,Green=2,Blue=3,RedEdge=4,NIR=5, 
                 index = c("NDVI","NDRE")) 
 
# Extracting data using the same fieldShape file from step 5: 
EX1.5b.Info<- fieldInfo(mosaic = EX1.5b.Indices$NDVI,fieldShape = EX1.Shape$fieldShape,n.core = 3) 
 

 
##################### 
### Hyperspectral ### 
##################### 
 
# Uploading hyperspectral file with 474 bands (EX_HYP.tif) 
EX.HYP<-stack("EX_HYP.tif") 
 
# Wavelengths (namesHYP.csv) 
NamesHYP<-as.character(read.csv("namesHYP.csv")$NameHYP) 
 
# Building RGB image  
R<-EX.HYP[[78]] # 651nm (Red) 
G<-EX.HYP[[46]] # 549nm (Green) 
B<-EX.HYP[[15]] # 450nm (Blue) 
RGB<-stack(c(R,G,B)) 
plotRGB(RGB, stretch="lin") 
 
# Removing soil using RGB (index NGRDI) 
RGB.S<-fieldMask(RGB,index="NGRDI",cropValue = 0.0, cropAbove = F) 
 
# Data frame with field information to make the Map 
Data<-read.csv("DataHYP.csv") 
Map<-fieldMap(fieldPlot = as.character(Data$Plot),fieldRow = as.character(Data$Range),fieldColumn = as.character(Data$Row),decreasing = T) 
 
# Building plot shapefile using RGB as base 
plotFile<-fieldShape(RGB.S,ncols = 14, nrows = 14, fieldMap = Map,fieldData = Data, ID = "Plot") 
 
# Removing soil using the RGB mask 
EX.HYP.S<-fieldMask(EX.HYP,mask = RGB.S$mask, plot = F) 
 
# Extracting data (474 bands) 
EX.HYP.I<-fieldInfo(EX.HYP.S$newMosaic,fieldShape = plotFile$fieldShape,n.core = 3) 
 
# Saving the new csv with hyperspectral information per plot 
DataHYP<-EX.HYP.I$fieldShape@data 
colnames(DataHYP)<-c(colnames(DataHYP)[1:9],NamesHYP) 
write.csv(DataHYP,"DataHypNew.csv",col.names = T,row.names = F) 
 
############### 
### Graphic ### 
############### 
 
dev.off() 
DataHYP1<-EX.HYP.I$plotValue[,-1] 
 
plot(x=as.numeric(NamesHYP),y=as.numeric(DataHYP1[1,]),type = "l",xlab = "Wavelength (nm)",ylab = "Reflectance", col="black",lwd=2,cex.lab=1.2) 
for(i in 2:dim(DataHYP1)[2]){ 
  lines(x=as.numeric(NamesHYP),y=as.numeric(DataHYP1[i,]),type = "l",col=i,lwd=2) 
} 
abline(v=445,col="blue",lwd=2,lty=2) 
abline(v=545,col="green",lwd=2,lty=2) 
abline(v=650,col="red",lwd=2,lty=2) 
abline(v=720,col="red",lwd=2,lty=3) 
abline(v=840,col="red",lwd=2,lty=4) 
legend(list(x = 2000,y = 0.5),c("Blue (445nm)","Green (545nm)","Red (650nm)","RedEdge (720nm)","NIR (840nm)"), 
       col =c("blue","green","red","red","red"),lty=c(2,2,2,3,4),box.lty=0) 
 

# Uploading file (EX_polygonShape.tif) 
EX.polygon<-stack("EX_polygonShape.tif") 
plotRGB(EX.polygon, r = 1, g = 2, b = 3) 
 
# Removing soil 
EX.polygon.RemSoil<- fieldMask(mosaic = EX.polygon) 
 
# Data frame with polygons information 
polygonData<-data.frame(ID=c("Polygon1","Polygon2","Polygon3"), 
                        FlowerColor=c("white","white","white"), 
                        FlowerPercent=c(20,40,50), 
                        LeafColor=c("dark","light","dark")) 
polygonData 
 
# Building plot shapefile with 3 polygons (select 4 points around the polygon area) 
EX.polygon.Shape<-fieldPolygon(mosaic = EX.polygon.RemSoil, 
                               nPolygon = 3,nPoint = 4,ID = "ID", 
                               polygonData = polygonData,cropPolygon = T, 
                               polygonID = c("Polygon1","Polygon2","Polygon3")) 
plotRGB(EX.polygon.Shape$cropField) 
 
# Building indice (NGRDI and BGI) 
EX.polygon.Indices<- fieldIndex(mosaic = EX.polygon.RemSoil$newMosaic, Red = 1, Green = 2, Blue = 3,  
                             index = c("NGRDI","BGI")) 
 
# Extracting data (NGRDI and BGI) 
EX.polygon.Info<- fieldInfo(mosaic = EX.polygon.Indices[[c("NGRDI","BGI")]], 
                   fieldShape = EX.polygon.Shape$fieldShape, n.core = 3) 
EX.polygon.Info$fieldShape@data 
 
# Making graphics (BGI) 
fieldPlot(fieldShape=EX.polygon.Info$fieldShape, 
          fieldAttribute="BGI", 
          mosaic=EX.polygon, color=c("red","blue"), alpha = 0.5) 

### Interpolating colors: c("white","black") 
fieldPlot(fieldShape=EX1.Info$fieldShape,fieldAttribute="Yield", mosaic=EX1.Indices, color=c("white","black"), alpha = 0.5) 
 
### Interpolating colors: c("red","blue") 
fieldPlot(fieldShape=EX1.Info$fieldShape,fieldAttribute="myIndex", mosaic=EX1.Indices, color=c("red","blue"), alpha = 0.5) 
 

### Images (single and multi layers) 
writeRaster(EX1.Indices, filename="EX1.Indices.tif", options="INTERLEAVE=BAND", overwrite=TRUE) 
# EX1.Indices.2 <- stack("EX1.Indices.tif") # Reading the saved image. 
 
### FieldShape file 
library(rgdal) 
writeOGR(EX1.Info$fieldShape, ".", "EX1.fieldShape", driver="ESRI Shapefile") 
# EX1.fieldShape.2 <- terra::vect("EX1.fieldShape.shp") # Reading the saved shapefile option 01. 
# EX1.fieldShape.2 <- readOGR("EX1.fieldShape.shp") # Reading the saved shapefile option 02. 
 
### CSV file (table) 
write.csv(EX1.Info$fieldShape@data,file = "EX1.Info.csv",col.names = T,row.names = F) 
# Data.EX1.Info<-read.csv("EX1.Info.csv",header = T,check.names = F) # Reading the saved data table. 
 

# Uploading file (odm_orthophoto.tif): 
EX.ODM<-stack("odm_orthophoto.tif") 
plotRGB(EX.ODM, r = 1, g = 2, b = 3) 
 
# Cropping the image to select only one trial (Selecting the same trial as EX.2 from step.13): 
EX.ODM.Crop <- fieldCrop(mosaic = EX.ODM) 
 
# Rotating the image using the same theta from step 3: 
EX.ODM.Rotated<-fieldRotate(EX.ODM.Crop,theta = 2.3) 
 
# Removing soil 
EX.ODM.RemSoil<- fieldMask(mosaic = EX.ODM.Rotated) 
 
# Building indices 
EX.ODM.Indices <- fieldIndex(EX.ODM.RemSoil$newMosaic,Red=1,Green=2,Blue=3, 
                 index = c("NGRDI","BGI"), myIndex = c("(Red-Blue)/Green")) 
 
# Extracting data using the same fieldShape file from step 5: 
EX.ODM.Info<- fieldInfo(mosaic = EX.ODM.Indices$myIndex,fieldShape = EX1.Shape$fieldShape,n.core = 3) 
 
EX.ODM.Info$plotValue$myIndex 
 

# Images names (folder directory: "./images/") 
pics<-list.files("./images/") 
 
# Vegetation indices 
index<- c("BGI","VARI","SCI") 
 
############ 
### Loop ### 
############ 
 
system.time({ # system.time: used to compare the processing time using loop and parallel 
EX.Table.Loop<-NULL 
for(i in 1:length(pics)){ 
  EX.L1<-stack(paste("./images/",pics[i],sep = "")) 
  plotRGB(EX.L1) 
  EX.L.Shape<-fieldPolygon(mosaic=EX.L1, extent=T, plot=F) # extent=T (The whole image area will be the shapefile) 
  EX.L2<-fieldMask(mosaic=EX.L1, index="BGI", cropValue=0.8, cropAbove=T, plot=F) # Select one index to identify leaves and remove the background 
  EX.L3<-fieldMask(mosaic=EX.L2$newMosaic, index="VARI", cropValue=0.1, cropAbove=T, plot=F) # Select one index to identify demaged area in the leaves   
  EX.L4<-fieldIndex(mosaic=EX.L2$newMosaic, index=index, plot=F) # Indices 
  EX.L5<-stack(EX.L3$mask, EX.L4[[index]]) # Making a new stack raster with new layers (demage area and indices) 
  EX.L.Info<- fieldInfo(mosaic=EX.L5, fieldShape=EX.L.Shape$fieldShape, projection=F) # projection=F (Ignore projection. Normally used only with remote sensing images) 
  plot(EX.L5,col = grey(1:100/100)) 
  EX.Table.Loop<-rbind(EX.Table.Loop, EX.L.Info$plotValue) # Combine information from all images in one table 
}}) 
rownames(EX.Table.Loop)<-pics 
EX.Table.Loop 
 
################ 
### Parallel ### 
################ 
 
# Required packages 
library(parallel) 
library(foreach) 
library(doParallel) 
 
# Number of cores 
n.core<-detectCores()-1 
 
# Starting parallel 
cl <- makeCluster(n.core, output = "") 
registerDoParallel(cl) 
system.time({ 
EX.Table.Parallel <- foreach(i = 1:length(pics), .packages = c("raster","FIELDimageR"),  
                     .combine = rbind) %dopar% { 
                       EX.L1<-stack(paste("./images/",pics[i],sep = "")) 
                       EX.L.Shape<-fieldPolygon(mosaic=EX.L1, extent=T, plot=F) # extent=T (The whole image area will be the shapefile) 
                       EX.L2<-fieldMask(mosaic=EX.L1, index="BGI", cropValue=0.8, cropAbove=T, plot=F) # Select one index to identify leaves and remove the background 
                       EX.L3<-fieldMask(mosaic=EX.L2$newMosaic, index="VARI", cropValue=0.1, cropAbove=T, plot=F) # Select one index to identify demaged area in the leaves   
                       EX.L4<-fieldIndex(mosaic=EX.L2$newMosaic, index=index, plot=F) # Indices 
                       EX.L5<-stack(EX.L3$mask, EX.L4[[index]]) # Making a new stack raster with new layers (demage area and indices) 
                       EX.L.Info<- fieldInfo(mosaic=EX.L5, fieldShape=EX.L.Shape$fieldShape, projection=F) # projection=F (Ignore projection. Normally used only with remote sensing images) 
                       EX.L.Info$plotValue # Combine information from all images in one table 
                     }}) 
stopCluster(cl) 
rownames(EX.Table.Parallel)<-pics 
EX.Table.Parallel  
 
