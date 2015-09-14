library(foreign)
library(sp)
library(raster)
library(maptools)
library(rgdal)


# Prepare Data ------------------------------------------------------------

    setwd('R://Mann Research/et_gender/')
    data = read.dta('Mike\'s_GPS_hunt12.dta')
    head(data)
    class(data$A9)
    
    # convert locations to decimal degrees
    data$LAT_DD= as.numeric(char2dms(paste(data$LAT_D,'d',data$LAT_M,'\'',data$LAT_S,'"N',sep='')))
    data$LON_DD= as.numeric(char2dms(paste(data$LON_D,'d',data$LON_M,'\'',data$LON_S,'"N',sep='')))
    coordinates(data) = ~ LON_DD+LAT_DD
    projection(data) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") #CRS("+proj=utm +zone=37 +ellps=clrk80 +units=m +no_defs")
    data = sp::spTransform(data,CRS("+proj=utm +zone=37 +a=6378249.145 +b=6356514.96582849 +units=m +no_defs"))
    

    # map
    et_map = readOGR('Boundary data','ETH_adm1_UTM')
    et_map = sp::spTransform(et_map,CRS("+proj=utm +zone=37 +a=6378249.145 +b=6356514.96582849 +units=m +no_defs"))
    
    plot(et_map)
    plot(data,add=T, col='blue')
    
    

# Extact data -------------------------------------------------------------
    # extract mean values for 1 hectare around house, sqrt(10000/pi) = 56.41 meters
    
  
    tifs = list.files('Data to Extract/','tif$')
    
    for(i in 1:length(tifs)){
      
      name = tifs[i]
      raster = raster(paste('Data to Extract',tifs[i],sep='/'))      
      windows()
      plot(raster)
      plot(data,add=T, col='red')
      ext_values = extract(raster,data,fun= function(x){mean(x,na.rm=T)},df=T,buffer=56.41,small=T)
      names(ext_values)=c('id','values')
      data[[paste(name)]] =ext_values[,2]
    }  
    
write.csv(data@data,'ET_gender_stats.csv')    
    
    
    
    
    
    
    
    

# OLD CODE ----------------------------------------------------------------
# 
#     
#     # Read in raster data 
#     ras = raster('G://Faculty//Mann//Projects//Ethiopia Project//GIS Data//STRM//UTM_Projected//slope_UTM.tif')
#     #plot(ras)
#     
#     empty_ras = ras
#     values(empty_ras) = NA
# 
# # Create house statistics -------------------------------------------------
# 
#     head(data)
#     data$id = 1:dim(data@data)[1]
#     
#     # Create buffer around homes
#     # Assign NAs to all but the house
#     
#     point = data[data$id==1,]
#     # get location of household
#     cell =  cellFromXY(ras,point)
#     head( cell)
#     row_col = rowColFromCell(ras,cell)
#     row_col
#     # limit raster to household 
#     empty_ras[row_col[1],row_col[2]]=1 #
#     #empty_ras = setValues(empty_ras,values=10000,index=cell)
#     
#     r_buf= raster::buffer(empty_ras,1000 ) # in meters is unprojected
#     plot(r_buf)
#     buffer::raster(home_raster,500 ) # in meters is unprojected
# 
# 
# 
# 
# 
# point = data.frame(lat=-50,lon=50)
# coordinates(point) = ~ lon+lat
# projection(point) = CRS("+proj=utm +zone=37 +ellps=clrk80 +units=m +no_defs")
# 
# r <- raster(ncols=10, nrows=10)
# projection(r) = CRS("+proj=utm +zone=37 +ellps=clrk80 +units=m +no_defs")
# values(r) = 5
# plot(r)
# plot(point, add= T)
# 
# cell_num = cellFromXY(r,point) # counts from left to right top to bottom 
# cell_num
# 
# #Get row and column number of home
# row_col = rowColFromCell(r,cell_num)
# row_col
# 
# # create a new buffer rater
# r2 = r
# r2[row_col[1],row_col[2]]=1000
# r2[r2!=1000]=NA
# plot(r2)
# plot(point, add=T)
# 
# r_buf= raster::buffer(r2,50 ) # in meters is unprojected
# plot(r_buf)
# 
