## read in libraries for raster and spatial data
library(raster)
library(rgdal)

## read in initial data (initial communities data comes from LEMMA GNN) (ecoregions data comes from LANDFIRE biophysical communities).
initial_communities <- raster("C:\\Users\\Chris\\Desktop\\chrisjonesLandis\\LANDIS_II_small\\BigSur_EDA\\initialcommunitiesmap.img")
ecoregions <- raster("C:\\Users\\Chris\\Desktop\\chrisjonesLandis\\LANDIS_II_small\\BigSur_EDA\\ecoregionsmap.img")

## Set 0s to NA in order to not have masked areas on the map affect the resolution change.
initial_communities[initial_communities==0] <- NA

## Use 2 methods for moving from 30m data to 90 m, 270 m, and 810 m 
## All from original resolution method using majority rules method
int_communities_90m <- aggregate(initial_communities, fact = 3, fun = modal, na.rm = TRUE) 
int_communities_270m <- aggregate (initial_communities, fact = 9, fun =modal, na.rm = TRUE)
int_communities_810m <- aggregate (initial_communities, fact = 27, fun =modal, na.rm = TRUE)

ecoregions_90m <- aggregate(ecoregions, fact = 3, fun = modal, na.rm = TRUE) 
ecoregions_270m <- aggregate (ecoregions, fact = 9, fun =modal, na.rm = TRUE)
ecoregions_810m <- aggregate (ecoregions, fact = 27, fun =modal, na.rm = TRUE)
## From newest resolution method
int_communities_270m_m2 <- aggregate(int_communities_90m, fact = 3, fun = modal, na.rm = TRUE)
int_communities_810m_m2 <- aggregate(int_communities_270m_m2, fact = 3, fun = modal, na.rm = TRUE)

ecoregions_270m_m2 <- aggregate(ecoregions_90m, fact = 3, fun = modal, na.rm = TRUE)
ecoregions_810m_m2 <- aggregate(ecoregions_270m_m2, fact = 3, fun = modal, na.rm = TRUE)

## Using nearest neighbor method
int_communities_90m_nn <- resample(initial_communities, int_communities_90m, method = "ngb", na.rm = TRUE) 
int_communities_270m_nn <- resample(initial_communities, int_communities_270m, method = "ngb", na.rm = TRUE)
int_communities_810m_nn <- resample(initial_communities, int_communities_810m, method = "ngb", na.rm = TRUE)

ecoregions_90m_nn <- resample(ecoregions, ecoregions_90m, method = "ngb", na.rm = TRUE) 
ecoregions_270m_nn <- resample(ecoregions, ecoregions_270m, method = "ngb", na.rm = TRUE)
ecoregions_810m_nn <- resample(ecoregions, ecoregions_810m, method = "ngb", na.rm = TRUE)

## create data frame with values for all methods and resolutions
# ecoregion data frame
eco_30m <- freq(ecoregions)
eco_90m <- freq(ecoregions_90m)
eco_270m <- freq(ecoregions_270m)
eco_810m <- freq(ecoregions_810m)
eco_270m_m2 <- freq(ecoregions_270m_m2)
eco_810m_m2 <- freq(ecoregions_810m_m2)
eco_90m_nn <- freq(ecoregions_90m_nn)
eco_270m_nn <- freq(ecoregions_270m_nn)
eco_810m_nn <- freq(ecoregions_810m_nn)
ecoregions_df <- data.frame(class = eco_30m[2:nrow(eco_30m),1], precent_30m = round(eco_30m[2:nrow(eco_30m),2]/sum(eco_30m[2:nrow(eco_30m),2])*100, 2))
ecoregions_df$precent_90m = round(eco_90m[2:nrow(eco_90m),2]/sum(eco_90m[2:nrow(eco_90m),2])*100, 2)
ecoregions_df$precent_270m = round(eco_270m[2:nrow(eco_270m),2]/sum(eco_270m[2:nrow(eco_270m),2])*100, 2)
ecoregions_df$precent_270m_m2 = round(eco_270m_m2[2:nrow(eco_270m_m2),2]/sum(eco_270m_m2[2:nrow(eco_270m_m2),2])*100, 2)
ecoregions_df$precent_810m = 0
ecoregions_df$precent_810m_m2 = 0
ecoregions_df$precent_810m[ecoregions_df$class %in% eco_810m[,1]] = round(eco_810m[2:nrow(eco_810m),2]/sum(eco_810m[2:nrow(eco_810m),2])*100, 2)
ecoregions_df$precent_810m_m2[ecoregions_df$class %in% eco_810m_m2[,1]] = round(eco_810m_m2[2:nrow(eco_810m_m2),2]/sum(eco_810m_m2[2:nrow(eco_810m_m2),2])*100, 2)
ecoregions_df$precent_90m_nn = 0
ecoregions_df$precent_270m_nn = 0
ecoregions_df$precent_810m_nn = 0
ecoregions_df$precent_90m_nn[ecoregions_df$class %in% eco_90m_nn[,1]] = round(eco_90m_nn[2:nrow(eco_90m_nn),2]/sum(eco_90m_nn[2:nrow(eco_90m_nn),2])*100, 2)
ecoregions_df$precent_270m_nn[ecoregions_df$class %in% eco_270m_nn[,1]] = round(eco_270m_nn[2:nrow(eco_270m_nn),2]/sum(eco_270m_nn[2:nrow(eco_270m_nn),2])*100, 2)
ecoregions_df$precent_810m_nn[ecoregions_df$class %in% eco_810m_nn[,1]] = round(eco_810m_nn[2:nrow(eco_810m_nn),2]/sum(eco_810m_nn[2:nrow(eco_810m_nn),2])*100, 2)

# initial communities data frame
init_comm_30m <- freq(initial_communities)
init_comm_90m <- freq(int_communities_90m)
init_comm_270m <- freq(int_communities_270m)
init_comm_810m <- freq(int_communities_810m)
init_comm_270m_m2 <- freq(int_communities_270m_m2)
init_comm_810m_m2 <- freq(int_communities_810m_m2)
init_comm_90m_nn <- freq(int_communities_90m_nn)
init_comm_270m_nn <- freq(int_communities_270m_nn)
init_comm_810m_nn <- freq(int_communities_810m_nn)
initial_communities_df <- data.frame(class = init_comm_30m[1:nrow(init_comm_30m)-1,1], precent_30m = round(init_comm_30m[1:nrow(init_comm_30m)-1,2]/sum(init_comm_30m[1:nrow(init_comm_30m)-1,2])*100, 2))
initial_communities_df$precent_90m = 0
initial_communities_df$precent_270m = 0
initial_communities_df$precent_270m_m2 = 0
initial_communities_df$precent_90m[initial_communities_df$class %in% init_comm_90m[,1]] = round(init_comm_90m[1:nrow(init_comm_90m),2]/sum(init_comm_90m[1:nrow(init_comm_90m)-1,2])*100, 2)
initial_communities_df$precent_270m[initial_communities_df$class %in% init_comm_270m[,1]] = round(init_comm_270m[1:nrow(init_comm_270m),2]/sum(init_comm_270m[1:nrow(init_comm_270m)-1,2])*100, 2)
initial_communities_df$precent_270m_m2[initial_communities_df$class %in% init_comm_270m_m2[,1]] = round(init_comm_270m_m2[1:nrow(init_comm_270m_m2),2]/sum(init_comm_270m_m2[1:nrow(init_comm_270m_m2)-1,2])*100, 2)
initial_communities_df$precent_810m = 0
initial_communities_df$precent_810m_m2 = 0
initial_communities_df$precent_810m[initial_communities_df$class %in% init_comm_810m[,1]] = round(init_comm_810m[1:nrow(init_comm_810m),2]/sum(init_comm_810m[1:nrow(init_comm_810m)-1,2])*100, 2)
initial_communities_df$precent_810m_m2[initial_communities_df$class %in% init_comm_810m_m2[,1]] = round(init_comm_810m_m2[1:nrow(init_comm_810m_m2),2]/sum(init_comm_810m_m2[1:nrow(init_comm_810m_m2)-1,2])*100, 2)
initial_communities_df$precent_90m_nn = 0
initial_communities_df$precent_270m_nn = 0
initial_communities_df$precent_810m_nn = 0
initial_communities_df$precent_90m_nn[initial_communities_df$class %in% init_comm_90m_nn[,1]] = round(init_comm_90m_nn[1:nrow(init_comm_90m_nn),2]/sum(init_comm_90m_nn[1:nrow(init_comm_90m_nn)-1,2])*100, 2)
initial_communities_df$precent_270m_nn[initial_communities_df$class %in% init_comm_270m_nn[,1]] = round(init_comm_270m_nn[1:nrow(init_comm_270m_nn),2]/sum(init_comm_270m_nn[1:nrow(init_comm_270m_nn)-1,2])*100, 2)
initial_communities_df$precent_810m_nn[initial_communities_df$class %in% init_comm_810m_nn[,1]] = round(init_comm_810m_nn[1:nrow(init_comm_810m_nn),2]/sum(init_comm_810m_nn[1:nrow(init_comm_810m_nn)-1,2])*100, 2)


library(reshape2)
eco_melt <- melt(ecoregions_df, id.vars = "class")

plot(eco_melt$class, eco_melt$value)


## Change NA values in initial communities back to 0
int_communities_90m[is.na(int_communities_90m)] <- 0
int_communities_270m[is.na(int_communities_270m)] <- 0
int_communities_810m[is.na(int_communities_810m)] <- 0

## Decided to use majority rule based on original resolution data (e.g. method 1 which was assumed to be the preferred method from the start)
writeRaster(x = ecoregions_90m, filename = "C:/Users/Chris/Desktop/landis_eda_res_ext_paper_data/ecoregions90m.tif", overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
writeRaster(x = ecoregions_270m, filename = "C:/Users/Chris/Desktop/landis_eda_res_ext_paper_data/ecoregions270m.tif", overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
writeRaster(x = ecoregions_810m, filename = "C:/Users/Chris/Desktop/landis_eda_res_ext_paper_data/ecoregions810m.tif", overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
writeRaster(x = int_communities_90m, filename = "C:/Users/Chris/Desktop/landis_eda_res_ext_paper_data/initialcommunities90m.tif", overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
writeRaster(x = int_communities_270m, filename = "C:/Users/Chris/Desktop/landis_eda_res_ext_paper_data/initialcommunities270m.tif", overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
writeRaster(x = int_communities_810m, filename = "C:/Users/Chris/Desktop/landis_eda_res_ext_paper_data/initialcommunities810m.tif", overwrite=TRUE, format = 'GTiff', datatype = "INT4S")

## Set up initial infections to match new resolutions
# init_infections <- raster("C:\\Users\\Chris\\Desktop\\chrisjonesLandis\\LANDIS_II_small\\BigSur_EDA\\SOD_positive_1996_UINT8.img")
init_infect <- raster("G:/FinalModelRunsFixed/sod/5405/eda/ramorum-10.img")
init_infections = raster(ext=initial_communities@extent, resolution=res(initial_communities),crs=projection(initial_communities))
init_infections[] = getValues(init_infect)
init_infections[init_infections == 0] <- NA
init_infections[init_infections > 0 & init_infections < 3] <- 0
init_infections[init_infections == 3] <- 1

init_infections_90m <- aggregate(init_infections, fact = 3, fun = modal, na.rm = TRUE) 
init_infections_270m <- aggregate (init_infections, fact = 9, fun =modal, na.rm = TRUE)
init_infections_810m <- aggregate (init_infections, fact = 27, fun =modal, na.rm = TRUE)

init_infections[is.na(init_infections)]<- 0
init_infections_90m[is.na(init_infections_90m)]<- 0
init_infections_270m[is.na(init_infections_270m)]<- 0
init_infections_810m[is.na(init_infections_810m)]<- 0

writeRaster(x = init_infections_90m, filename = "C:/Users/Chris/Desktop/landis_eda_res_ext_paper_data/initialinfections90m.tif", overwrite=TRUE, format = 'GTiff', datatype = "INT1U")
writeRaster(x = init_infections_270m, filename = "C:/Users/Chris/Desktop/landis_eda_res_ext_paper_data/initialinfections270m.tif", overwrite=TRUE, format = 'GTiff', datatype = "INT1U")
writeRaster(x = init_infections_810m, filename = "C:/Users/Chris/Desktop/landis_eda_res_ext_paper_data/initialinfections810m.tif", overwrite=TRUE, format = 'GTiff', datatype = "INT1U")

## Create random samples of half the extent of the study area
nrow_h_30 <- round(nrow(ecoregions)/2)
nrow_h_90 <- round(nrow(ecoregions_90m)/2)
nrow_h_270 <- round(nrow(ecoregions_270m)/2)
nrow_h_810 <- round(nrow(ecoregions_810m)/2)

ncols_30 <- ncol(ecoregions)
ncols_90 <- ncol(ecoregions_90m)
ncols_270 <- ncol(ecoregions_270m)
ncols_810 <- ncol(ecoregions_810m)

x_30 <- round(runif(10, 1, nrow_h_30))
x_90 <- round(runif(10, 1, nrow_h_90))
x_270 <- round(runif(10, 1, nrow_h_270))
x_810 <- round(runif(10, 1, nrow_h_810))

output_dir <- "C:/Users/Chris/Desktop/landis_eda_res_ext_paper_data/half"
dir.create(output_dir)

for (i in 1:length(x)) {
  output_dir2 <- paste(output_dir,'/',i,sep = '')
  dir.create(output_dir2)
  ecoregions_30 <- crop(ecoregions, extent(ecoregions, x_30[i], x_30[i]+nrow_h_30, 1, ncols_30))
  ecoregions_90 <- crop(ecoregions_90m, extent(ecoregions_90m, x_90[i], x_90[i]+nrow_h_90, 1, ncols_90))
  ecoregions_270 <- crop(ecoregions_270m, extent(ecoregions_270m, x_270[i], x_270[i]+nrow_h_270, 1, ncols_270))
  ecoregions_810 <- crop(ecoregions_810m, extent(ecoregions_810m, x_810[i], x_810[i]+nrow_h_810, 1, ncols_810))

  init_comm_30 <- crop(initial_communities, extent(initial_communities, x_30[i], x_30[i]+nrow_h_30, 1, ncols_30))
  init_comm_90 <- crop(int_communities_90m, extent(int_communities_90m, x_90[i], x_90[i]+nrow_h_90, 1, ncols_90))
  init_comm_270 <- crop(int_communities_270m, extent(int_communities_270m, x_270[i], x_270[i]+nrow_h_270, 1, ncols_270))
  init_comm_810 <- crop(int_communities_810m, extent(int_communities_810m, x_810[i], x_810[i]+nrow_h_810, 1, ncols_810))
  
  init_infections_30 <- crop(init_infections, extent(init_infections, x_30[i], x_30[i]+nrow_h_30, 1, ncols_30))
  init_infections_90 <- crop(init_infections_90m, extent(init_infections_90m, x_90[i], x_90[i]+nrow_h_90, 1, ncols_90))
  init_infections_270 <- crop(init_infections_270m, extent(init_infections_270m, x_270[i], x_270[i]+nrow_h_270, 1, ncols_270))
  init_infections_810 <- crop(init_infections_810m, extent(init_infections_810m, x_810[i], x_810[i]+nrow_h_810, 1, ncols_810))
  
  writeRaster(x = ecoregions_30, filename = paste(output_dir2,'ecoregions_30.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  writeRaster(x = ecoregions_90, filename = paste(output_dir2,'ecoregions_90.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  writeRaster(x = ecoregions_270, filename = paste(output_dir2,'ecoregions_270.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  writeRaster(x = ecoregions_810, filename = paste(output_dir2,'ecoregions_810.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  
  writeRaster(x = init_comm_30, filename = paste(output_dir2,'init_comm_30.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  writeRaster(x = init_comm_90, filename = paste(output_dir2,'init_comm_90.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  writeRaster(x = init_comm_270, filename = paste(output_dir2,'init_comm_270.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  writeRaster(x = init_comm_810, filename = paste(output_dir2,'init_comm_810.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  
  writeRaster(x = init_infections_30, filename = paste(output_dir2,'init_infections_30.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT1U")
  writeRaster(x = init_infections_90, filename = paste(output_dir2,'init_infections_90.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT1U")
  writeRaster(x = init_infections_270, filename = paste(output_dir2,'init_infections_270.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT1U")
  writeRaster(x = init_infections_810, filename = paste(output_dir2,'init_infections_810.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT1U")
}
  
## Create random samples of quarter the extent of the study area
nrow_30 <- round(nrow(ecoregions)/4)
nrow_90 <- round(nrow(ecoregions_90m)/4)
nrow_270 <- round(nrow(ecoregions_270m)/4)
nrow_810 <- round(nrow(ecoregions_810m)/4)

ncols_30 <- ncol(ecoregions)
ncols_90 <- ncol(ecoregions_90m)
ncols_270 <- ncol(ecoregions_270m)
ncols_810 <- ncol(ecoregions_810m)

x_30 <- round(runif(10, 1, nrow(ecoregions) - nrow_30))
x_90 <- round(runif(10, 1, nrow(ecoregions_90m) - nrow_90))
x_270 <- round(runif(10, 1, nrow(ecoregions_270m) - nrow_270))
x_810 <- round(runif(10, 1, nrow(ecoregions_810m) - nrow_810))

output_dir <- "C:/Users/Chris/Desktop/landis_eda_res_ext_paper_data/quarter"
dir.create(output_dir)

for (i in 1:length(x)) {
  output_dir2 <- paste(output_dir,'/',i,sep = '')
  dir.create(output_dir2)
  ecoregions_30 <- crop(ecoregions, extent(ecoregions, x_30[i], x_30[i]+nrow_30, 1, ncols_30))
  ecoregions_90 <- crop(ecoregions_90m, extent(ecoregions_90m, x_90[i], x_90[i]+nrow_90, 1, ncols_90))
  ecoregions_270 <- crop(ecoregions_270m, extent(ecoregions_270m, x_270[i], x_270[i]+nrow_270, 1, ncols_270))
  ecoregions_810 <- crop(ecoregions_810m, extent(ecoregions_810m, x_810[i], x_810[i]+nrow_810, 1, ncols_810))
  
  init_comm_30 <- crop(initial_communities, extent(initial_communities, x_30[i], x_30[i]+nrow_30, 1, ncols_30))
  init_comm_90 <- crop(int_communities_90m, extent(int_communities_90m, x_90[i], x_90[i]+nrow_90, 1, ncols_90))
  init_comm_270 <- crop(int_communities_270m, extent(int_communities_270m, x_270[i], x_270[i]+nrow_270, 1, ncols_270))
  init_comm_810 <- crop(int_communities_810m, extent(int_communities_810m, x_810[i], x_810[i]+nrow_810, 1, ncols_810))
  
  init_infections_30 <- crop(init_infections, extent(init_infections, x_30[i], x_30[i]+nrow_30, 1, ncols_30))
  init_infections_90 <- crop(init_infections_90m, extent(init_infections_90m, x_90[i], x_90[i]+nrow_90, 1, ncols_90))
  init_infections_270 <- crop(init_infections_270m, extent(init_infections_270m, x_270[i], x_270[i]+nrow_270, 1, ncols_270))
  init_infections_810 <- crop(init_infections_810m, extent(init_infections_810m, x_810[i], x_810[i]+nrow_810, 1, ncols_810))
  
  writeRaster(x = ecoregions_30, filename = paste(output_dir2,'ecoregions_30.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  writeRaster(x = ecoregions_90, filename = paste(output_dir2,'ecoregions_90.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  writeRaster(x = ecoregions_270, filename = paste(output_dir2,'ecoregions_270.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  writeRaster(x = ecoregions_810, filename = paste(output_dir2,'ecoregions_810.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  
  writeRaster(x = init_comm_30, filename = paste(output_dir2,'init_comm_30.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  writeRaster(x = init_comm_90, filename = paste(output_dir2,'init_comm_90.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  writeRaster(x = init_comm_270, filename = paste(output_dir2,'init_comm_270.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  writeRaster(x = init_comm_810, filename = paste(output_dir2,'init_comm_810.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  
  writeRaster(x = init_infections_30, filename = paste(output_dir2,'init_infections_30.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT1U")
  writeRaster(x = init_infections_90, filename = paste(output_dir2,'init_infections_90.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT1U")
  writeRaster(x = init_infections_270, filename = paste(output_dir2,'init_infections_270.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT1U")
  writeRaster(x = init_infections_810, filename = paste(output_dir2,'init_infections_810.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT1U")
}

## Create random samples of 1/8th the extent of the study area
nrow_30 <- round(nrow(ecoregions)/8)
nrow_90 <- round(nrow(ecoregions_90m)/8)
nrow_270 <- round(nrow(ecoregions_270m)/8)
nrow_810 <- round(nrow(ecoregions_810m)/8)

ncols_30 <- ncol(ecoregions)
ncols_90 <- ncol(ecoregions_90m)
ncols_270 <- ncol(ecoregions_270m)
ncols_810 <- ncol(ecoregions_810m)

x_30 <- round(runif(10, 1, nrow(ecoregions) - nrow_30))
x_90 <- round(runif(10, 1, nrow(ecoregions_90m) - nrow_90))
x_270 <- round(runif(10, 1, nrow(ecoregions_270m) - nrow_270))
x_810 <- round(runif(10, 1, nrow(ecoregions_810m) - nrow_810))

output_dir <- "C:/Users/Chris/Desktop/landis_eda_res_ext_paper_data/eighth"
dir.create(output_dir)

for (i in 1:length(x)) {
  output_dir2 <- paste(output_dir,'/',i,sep = '')
  dir.create(output_dir2)
  ecoregions_30 <- crop(ecoregions, extent(ecoregions, x_30[i], x_30[i]+nrow_30, 1, ncols_30))
  ecoregions_90 <- crop(ecoregions_90m, extent(ecoregions_90m, x_90[i], x_90[i]+nrow_90, 1, ncols_90))
  ecoregions_270 <- crop(ecoregions_270m, extent(ecoregions_270m, x_270[i], x_270[i]+nrow_270, 1, ncols_270))
  ecoregions_810 <- crop(ecoregions_810m, extent(ecoregions_810m, x_810[i], x_810[i]+nrow_810, 1, ncols_810))
  
  init_comm_30 <- crop(initial_communities, extent(initial_communities, x_30[i], x_30[i]+nrow_30, 1, ncols_30))
  init_comm_90 <- crop(int_communities_90m, extent(int_communities_90m, x_90[i], x_90[i]+nrow_90, 1, ncols_90))
  init_comm_270 <- crop(int_communities_270m, extent(int_communities_270m, x_270[i], x_270[i]+nrow_270, 1, ncols_270))
  init_comm_810 <- crop(int_communities_810m, extent(int_communities_810m, x_810[i], x_810[i]+nrow_810, 1, ncols_810))
  
  init_infections_30 <- crop(init_infections, extent(init_infections, x_30[i], x_30[i]+nrow_30, 1, ncols_30))
  init_infections_90 <- crop(init_infections_90m, extent(init_infections_90m, x_90[i], x_90[i]+nrow_90, 1, ncols_90))
  init_infections_270 <- crop(init_infections_270m, extent(init_infections_270m, x_270[i], x_270[i]+nrow_270, 1, ncols_270))
  init_infections_810 <- crop(init_infections_810m, extent(init_infections_810m, x_810[i], x_810[i]+nrow_810, 1, ncols_810))
  
  writeRaster(x = ecoregions_30, filename = paste(output_dir2,'ecoregions_30.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  writeRaster(x = ecoregions_90, filename = paste(output_dir2,'ecoregions_90.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  writeRaster(x = ecoregions_270, filename = paste(output_dir2,'ecoregions_270.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  writeRaster(x = ecoregions_810, filename = paste(output_dir2,'ecoregions_810.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  
  writeRaster(x = init_comm_30, filename = paste(output_dir2,'init_comm_30.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  writeRaster(x = init_comm_90, filename = paste(output_dir2,'init_comm_90.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  writeRaster(x = init_comm_270, filename = paste(output_dir2,'init_comm_270.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  writeRaster(x = init_comm_810, filename = paste(output_dir2,'init_comm_810.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  
  writeRaster(x = init_infections_30, filename = paste(output_dir2,'init_infections_30.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT1U")
  writeRaster(x = init_infections_90, filename = paste(output_dir2,'init_infections_90.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT1U")
  writeRaster(x = init_infections_270, filename = paste(output_dir2,'init_infections_270.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT1U")
  writeRaster(x = init_infections_810, filename = paste(output_dir2,'init_infections_810.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT1U")
}


## Create random samples of 1/16th the extent of the study area
nrow_30 <- round(nrow(ecoregions)/16)
nrow_90 <- round(nrow(ecoregions_90m)/16)
nrow_270 <- round(nrow(ecoregions_270m)/16)
nrow_810 <- round(nrow(ecoregions_810m)/16)

ncols_30 <- ncol(ecoregions)
ncols_90 <- ncol(ecoregions_90m)
ncols_270 <- ncol(ecoregions_270m)
ncols_810 <- ncol(ecoregions_810m)

x_30 <- round(runif(10, 1, nrow(ecoregions) - nrow_30))
x_90 <- round(runif(10, 1, nrow(ecoregions_90m) - nrow_90))
x_270 <- round(runif(10, 1, nrow(ecoregions_270m) - nrow_270))
x_810 <- round(runif(10, 1, nrow(ecoregions_810m) - nrow_810))

output_dir <- "C:/Users/Chris/Desktop/landis_eda_res_ext_paper_data/sixtenth"
dir.create(output_dir)

for (i in 1:length(x)) {
  output_dir2 <- paste(output_dir,'/',i,sep = '')
  dir.create(output_dir2)
  ecoregions_30 <- crop(ecoregions, extent(ecoregions, x_30[i], x_30[i]+nrow_30, 1, ncols_30))
  ecoregions_90 <- crop(ecoregions_90m, extent(ecoregions_90m, x_90[i], x_90[i]+nrow_90, 1, ncols_90))
  ecoregions_270 <- crop(ecoregions_270m, extent(ecoregions_270m, x_270[i], x_270[i]+nrow_270, 1, ncols_270))
  ecoregions_810 <- crop(ecoregions_810m, extent(ecoregions_810m, x_810[i], x_810[i]+nrow_810, 1, ncols_810))
  
  init_comm_30 <- crop(initial_communities, extent(initial_communities, x_30[i], x_30[i]+nrow_30, 1, ncols_30))
  init_comm_90 <- crop(int_communities_90m, extent(int_communities_90m, x_90[i], x_90[i]+nrow_90, 1, ncols_90))
  init_comm_270 <- crop(int_communities_270m, extent(int_communities_270m, x_270[i], x_270[i]+nrow_270, 1, ncols_270))
  init_comm_810 <- crop(int_communities_810m, extent(int_communities_810m, x_810[i], x_810[i]+nrow_810, 1, ncols_810))
  
  init_infections_30 <- crop(init_infections, extent(init_infections, x_30[i], x_30[i]+nrow_30, 1, ncols_30))
  init_infections_90 <- crop(init_infections_90m, extent(init_infections_90m, x_90[i], x_90[i]+nrow_90, 1, ncols_90))
  init_infections_270 <- crop(init_infections_270m, extent(init_infections_270m, x_270[i], x_270[i]+nrow_270, 1, ncols_270))
  init_infections_810 <- crop(init_infections_810m, extent(init_infections_810m, x_810[i], x_810[i]+nrow_810, 1, ncols_810))
  
  writeRaster(x = ecoregions_30, filename = paste(output_dir2,'ecoregions_30.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  writeRaster(x = ecoregions_90, filename = paste(output_dir2,'ecoregions_90.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  writeRaster(x = ecoregions_270, filename = paste(output_dir2,'ecoregions_270.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  writeRaster(x = ecoregions_810, filename = paste(output_dir2,'ecoregions_810.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  
  writeRaster(x = init_comm_30, filename = paste(output_dir2,'init_comm_30.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  writeRaster(x = init_comm_90, filename = paste(output_dir2,'init_comm_90.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  writeRaster(x = init_comm_270, filename = paste(output_dir2,'init_comm_270.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  writeRaster(x = init_comm_810, filename = paste(output_dir2,'init_comm_810.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT4S")
  
  writeRaster(x = init_infections_30, filename = paste(output_dir2,'init_infections_30.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT1U")
  writeRaster(x = init_infections_90, filename = paste(output_dir2,'init_infections_90.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT1U")
  writeRaster(x = init_infections_270, filename = paste(output_dir2,'init_infections_270.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT1U")
  writeRaster(x = init_infections_810, filename = paste(output_dir2,'init_infections_810.tif',sep = '/'), overwrite=TRUE, format = 'GTiff', datatype = "INT1U")
}
