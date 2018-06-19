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




## Decided to use majority rule based on original resolution data (e.g. method 1 which was assumed to be the preferred method from the start)
writeRaster(ecoregions_90m, )