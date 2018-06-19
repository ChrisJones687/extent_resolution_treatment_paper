library(raster)
library(rgdal)

initial_communities <- raster("C:\\Users\\Chris\\Desktop\\chrisjonesLandis\\LANDIS_II_small\\BigSur_EDA\\initialcommunitiesmap.img")
ecoregions <- raster("C:\\Users\\Chris\\Desktop\\chrisjonesLandis\\LANDIS_II_small\\BigSur_EDA\\ecoregionsmap.img")


rna <- r 
rna[rna==0] <- NA
m <- aggregate(r, fact = 3, fun = modal, na.rm = TRUE)
m_2 <- aggregate(rna, fact = 3, fun = modal, na.rm = TRUE)
m2 <- aggregate (r, fact = 9, fun =modal, na.rm = TRUE)
m2_2 <- aggregate(m_2, fact = 3, fun = modal, na.rm = TRUE)
m3 <- aggregate (r, fact = 27, fun =modal, na.rm = TRUE)
m3_2 <- aggregate(m2_2, fact = 3, fun = modal, na.rm = TRUE)
plot(r)
plot(m)
plot(m_2)
plot(m2_2)
plot(m3_2)
plot(m3)
