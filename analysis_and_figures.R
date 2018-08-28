## Data Analysis and Figures for paper
# Load in required libraries
library(sp)
library(raster)
library(rgdal)
library(data.table)
library(ggplot2)

# Create Directory and list of files with all log files from extent resolution runs
directories_full = list.dirs("D:/landis_extent_resolution_runs/full", full.names = TRUE)
directories_half = list.dirs("D:/landis_extent_resolution_runs/half", full.names = TRUE)
directories_quarter = list.dirs("D:/landis_extent_resolution_runs/quarter", full.names = TRUE)
directories_eighth = list.dirs("D:/landis_extent_resolution_runs/eighth", full.names = TRUE)
directories_sixteenth = list.dirs("D:/landis_extent_resolution_runs/sixtenth", full.names = TRUE)

size_full = ncell(raster("D:/landis_extent_resolution_runs/full/bigsur_eda_30m_1/ecoregions_30.tif"))*30*30
size_half = ncell(raster("D:/landis_extent_resolution_runs/half/bigsur_eda_30m_1/ecoregions_30.tif"))*30*30
size_quarter = ncell(raster("D:/landis_extent_resolution_runs/quarter/bigsur_eda_30m_1/ecoregions_30.tif"))*30*30
size_eighth = ncell(raster("D:/landis_extent_resolution_runs/eighth/bigsur_eda_30m_1/ecoregions_30.tif"))*30*30
size_sixteenth = ncell(raster("D:/landis_extent_resolution_runs/sixtenth/bigsur_eda_30m_1/ecoregions_30.tif"))*30*30


files_full_eda = list.files(path= directories_full, pattern="eda-log.csv", full.names = TRUE)
files_half_eda = list.files(path= directories_half, pattern="eda-log.csv", full.names = TRUE)
files_quarter_eda = list.files(path= directories_quarter, pattern="eda-log.csv", full.names = TRUE)
files_eighth_eda = list.files(path= directories_eighth, pattern="eda-log.csv", full.names = TRUE)
files_sixteenth_eda = list.files(path= directories_sixteenth, pattern="eda-log.csv", full.names = TRUE)

# Read in data frames into a lists for looping
eda_full_list <-lapply(files_full_eda, read.csv) #List for storing data frames from eda model runs at different resolutins
eda_half_list <-lapply(files_half_eda, read.csv) #List for storing data frames from eda model runs at different resolutins
eda_quarter_list <-lapply(files_quarter_eda, read.csv) #List for storing data frames from eda model runs at different resolutins
eda_eighth_list <-lapply(files_eighth_eda, read.csv) #LList for storing data frames from eda model runs at different resolutins
eda_sixteenth_list <-lapply(files_sixteenth_eda, read.csv) #List for storing data frames from eda model runs at different resolutins

area_list <- c(rep(270*270,10), rep(30*30,10), rep(810*810,10), rep(90*90,10))
resolution_list <- c(rep(270,10), rep(30,10), rep(810,10), rep(90,10))
## calculate total diseased area (number of diseased sites isn't comparable across resolutions)
for (i in 1:length(eda_sixteenth_list)) {
  eda_sixteenth_list[[i]]$diseased_area <- eda_sixteenth_list[[i]]$DiseasedSites*area_list[[i]]/(1*10^6)
  eda_sixteenth_list[[i]]$percent_diseased_area <- eda_sixteenth_list[[i]]$DiseasedSites*area_list[[i]]/size_sixteenth
  eda_sixteenth_list[[i]]$resolution <- resolution_list[[i]]
  eda_sixteenth_list[[i]]$X <- 0
  eda_sixteenth_list[[i]]$AgentName <- 0
  
  eda_eighth_list[[i]]$diseased_area <- eda_eighth_list[[i]]$DiseasedSites*area_list[[i]]/(1*10^6)
  eda_eighth_list[[i]]$percent_diseased_area <- eda_eighth_list[[i]]$DiseasedSites*area_list[[i]]/size_eighth
  eda_eighth_list[[i]]$resolution <- resolution_list[[i]]
  eda_eighth_list[[i]]$X <- 0
  eda_eighth_list[[i]]$AgentName <- 0
  
  eda_quarter_list[[i]]$diseased_area <- eda_quarter_list[[i]]$DiseasedSites*area_list[[i]]/(1*10^6)
  eda_quarter_list[[i]]$percent_diseased_area <- eda_quarter_list[[i]]$DiseasedSites*area_list[[i]]/size_quarter
  eda_quarter_list[[i]]$resolution <- resolution_list[[i]]
  eda_quarter_list[[i]]$X <- 0
  eda_quarter_list[[i]]$AgentName <- 0
  
  eda_half_list[[i]]$diseased_area <- eda_half_list[[i]]$DiseasedSites*area_list[[i]]/(1*10^6)
  eda_half_list[[i]]$percent_diseased_area <- eda_half_list[[i]]$DiseasedSites*area_list[[i]]/size_half
  eda_half_list[[i]]$resolution <- resolution_list[[i]]
  eda_half_list[[i]]$X <- 0
  eda_half_list[[i]]$AgentName <- 0
  
  eda_full_list[[i]]$diseased_area <- eda_full_list[[i]]$DiseasedSites*area_list[[i]]/(1*10^6)
  eda_full_list[[i]]$percent_diseased_area <- eda_full_list[[i]]$DiseasedSites*area_list[[i]]/size_full
  eda_full_list[[i]]$resolution <- resolution_list[[i]]
  eda_full_list[[i]]$X <- 0
  eda_full_list[[i]]$AgentName <- 0
}

eda_mean_sixteenth <- as.data.frame(rbindlist(eda_sixteenth_list)[,lapply(.SD,mean), list(Time, resolution)])
eda_sd_sixteenth <- as.data.frame(rbindlist(eda_sixteenth_list)[,lapply(.SD,sd), list(Time, resolution)])
eda_mean_sixteenth$extent <- 0.0625

eda_mean_eighth <- as.data.frame(rbindlist(eda_eighth_list)[,lapply(.SD,mean), list(Time, resolution)])
eda_sd_eighth <- as.data.frame(rbindlist(eda_eighth_list)[,lapply(.SD,sd), list(Time, resolution)])
eda_mean_eighth$extent <- 0.125

eda_mean_quarter <- as.data.frame(rbindlist(eda_quarter_list)[,lapply(.SD,mean), list(Time, resolution)])
eda_sd_quarter <- as.data.frame(rbindlist(eda_quarter_list)[,lapply(.SD,sd), list(Time, resolution)])
eda_mean_quarter$extent <- 0.25

eda_mean_half <- as.data.frame(rbindlist(eda_half_list)[,lapply(.SD,mean), list(Time, resolution)])
eda_sd_half <- as.data.frame(rbindlist(eda_half_list)[,lapply(.SD,sd), list(Time, resolution)])
eda_mean_half$extent <- 0.5

eda_mean_full <- as.data.frame(rbindlist(eda_full_list)[,lapply(.SD,mean), list(Time, resolution)])
eda_sd_full <- as.data.frame(rbindlist(eda_full_list)[,lapply(.SD,sd), list(Time, resolution)])
eda_mean_full$extent <- 1

eda_mean <- rbind(eda_mean_sixteenth,eda_mean_eighth,eda_mean_quarter,eda_mean_half, eda_mean_full)
eda_sd <- rbind(eda_sd_sixteenth,eda_sd_eighth,eda_sd_quarter,eda_sd_half, eda_sd_full)

eda_mean$min_percent_diseased_area <- eda_mean$percent_diseased_area-(eda_sd$percent_diseased_area/7)
eda_mean$max_percent_diseased_area <- eda_mean$percent_diseased_area+(eda_sd$percent_diseased_area/7)


title = "Diseased area over time"
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top", legend.title=element_blank())
theme = theme_update(axis.text = element_text(colour="black"), axis.ticks=element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line())
plot = ggplot(eda_mean, aes(Time, percent_diseased_area, color=factor(resolution), shape = factor(extent)))+geom_line(aes(color = factor(resolution), linetype = factor(extent)), size=1)
#plot = plot+scale_color_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))+scale_fill_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))
plot = plot+ggtitle(title)
plot = plot + theme(axis.text=element_text(size=10),axis.title=element_text(size=16,vjust=0.35),legend.text=element_text(size=12),plot.title=element_text(size=22))
plot = plot + scale_x_continuous(name="Date", breaks=seq(2000,2100,20))
plot = plot + scale_y_continuous(name=expression("Diseased area (m^2"))
plot = plot +geom_ribbon(data = eda_mean, aes(ymin=min_percent_diseased_area, ymax=max_percent_diseased_area, fill = factor(resolution), color = factor(resolution)), alpha=0.3)
#plot= plot+theme(legend.position="none")
plot


title = "Diseased area over time"
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top", legend.title=element_blank())
theme = theme_update(axis.text = element_text(colour="black"), axis.ticks=element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line())
plot = ggplot(eda_mean_quarter, aes(Time, diseased_area, color=factor(resolution), shape = factor(extent)))+geom_line(aes(color = factor(resolution), linetype = factor(extent)), size=1)
#plot = plot+scale_color_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))+scale_fill_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))
plot = plot+ggtitle(title)
plot = plot + theme(axis.text=element_text(size=10),axis.title=element_text(size=16,vjust=0.35),legend.text=element_text(size=12),plot.title=element_text(size=22))
plot = plot + scale_x_continuous(name="Date", breaks=seq(2000,2100,20))
plot = plot + scale_y_continuous(name=expression("Diseased area (m^2"))
plot = plot +geom_ribbon(data = eda_mean_quarter, aes(ymin=min_diseased_area, ymax=max_diseased_area, fill = factor(resolution), color = factor(resolution)), alpha=0.3)
#plot= plot+theme(legend.position="none")
plot

## Comparing ecoregions across the different resolutions
ecoregion_30 = raster("D:/landis_extent_resolution_runs/full/bigsur_eda_30m_1/ecoregions_30.tif")
ecoregion_90 = raster("D:/landis_extent_resolution_runs/full/bigsur_eda_90m_1/ecoregions_90.tif")
ecoregion_270 = raster("D:/landis_extent_resolution_runs/full/bigsur_eda_270m_1/ecoregions_270.tif")
ecoregion_810 = raster("D:/landis_extent_resolution_runs/full/bigsur_eda_810m_1/ecoregions_810.tif")

ecoregion_30m <- freq(ecoregion_30)
ecoregion_90m <- freq(ecoregion_90)
ecoregion_270m <- freq(ecoregion_270)
ecoregion_810m <- freq(ecoregion_810)

ecoregions_df <- data.frame(class = ecoregion_30m[2:nrow(ecoregion_30m),1], precent_30m = round(ecoregion_30m[2:nrow(ecoregion_30m),2]/sum(ecoregion_30m[2:nrow(ecoregion_30m),2])*100, 2))
ecoregions_df$precent_90m = round(ecoregion_90m[2:nrow(ecoregion_90m),2]/sum(ecoregion_90m[2:nrow(ecoregion_90m),2])*100, 2)
ecoregions_df$precent_270m = 0
ecoregions_df$precent_270m[ecoregions_df$class %in% ecoregion_270m[,1]] = round(ecoregion_270m[2:nrow(ecoregion_270m),2]/sum(ecoregion_270m[2:nrow(ecoregion_270m),2])*100, 2)
ecoregions_df$precent_810m = 0
ecoregions_df$precent_810m[ecoregions_df$class %in% ecoregion_810m[,1]] = round(ecoregion_810m[2:nrow(ecoregion_810m),2]/sum(ecoregion_810m[2:nrow(ecoregion_810m),2])*100, 2)

#add plot

## comparing initial communities across different resolutions
initial_communities_30 = raster("D:/landis_extent_resolution_runs/full/bigsur_eda_30m_1/init_comm_30.tif")
initial_communities_90 = raster("D:/landis_extent_resolution_runs/full/bigsur_eda_90m_1/init_comm_90.tif")
initial_communities_270 = raster("D:/landis_extent_resolution_runs/full/bigsur_eda_270m_1/init_comm_270.tif")
initial_communities_810 = raster("D:/landis_extent_resolution_runs/full/bigsur_eda_810m_1/init_comm_810.tif")

initial_communities_30m <- freq(initial_communities_30)
initial_communities_90m <- freq(initial_communities_90)
initial_communities_270m <- freq(initial_communities_270)
initial_communities_810m <- freq(initial_communities_810)

initial_communitiess_df <- data.frame(class = initial_communities_30m[3:nrow(initial_communities_30m),1], precent_30m = round((initial_communities_30m[3:nrow(initial_communities_30m),2]/sum(initial_communities_30m[3:nrow(initial_communities_30m),2]))*100, 2))
initial_communitiess_df$precent_90m = 0
initial_communitiess_df$precent_90m[initial_communitiess_df$class %in% initial_communities_90m[,1]] = round(initial_communities_90m[3:nrow(initial_communities_90m),2]/sum(initial_communities_90m[3:nrow(initial_communities_90m),2])*100, 2)
initial_communitiess_df$precent_270m = 0
initial_communitiess_df$precent_270m[initial_communitiess_df$class %in% initial_communities_270m[,1]] = round(initial_communities_270m[3:nrow(initial_communities_270m),2]/sum(initial_communities_270m[3:nrow(initial_communities_270m),2])*100, 2)
initial_communitiess_df$precent_810m = 0
initial_communitiess_df$precent_810m[initial_communitiess_df$class %in% initial_communities_810m[,1]] = round(initial_communities_810m[3:nrow(initial_communities_810m),2]/sum(initial_communities_810m[3:nrow(initial_communities_810m),2])*100, 2)
initial_communitiess_df <- initial_communitiess_df[initial_communitiess_df$precent_30m > 0.05,]

## comparing species composition across different resolutions at the full extent.
species_composition_30 <- read.csv("D:/landis_extent_resolution_runs/full/bigsur_eda_30m_1/spp-biomass-log.csv")
species_composition_90 <- read.csv("D:/landis_extent_resolution_runs/full/bigsur_eda_90m_1/spp-biomass-log.csv")
species_composition_270 <- read.csv("D:/landis_extent_resolution_runs/full/bigsur_eda_270m_1/spp-biomass-log.csv")
species_composition_810 <- read.csv("D:/landis_extent_resolution_runs/full/bigsur_eda_810m_2/spp-biomass-log.csv")

species_composition_30[is.na(species_composition_30)] <- 0
species_composition_90[is.na(species_composition_90)] <- 0
species_composition_270[is.na(species_composition_270)] <- 0
species_composition_810[is.na(species_composition_810)] <- 0

species_composition_30 <- species_composition_30[ , c(1,3:33)]
species_composition_90 <- species_composition_90[ , c(1,3:33)]
species_composition_270 <- species_composition_270[ , c(1,3:33)]
species_composition_810 <- species_composition_810[ , c(1,3:33)]

species_composition_30m <- setDT(species_composition_30)[,lapply(.SD,sum), by = Time]
species_composition_90m <- setDT(species_composition_90)[,lapply(.SD,sum), by = Time]
species_composition_270m <- setDT(species_composition_270)[,lapply(.SD,sum), by = Time]
species_composition_810m <- setDT(species_composition_810)[,lapply(.SD,sum), by = Time]

species_composition_30m$resolution <- 30                                 
species_composition_90m$resolution <- 90
species_composition_270m$resolution <- 270
species_composition_810m$resolution <- 810

species_composition <- rbind(species_composition_30m, species_composition_90m, species_composition_270m, species_composition_810m)
for (i in 1:nrow(species_composition)) {
  species_composition[i,4:31] <- species_composition[i,4:31]/sum(species_composition[i,4:31])*100
}

title = "Species Composition over time"
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top", legend.title=element_blank())
theme = theme_update(axis.text = element_text(colour="black"), axis.ticks=element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line())
plot = ggplot(species_composition, aes(Time, SppBiomass_Queragri, color=factor(resolution)))+geom_line(aes(color = factor(resolution)))
#plot = plot+scale_color_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))+scale_fill_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))
plot = plot+ggtitle(title)
plot = plot + theme(axis.text=element_text(size=10),axis.title=element_text(size=16,vjust=0.35),legend.text=element_text(size=12),plot.title=element_text(size=22))
plot = plot + scale_x_continuous(name="Date", breaks=seq(2000,2100,20))
plot = plot + scale_y_continuous(name=expression("Diseased area (m^2"))
plot

