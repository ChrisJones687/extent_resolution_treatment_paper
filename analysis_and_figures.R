## Data Analysis and Figures for paper
# Load in required libraries
library(sp)
library(raster)
library(rgdal)
library(data.table)
library(ggplot2)

# Create Directory and list of files with all log files from extent resolution runs
directories_full = list.dirs("C:/Users/Chris/Google Drive/landis_extent_resolution_runs/full", full.names = TRUE)
directories_half = list.dirs("C:/Users/Chris/Google Drive/landis_extent_resolution_runs/half", full.names = TRUE)
directories_quarter = list.dirs("C:/Users/Chris/Google Drive/landis_extent_resolution_runs/quarter", full.names = TRUE)
directories_eighth = list.dirs("C:/Users/Chris/Google Drive/landis_extent_resolution_runs/eighth", full.names = TRUE)
directories_sixteenth = list.dirs("C:/Users/Chris/Google Drive/landis_extent_resolution_runs/sixtenth", full.names = TRUE)

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
  eda_sixteenth_list[[i]]$resolution <- resolution_list[[i]]
  eda_sixteenth_list[[i]]$X <- 0
  eda_sixteenth_list[[i]]$AgentName <- 0
}

eda_mean <- as.data.frame(rbindlist(eda_sixteenth_list)[,lapply(.SD,mean), list(Time, resolution)])
eda_sd <- as.data.frame(rbindlist(eda_sixteenth_list)[,lapply(.SD,sd), list(Time, resolution)])

title = "Diseased area over time"
theme = theme_set(theme_minimal())
theme = theme_update(legend.position="top", legend.title=element_blank())
theme = theme_update(axis.text = element_text(colour="black"), axis.ticks=element_blank(), plot.title = element_text(hjust = 0.5), axis.line = element_line())
plot = ggplot(eda_mean, aes(Time, diseased_area, color=factor(resolution)))+geom_line(aes(color = factor(resolution)), size=1)
#plot = plot+scale_color_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))+scale_fill_manual(values=c("#CC0000","#7BAFD4", "#CD660D"))
plot = plot+ggtitle(title)
plot = plot + theme(axis.text=element_text(size=10),axis.title=element_text(size=16,vjust=0.35),legend.text=element_text(size=12),plot.title=element_text(size=22))
plot = plot + scale_x_continuous(name="Date", breaks=seq(2000,2100,20))
plot = plot + scale_y_continuous(name=expression("Diseased area (m^2"))

eda_mean$min_diseased_area <- eda_mean$diseased_area-(eda_sd$diseased_area/7)
eda_mean$max_diseased_area <- eda_mean$diseased_area+(eda_sd$diseased_area/7)

plot = plot +geom_ribbon(data = eda_mean, aes(ymin=min_diseased_area, ymax=max_diseased_area, fill = factor(resolution), color = factor(resolution)), alpha=0.3)

#plot= plot+theme(legend.position="none")
plot
