## Data Analysis and Figures for paper
# Load in required libraries
library(sp)
library(raster)
library(rgdal)

# Create Directory and list of files with all log files from extent resolution runs
directories_full = list.dirs("C:/Users/Chris/Google Drive/landis_extent_resolution_runs/full", full.names = TRUE)
directories_half = list.dirs("C:/Users/Chris/Google Drive/landis_extent_resolution_runs/half", full.names = TRUE)
directories_quarter = list.dirs("C:/Users/Chris/Google Drive/landis_extent_resolution_runs/quarter", full.names = TRUE)
directories_eighth = list.dirs("C:/Users/Chris/Google Drive/landis_extent_resolution_runs/eighth", full.names = TRUE)
directories_sixteenth = list.dirs("C:/Users/Chris/Google Drive/landis_extent_resolution_runs/sixtenth", full.names = TRUE)

dirs_sixteenth_eda = directories_sixteenth[seq(3,(length(directories_sixteenth)-6),10)]

files = list.files(path= dirsf, pattern="log.csv", full.names = TRUE)