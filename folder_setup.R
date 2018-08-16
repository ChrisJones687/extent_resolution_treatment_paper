## Set up Folder with subfolders for all of the data to be stored
setwd("C:/Users/Chris/Desktop")
new_dir <- "landis_extent_resolution_runs"
dir.create(new_dir)
dir_extent_list <- c("full", "half", "quarter", "eigth", "sixtenth")
dir_resolution_list <- c("30m", "90m", "270m", "810m")
dir_runs_list <- seq(1,10,1)

setwd("C:/Users/Chris/Desktop/landis_extent_resolution_runs")
## set up folders and copy all files for set up into them
for(ext in 1:length(dir_extent_list)){
  dir.create(dir_extent_list[ext])
  for(res in 1:length(dir_resolution_list)){
    copy_folder <- paste("C:/Users/Chris/Desktop/bigsur_eda_",dir_resolution_list[res], sep="")
    for (runs in 1:length(dir_runs_list)){
      new_folder <- paste(dir_extent_list[ext],"/","bigsur_eda_",dir_resolution_list[res],"_",dir_runs_list[runs], sep="")
      dir.create(new_folder)
      file.copy(list.files(copy_folder, full.names = TRUE),new_folder, overwrite = TRUE, recursive = TRUE)
      
    }
  }
}

dir_extent_list <- c("half", "quarter", "eighth", "sixtenth")
dir_resolution_list <- c("30", "90", "270", "810")
dir_runs_list <- seq(1,10,1)
## move ecoregions, initial infections and initial communities files to new folders
for(ext in 1:length(dir_extent_list)){
  for(res in 1:length(dir_resolution_list)){
    for (runs in 1:length(dir_runs_list)){
      to_folder <- paste(dir_extent_list[ext],"/","bigsur_eda_",dir_resolution_list[res],"m_",dir_runs_list[runs], sep="")
      eco <- paste("C:/Users/Chris/Desktop/landis_eda_res_ext_paper_data/",dir_extent_list[ext],"/",dir_runs_list[runs],"/ecoregions_",dir_resolution_list[res],".tif", sep="")
      init_inf <- paste("C:/Users/Chris/Desktop/landis_eda_res_ext_paper_data/",dir_extent_list[ext],"/",dir_runs_list[runs],"/init_infections_",dir_resolution_list[res],".tif", sep="")
      init_comm <- paste("C:/Users/Chris/Desktop/landis_eda_res_ext_paper_data/",dir_extent_list[ext],"/",dir_runs_list[runs],"/init_comm_",dir_resolution_list[res],".tif", sep="")
      file.copy(eco, to_folder, overwrite = TRUE)
      file.copy(init_inf, to_folder, overwrite = TRUE)
      file.copy(init_comm, to_folder, overwrite = TRUE)

    }
  }
}

