# Collation script for trajectory data used in the Minjerribah and K'gari hysplit analysis

# Matt Harris, GNS Science
# Last updated 2024-09-10
# Contact: m.harris@gns.cri.nz
# github.com/MRPHarris

# Written and run in:
#   Rstudio v2024.04.2
#   R v4.4.0

##### SETUP (Always Run) #####

## Section notes
# Loading of packages, setting of directories.

## Load pacman for pacman::p_load() 
library(pacman)

## Load core packages. 
pacman::p_load(splitr, openair, lubridate, magrittr, tibble, dplyr, R.utils, ggplot2, stringr,here)

## poorly cleaned list of extra packages. These may or may not be required.
# pacman::p_load(splitr, openair, lubridate, magrittr, tibble, dplyr, R.utils, raster, rgdal, mapproj, rgdal, ggplot2, rgeos, maps, readr, RColorBrewer, 
#                gganimate, gifski, magick, png, transformr, viridis, marmap, ggmap, ggspatial, cowplot, mapdata, ggrepel, rnaturalearth, sf) # gif/gganimate packages

## Set directories
# Data directory; data is imported from here.
data_directory_reanalysis <- paste0(export_dir,"data/reanalysis/")
data_directory_gdas1 <- paste0(export_dir,"data/GDAS1/")
# Export directory: outputs will be sent here. Currently just a here() call.
export_dir <- paste0(here(),"/")
# export_dir <- "E:/mh work live 2024-06-23/JTibby via HCadd HYSPLIT/"
wd <- export_dir
setwd(wd)

# Site latitudes and longitudes
# Sand Cape, Fraser Island
SC_lat <- -24.73
SC_lon <- 153.21
# Point Lookout, North Stradbroke Island
PL_lat <- -27.44
PL_lon <- 153.55

##### Trajectory data wrangling (creation of data frames for subsequent analysis) #####

## Section notes
# Combining the data frames that will be used for analysis in subsequent sections.

##### == PRE-ANALYSIS: Compiling trajectory data = #####

## The code below was used to compile the original trajectory datasets.


### REANALYSIS 1950:1999 ###
# Required functions
Add_traj_identifier <- function(x, ntraj_1 = FALSE){
  # This function assumes that convert_openair() has been used on the dataset. 
  x_new <- as.data.frame(x)
  # Compute trajectory number. Starts at 1. 
  if(!isTRUE(ntraj_1)){
    x_new$start.time.minutes <- substr(x_new[,12],12,19)
    x_new$start.time.minutes <- 60*24*as.numeric(chron::times(x_new$start.time.minutes))
    x_new$trajectory <- cumsum(c(0, as.numeric(diff(x_new$start.time.minutes)) != 0)) + 1
  } else {
    x_new$diffs <- 1
    x_new$diffs[2:nrow(x_new)] <- diff(x_new$hour.inc) 
    x_new$trajectory <- cumsum(c(0,as.numeric(x_new$diffs[2:nrow(x_new)]) != -1)) + 1
  }
  #  x_new <<- as.data.frame(x)
  x_new
  #  rm(x_new, envir = parent.frame())
}
# adj longitude to adjust West (negative) values.
adjust_longitude <- function(df){
  df_new <- df
  adj180_index <- df_new$lon < 0
  df_new$lon[adj180_index] <- (180 + 180-abs(df_new$lon[adj180_index]))
  df_new
}

## Importing (ONLY RUN IF RE-TARGETING RAW DATA. IF NOT, IMPORT COLLATED .CSVs)
# Load loop for SAR 1 to 7

# This could absolutely be made more efficient but as it's only a once or twice-off, I'm going to leave the brute-force approach.
fixyear <- function(trajdata,
                    wrong_years = seq(2049,2099,1),
                    replacement_years = seq(1949,1999,1)){
  ## vars for intra-function testing
  # trajdata = trajfile
  # wrong_years = c(2049,2050)
  # replacement_years = c(1949,1950)
  ## Simple loop with gsub
  yit_list = vector('list',length(wrong_years))
  for(y in seq_along(wrong_years)){
    # Fix each.
    # gsub year column
    trajdata$year[which(trajdata$year == wrong_years[y])] <- replacement_years[y]
    # gsub date.inc column
    trajdata$date.inc <- gsub(as.character(wrong_years[y]),as.character(replacement_years[y]),trajdata$date.inc)
    # trajdata$date.inc <- gsub("2050","1950",trajdata$date.inc)
  }
  return(trajdata)
}

## Importing (ONLY RUN IF RE-TARGETING RAW DATA. IF NOT, IMPORT COLLATED .CSVs)
site_names <- c("SC","PL")
years <- seq(1950,2022,1)
sites_list <- vector(mode = "list", length = 2) %>% 'names<-'(c(site_names))
years_it <- vector(mode = "list", length = length(years))
# dates_run <- c("2020-11-27","2020-11-28",
#                "2020-12-17","2020-12-18",
#                "2024-08-26","2024-08-27")
filename_years_list <- vector(mode = 'list', length = length(years))
filename_years_list[c(1:length(years))] <- years
# Get list of all files in the folder.
target_file_list_short <- list.files(data_directory_reanalysis)
target_file_list_long <- list.files(data_directory_reanalysis, full.names = T)
# Loop
for(i in seq_along(sites_list)){
  i = 2
  this_site = site_names[i]
  # Find files that start with this string
  filestr <- unlist(lapply(strsplit(target_file_list_short,"-"),"[[",1))
  files_this_it <- which(filestr == this_site)
  # Ok, now loop through these files
  file_it_list <- vector('list', length = length(files_this_it))
  for(f in seq_along(file_it_list)){
    # f = 1
    message("Importing ",target_file_list_short[files_this_it[f]])
    # Ok, now dealing with individual files.
    trajfile <- read.csv(target_file_list_long[f], header = TRUE)
    # In some instances the year and date.inc vars show wrong years. Notably 1949 always gets displayed as 2049.
    # Applied on a bulk basis just in case doing it on the final frame would cause problems
    trajfile <- fixyear(trajdata = trajfile) 
    # Get rid of a column to save some space
    trajfile <- subset(trajfile,select = -c(trajectory,diffs))
    # Assign to list
    file_it_list[[f]] <- trajfile
  }
  # bind frame
  message("Combining and exporting data. This may take a moment.")
  traj_frame_bound <- rlist::list.rbind(file_it_list)
  traj_frame_bound <- adjust_longitude(traj_frame_bound)
  traj_frame_bound <- Add_traj_identifier(traj_frame_bound, ntraj_1 = TRUE)
  traj_frame_bound <- subset(traj_frame_bound, select = -c(receptor,pressure,date.inc,diffs,date.inc))
  # assign(paste0(site_names[i],"_72hr1TPD2000m_Reanalysis_1950_2022"),frame_tobind, envir = parent.frame())
  write.csv(traj_frame_bound,paste0(export_dir,site_names[i],"_72hr1TPD2000m_Reanalysis_1950_2022.csv"), row.names = FALSE)
  rm(traj_frame_bound)
  rm(file_it_list)
  gc()
  message("...","\n",site_names[i]," compiled and exported")
}
