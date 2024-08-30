# Comparing rainfall-biased airmass patterns at two sites in south eastern QLD #

# Matt Harris, Keele University
# Last updated 25/03/21
# Contact: m.r.p.harris@keele.ac.uk

# Written and run in:
#   Rstudio v1.3.959
#   R v4.0.2
#   Windows 10 (Education) 

##### NOTES  #####

# R: https://www.r-project.org/
# Rstudio: https://rstudio.com/products/rstudio/download/
# HYSPLIT: https://www.ready.noaa.gov/HYSPLIT.php

## Package manuals:
#     SplitR: https://rdrr.io/github/rich-iannone/SplitR/

##### SETUP (Always Run) #####

# Required packages

# Notes on setup
# Nothing to see here
# Load pacman for pacman::p_load() 
library(pacman)

# Load core packages. 
pacman::p_load(splitr, openair, lubridate, magrittr, tibble, dplyr, R.utils, ggplot2, stringr)

# poorly cleaned list of extra packages. These may or may not be required.
# pacman::p_load(splitr, openair, lubridate, magrittr, tibble, dplyr, R.utils, raster, rgdal, mapproj, rgdal, ggplot2, rgeos, maps, readr, RColorBrewer, 
#                gganimate, gifski, magick, png, transformr, viridis, marmap, ggmap, ggspatial, cowplot, mapdata, ggrepel, rnaturalearth, sf) # gif/gganimate packages


# Set directories
export_dir <- "E:/mh work live 2024-06-23/JTibby via HCadd HYSPLIT/"
data_directory_reanalysis <- paste0(export_dir,"data/reanalysis/")
data_directory_gdas1 <- paste0(export_dir,"data/GDAS1/")
wd <- export_dir
setwd(wd)
# Set the directory where meteorological data is stored. Update as needed.
met_dir_g_main <- "G:/DATA/HYSPLIT Files/GDAS1 Meteorological Data/"

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

### GDAS1 2005:2020
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

fixyear <- function(trajdata,
                    problem_dateinc_years = c(1950,1951)){}


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
site_names <- c("SC","PL")
years <- seq(1950,2022,1)
sites_it <- vector(mode = "list", length = 2)
years_it <- vector(mode = "list", length = length(years))
# dates_run <- c("2020-11-27","2020-11-28",
#                "2020-12-17","2020-12-18",
#                "2024-08-26","2024-08-27")
filename_years_list <- vector(mode = 'list', length = length(years))
filename_years_list[c(1:length(years))] <- years
# dates run
# datesrun_list_PL <- vector(mode = 'list', length = length(years))
# datesrun_list_PL[c(1:16)] <- dates_run[1]
# datesrun_list_PL[c(17:50)] <- dates_run[2]
# datesrun_list_SC <- vector(mode = 'list', length = length(years))
# datesrun_list_SC[c(1:31)] <- dates_run[3]
# datesrun_list_SC[c(32:50)] <- dates_run[4]
# Loop will import and combine trajectory data.

# Revising this loop because it currently makes no sense
# Get list of all files in the folder.
target_file_list_short <- list.files(data_directory_reanalysis)
target_file_list_long <- list.files(data_directory_reanalysis, full.names = T)
for(i in seq_along(sites_it)){
  i = 1
  #
  this_site = site_names[i]
  # Find files that start with this string
  filestr <- unlist(lapply(strsplit(target_file_list_short,"-"),"[[",1))
  files_this_it <- which(filestr == this_site)
  # Ok, now loop through these files
  file_it_list <- vector('list', length = length(files_this_it))
  for(f in seq_along(file_it_list)){
    f = 1
    # year issues
    # 1949 becomes 2049 (both year and date.inc)
    # 1950 becomes 2050 (only  year, not date.inc)
    # 1951
    
    message("Importing ",target_file_list_short[files_this_it[f]])
    # Ok, now dealing with individual files.
    trajfile <- read.csv(target_file_list_long[f], header = TRUE)
    ## FIXING YEAR BUG
    # In some instances the year and date.inc vars show wrong years. Notably 1949 always gets displayed as 2049.
    YSH_run_year
    
    
    ## Check for bugged year variable
    trajfile_year <- as.character(trajfile$year[1])
    trajfile_year_dateinc <- format(as.Date(trajfile$date.inc[1], format="%Y-%m-%d"),"%Y")
     # This doesn't fix it. Figure out which years are affected and only apply it to those files (by detecting from file name).
    if(trajfile_year != trajfile_year_dateinc){
      message("Year bug. Fixing.")
      trajfile$year = as.numeric(format(as.Date(trajfile$date.inc, format="%Y-%m-%d"),"%Y"))
    }
    
    if(as.numeric(substr(trajfile$year[1], 1, 1)) == 2){
      # This means the year is busted. Subtract 100 years. This is wrong for most; only 1950 and 1949 are affected
      trajfile$year <- trajfile$year-100
    }
  }
}


for(i in seq_along(sites_it)){
  message("Importing ", site_names[i], " data")
  for(y in seq_along(years_it)){
    # iterate along years
    if(i == 1){
      # This is PL; use PL dates
      # import_dates <- datesrun_list_SC
    } else if(i == 2){
      # import_dates <- datesrun_list_PL
    }
    if(y == 1){
      # import each year for that year.
      trajfile <- read.csv(paste0(data_directory_reanalysis,site_names[i],"-",years[y],"-",import_dates[y],"_72hr1TPD2000m.csv"), header = TRUE)
      if(as.numeric(substr(trajfile$year[1], 1, 1)) == 2){
        # This means the year is busted. Subtract 100 years.
        trajfile$year <- trajfile$year-100
      }
      trajfile <- subset(trajfile,select = -c(trajectory))
      frame_tobind <<- trajfile
      #assign(paste0(site_names[i],"_",years[y]), trajfile, envir = parent.frame())
      #assign("frame_tobind", trajfile, envir = a)
      #assign("frame_tobind", trajfile, envir = parent.frame())
      message(paste0(site_names[i]," ",years[y]," imported"))
    } else {
      # import each year for that year.
      trajfile <- read.csv(paste0(data_directory_reanalysis,site_names[i],"-",years[y],"-",import_dates[y],"_72hr1TPD2000m.csv"), header = TRUE)
      if(as.numeric(substr(trajfile$year[1], 1, 1)) == 2){
        # This means the year is busted. Subtract 100 years.
        trajfile$year <- trajfile$year-100
      }
      trajfile <- subset(trajfile,select = -c(trajectory))
      #assign(paste0(site_names[i],"_",years[y]), trajfile, envir = parent.frame())
      newframe <- rbind(frame_tobind,trajfile)
      frame_tobind <- newframe
      #assign("frame_tobind", newframe, envir = parent.frame())
      message(paste0(site_names[i]," ",years[y]," imported"))
    }
    # now make overall df with trajectory identifier
    #rm(frame_tobind, envir = parent.frame())
  }
  # Lat/long adjustments. This replaces negative (West) lon values with 180+(diff)
  frame_tobind <- adjust_longitude(frame_tobind)
  frame_tobind <- Add_traj_identifier(frame_tobind, ntraj_1 = TRUE)
  frame_tobind <- subset(frame_tobind, select = -c(receptor,pressure,date.inc,diffs,date.inc))
  assign(paste0(site_names[i],"_72hr1TPD2000m_Reanalysis"),frame_tobind, envir = parent.frame())
  message("...","\n",site_names[i]," imported")
}
# export
write.csv(PL_72hr1TPD2000m_Reanalysis,paste0(export_dir,"PL_72hr1TPD2000m_Reanalysis_updated.csv"), row.names = FALSE)
write.csv(SC_72hr1TPD2000m_Reanalysis,paste0(export_dir,"SC_72hr1TPD2000m_Reanalysis_updated.csv"), row.names = FALSE)
# changes
PL_72hr1TPD2000m_Reanalysis <- adjust_longitude(PL_72hr1TPD2000m_Reanalysis)
SC_72hr1TPD2000m_Reanalysis <- adjust_longitude(SC_72hr1TPD2000m_Reanalysis)


##### == SETUP: Mapping (Always Run) ==  #####

## World data
world <- map_data("worldHires")

## 'Full' Basemap. Medium Zoom.
min_lon = 110 #left bounds
max_lon = 190 #right bounds
min_lat = -60 #lower bounds
max_lat = 10 # upper bounds
EastAus_Data <- world %>% 
  filter (lat > min_lat) %>%
  filter (lat < max_lat) %>%
  filter (long > min_lon) %>%
  filter (long < max_lon) %>%
  fortify()
rect_data_eastaus <- matrix(c(min_lon,max_lon,min_lat,max_lat))
EastAus_Full_Basemap <- list(
  geom_rect(aes(xmin = rect_data_eastaus[1]+5, xmax = rect_data_eastaus[2]-5, ymin = rect_data_eastaus[3]+5,ymax = rect_data_eastaus[4]-5), 
            fill = "steelblue", alpha = 0.7),
  geom_map(data = EastAus_Data, map = EastAus_Data,
           aes(
             # x = long, y = lat, 
               group = group, map_id = region),
           fill = "white", colour = "#7f7f7f", size = 0.5),
  coord_map("rectangular", lat0 = 0, xlim = c(min_lon+5,max_lon-5), ylim = c(min_lat+5, max_lat-5)),
  scale_x_continuous(
    expand = c(0,0), limits = c(min_lon+5,max_lon-5), breaks = seq(min_lon+5,max_lon-5, 25)),
  scale_y_continuous(
    expand = c(0,0), limits = c(min_lat+5, max_lat-5), breaks = seq(min_lat+5, max_lat-5, 10)),
  theme_bw(),
  theme(
    panel.grid = element_line(size = 0.1, colour = "grey15", linetype = "dashed")),
  labs(title="", x="", y="")
)

## 'Zoomin' Basemap. Closer in. 
min_lon = 140 #left bounds
max_lon = 170 #right bounds
min_lat = -40 #lower bounds
max_lat = -10 # upper bounds
EastAus_Zoomin_Data <- world %>% 
  filter (lat > min_lat) %>%
  filter (lat < max_lat) %>%
  filter (long > min_lon) %>%
  filter (long < max_lon) %>%
  fortify()
rect_data_eastaus_zoomin <- matrix(c(min_lon,max_lon,min_lat,max_lat))
EastAus_Zoomin_Basemap <- list(
  geom_rect(aes(xmin = rect_data_eastaus_zoomin[1]+5, 
                xmax = rect_data_eastaus_zoomin[2]-5, 
                ymin = rect_data_eastaus_zoomin[3]+5,
                ymax = rect_data_eastaus_zoomin[4]-5), 
            fill = "steelblue", alpha = 0.7),
  geom_map(data = EastAus_Zoomin_Data, map = EastAus_Zoomin_Data,
           aes(
             # x = long, y = lat, 
             group = group, map_id = region),
           fill = "white", colour = "#7f7f7f", size = 0.5),
  coord_map("rectangular", lat0 = 0, xlim = c(min_lon+5,max_lon-5), ylim = c(min_lat+5, max_lat-5)),
  scale_x_continuous(
    expand = c(0,0), limits = c(min_lon+5,max_lon-5), breaks = seq(min_lon+5,max_lon-5, 25)),
  scale_y_continuous(
    expand = c(0,0), limits = c(min_lat+5, max_lat-5), breaks = seq(min_lat+5, max_lat-5, 10)),
  theme_bw(),
  theme(
    panel.grid = element_line(size = 0.1, colour = "grey15", linetype = "dashed")),
  labs(title="", x="", y="")
)

## 'Zoomout' Basemap. Zoomed further out. More distorted.
min_lon = 80 #left bounds
max_lon = 190 #right bounds
min_lat = -65 #lower bounds
max_lat = 10 # upper bounds
EastAus_Zoomout_Data <- world %>% 
  filter (lat > min_lat) %>%
  filter (lat < max_lat) %>%
  filter (long > min_lon) %>%
  filter (long < max_lon) %>%
  fortify()
rect_data_eastaus_zoomout <- matrix(c(min_lon,max_lon,min_lat,max_lat))
EastAus_Zoomout_Basemap <- list(
  geom_rect(aes(xmin = rect_data_eastaus_zoomout[1]+5, 
                xmax = rect_data_eastaus_zoomout[2]-5, 
                ymin = rect_data_eastaus_zoomout[3]+5,
                ymax = rect_data_eastaus_zoomout[4]-5), 
            fill = "steelblue", alpha = 0.7),
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(
             # x = long, y = lat, 
             group = group, map_id = region),
           fill = "white", colour = "#7f7f7f", size = 0.5),
  coord_map("rectangular", lat0 = 0, xlim = c(min_lon+5,max_lon-5), ylim = c(min_lat+5, max_lat-5)),
  scale_x_continuous(
    expand = c(0,0), limits = c(min_lon+5,max_lon-5), breaks = seq(min_lon+5,max_lon-5, 25)),
  scale_y_continuous(
    expand = c(0,0), limits = c(min_lat+5, max_lat-5), breaks = seq(min_lat+5, max_lat-5, 10)),
  theme_bw(),
  theme(
    panel.grid = element_line(size = 0.1, colour = "grey15", linetype = "dashed")),
  labs(title="", x="", y="")
)
#ggplot() + 
#  EastAus_Zoomout_Basemap

## SC_Point
SC_point_df <- as.data.frame(matrix(NA,1,2))
SC_point_df[,1] <- SC_lat
SC_point_df[,2] <- SC_lon
colnames(SC_point_df) <- c("lat","lon")
SC_point <- list(
  geom_point(data = SC_point_df, aes(x = lon, y = lat), shape = 21, fill = "white", size = 3)
)

## PL_Point
PL_point_df <- as.data.frame(matrix(NA,1,2))
PL_point_df[,1] <- PL_lat
PL_point_df[,2] <- PL_lon
colnames(PL_point_df) <- c("lat","lon")
PL_point <- list(
  geom_point(data = PL_point_df, aes(x = lon, y = lat), shape = 21, fill = "white", size = 3)
)

## Themes
plot_themes_monthfill <- list(
  theme(
    plot.title = element_text(size = 9),
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    axis.text = element_text(size = 6)
  ),
  guides(
    fill=guide_legend(title="Month#"))
)

## Map to confirm code ran
#png(filename = paste0(export_dir,"Zoomout_Basemap.png"), units = "cm", height = 10, width = 15, res = 300)
ggplot() + 
  EastAus_Zoomout_Basemap +
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(
             # x = long, y = lat, 
             group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  PL_point +
  SC_point +
  plot_themes_monthfill
#dev.off()


##### == SETUP: Importing and collating trajectory data ==  #####

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

# re-import
# Reanalysis 1950:1999
PL_72hr1TPD2000m_Reanalysis <- adjust_longitude(read.csv(paste0(export_dir,"PL_72hr1TPD2000m_Reanalysis.csv"), header = TRUE))
SC_72hr1TPD2000m_Reanalysis <- adjust_longitude(read.csv(paste0(export_dir,"SC_72hr1TPD2000m_Reanalysis.csv"), header = TRUE))
# GDAS1 2005:2020
PL_72hr1TPD2000m_GDAS1 <- adjust_longitude(read.csv(paste0(export_dir,"PL_72hr1TPD2000m_GDAS1.csv"), header = TRUE))
SC_72hr1TPD2000m_GDAS1 <- adjust_longitude(read.csv(paste0(export_dir,"SC_72hr1TPD2000m_GDAS1.csv"), header = TRUE))

# add starting months for by-month sorting later
add_starting_months <- function(traj_df){
  if("starting_month" %in% colnames(traj_df))
  {
    stop(paste0(deparse(substitute(traj_df))," already has a starting_month column!"))
  }
  traj_df$starting_month <- as.numeric(substr(traj_df$date.start, 6, 7))
  traj_df
}
SC_72hr1TPD2000m_Reanalysis <- add_starting_months(SC_72hr1TPD2000m_Reanalysis)
PL_72hr1TPD2000m_Reanalysis <- add_starting_months(PL_72hr1TPD2000m_Reanalysis)
SC_72hr1TPD2000m_GDAS1 <- add_starting_months(SC_72hr1TPD2000m_GDAS1)
PL_72hr1TPD2000m_GDAS1 <- add_starting_months(PL_72hr1TPD2000m_GDAS1)

# Sort for summer months

traj_df <- SC_72hr1TPD2000m_GDAS1
group_by_months <- function(traj_df,months = c(12,1,2)){
  if(!"starting_month" %in% colnames(traj_df)){
    traj_df$starting_month <- as.numeric(substr(traj_df$date.start, 6, 7))
    traj_df
  }
  traj_df_mod <- traj_df
  traj_df_mod <- traj_df_mod[traj_df_mod$starting_month %in% months,]
  traj_df_mod
}
SC_DJF <- group_by_months(SC_72hr1TPD2000m_Reanalysis)
PLK_DJF <- group_by_months(PL_72hr1TPD2000m_Reanalysis)

SC_DJF <- SC_72hr1TPD2000m_Reanalysis[which(
  SC_72hr1TPD2000m_Reanalysis$starting_month == 1 | 
    SC_72hr1TPD2000m_Reanalysis$starting_month == 2 |
    SC_72hr1TPD2000m_Reanalysis$starting_month == 12),]
PL_DJF <- PL_72hr1TPD2000m_Reanalysis[which(
  PL_72hr1TPD2000m_Reanalysis$starting_month == 1 | 
    PL_72hr1TPD2000m_Reanalysis$starting_month == 2 |
    PL_72hr1TPD2000m_Reanalysis$starting_month == 12),]
# sort for winter months
SC_JJ <- SC_72hr1TPD2000m_Reanalysis[which(
  SC_72hr1TPD2000m_Reanalysis$starting_month == 5 | 
    SC_72hr1TPD2000m_Reanalysis$starting_month == 6),]
PL_JJ <- PL_72hr1TPD2000m_Reanalysis[which(
  PL_72hr1TPD2000m_Reanalysis$starting_month == 5 | 
    PL_72hr1TPD2000m_Reanalysis$starting_month == 6),]

#Get just the endpoints
SC_72hr1TPD2000m_Reanalysis_Endpts <- SC_72hr1TPD2000m_Reanalysis[which(SC_72hr1TPD2000m_Reanalysis$hour.inc == -72),]
PL_72hr1TPD2000m_Reanalysis_Endpts <- PL_72hr1TPD2000m_Reanalysis[which(PL_72hr1TPD2000m_Reanalysis$hour.inc == -72),]

##### == [REANALYSIS] GENERAL AIRMASS PATTERNS: plots #####

## In this section:
# 1) Endpoint densities for both sites
# 2) Summer airmass densities (all points)
# 3) Winter airmass densities (all points)

## Endpoint cloud: SC
png(filename = paste0(export_dir,"SC_EndPointCloud.png"), units = "cm", height = 10, width = 15, res = 300)
ggplot() +
  EastAus_Zoomout_Basemap +
  geom_point(data = SC_72hr1TPD2000m_Reanalysis_Endpts, aes(x = lon, y = lat), shape = 21, colour = "grey5", fill = "red", size = 0.5, alpha = 0.5) +
  stat_density2d(
    data = SC_72hr1TPD2000m_Reanalysis_Endpts, aes(x = lon, y = lat), size = 0.7, geom = "contour", colour = "grey12", alpha = 1) +
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  labs(title = "72-hour daily trajectory starting points to Sandy Cape 1950-1999") +
  SC_point +
  plot_themes_monthfill
dev.off()
## Endpoint cloud: PL
png(filename = paste0(export_dir,"PL_EndPointCloud.png"), units = "cm", height = 10, width = 15, res = 300)
ggplot() +
  EastAus_Zoomout_Basemap +
  geom_point(data = PL_72hr1TPD2000m_Reanalysis_Endpts, aes(x = lon, y = lat), shape = 21, colour = "grey5", fill = "red", size = 0.5, alpha = 0.5) +
  stat_density2d(
    data = PL_72hr1TPD2000m_Reanalysis_Endpts, aes(x = lon, y = lat), size = 0.7, geom = "contour", colour = "grey12", alpha = 1) +
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  labs(title = "72-hour daily trajectory starting points to Point Lookout 1950-1999") +
  PL_point +
  plot_themes_monthfill
dev.off()

## Summer point densities
# SC
png(filename = paste0(export_dir,"SC_Summer_Density.png"),units = "cm", height = 8, width = 7, res = 300)
ggplot() +
  EastAus_Zoomin_Basemap +
  stat_density2d(
    data = SC_DJF, aes(x = lon, y = lat), size = 0.5, bins = 15, geom = "contour", alpha = 0.8, colour = "red") +
  #geom_point(data = SC_DJF, aes(x = lon, y = lat), shape = 21, colour = "grey5", fill = "red", size = 0.5, alpha = 0.5) +
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  labs(title = "Summer Trajectory Point Density to SC") +
  SC_point +
  plot_themes_monthfill
dev.off()
# PL
png(filename = paste0(export_dir,"PL_Summer_Density.png"),units = "cm", height = 8, width = 7, res = 300)
ggplot() +
  EastAus_Zoomin_Basemap +
  stat_density2d(
    data = PL_DJF, aes(x = lon, y = lat), size = 0.5, bins = 15, geom = "contour", alpha = 0.8, colour = "red") +
  #geom_point(data = PL_DJF, aes(x = lon, y = lat), shape = 21, colour = "grey5", fill = "red", size = 0.5, alpha = 0.5) +
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  labs(title = "Summer Trajectory Point Density to PL") +
  PL_point +
  plot_themes_monthfill
dev.off()

## Winter point densities
# SC
png(filename = paste0(export_dir,"SC_Winter_Density.png"),units = "cm", height = 8, width = 7, res = 300)
ggplot() +
  EastAus_Zoomin_Basemap +
  stat_density2d(
    data = SC_JJ, aes(x = lon, y = lat), size = 0.5, bins = 15, geom = "contour", alpha = 0.8, colour = "red") +
  #geom_point(data = SC_JJ, aes(x = lon, y = lat), shape = 21, colour = "grey5", fill = "red", size = 0.5, alpha = 0.5) +
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  labs(title = "Winter Trajectory Point Density to SC") +
  SC_point +
  plot_themes_monthfill
dev.off()
# PL
png(filename = paste0(export_dir,"PL_Winter_Density.png"),units = "cm", height = 8, width = 7, res = 300)
ggplot() +
  EastAus_Zoomin_Basemap +
  stat_density2d(
    data = PL_JJ, aes(x = lon, y = lat), size = 0.5, bins = 15, geom = "contour", alpha = 0.8, colour = "red") +
  #geom_point(data = PL_JJ, aes(x = lon, y = lat), shape = 21, colour = "grey5", fill = "red", size = 0.5, alpha = 0.5) +
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  labs(title = "Winter Trajectory Point Density to PL") +
  PL_point +
  plot_themes_monthfill
dev.off()

## Full extent polygon testing (needs a better projection with larger map bounds)
# from https://stats.stackexchange.com/questions/22805/how-to-draw-neat-polygons-around-scatterplot-regions-in-ggplot2
#df <- SC_72hr1TPD2000m_Reanalysis[which(SC_72hr1TPD2000m_Reanalysis$hour.inc == -72),]
#find_hull <- function(df) df[chull(df$lat, df$lon), ]
#hulls <- find_hull(df)
#png("test.png")
#ggplot(data = df, aes(x = lon, y = lat)) +
#  EastAus_Zoomout_Basemap +
#  #geom_point() + 
#  geom_polygon(data = hulls, alpha = 0.5) +
#  labs(x = "lon", y = "lat")
#dev.off()

##### == [REANALYSIS] GENERAL AIRMASS PATTERNS: animations #####

## Animations of endpoint spatial patterns
# SC
SC_Endpoint_densities <- ggplot() +
  EastAus_Zoomout_Basemap +
  geom_point(data = SC_72hr1TPD2000m_Reanalysis_Endpts, aes(x = lon, y = lat, group = as.factor(starting_month)),fill = "grey12", shape = 21, colour = "black", size = 0.5, alpha = 0.5) +
  stat_density2d(
    data = SC_72hr1TPD2000m_Reanalysis_Endpts, aes(x = lon, y = lat, group = as.factor(starting_month)), colour = "red", bins = 8, size = 1, geom = "contour", alpha = 0.8) +
  geom_map(data = EastAus_Data, map = EastAus_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  SC_point +
  plot_themes_monthfill +
  labs(caption = "MH 18/03/21") 

anim <- SC_Endpoint_densities +
  # transition_reveal(along = month)
  transition_states(starting_month) +
  ease_aes('linear') +
  ggtitle("starting_month: {closest_state}")
anim_save("SC_Endpoint_Densities_MonthlyAnim.gif", anim)
# PL
PL_Endpoint_densities <- ggplot() +
  EastAus_Zoomout_Basemap +
  geom_point(data = PL_72hr1TPD2000m_Reanalysis_Endpts, aes(x = lon, y = lat, group = as.factor(starting_month)),fill = "grey12", shape = 21, colour = "black", size = 0.5, alpha = 0.5) +
  stat_density2d(
    data = PL_72hr1TPD2000m_Reanalysis_Endpts, aes(x = lon, y = lat, group = as.factor(starting_month)), colour = "red", bins = 8, size = 1, geom = "contour", alpha = 0.8) +
  geom_map(data = EastAus_Data, map = EastAus_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  PL_point + 
  plot_themes_monthfill +
  labs(caption = "MH 18/03/21") 

anim <- PL_Endpoint_densities +
  # transition_reveal(along = month)
  transition_states(starting_month) +
  ease_aes('linear') +
  ggtitle("starting_month: {closest_state}")
anim_save("PL_Endpoint_Densities_MonthlyAnim.gif", anim)

##### == SETUP: Importing, sorting rainfall data and trajectories  == #####

# Rainfall thresholds

# Basic contour density plot of points from the two sites of 
# 2) all > 100mm precipitation events
# 3) Summer >100mm
# 4) Winter >100mm 
# 4) all >50mm precipitation events
# 5) 
# 2) all > 50mm precipitation events

# load in rainfall data
BOM_SC_rainfall_data <- read.csv(paste0(export_dir,"BOM rainfall/SandyCape_Data.csv"), header = TRUE)
BOM_PL_rainfall_data <- read.csv(paste0(export_dir, "BOM rainfall/PointLookout_Data.csv"), header = TRUE)
BOM_PLB_rainfall <- read.csv(paste0(export_dir,"BOM rainfall/PointLookout_Bowls_Data.csv"), header = TRUE)

# Point Lookout combined. <1997 is bowls club, more recent is the PL station. 
BOM_PLB_toadd <- BOM_PLB_rainfall[BOM_PLB_rainfall$Year <= 1996,]
#max(unique(BOM_PLB_toadd$Year))
#min(unique(BOM_PLB_toadd$Year))
BOM_PLCombined_rainfall_data <- rbind(BOM_PLB_toadd,BOM_PL_rainfall_data)
#max(unique(BOM_PLCombined_rainfall_data$Year))
#min(unique(BOM_PLCombined_rainfall_data$Year))

# Select for 1950-1999
BOM_SC_rainfall_data <- BOM_SC_rainfall_data[which(BOM_SC_rainfall_data$Year >= 1950),]
BOM_PLCombined_rainfall_data <- BOM_PLCombined_rainfall_data[which(BOM_PLCombined_rainfall_data$Year >= 1950),]
BOM_SC_rainfall_data <- BOM_SC_rainfall_data[which(BOM_SC_rainfall_data$Year < 2000),]
BOM_PLCombined_rainfall_data <- BOM_PLCombined_rainfall_data[which(BOM_PLCombined_rainfall_data$Year < 2000),]

# Add an index
BOM_PLCombined_rainfall_data$rnum <- seq.int(nrow(BOM_PLCombined_rainfall_data))
BOM_SC_rainfall_data$rnum <- seq.int(nrow(BOM_SC_rainfall_data))

# add similar starting date YYYY-MM-DD
ymd_similar <- function(df){
  new_df_temp <- df
  new_df <- df
  new_df_temp$month_pad <- stringr::str_pad(new_df_temp$Month,2,pad = "0")
  new_df_temp$day_pad <- stringr::str_pad(new_df_temp$Day,2,pad = "0")
  new_df$date_yyyymmdd <- paste0(new_df$Year,"-",new_df_temp$month_pad,"-",new_df_temp$day_pad)
  new_df
}
BOM_SC_rainfall_data <- ymd_similar(BOM_SC_rainfall_data)
BOM_PLCombined_rainfall_data <- ymd_similar(BOM_PLCombined_rainfall_data)

## Sort for any rainfall event (>5mm)
# SC
BOM_SC_5 <- BOM_SC_rainfall_data[which(!is.na(BOM_SC_rainfall_data$Rainfall.amount..millimetres.)),]
BOM_SC_5 <- BOM_SC_5[which(BOM_SC_5$Rainfall.amount..millimetres. > 5),]
# PL
BOM_PLC_5 <- BOM_PLCombined_rainfall_data[which(!is.na(BOM_PLCombined_rainfall_data$Rainfall.amount..millimetres.)),]
BOM_PLC_5 <- BOM_PLC_5[which(BOM_PLC_5$Rainfall.amount..millimetres. > 5),]

## Sort for 20mm 
# SC
BOM_SC_20 <- BOM_SC_rainfall_data[which(!is.na(BOM_SC_rainfall_data$Rainfall.amount..millimetres.)),]
BOM_SC_20 <- BOM_SC_20[which(BOM_SC_20$Rainfall.amount..millimetres. > 20),]
# PL
BOM_PLC_20 <- BOM_PLCombined_rainfall_data[which(!is.na(BOM_PLCombined_rainfall_data$Rainfall.amount..millimetres.)),]
BOM_PLC_20 <- BOM_PLC_20[which(BOM_PLC_20$Rainfall.amount..millimetres. > 20),]

## Sort for large rainfall events (>100mm)
# SC
BOM_SC_100 <- BOM_SC_rainfall_data[which(!is.na(BOM_SC_rainfall_data$Rainfall.amount..millimetres.)),]
BOM_SC_100 <- BOM_SC_100[which(BOM_SC_100$Rainfall.amount..millimetres. >= 100),]
# PL
BOM_PLC_100 <- BOM_PLCombined_rainfall_data[which(!is.na(BOM_PLCombined_rainfall_data$Rainfall.amount..millimetres.)),]
BOM_PLC_100 <- BOM_PLC_100[which(BOM_PLC_100$Rainfall.amount..millimetres. >= 100),]

## Parse for very large rainfall events (>200mm)
# SC
BOM_SC_200 <- BOM_SC_rainfall_data[which(!is.na(BOM_SC_rainfall_data$Rainfall.amount..millimetres.)),]
BOM_SC_200 <- BOM_SC_200[which(BOM_SC_200$Rainfall.amount..millimetres. >= 200),]
# PL
BOM_PLC_200 <- BOM_PLCombined_rainfall_data[which(!is.na(BOM_PLCombined_rainfall_data$Rainfall.amount..millimetres.)),]
BOM_PLC_200 <- BOM_PLC_200[which(BOM_PLC_200$Rainfall.amount..millimetres. >= 200),]

## Parse for smaller rainfall events (0<100mm)
# SC
BOM_SC_0to100 <- BOM_SC_rainfall_data[which(!is.na(BOM_SC_rainfall_data$Rainfall.amount..millimetres.)),]
BOM_SC_0to100 <- BOM_SC_0to100[which(BOM_SC_0to100$Rainfall.amount..millimetres. > 0),]
BOM_SC_0to100 <- BOM_SC_0to100[which(BOM_SC_0to100$Rainfall.amount..millimetres. < 100),]
# PL
BOM_PLC_0to100 <- BOM_PLCombined_rainfall_data[which(!is.na(BOM_PLCombined_rainfall_data$Rainfall.amount..millimetres.)),]
BOM_PLC_0to100 <- BOM_PLC_0to100[which(BOM_PLC_0to100$Rainfall.amount..millimetres. > 0),]
BOM_PLC_0to100 <- BOM_PLC_0to100[which(BOM_PLC_0to100$Rainfall.amount..millimetres. < 100),]

## Parse for days with no rainfall (0mm)
# SC
BOM_SC_0 <- BOM_SC_rainfall_data[which(!is.na(BOM_SC_rainfall_data$Rainfall.amount..millimetres.)),]
BOM_SC_0 <- BOM_SC_0[which(BOM_SC_0$Rainfall.amount..millimetres. == 0),]
# PL
BOM_PLC_0 <- BOM_PLCombined_rainfall_data[which(!is.na(BOM_PLCombined_rainfall_data$Rainfall.amount..millimetres.)),]
BOM_PLC_0 <- BOM_PLC_0[which(BOM_PLC_0$Rainfall.amount..millimetres. == 0),]

# Extract data which matches the dates of large rainfall events
SC_100mmRainfall_Trajectories <- SC_72hr1TPD2000m_Reanalysis[SC_72hr1TPD2000m_Reanalysis$date.start %in% (BOM_SC_100$date_yyyymmdd),]
PL_100mmRainfall_Trajectories <- PL_72hr1TPD2000m_Reanalysis[PL_72hr1TPD2000m_Reanalysis$date.start %in% (BOM_PLC_100$date_yyyymmdd),]
SC_200mmLargeRainfall_Trajectories <- SC_72hr1TPD2000m_Reanalysis[SC_72hr1TPD2000m_Reanalysis$date.start %in% (BOM_SC_200$date_yyyymmdd),]
PL_200mmLargeRainfall_Trajectories <- PL_72hr1TPD2000m_Reanalysis[PL_72hr1TPD2000m_Reanalysis$date.start %in% (BOM_PLC_200$date_yyyymmdd),]

# Get endpoints
SC_100mmRainfall_Endpts <- SC_100mmRainfall_Trajectories[which(SC_100mmRainfall_Trajectories$hour.inc == -72),]
PL_100mmRainfall_Endpts <- PL_100mmRainfall_Trajectories[which(PL_100mmRainfall_Trajectories$hour.inc == -72),]

#Extract data which matches the dates of small rainfall events
SC_SmallRainfall_Trajectories <- SC_72hr1TPD2000m_Reanalysis[SC_72hr1TPD2000m_Reanalysis$date.start %in% (BOM_SC_0to100$date_yyyymmdd),]
PL_SmallRainfall_Trajectories <- PL_72hr1TPD2000m_Reanalysis[PL_72hr1TPD2000m_Reanalysis$date.start %in% (BOM_PLC_0to100$date_yyyymmdd),]
# Endpoints
SC_SmallRainfall_Endpts <- SC_SmallRainfall_Trajectories[which(SC_SmallRainfall_Trajectories$hour.inc == -72),]
PL_SmallRainfall_Endpts <- PL_SmallRainfall_Trajectories[which(PL_SmallRainfall_Trajectories$hour.inc == -72),]

##### == [REANALYSIS] LARGE RAINFALL EVENTS (>100mm): plots  #####

## Path plots of large rainfall day trajectories
# SC
png(filename = paste0(export_dir,"SC_LargeRainfall_Paths.png"), units = "cm", height = 12, width = 16, res = 300)
ggplot() +
  EastAus_Full_Basemap +
  #stat_density2d(
  #  data = SC_JJ, aes(x = lon, y = lat), size = 0.5, bins = 15, geom = "contour", alpha = 0.8, colour = "red") +
  geom_path(data = SC_100mmRainfall_Trajectories, aes(x = lon, y = lat, group = trajectory), colour = "darkred",size = 0.5, alpha = 0.8) +
  geom_path(data = SC_200mmLargeRainfall_Trajectories, aes(x = lon, y = lat, group = trajectory), colour = "yellow",size = 0.5, alpha = 0.8) +
  geom_point(data = SC_200mmLargeRainfall_Trajectories, aes(x = lon, y = lat), shape = 21, colour = "grey5", fill = "yellow", size = 0.5, alpha = 0.5) +
  geom_point(data = SC_100mmRainfall_Endpts, aes(x = lon, y = lat), shape = 4, colour = "black", size = 1.2, alpha = 1) +
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  labs(title = "Trajectory paths from large rainfall events to SC") +
  SC_point +
  plot_themes_monthfill
dev.off()
# PL
png(filename = paste0(export_dir,"PL_LargeRainfall_Paths.png"), units = "cm", height = 12, width = 16, res = 300)
ggplot() +
  EastAus_Full_Basemap +
  #stat_density2d(
  #  data = PL_JJ, aes(x = lon, y = lat), size = 0.5, bins = 15, geom = "contour", alpha = 0.8, colour = "red") +
  geom_path(data = PL_100mmRainfall_Trajectories, aes(x = lon, y = lat, group = trajectory), colour = "darkred",size = 0.5, alpha = 0.8) +
  geom_path(data = PL_200mmLargeRainfall_Trajectories, aes(x = lon, y = lat, group = trajectory), colour = "yellow",size = 0.5, alpha = 0.8) +
  geom_point(data = PL_200mmLargeRainfall_Trajectories, aes(x = lon, y = lat), shape = 21, colour = "grey5", fill = "yellow", size = 0.5, alpha = 0.5) +
  geom_point(data = PL_100mmRainfall_Endpts, aes(x = lon, y = lat), shape = 4, colour = "black", size = 1.2, alpha = 1) +
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  labs(title = "Trajectory paths from large rainfall events to PL") +
  PL_point +
  plot_themes_monthfill
dev.off()

## Density plots
# SC - with trajectories
png(filename = paste0(export_dir,"SC_LargeRainfall_DensityPlusPaths.png"), units = "cm", height = 12, width = 16, res = 300)
ggplot() +
  EastAus_Zoomin_Basemap +
  geom_path(data = SC_100mmRainfall_Trajectories, aes(x = lon, y = lat, group = trajectory), colour = "grey8", size = 0.5, alpha = 0.2) +
  stat_density2d(
    data = SC_100mmRainfall_Trajectories, aes(x = lon, y = lat), size = 0.5, bins = 8, geom = "contour", alpha = 0.8, colour = "red") +
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  labs(title = "Trajectory density of large rainfall events to SC") +
  SC_point +
  plot_themes_monthfill
dev.off()
# SC - no trajectories
png(filename = paste0(export_dir,"SC_LargeRainfall_Density.png"), units = "cm", height = 12, width = 16, res = 300)
ggplot() +
  EastAus_Zoomin_Basemap +
  #geom_path(data = SC_100mmRainfall_Trajectories, aes(x = lon, y = lat, group = trajectory), colour = "grey8", size = 0.5, alpha = 0.2) +
  stat_density2d(
    data = SC_100mmRainfall_Trajectories, aes(x = lon, y = lat), size = 0.5, bins = 8, geom = "contour", alpha = 0.8, colour = "red") +
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  labs(title = "Trajectory density of large rainfall events to SC") +
  SC_point +
  plot_themes_monthfill
dev.off()
# PL - with trajectories
png(filename = paste0(export_dir,"PL_LargeRainfall_DensityPlusPaths.png"), units = "cm", height = 12, width = 16, res = 300)
ggplot() +
  EastAus_Zoomin_Basemap +
  geom_path(data = PL_100mmRainfall_Trajectories, aes(x = lon, y = lat, group = trajectory), colour = "grey8", size = 0.5, alpha = 0.2) +
  stat_density2d(
    data = PL_100mmRainfall_Trajectories, aes(x = lon, y = lat), size = 0.5, bins = 8, geom = "contour", alpha = 0.8, colour = "red") +
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  labs(title = "Trajectory density of large rainfall events to PL") +
  PL_point +
  plot_themes_monthfill
dev.off()
# PL - no trajectories
png(filename = paste0(export_dir,"PL_LargeRainfall_Density.png"), units = "cm", height = 12, width = 16, res = 300)
ggplot() +
  EastAus_Zoomin_Basemap +
  #geom_path(data = PL_100mmRainfall_Trajectories, aes(x = lon, y = lat, group = trajectory), colour = "grey8", size = 0.5, alpha = 0.2) +
  stat_density2d(
    data = PL_100mmRainfall_Trajectories, aes(x = lon, y = lat), size = 0.5, bins = 8, geom = "contour", alpha = 0.8, colour = "red") +
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  labs(title = "Trajectory density of large rainfall events to PL") +
  PL_point +
  plot_themes_monthfill
dev.off()

##### == [REANALYSIS] LARGE RAINFALL EVENTS (>100mm): animations #####

## Animating the Seasons
# SC
SC_LargeRainfall_Paths <- ggplot() +
  EastAus_Full_Basemap +
  #stat_density2d(
  #  data = SC_JJ, aes(x = lon, y = lat), size = 0.5, bins = 15, geom = "contour", alpha = 0.8, colour = "red") +
  geom_path(data = SC_100mmRainfall_Trajectories, aes(x = lon, y = lat, group = trajectory), colour = "darkred",size = 0.8, alpha = 0.8) +
  #geom_path(data = SC_200mmLargeRainfall_Trajectories, aes(x = lon, y = lat, group = trajectory), colour = "yellow",size = 0.5, alpha = 0.8) +
  geom_point(data = SC_100mmRainfall_Trajectories, aes(x = lon, y = lat, group = trajectory), shape = 21, colour = "grey5", fill = "darkred", size = 0.8, alpha = 0.5) +
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  labs(title = "Trajectory paths from large rainfall events to SC") +
  SC_point +
  plot_themes_monthfill +
  labs(caption = "MH 23/03/21") 
anim <- SC_LargeRainfall_Paths +
  # transition_reveal(along = month)
  transition_states(starting_month) +
  ease_aes('linear') +
  ggtitle("starting_month: {closest_state}")
anim_save("SC_LargeRainfallPaths_MonthlyAnim.gif", anim)
# PL
PL_LargeRainfall_Paths <- ggplot() +
  EastAus_Full_Basemap +
  #stat_density2d(
  #  data = PL_JJ, aes(x = lon, y = lat), size = 0.5, bins = 15, geom = "contour", alpha = 0.8, colour = "red") +
  geom_path(data = PL_100mmRainfall_Trajectories, aes(x = lon, y = lat, group = trajectory), colour = "darkred",size = 0.8, alpha = 0.8) +
  #geom_path(data = PL_200mmLargeRainfall_Trajectories, aes(x = lon, y = lat, group = trajectory), colour = "yellow",size = 0.5, alpha = 0.8) +
  geom_point(data = PL_100mmRainfall_Trajectories, aes(x = lon, y = lat, group = trajectory), shape = 21, colour = "grey5", fill = "darkred", size = 0.8, alpha = 0.5) +
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  labs(title = "Trajectory paths from large rainfall events to PL") +
  PL_point +
  plot_themes_monthfill +
  labs(caption = "MH 23/03/21") 
anim <- PL_LargeRainfall_Paths +
  # transition_reveal(along = month)
  transition_states(starting_month) +
  ease_aes('linear') +
  ggtitle("starting_month: {closest_state}")
anim_save("PL_LargeRainfallPaths_MonthlyAnim.gif", anim)

##### == [REANALYSIS] SMALL RAINFALL EVENTS (0<100mm): plots #####


# SC
png(filename = paste0(export_dir,"SC_SmallRainfall_Endpts.png"), units = "cm", height = 12, width = 16, res = 300)
ggplot() +
  EastAus_Full_Basemap +
  stat_density2d(
    data = SC_SmallRainfall_Endpts, aes(x = lon, y = lat), size = 0.5, bins = 15, geom = "contour", alpha = 0.8, colour = "red") +
  #geom_path(data = SC_SmallRainfall_Trajectories, aes(x = lon, y = lat, group = trajectory), colour = "darkred",size = 0.5, alpha = 0.5) +
  geom_point(data = SC_SmallRainfall_Endpts, aes(x = lon, y = lat), shape = 4, colour = "black", size = 1.2, alpha = 1) +
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  labs(title = "Trajectory paths from small rainfall events to SC") +
  SC_point +
  plot_themes_monthfill
dev.off()
# PL
png(filename = paste0(export_dir,"PL_SmallRainfall_Endpts.png"), units = "cm", height = 12, width = 16, res = 300)
ggplot() +
  EastAus_Full_Basemap +
  #stat_density2d(
  #  data = PL_JJ, aes(x = lon, y = lat), size = 0.5, bins = 15, geom = "contour", alpha = 0.8, colour = "red") +
  #geom_path(data = PL_SmallRainfall_Trajectories, aes(x = lon, y = lat, group = trajectory), colour = "darkred",size = 0.5, alpha = 0.5) +
  geom_point(data = PL_SmallRainfall_Endpts, aes(x = lon, y = lat), shape = 4, colour = "black", size = 1.2, alpha = 1) +
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  labs(title = "Trajectory paths from small rainfall events to PL") +
  PL_point +
  plot_themes_monthfill
dev.off()

## Animating 'any' rainfall event endpoint densities ##
# Trajectories 
SC_5Rainfall_Traj <- SC_72hr1TPD2000m_Reanalysis[SC_72hr1TPD2000m_Reanalysis$date.start %in% (BOM_SC_5$date_yyyymmdd),]
PL_5Rainfall_Traj <- PL_72hr1TPD2000m_Reanalysis[PL_72hr1TPD2000m_Reanalysis$date.start %in% (BOM_PLC_5$date_yyyymmdd),]
# Endpoints
SC_5Rainfall_Traj <- SC_5Rainfall_Traj[which(SC_5Rainfall_Traj$hour.inc == -72),]
PL_5Rainfall_Traj <- PL_5Rainfall_Traj[which(PL_5Rainfall_Traj$hour.inc == -72),]

# SC
SC_5Rainfall_Endpoint_densities <- ggplot() +
  EastAus_Zoomout_Basemap +
  geom_point(data = SC_5Rainfall_Traj, aes(x = lon, y = lat, group = as.factor(starting_month)),fill = "grey12", shape = 21, colour = "black", size = 0.5, alpha = 0.5) +
  stat_density2d(
    data = SC_5Rainfall_Traj, aes(x = lon, y = lat, group = as.factor(starting_month)), colour = "red", bins = 8, size = 1, geom = "contour", alpha = 0.8) +
  geom_map(data = EastAus_Data, map = EastAus_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  SC_point +
  plot_themes_monthfill +
  labs(caption = "MH 18/03/21") 
anim <- SC_5Rainfall_Endpoint_densities +
  # transition_reveal(along = month)
  transition_states(starting_month) +
  ease_aes('linear') +
  ggtitle("starting_month: {closest_state}")
anim_save("SC_5mmRainfall_Endpoint_MonthlyAnim.gif", anim)

# PL
PL_5Rainfall_Endpoint_densities <- ggplot() +
  EastAus_Zoomout_Basemap +
  geom_point(data = PL_5Rainfall_Traj, aes(x = lon, y = lat, group = as.factor(starting_month)),fill = "grey12", shape = 21, colour = "black", size = 0.5, alpha = 0.5) +
  stat_density2d(
    data = PL_5Rainfall_Traj, aes(x = lon, y = lat, group = as.factor(starting_month)), colour = "red", bins = 8, size = 1, geom = "contour", alpha = 0.8) +
  geom_map(data = EastAus_Data, map = EastAus_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  PL_point +
  plot_themes_monthfill +
  labs(caption = "MH 18/03/21") 
anim <- PL_5Rainfall_Endpoint_densities +
  # transition_reveal(along = month)
  transition_states(starting_month) +
  ease_aes('linear') +
  ggtitle("starting_month: {closest_state}")
anim_save("PL_5mmRainfall_Endpoint_MonthlyAnim.gif", anim)



##### == SETUP: SOI Data == #####

SOI_index <- read.table(file = "soi_monthly_bom.txt",sep = ",")
# post-1950
SOI_index <- SOI_index[substr(SOI_index$V1,1,4) > 1949,]
SOI_index <- SOI_index[substr(SOI_index$V1,1,4) < 2000,]
# La Nina
SOI_LaNina20 <- SOI_index[which(SOI_index$V2 >= 20),]
# El Nina
SOI_ElNino20 <- SOI_index[which(SOI_index$V2 <= -20),]

# Any rainfall event >2mm that falls within an extreme La Nina Month
SOI_LaNina20$month <- substr(SOI_LaNina20$V1,5,6)
SOI_ElNino20$month <- substr(SOI_ElNino20$V1,5,6)

#### NB: 'Any' below refers to >20mm rainfall. See initial rainfall parsing.
# Trajectories for days with some rainfall
SC_5Rainfall_Trajectories <- SC_72hr1TPD2000m_Reanalysis[SC_72hr1TPD2000m_Reanalysis$date.start %in% (BOM_SC_20$date_yyyymmdd),]
PL_5Rainfall_Trajectories <- PL_72hr1TPD2000m_Reanalysis[PL_72hr1TPD2000m_Reanalysis$date.start %in% (BOM_PLC_20$date_yyyymmdd),]

# Now dice this by starting months that match SOI_LaNina20. Months in the latter case have to be padded to match.
# LaNina
SC_5Rainfall_Traj_LaNina20 <- SC_5Rainfall_Trajectories[SC_5Rainfall_Trajectories$starting_month %in% (str_pad(SOI_LaNina20$month,2,side = "left", "0")),]
PL_5Rainfall_Traj_LaNina20 <- PL_5Rainfall_Trajectories[PL_5Rainfall_Trajectories$starting_month %in% (str_pad(SOI_LaNina20$month,2,side = "left", "0")),]
# ElNino
SC_5Rainfall_Traj_ElNino20 <- SC_5Rainfall_Trajectories[SC_5Rainfall_Trajectories$starting_month %in% (str_pad(SOI_ElNino20$month,2,side = "left", "0")),]
PL_5Rainfall_Traj_ElNino20 <- PL_5Rainfall_Trajectories[PL_5Rainfall_Trajectories$starting_month %in% (str_pad(SOI_ElNino20$month,2,side = "left", "0")),]

##### == [REANALYSIS] SOI AND RAINFALL: plots #####

## LaNina Rainfall
# SC - Trajectories
png(filename = paste0(export_dir,"SC_LaNina20_RainfallPaths.png"), units = "cm", height = 12, width = 16, res = 300)
ggplot() +
  EastAus_Full_Basemap +
  geom_path(data = SC_5Rainfall_Traj_LaNina20, aes(x = lon, y = lat, group = trajectory), colour = "darkred",size = 0.5, alpha = 0.8) +
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  labs(title = "Trajectory paths of rainfall events (>20mm) to SC during strong LaNina") +
  SC_point +
  plot_themes_monthfill
dev.off()
# SC - Density with trajectories
png(filename = paste0(export_dir,"SC_LaNina20_RainfallDensityPlusPaths.png"), units = "cm", height = 12, width = 16, res = 300)
ggplot() +
  EastAus_Full_Basemap +
  stat_density2d(
    data = SC_5Rainfall_Traj_LaNina20, aes(x = lon, y = lat), size = 0.5, bins = 20, geom = "contour", alpha = 0.8, colour = "red") +
  geom_path(data = SC_5Rainfall_Traj_LaNina20, aes(x = lon, y = lat, group = trajectory), colour = "grey8", size = 0.5, alpha = 0.2) +
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  labs(title = "Trajectory paths and density of rainfall events (>20mm) to SC during strong LaNina") +
  SC_point +
  plot_themes_monthfill
dev.off()
# SC - Density
png(filename = paste0(export_dir,"SC_LaNina20_RainfallDensity.png"), units = "cm", height = 12, width = 16, res = 300)
ggplot() +
  EastAus_Full_Basemap +
  stat_density2d(
    data = SC_5Rainfall_Traj_LaNina20, aes(x = lon, y = lat), size = 0.5, bins = 20, geom = "contour", alpha = 0.8, colour = "red") +
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  labs(title = "Trajectory density of rainfall events (>20mm) to SC during strong LaNina") +
  SC_point +
  plot_themes_monthfill
dev.off()
# PL - Trajectories
png(filename = paste0(export_dir,"PL_LaNina20_RainfallPaths.png"), units = "cm", height = 12, width = 16, res = 300)
ggplot() +
  EastAus_Full_Basemap +
  geom_path(data = PL_5Rainfall_Traj_LaNina20, aes(x = lon, y = lat, group = trajectory), colour = "darkred",size = 0.5, alpha = 0.8) +
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  labs(title = "Trajectory paths of rainfall events (>20mm) to PL during strong LaNina") +
  PL_point +
  plot_themes_monthfill
dev.off()
# PL - Density with trajectories
png(filename = paste0(export_dir,"PL_LaNina20_RainfallDensityPlusPaths.png"), units = "cm", height = 12, width = 16, res = 300)
ggplot() +
  EastAus_Full_Basemap +
  stat_density2d(
    data = PL_5Rainfall_Traj_LaNina20, aes(x = lon, y = lat), size = 0.5, bins = 20, geom = "contour", alpha = 0.8, colour = "red") +
  geom_path(data = PL_5Rainfall_Traj_LaNina20, aes(x = lon, y = lat, group = trajectory), colour = "grey8", size = 0.5, alpha = 0.2) +
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  labs(title = "Trajectory paths and density of rainfall events (>20mm) to PL during strong LaNina") +
  PL_point +
  plot_themes_monthfill
dev.off()
# PL - Density 
png(filename = paste0(export_dir,"PL_LaNina20_RainfallDensity.png"), units = "cm", height = 12, width = 16, res = 300)
ggplot() +
  EastAus_Full_Basemap +
  stat_density2d(
    data = PL_5Rainfall_Traj_LaNina20, aes(x = lon, y = lat), size = 0.5, bins = 20, geom = "contour", alpha = 0.8, colour = "red") +
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  labs(title = "Trajectory density of rainfall events (>20mm) to PL during strong LaNina") +
  PL_point +
  plot_themes_monthfill
dev.off()


## El Nino Rainfall
# SC - Trajectories
png(filename = paste0(export_dir,"SC_ElNino20_RainfallPaths.png"), units = "cm", height = 12, width = 16, res = 300)
ggplot() +
  EastAus_Full_Basemap +
  geom_path(data = SC_5Rainfall_Traj_ElNino20, aes(x = lon, y = lat, group = trajectory), colour = "darkred",size = 0.5, alpha = 0.8) +
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  labs(title = "Trajectory paths of rainfall events (>20mm) to SC during strong ElNino") +
  SC_point +
  plot_themes_monthfill
dev.off()
# SC - Density with trajectories
png(filename = paste0(export_dir,"SC_ElNino20_RainfallDensityPlusPaths.png"), units = "cm", height = 12, width = 16, res = 300)
ggplot() +
  EastAus_Full_Basemap +
  stat_density2d(
    data = SC_5Rainfall_Traj_ElNino20, aes(x = lon, y = lat), size = 0.5, bins = 20, geom = "contour", alpha = 0.8, colour = "red") +
  geom_path(data = SC_5Rainfall_Traj_ElNino20, aes(x = lon, y = lat, group = trajectory), colour = "grey8", size = 0.5, alpha = 0.2) +
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  labs(title = "Trajectory paths and density of rainfall events (>20mm) to SC during strong ElNino") +
  SC_point +
  plot_themes_monthfill
dev.off()
# SC - Density
png(filename = paste0(export_dir,"SC_ElNino20_RainfallDensity.png"), units = "cm", height = 12, width = 16, res = 300)
ggplot() +
  EastAus_Full_Basemap +
  stat_density2d(
    data = SC_5Rainfall_Traj_ElNino20, aes(x = lon, y = lat), size = 0.5, bins = 20, geom = "contour", alpha = 0.8, colour = "red") +
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  labs(title = "Trajectory density of rainfall events (>20mm) to SC during strong ElNino") +
  SC_point +
  plot_themes_monthfill
dev.off()
# PL - Trajectories
png(filename = paste0(export_dir,"PL_ElNino20_RainfallPaths.png"), units = "cm", height = 12, width = 16, res = 300)
ggplot() +
  EastAus_Full_Basemap +
  geom_path(data = PL_5Rainfall_Traj_ElNino20, aes(x = lon, y = lat, group = trajectory), colour = "darkred",size = 0.5, alpha = 0.8) +
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  labs(title = "Trajectory paths of rainfall events (>20mm) to PL during strong ElNino") +
  PL_point +
  plot_themes_monthfill
dev.off()
# PL - Density with trajectories
png(filename = paste0(export_dir,"PL_ElNino20_RainfallDensityPlusPaths.png"), units = "cm", height = 12, width = 16, res = 300)
ggplot() +
  EastAus_Full_Basemap +
  stat_density2d(
    data = PL_5Rainfall_Traj_ElNino20, aes(x = lon, y = lat), size = 0.5, bins = 20, geom = "contour", alpha = 0.8, colour = "red") +
  geom_path(data = PL_5Rainfall_Traj_ElNino20, aes(x = lon, y = lat, group = trajectory), colour = "grey8", size = 0.5, alpha = 0.2) +
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  labs(title = "Trajectory paths and density of rainfall events (>20mm) to PL during strong ElNino") +
  PL_point +
  plot_themes_monthfill
dev.off()
# PL - Density 
png(filename = paste0(export_dir,"PL_ElNino20_RainfallDensity.png"), units = "cm", height = 12, width = 16, res = 300)
ggplot() +
  EastAus_Full_Basemap +
  stat_density2d(
    data = PL_5Rainfall_Traj_ElNino20, aes(x = lon, y = lat), size = 0.5, bins = 20, geom = "contour", alpha = 0.8, colour = "red") +
  geom_map(data = EastAus_Zoomout_Data, map = EastAus_Zoomout_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  labs(title = "Trajectory density of rainfall events (>20mm) to PL during strong ElNino") +
  PL_point +
  plot_themes_monthfill
dev.off()




##### == FINALISING: Saving Data #####

# Running the code in this section will save all the generated data files within 
# a folder. Adjust the directory within save_files_csv to alter the destination.
# Saving lines are hashed out - unhash them to save.

# Cleaning up
rm(BOM_PLB_toadd,BOM_PLB_rainfall,BOM_PL_rainfall_data,SC_point_df,PL_point_df,
   SC_lat,SC_lon,SC_point,PL_lat,PL_lon,PL_point)

# save as csv functionZ
save_files_csv <- function(file_list){
  it_list <- vector(mode = 'list', length = length(file_list))
  for(i in seq_along(file_list)){
    write.csv(x = file_list[[i]],file = paste0(export_dir,"Files to upload 240321/",names(file_list)[i],".csv"), row.names = FALSE)
  }
}

# Function to save csvs based upon character vector of file names
# This just saves the text of the filename! Need to coerce the data to a list.
# Get list of files, name accordingly
file_list <- lapply(ls(), get) # run twice so file_list is stored in some form too.
file_list <- lapply(ls(), get) # run twice so file_list is stored in some form too.
names(file_list) <- ls()

# Rainfall data
Rainfall_file_names <- file_list[grep("BOM_",names(file_list))]
# Get nrow to determine how many rainfall events fit the criteria
nrow_rainfalldata <- data.frame(matrix(NA,nrow = length(Rainfall_file_names),ncol = 2))
for(i in seq_along(Rainfall_file_names)){
  nrow_rainfalldata[i,1] <- names(Rainfall_file_names)[i]
  nrow_rainfalldata[i,2] <- nrow(Rainfall_file_names[[i]])
}
#write.csv(x = nrow_rainfalldata, file = "nrainfall_events.csv",row.names = FALSE)
#save_files_csv(Rainfall_file_names)
it_list <- vector(mode = "list", length = length(Rainfall_file_names))
for(i in seq_along(it_list)){
  rm(list = names(Rainfall_file_names)[i])
}
rm(Rainfall_file_names)

# Trajectory data: Sandy Cape
file_list <- lapply(ls(), get) # run twice so file_list is stored in some form too.
names(file_list) <- ls()
SC_file_names <- file_list[grep("SC_",names(file_list))]
#save_files_csv(SC_file_names)
list_trajectoriesfilenames <- names(SC_file_names)
it_list <- vector(mode = "list", length = length(SC_file_names))
for(i in seq_along(it_list)){
  rm(list = names(SC_file_names)[i])
}
rm(SC_file_names)

# Trajectory data: Point Lookout
file_list <- lapply(ls(), get) # run twice so file_list is stored in some form too.
names(file_list) <- ls()
PL_file_names <- file_list[grep("PL_",names(file_list))]
#save_files_csv(PL_file_names)
list_trajectoriesfilenames <- c(names(PL_file_names),list_trajectoriesfilenames)
it_list <- vector(mode = "list", length = length(PL_file_names))
for(i in seq_along(it_list)){
  rm(list = names(PL_file_names)[i])
}
rm(PL_file_names)

# SOI Data
file_list <- lapply(ls(), get) # run twice so file_list is stored in some form too.
names(file_list) <- ls()
SOI_file_names <- file_list[grep("SOI_",names(file_list))]
#save_files_csv(SOI_file_names)
filenames_SOI <- names(SOI_file_names)
it_list <- vector(mode = "list", length = length(SOI_file_names))
for(i in seq_along(it_list)){
  rm(list = names(SOI_file_names)[i])
}
rm(SOI_file_names)

##### == PRE-ANALYSIS: Compiling trajectory data = #####

## The code below was used to compile the original trajectory datasets.

### GDAS1 2005:2020
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
data_directory_gdas1 <- "C:/Users/matth/Desktop/University Files/mh phd 2020/grants and collaborations/JTibby via HCadd HYSPLIT/data/GDAS1/"
# Set params
years <- seq(2005,2020,1)
sites_it <- vector(mode = "list", length = 2)
years_it <- vector(mode = "list", length = length(years))
site_names <- c("SC","PL")
dates_run <- c("2021-03-25")
filename_years_list <- vector(mode = 'list', length = length(years))
filename_years_list[c(1:length(years))] <- years
met_type = c("GDAS1")
# dates run
datesrun_list_PL <- vector(mode = 'list', length = length(years))
datesrun_list_PL[c(1:length(years))] <- dates_run[1]
datesrun_list_SC <- vector(mode = 'list', length = length(years))
datesrun_list_SC[c(1:length(years))] <- dates_run[1]
for(i in seq_along(sites_it)){
  message("Importing ", site_names[i]," ",met_type," trajectory data")
  for(y in seq_along(years_it)){
    # iterate along years
    if(i == 1){
      # This is PL; use PL dates
      import_dates <- datesrun_list_SC
    } else if(i == 2){
      import_dates <- datesrun_list_PL
    }
    if(y == 1){
      # import each year for that year.
      trajfile <- read.csv(paste0(data_directory_gdas1,site_names[i],"-",years[y],"-",import_dates[y],"_72hr1TPD2000m.csv"), header = TRUE)
      date_start_year <- as.numeric(substr(trajfile$date.start[1], 1, 4))
      year_start <- as.numeric(trajfile$year[1])
      if(date_start_year != year_start){
        # This means the year is busted. Subtract 100 years.
        trajfile$year <- as.numeric(substr(trajfile$date.inc, 1, 4))
      }
      trajfile <- subset(trajfile,select = -c(trajectory))
      frame_tobind <<- trajfile
      #assign(paste0(site_names[i],"_",years[y]), trajfile, envir = parent.frame())
      #assign("frame_tobind", trajfile, envir = a)
      #assign("frame_tobind", trajfile, envir = parent.frame())
      message(paste0(site_names[i]," ",years[y]," imported"))
    } else {
      # import each year for that year.
      trajfile <- read.csv(paste0(data_directory_gdas1,site_names[i],"-",years[y],"-",import_dates[y],"_72hr1TPD2000m.csv"), header = TRUE)
      date_start_year <- as.numeric(substr(trajfile$date.start[1], 1, 4))
      year_start <- as.numeric(trajfile$year[1])
      if(date_start_year != year_start){
        # This means the year is busted. Subtract 100 years.
        trajfile$year <- as.numeric(substr(trajfile$date.inc, 1, 4))
      }
      trajfile <- subset(trajfile,select = -c(trajectory))
      #assign(paste0(site_names[i],"_",years[y]), trajfile, envir = parent.frame())
      newframe <- rbind(frame_tobind,trajfile)
      frame_tobind <- newframe
      #assign("frame_tobind", newframe, envir = parent.frame())
      message(paste0(site_names[i]," ",years[y]," imported"))
    }
    # now make overall df with trajectory identifier
    #rm(frame_tobind, envir = parent.frame())
  }
  # Lat/long adjustments. This replaces negative (West) lon values with 180+(diff)
  frame_tobind <- adjust_longitude(frame_tobind)
  frame_tobind <- Add_traj_identifier(frame_tobind, ntraj_1 = TRUE)
  frame_tobind <- subset(frame_tobind, select = -c(receptor,pressure,date.inc,diffs,date.inc))
  assign(paste0(site_names[i],"_72hr1TPD2000m","_",met_type),frame_tobind, envir = parent.frame())
  message("...","\n",site_names[i]," imported")
}

# export
write.csv(PL_72hr1TPD2000m_GDAS1,paste0(export_dir,"PL_72hr1TPD2000m_GDAS1.csv"), row.names = FALSE)
write.csv(SC_72hr1TPD2000m_GDAS1,paste0(export_dir,"SC_72hr1TPD2000m_GDAS1.csv"), row.names = FALSE)
# changes
PL_72hr1TPD2000m_GDAS1 <- adjust_longitude(PL_72hr1TPD2000m_GDAS1)
SC_72hr1TPD2000m_GDAS1 <- adjust_longitude(SC_72hr1TPD2000m_GDAS1)


### REANALYSIS 1950:1999 ###
data_directory_reanalysis <- "C:/Users/matth/Desktop/University Files/mh phd 2020/grants and collaborations/JTibby via HCadd HYSPLIT/data/reanalysis/"
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
sites_it <- vector(mode = "list", length = 2)
years_it <- vector(mode = "list", length = 50)
site_names <- c("SC","PL")
years <- seq(1950,1999,1)
dates_run <- c("2020-11-27","2020-11-28",
               "2020-12-17","2020-12-18")
filename_years_list <- vector(mode = 'list', length = 50)
filename_years_list[c(1:50)] <- seq(1950,1999,1)
# dates run
datesrun_list_PL <- vector(mode = 'list', length = 50)
datesrun_list_PL[c(1:16)] <- dates_run[1]
datesrun_list_PL[c(17:50)] <- dates_run[2]
datesrun_list_SC <- vector(mode = 'list', length = 50)
datesrun_list_SC[c(1:31)] <- dates_run[3]
datesrun_list_SC[c(32:50)] <- dates_run[4]
# Loop will import and combine trajectory data.
for(i in seq_along(sites_it)){
  message("Importing ", site_names[i], " data")
  for(y in seq_along(years_it)){
    # iterate along years
    if(i == 1){
      # This is PL; use PL dates
      import_dates <- datesrun_list_SC
    } else if(i == 2){
      import_dates <- datesrun_list_PL
    }
    if(y == 1){
      # import each year for that year.
      trajfile <- read.csv(paste0(data_directory,site_names[i],"-",years[y],"-",import_dates[y],"_72hr1TPD2000m.csv"), header = TRUE)
      if(as.numeric(substr(trajfile$year[1], 1, 1)) == 2){
        # This means the year is busted. Subtract 100 years.
        trajfile$year <- trajfile$year-100
      }
      trajfile <- subset(trajfile,select = -c(trajectory))
      frame_tobind <<- trajfile
      #assign(paste0(site_names[i],"_",years[y]), trajfile, envir = parent.frame())
      #assign("frame_tobind", trajfile, envir = a)
      #assign("frame_tobind", trajfile, envir = parent.frame())
      message(paste0(site_names[i]," ",years[y]," imported"))
    } else {
      # import each year for that year.
      trajfile <- read.csv(paste0(data_directory,site_names[i],"-",years[y],"-",import_dates[y],"_72hr1TPD2000m.csv"), header = TRUE)
      if(as.numeric(substr(trajfile$year[1], 1, 1)) == 2){
        # This means the year is busted. Subtract 100 years.
        trajfile$year <- trajfile$year-100
      }
      trajfile <- subset(trajfile,select = -c(trajectory))
      #assign(paste0(site_names[i],"_",years[y]), trajfile, envir = parent.frame())
      newframe <- rbind(frame_tobind,trajfile)
      frame_tobind <- newframe
      #assign("frame_tobind", newframe, envir = parent.frame())
      message(paste0(site_names[i]," ",years[y]," imported"))
    }
    # now make overall df with trajectory identifier
    #rm(frame_tobind, envir = parent.frame())
  }
  # Lat/long adjustments. This replaces negative (West) lon values with 180+(diff)
  frame_tobind <- adjust_longitude(frame_tobind)
  frame_tobind <- Add_traj_identifier(frame_tobind, ntraj_1 = TRUE)
  frame_tobind <- subset(frame_tobind, select = -c(receptor,pressure,date.inc,diffs,date.inc))
  assign(paste0(site_names[i],"_72hr1TPD2000m_Reanalysis"),frame_tobind, envir = parent.frame())
  message("...","\n",site_names[i]," imported")
}
# export
write.csv(PL_72hr1TPD2000m_Reanalysis,paste0(export_dir,"PL_72hr1TPD2000m_Reanalysis.csv"), row.names = FALSE)
write.csv(SC_72hr1TPD2000m_Reanalysis,paste0(export_dir,"SC_72hr1TPD2000m_Reanalysis.csv"), row.names = FALSE)
# changes
PL_72hr1TPD2000m_Reanalysis <- adjust_longitude(PL_72hr1TPD2000m_Reanalysis)
SC_72hr1TPD2000m_Reanalysis <- adjust_longitude(SC_72hr1TPD2000m_Reanalysis)

##### OLD CODE: Endpoint density testing #####

SC_72hr1TPD2000m_Reanalysis$starting_month <- as.numeric(substr(SC_72hr1TPD2000m_Reanalysis$date.start, 6, 7))
Test2 <- SC_72hr1TPD2000m_Reanalysis[SC_72hr1TPD2000m_Reanalysis$hour.inc == -72,]
SC_Endpoint_densities <- ggplot() +
  EastAus_Full_Basemap +
  geom_point(data = Test2, aes(x = lon, y = lat, group = as.factor(starting_month)),fill = "white", shape = 21, colour = "black", size = 0.5, alpha = 0.5) +
  stat_density2d(
    data = Test2, aes(x = lon, y = lat, group = as.factor(starting_month)), colour = "black", bins = 8, size = 1, geom = "contour", alpha = 0.8) +
  geom_map(data = EastAus_Data, map = EastAus_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) +
  labs(caption = "MH 18/03/21") 

anim = SC_Endpoint_densities +
  # transition_reveal(along = month)
  transition_states(starting_month) +
  ease_aes('linear') +
  ggtitle("starting_month: {closest_state}")
anim_save("SC_draft_animation.gif", anim)

##### OLD CODE: Looking at 1962 #####

SC_72hr1TPD2000m_Reanalysis$starting_year <- as.numeric(substr(SC_72hr1TPD2000m_Reanalysis$date.start, 1, 4))

png(filename = paste0(export_dir,"SC_Summer_Endpoints_density.png"),units = "cm", height = 16, width = 15, res = 300)
ggplot() +
  EastAus_Full_Basemap +
  stat_density2d(
    data = SC_SummerData_Endpoints, aes(x = lon, y = lat, group = as.factor(starting_month), colour = as.factor(starting_month)), size = 0.5, geom = "contour", alpha = 0.8) +
  geom_map(data = EastAus_Data, map = EastAus_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) 
dev.off()

# Rainfall data for 1962
BOM_SC_rainfall_data_1962 <- BOM_SC_rainfall_data[BOM_SC_rainfall_data$Year == 1962,]
BOM_PLB_rainfall_1962 <- BOM_PLB_rainfall[BOM_PLB_rainfall$Year == 1962,]
# Calculating smoothing averages 
SC_index <- which(BOM_SC_rainfall_data$Year == 1961 | BOM_SC_rainfall_data$Year == 1962 | BOM_SC_rainfall_data$Year == 1963)
SC_1962_extended <- BOM_SC_rainfall_data[SC_index,]
SC_1962_extended$smth10p <- forecast::ma(SC_1962_extended$Rainfall.amount..millimetres.,10)
BOM_SC_rainfall_data_1962 <- SC_1962_extended[SC_1962_extended$Year == 1962,]
BOM_SC_rainfall_data_1962$rnum <- seq.int(nrow(BOM_SC_rainfall_data_1962))
PLB_index <- which(BOM_PLB_rainfall$Year == 1961 | BOM_PLB_rainfall$Year == 1962 | BOM_PLB_rainfall$Year == 1963)
PLB_1962_extended <- BOM_PLB_rainfall[PLB_index,]
PLB_1962_extended$smth10p <- forecast::ma(PLB_1962_extended$Rainfall.amount..millimetres.,10)
BOM_PLB_rainfall_1962 <- PLB_1962_extended[PLB_1962_extended$Year == 1962,]
BOM_PLB_rainfall_1962$rnum <- seq.int(nrow(BOM_PLB_rainfall_1962))


plotthemes <- list(
  scale_x_continuous(expand = c(0,0)),
  scale_y_continuous(expand = c(0,0),limits = c(0, 40)),
  theme_classic(),
  theme(
    text = element_text(size = 8),
    plot.margin = unit(c(t = -7, r = -5, b = -7, l = -5), "pt")),
  labs(title="", x="", y="")
)

lsize = 0.5
# Rainfall plots
SC_1 <- ggplot() +
  geom_line(data = BOM_SC_rainfall_data_1962, aes(x = rnum, y = Rainfall.amount..millimetres.), colour = "darkblue", size = lsize) +
  plotthemes
SC_2 <- ggplot() +
  geom_line(data = BOM_SC_rainfall_data_1962, aes(x = rnum, y = smth10p), colour = "darkblue", size = lsize) +
  plotthemes
PLB_1 <- ggplot() +
  geom_line(data = BOM_PLB_rainfall_1962, aes(x = rnum, y = Rainfall.amount..millimetres.), colour = "darkblue", size = lsize) +
  plotthemes
PLB_2 <- ggplot() +
  geom_line(data = BOM_PLB_rainfall_1962, aes(x = rnum, y = smth10p), colour = "darkblue", size = lsize) +
  plotthemes

png("1962_rainfall_plots.png",units = "cm", height = 8, width = 16, res = 300)
cowplot::plot_grid(SC_1,SC_2,PLB_1,PLB_2,
                   nrow = 4)
dev.off()

# total rainfall figures
total_rainfall_SC <- sum(BOM_SC_rainfall_data_1962$Rainfall.amount..millimetres.)
total_rainfall_PLB_1962 <- sum(BOM_PLB_rainfall_1962$Rainfall.amount..millimetres.)

# plots for 1962 - summer vs winter.

# parse both datasets for starting years 
SC_72hr1TPD2000m_Reanalysis$starting_year <- as.numeric(substr(SC_72hr1TPD2000m_Reanalysis$date.start, 1, 4))
SC_72hr1TPD2000m_Reanalysis$starting_month <- as.numeric(substr(SC_72hr1TPD2000m_Reanalysis$date.start, 6, 7))
PL_72hr1TPD2000m_Reanalysis$starting_year <- as.numeric(substr(PL_72hr1TPD2000m_Reanalysis$date.start, 1, 4))
PL_72hr1TPD2000m_Reanalysis$starting_month <- as.numeric(substr(PL_72hr1TPD2000m_Reanalysis$date.start, 6, 7))

# summer trajectories, both sites
SC_1962 <- SC_72hr1TPD2000m_Reanalysis[SC_72hr1TPD2000m_Reanalysis$starting_year == 1962,]
SC_1962 <- subset(SC_1962, select = -c(trajectory))
SC_1962 <- Add_traj_identifier(SC_1962, ntraj_1 = TRUE)

PL_1962 <- SC_72hr1TPD2000m_Reanalysis[PL_72hr1TPD2000m_Reanalysis$starting_year == 1962,]
PL_1962 <- subset(PL_1962, select = -c(trajectory))
PL_1962 <- Add_traj_identifier(PL_1962, ntraj_1 = TRUE)



# All trajectories together
# SC
png("1962_spaghetti_SC.png",units = "cm", height = 16, width = 15, res = 300)
ggplot() +
  EastAus_Full_Basemap +
  geom_path(data = SC_1962, aes(x = lon, y = lat, colour = as.factor(starting_month), group = trajectory), shape = 21,  size = 1, alpha = 0.7) +
  geom_map(data = EastAus_Data, map = EastAus_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) 
dev.off()
# PL
png("1962_spaghetti_PL.png",units = "cm", height = 16, width = 15, res = 300)
ggplot() +
  EastAus_Full_Basemap +
  geom_path(data = PL_1962, aes(x = lon, y = lat, colour = as.factor(starting_month), group = trajectory), shape = 21,  size = 1, alpha = 0.7) +
  geom_map(data = EastAus_Data, map = EastAus_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) 
dev.off()

# SUMMER
SC_1962_Summer <- SC_1962[which(SC_1962$starting_month == 12 | SC_1962$starting_month == 2 | SC_1962$starting_month == 1),]
PL_1962_Summer <- PL_1962[which(PL_1962$starting_month == 12 | PL_1962$starting_month == 2 | PL_1962$starting_month == 1),]
png("1962_spaghetti_SC_summer.png",units = "cm", height = 16, width = 15, res = 300)
ggplot() +
  EastAus_Full_Basemap +
  geom_path(data = SC_1962_Summer, aes(x = lon, y = lat, colour = as.factor(starting_month)), shape = 21,  size = 1, alpha = 0.7) +
  geom_map(data = EastAus_Data, map = EastAus_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) 
dev.off()

png("1962_spaghetti_PL_summer.png",units = "cm", height = 16, width = 15, res = 300)
ggplot() +
  EastAus_Full_Basemap +
  geom_point(data = PL_1962_Summer, aes(x = lon, y = lat, colour = as.factor(starting_month)), shape = 21,  size = 1, alpha = 0.7) +
  geom_map(data = EastAus_Data, map = EastAus_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) 
dev.off()

# Winter
SC_1962_Winter <- SC_1962[which(SC_1962$starting_month == 5 | SC_1962$starting_month == 6 | SC_1962$starting_month == 7),]
PL_1962_Winter <- PL_1962[which(PL_1962$starting_month == 5 | PL_1962$starting_month == 6 | PL_1962$starting_month == 7),]

SC_1962_Winter <- SC_1962[which(SC_1962$starting_month == 12 | SC_1962$starting_month == 2 | SC_1962$starting_month == 1),]
PL_1962_Winter <- PL_1962[which(PL_1962$starting_month == 12 | PL_1962$starting_month == 2 | PL_1962$starting_month == 1),]
png("1962_spaghetti_SC_Winter.png",units = "cm", height = 16, width = 15, res = 300)
ggplot() +
  EastAus_Full_Basemap +
  geom_point(data = SC_1962_Winter, aes(x = lon, y = lat), colour = 'black',  size = 1, alpha = 0.7) +
  geom_map(data = EastAus_Data, map = EastAus_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) 
dev.off()

png("1962_spaghetti_PL_Winter.png",units = "cm", height = 16, width = 15, res = 300)
ggplot() +
  EastAus_Full_Basemap +
  geom_point(data = PL_1962_Winter, aes(x = lon, y = lat),  colour = 'black', size = 1, alpha = 0.7) +
  geom_map(data = EastAus_Data, map = EastAus_Data,
           aes(x = long, y = lat, group = group, map_id = region),
           fill = "transparent", colour = "grey8", size = 0.2) 
dev.off()



geom_path(data = PH_72hr_testdata, aes(x = lon, y = lat, color = as.factor(trajectory), group = trajectory), size = 1, alpha = 1) +
  

# Can the trajectories tell us about why SC recieved less rainfall than point lookout? 

# MONTHLY AN


  
##### OLD CODE: GDAS 1 importing #####

data_directory_gdas1 <- "C:/Users/matth/Desktop/University Files/mh phd 2020/grants and collaborations/JTibby via HCadd HYSPLIT/data/GDAS1/"
# Set params
years <- seq(1950,2022,1)
sites_it <- vector(mode = "list", length = 2)
years_it <- vector(mode = "list", length = length(years))
site_names <- c("SC","PL")
dates_run <- c("2024-08-26")
filename_years_list <- vector(mode = 'list', length = length(years))
filename_years_list[c(1:length(years))] <- years
met_type = c("GDAS1")
# dates run
datesrun_list_PL <- vector(mode = 'list', length = length(years))
datesrun_list_PL[c(1:length(years))] <- dates_run[1]
datesrun_list_SC <- vector(mode = 'list', length = length(years))
datesrun_list_SC[c(1:length(years))] <- dates_run[1]
for(i in seq_along(sites_it)){
  message("Importing ", site_names[i]," ",met_type," trajectory data")
  for(y in seq_along(years_it)){
    # iterate along years
    if(i == 1){
      # This is PL; use PL dates
      import_dates <- datesrun_list_SC
    } else if(i == 2){
      import_dates <- datesrun_list_PL
    }
    if(y == 1){
      # import each year for that year.
      trajfile <- read.csv(paste0(data_directory_gdas1,site_names[i],"-",years[y],"-",import_dates[y],"_72hr1TPD2000m.csv"), header = TRUE)
      date_start_year <- as.numeric(substr(trajfile$date.start[1], 1, 4))
      year_start <- as.numeric(trajfile$year[1])
      if(date_start_year != year_start){
        # This means the year is busted. Subtract 100 years.
        trajfile$year <- as.numeric(substr(trajfile$date.inc, 1, 4))
      }
      trajfile <- subset(trajfile,select = -c(trajectory))
      frame_tobind <<- trajfile
      #assign(paste0(site_names[i],"_",years[y]), trajfile, envir = parent.frame())
      #assign("frame_tobind", trajfile, envir = a)
      #assign("frame_tobind", trajfile, envir = parent.frame())
      message(paste0(site_names[i]," ",years[y]," imported"))
    } else {
      # import each year for that year.
      trajfile <- read.csv(paste0(data_directory_gdas1,site_names[i],"-",years[y],"-",import_dates[y],"_72hr1TPD2000m.csv"), header = TRUE)
      date_start_year <- as.numeric(substr(trajfile$date.start[1], 1, 4))
      year_start <- as.numeric(trajfile$year[1])
      if(date_start_year != year_start){
        # This means the year is busted. Subtract 100 years.
        trajfile$year <- as.numeric(substr(trajfile$date.inc, 1, 4))
      }
      trajfile <- subset(trajfile,select = -c(trajectory))
      #assign(paste0(site_names[i],"_",years[y]), trajfile, envir = parent.frame())
      newframe <- rbind(frame_tobind,trajfile)
      frame_tobind <- newframe
      #assign("frame_tobind", newframe, envir = parent.frame())
      message(paste0(site_names[i]," ",years[y]," imported"))
    }
    # now make overall df with trajectory identifier
    #rm(frame_tobind, envir = parent.frame())
  }
  # Lat/long adjustments. This replaces negative (West) lon values with 180+(diff)
  frame_tobind <- adjust_longitude(frame_tobind)
  frame_tobind <- Add_traj_identifier(frame_tobind, ntraj_1 = TRUE)
  frame_tobind <- subset(frame_tobind, select = -c(receptor,pressure,date.inc,diffs,date.inc))
  assign(paste0(site_names[i],"_72hr1TPD2000m","_",met_type),frame_tobind, envir = parent.frame())
  message("...","\n",site_names[i]," imported")
}

# export
write.csv(PL_72hr1TPD2000m_GDAS1,paste0(export_dir,"PL_72hr1TPD2000m_GDAS1.csv"), row.names = FALSE)
write.csv(SC_72hr1TPD2000m_GDAS1,paste0(export_dir,"SC_72hr1TPD2000m_GDAS1.csv"), row.names = FALSE)
# changes
PL_72hr1TPD2000m_GDAS1 <- adjust_longitude(PL_72hr1TPD2000m_GDAS1)
SC_72hr1TPD2000m_GDAS1 <- adjust_longitude(SC_72hr1TPD2000m_GDAS1)

