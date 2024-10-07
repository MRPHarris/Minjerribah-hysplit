# Functions for K'gari and Minjerribah HYSPLIT work

# Last updated 2024-10-03
# Matt Harris
# m.harris@gns.cri.nz
# https://www.github.com/MRPHarris

# Written and run in:
#   Rstudio v2024.04.2
#   R v4.4.0
#   Windows 10 (Education)

# To be sourced in scripts/notebook

# For splitr endpoints read in with trajectory_read. Not used; here for posterity (used at some prior step in the workflow - see the 'runmodels' or 'analysis' scripts).
convert_openair <- function(x) {
  drop_columns_openair = c(
    "lat_i","lon_i","theta","rainfall","mixdepth","rh","sp_humidity",
    "h2o_mixrate","terr_msl","sun_flux", "air_temp"
  )
  x <- x[ , !(names(x) %in% drop_columns_openair)]
  x$year = 2000 + x$year
  x$receptor = 1
  x <- x %>% rename(hour.inc = hour_along, date = traj_dt_i, date2 = traj_dt)
}

# For the exported CSVs. Used before clustering.
format_for_openair <- function(my_trajectories){
  # my_trajectories = SC_100mmRainfall_Trajectories
  new_traj <- my_trajectories %>%
    mutate(receptor = 1) %>%
    mutate(pressure = NA) %>%
    dplyr::select(-c(trajectory)) %>%
    mutate(date.start = as.Date(date.start)) %>%
    mutate(date = paste(date.start,"00:00:00")) %>%
    mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>%
    mutate(date2 = date + (hour.inc*3600)) %>%
    # mutate(date = format(date, format = "%Y-%m-%d %H:%M:%S")) %>%
    # mutate(date = ymd_hms(date)) %>%
    dplyr::select(receptor,year,month,day,hour,hour.inc,lat,lon,height,pressure,date2,date)
  return(new_traj)
}

# For rainfall-trajectory date matching. Adds similar starting date YYYY-MM-DD
ymd_similar <- function(df){
  new_df_temp <- df
  new_df <- df
  new_df_temp$month_pad <- stringr::str_pad(new_df_temp$Month,2,pad = "0")
  new_df_temp$day_pad <- stringr::str_pad(new_df_temp$Day,2,pad = "0")
  new_df$date_yyyymmdd <- paste0(new_df$Year,"-",new_df_temp$month_pad,"-",new_df_temp$day_pad)
  new_df
}

# Calculate bearings from cluster termination points to specified point. Used to offset plot labels based on angle to sites.
clus_bearings <- function(cluster_results,
                          target_point){
  cluster_tpoints = cluster_results$data$results %>% 
    filter(hour.inc == -72) %>% 
    ungroup()
  clus_tpoint_data = cluster_tpoints %>% 
    dplyr::select(c(cluster,lat,lon, freq)) %>%
    mutate(bearing = NA)
  plist <- vector('list', length = nrow(clus_tpoint_data)) 
  for(p in seq_along(plist)){
    this_lon = clus_tpoint_data$lon[p]
    this_lat = clus_tpoint_data$lat[p]
    clus_tpoint_data$bearing[p] = ((bearing(target_point, c(this_lon,this_lat)) + 360) %% 360)
  }
  clus_tpoint_data
}

# Generate a sequence of dates back from a supplied date and a duration of the sequence in days. Recursion for multiple dates (returns a sequence of sequences)
date_seq <- function(end_date,back_duration,include_end = TRUE){
  # Test vars
  # end_date = PL_100mm_g1d$date_yyyymmdd
  # back_duration = PL_100mm_g1d$Period.over.which.rainfall.was.measured..days. %>% as.numeric()
  # include_end = TRUE
  # Assumes the current day is included in the measurement day period (back_duration)
  if(length(end_date) > 1){
    # Multiples! Recurse.
    if(length(end_date) != length(back_duration)){
      stop("end dates and sequence durations are not the same length.")
    }
    it_list <- vector('list', length(end_date))
    for(i in seq_along(it_list)){
      # message(i)
      it_list[[i]] <- date_seq(end_date = end_date[i],
                               back_duration = back_duration[i],
                               include_end = include_end)
    }
    sequence <- Reduce('c',it_list)
    sequence
  } else {
    # One sequence; no recursion.
    # Ensure var is lubridate ymd
    end_date = ymd(end_date)
    # Tweak duration 
    if(include_end){
      back_duration = back_duration - 1
    }
    # Do the seq
    first_date = end_date - back_duration
    sequence = seq(ymd(first_date),ymd(end_date),by = "1 day")
    sequence
  }
}

# This uses various vars in the rainfall calculations to distinguish rainfall events. Relies on event type (multi-day, single day) and differences in dates.
# long and lazy by-row list iteration 
event_identifier <- function(frame_100mm){
  event_ids <- vector('list', length = nrow(frame_100mm))
  for(r in seq_along(event_ids)){
    if(r == 1){
      event_ids[[r]] = 1
    } else {
      # flags
      this_flag = frame_100mm$event_flag[r]
      prev_flag = frame_100mm$event_flag[r-1]
      # dates
      this_date = frame_100mm$date_yyyymmdd[r]
      prev_date = frame_100mm$date_yyyymmdd[r-1]
      # date diffs 
      diff_date = frame_100mm$date_diff[r]
      # Now, conditions.
      if(this_flag == 1 && prev_flag == 1){
        # Both individual events! Add 1 and move on
        event_ids[[r]] = event_ids[[r-1]] + 1
      } else if(this_flag != prev_flag){
        # Change of event type (individual to multi-day or visa versa)
        event_ids[[r]] = event_ids[[r-1]] + 1
      } else if(this_flag == 2 && prev_flag == 2){
        # Both multi-day. But, are they the same event? Check the date diff.
        if(diff_date == 1){
          # same event.
          event_ids[[r]] = event_ids[[r-1]]
        } else {
          # different event.
          event_ids[[r]] = event_ids[[r-1]] + 1
        }
      }
    } 
  }
  event_ids <- unlist(event_ids)
  event_ids # pipe this when using fn
}

# Fixes incorrect years in HYSPLIT output frames. Some years are erroneously returned
# as being in the second half of the 21st century. First digit substitution error somewhere.
# Clumsy fn to fix.
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

# Add a unique trajectory identifier. Two methods depending on the ntraj_1 var. I forget
# why this was a necessary inclusion. 
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
