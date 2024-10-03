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

