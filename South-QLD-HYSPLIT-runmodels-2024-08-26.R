# Examining precipiation-linked airmass changes in South-Eastern Queensland: Windows 7 version # 

# Matt Harris, GNS Science
# Last updated 26/08/24
# Contact: m.harris@gns.cri.nz

# This version of the script is modified for the Chronos lab computer at UNSW, Sydney,
#     which runs on Windows 7. To run on Windows 10, simply replace my altered 
#     hysplit_trajectory_mod() function with hysplit_trajectory().

# Note in 2024: The bugs in the splitr package that necessitated the rewrite of the hysplit_trajectory
#     function have now been fixed. You should be able to run this script using splitr without 
#     all the extra functions in this script.

# Written and run in:
#   Rstudio v1.2.1335
#   R v4.0.2
#   Windows 7 (Professional) 

# !!!! UPDATE 'last updated' FIELD ON EACH USE !!!! #

# Refer to Master script for code snippets + notes. 

##### EXTERNAL RESOURCES #####

# R: https://www.r-project.org/
# Rstudio: https://rstudio.com/products/rstudio/download/
# HYSPLIT: https://www.ready.noaa.gov/HYSPLIT.php

# Package manuals:
#     SplitR:               https://rdrr.io/github/rich-iannone/SplitR/
#     openair (CRAN):       https://cran.r-project.org/web/packages/openair/index.html
#     there is a full version of the openair manual (w/ appendix) on the project website.
#     mapproject used for openair plotting functions (CRAN):    https://cran.r-project.org/web/packages/mapproj/index.html

##### Setup [ALWAYS RUN] #####

# Load pacman for pacman::p_load() 
library(pacman)

# Load packages
pacman::p_load(splitr, openair, lubridate, magrittr, tibble, dplyr, R.utils)

# Package list for code sitting in the DRAFT CODE sections. Needs cleaning.
# pacman::p_load(splitr, openair, lubridate, here, magrittr, tibble, dplyr, maps, esd, gt, R.utils, ggplot2,
#               raster, rgdal, maptools, rgeos, autoimage, akima, ggmap, mapproj)

# Set wd
setwd("C:/hysplit/working")
# Set the directory where meteorological data is stored. Update as needed.
# met_dir_g <- "G:/DATA/HYSPLIT Files/GDAS1 Meteorological Data/"
met_dir_e <- "E:/DATA/HYSPLIT Files/Reanalysis Meteorological Data/"


# Site 1: Point Lookout Weather Station, North Stradbroke Island
PL_lat <- -27.44
PL_lon <- 153.55
# Site 2: Sandy Cape Weather Station, Fraser Island
SC_lat <- -24.73
SC_lon <- 153.21

##### Utility functions incl. new trajectory function #####

# As of https://github.com/rich-iannone/splitr/commit/44851122514e1296291f0e9b082326717549715f, these functions should
#     no longer be needed. You can just run the code in the subsequent sections with hysplit_trajectory() instead of hysplit_trajectory_mod().
#     I've left the script as-is for the sake of reproducibility.

# Modified trajectory file. All this does is use the system's local time settings, rather than "en_US.UTF-8".
hysplit_trajectory_mod <- function (lat = 49.263, lon = -123.25, height = 50, duration = 24, 
                                    days = NULL, daily_hours = 0, direction = "forward", 
                                    met_type = "reanalysis", vert_motion = 0, model_height = 20000, 
                                    extended_met = FALSE, config = NULL, ascdata = NULL, traj_name = NULL, 
                                    binary_path = NULL, met_dir = NULL, exec_dir = NULL, clean_up = FALSE) 
{
  #Source the two new functions
  if (is.null(exec_dir)) 
    exec_dir <- getwd()
  if (is.null(met_dir)) 
    met_dir <- getwd()
  binary_name <- "hyts_std"
  binary_path <-
    system.file(
      file.path("win", paste0(binary_name, ".exe")),
      package = "splitr"
    )
  system_type <- get_os()
  if (is.null(traj_name)) {
    folder_name <- paste0("traj-", format(Sys.time(), 
                                          "%Y-%m-%d-%H-%M-%S"))
  }
  else if (!is.null(traj_name)) {
    folder_name <- traj_name
  }
  if (is.null(config)) {
    config_list <- set_config()
  }
  else {
    config_list <- config
  }
  if (is.null(ascdata)) {
    ascdata_list <- set_ascdata()
  }
  else {
    ascdata_list <- ascdata
  }
  if (isTRUE(extended_met)) {
    tm_names <- config_list %>% names() %>% vapply(FUN.VALUE = logical(1), 
                                                   USE.NAMES = FALSE, FUN = function(y) y %>% tidy_grepl("^tm_")) %>% 
      which()
    config_list[tm_names] <- 1
  }
  config_list %>% write_config_list(dir = exec_dir)
  ascdata_list %>% write_ascdata_list(dir = exec_dir)
  if (length(lat) != length(lon)) {
    stop("The coordinate vectors are not the same length.", 
         call. = FALSE)
  }
  download_met_files_mod <- function(met_type,
                                     days,
                                     duration,
                                     direction,
                                     met_dir) {
    if (met_type == "gdas1") {
      get_met_gdas1_mod <- function(days,
                                    duration,
                                    direction,
                                    path_met_files) {
        
        # Determine the minimum date (as a `Date`) for the model run
        if (direction == "backward") {
          min_date <- 
            (lubridate::as_date(days[1]) - (duration / 24)) %>%
            lubridate::floor_date(unit = "day")
        } else if (direction == "forward") {
          min_date <- 
            (lubridate::as_date(days[1])) %>%
            lubridate::floor_date(unit = "day")
        }
        
        # Determine the maximum date (as a `Date`) for the model run
        if (direction == "backward") {
          max_date <- 
            (lubridate::as_date(days[length(days)])) %>%
            lubridate::floor_date(unit = "day")
        } else if (direction == "forward") {
          max_date <- 
            (lubridate::as_date(days[length(days)]) + (duration / 24)) %>%
            lubridate::floor_date(unit = "day")
        }
        
        met_days <- 
          seq(min_date, max_date, by = "1 day") %>% 
          lubridate::day()
        
        month_names <- 
          seq(min_date, max_date, by = "1 day") %>%
          lubridate::month(label = TRUE, abbr = TRUE, locale = Sys.getlocale("LC_TIME"))  %>%
          as.character() %>%
          tolower()
        
        # This was the previous version of the above block, and threw up an error. 
        #month_names <- 
        #  seq(min_date, max_date, by = "1 day") %>%
        #  lubridate::month(label = TRUE, abbr = TRUE, locale = "en_US.UTF-8")  %>%
        #  as.character() %>%
        #  tolower()
        
        met_years <- 
          seq(min_date, max_date, by = "1 day") %>%
          lubridate::year() %>% 
          substr(3, 4)
        
        # Only consider the weeks of the month we need:
        #.w1 - days 1-7
        #.w2 - days 8-14
        #.w3 - days 15-21
        #.w4 - days 22-28
        #.w5 - days 29 - rest of the month 
        
        met_week <- ceiling(met_days / 7)
        
        files <- paste0("gdas1.", month_names, met_years, ".w", met_week) %>% unique()
        
        get_met_files(
          files = files,
          path_met_files = path_met_files,
          ftp_dir = "ftp://arlftp.arlhq.noaa.gov/archives/gdas1"
        )
        
      }
      met_files <-
        get_met_gdas1_mod(
          days = days,
          duration = duration,
          direction = direction,
          path_met_files = met_dir
        )
    }
    
    if (met_type == "gdas0.5") {
      
      met_files <-
        get_met_gdas0p5(
          days = days,
          duration = duration,
          direction = direction,
          path_met_files = met_dir
        )
    }
    
    if (met_type == "gfs0.25") {
      
      met_files <-
        get_met_gfs0p25(
          days = days,
          duration = duration,
          direction = direction,
          path_met_files = met_dir
        )
    }
    
    if (met_type == "reanalysis") {
      
      met_files <-
        get_met_reanalysis(
          days = days,
          duration = duration,
          direction = direction,
          path_met_files = met_dir
        )
    }
    
    if (met_type == "nam12") {
      
      met_files <-
        get_met_nam12(
          days = days,
          duration = duration,
          direction = direction,
          path_met_files = met_dir
        )
    }
    
    if (met_type == "narr") {
      
      met_files <-
        get_met_narr(
          days = days,
          duration = duration,
          direction = direction,
          path_met_files = met_dir
        )
    }
    
    if (met_type == "era5") {
      
      met_files <-
        get_met_era5(
          days = days,
          duration = duration,
          direction = direction,
          path_met_files = met_dir
        )
    }
    
    met_files
  }
  met_files <- download_met_files_mod(met_type = met_type, days = days, 
                                      duration = duration, direction = direction, met_dir = met_dir)
  receptors_tbl <- dplyr::tibble(lat = lat, lon = lon) %>% 
    dplyr::group_by(lat, lon) %>% tidyr::expand(height = height) %>% 
    dplyr::ungroup() %>% dplyr::mutate(receptor = dplyr::row_number()) %>% 
    dplyr::select(receptor, dplyr::everything())
  receptors <- seq(nrow(receptors_tbl))
  ensemble_tbl <- dplyr::tibble()
  recep_file_path_stack <- c()
  for (receptor in receptors) {
    receptor_vals <- get_receptor_values(receptors_tbl = receptors_tbl, 
                                         receptor_i = receptor)
    receptor_i <- receptor_vals$receptor
    lat_i <- receptor_vals$lat
    lon_i <- receptor_vals$lon
    height_i <- receptor_vals$height
    list_run_days <- days %>% as.character()
    trajectory_files <- c()
    for (i in seq(list_run_days)) {
      start_year_GMT <- to_short_year(list_run_days[i])
      start_month_GMT <- to_short_month(list_run_days[i])
      start_day_GMT <- to_short_day(list_run_days[i])
      if (inherits(daily_hours, "numeric")) {
        daily_hours <- formatC(sort(daily_hours), width = 2, 
                               flag = 0)
      }
      for (j in daily_hours) {
        start_hour_GMT <- j
        if (start_year_GMT > 40) {
          full_year_GMT <- paste0("19", start_year_GMT)
        }
        else {
          full_year_GMT <- paste0("20", start_year_GMT)
        }
        output_filename <- get_traj_output_filename(traj_name = traj_name, 
                                                    site = receptor_i, direction = direction, year = start_year_GMT, 
                                                    month = start_month_GMT, day = start_day_GMT, 
                                                    hour = start_hour_GMT, lat = lat_i, lon = lon_i, 
                                                    height = height_i, duration = duration)
        trajectory_files <- c(trajectory_files, output_filename)
        write_traj_control_file(start_year_GMT = start_year_GMT, 
                                start_month_GMT = start_month_GMT, start_day_GMT = start_day_GMT, 
                                start_hour_GMT = start_hour_GMT, lat = lat_i, 
                                lon = lon_i, height = height_i, direction = direction, 
                                duration = duration, vert_motion = vert_motion, 
                                model_height = model_height, met_files = met_files, 
                                output_filename = output_filename, system_type = system_type, 
                                met_dir = met_dir, exec_dir = exec_dir)
        sys_cmd <- paste0("(cd \"", exec_dir, "\" && \"", 
                          binary_path, "\" ", to_null_dev(system_type = system_type), 
                          ")")
        execute_on_system(sys_cmd, system_type = system_type)
      }
    }
    recep_file_path <- file.path(exec_dir, receptor_i, folder_name)
    recep_file_path_stack <- c(recep_file_path_stack, file.path(exec_dir, 
                                                                receptor_i))
    if (!dir.exists(recep_file_path)) {
      dir.create(path = recep_file_path, recursive = TRUE)
    }
    file.copy(from = file.path(exec_dir, trajectory_files), 
              to = recep_file_path, copy.mode = TRUE)
    unlink(file.path(exec_dir, trajectory_files), force = TRUE)
    traj_tbl <- trajectory_read(output_folder = recep_file_path) %>% 
      dplyr::as_tibble() %>% dplyr::mutate(receptor = receptor_i, 
                                           lat_i = lat_i, lon_i = lon_i, height_i = height_i)
    ensemble_tbl <- ensemble_tbl %>% dplyr::bind_rows(traj_tbl)
  }
  if (clean_up) {
    unlink(file.path(exec_dir, traj_output_files()), force = TRUE)
    unlink(recep_file_path_stack, recursive = TRUE, force = TRUE)
  }
  ensemble_tbl <- ensemble_tbl %>% dplyr::select(-c(year, month, 
                                                    day, hour)) %>% dplyr::select(receptor, hour_along, traj_dt, 
                                                                                  lat, lon, height, traj_dt_i, lat_i, lon_i, height_i, 
                                                                                  dplyr::everything()) %>% dplyr::group_by(receptor, hour_along, 
                                                                                                                           traj_dt, traj_dt_i, lat_i, lon_i, height_i) %>% dplyr::slice(1) %>% 
    dplyr::ungroup()
  if (direction == "forward") {
    ensemble_tbl <- ensemble_tbl %>% dplyr::arrange(receptor, 
                                                    traj_dt_i)
  }
  else {
    ensemble_tbl <- ensemble_tbl %>% dplyr::arrange(receptor, 
                                                    traj_dt_i, dplyr::desc(hour_along))
  }
  ensemble_tbl %>% dplyr::right_join(ensemble_tbl %>% dplyr::select(receptor, 
                                                                    traj_dt_i, lat_i, lon_i, height_i) %>% dplyr::distinct() %>% 
                                       dplyr::mutate(run = dplyr::row_number()), by = c("receptor", 
                                                                                        "traj_dt_i", "lat_i", "lon_i", "height_i")) %>% 
    dplyr::select(run, dplyr::everything())
}

# nb binary_path was edited: the new trajectory command above essentially removes this function. 
set_binary_path <- function(binary_path,
                            binary_name) {
  
  # By default, binary names should be either:
  #  - hyts_std (trajectory models)
  #  - hycs_std (dispersion models)
  
  # If a user uses another binary name, the path to it should also be specified
  
  if (is.null(binary_path)) {
    
    system_os <- get_os()
    
    if (system_os == "mac") {
      binary_path <-
        system.file(
          file.path("osx", binary_name),
          package = "splitr"
        )
    }
    
    if (system_os == "unix") {
      binary_path <-
        system.file(
          file.path("linux-amd64", binary_name),
          package = "splitr"
        )
    }
    
    if (system_os == "win") {
      binary_path <-
        system.file(
          file.path("win", paste0(binary_name, ".exe")),
          package = "splitr"
        )
    }
  } else {
    binary_path <- paste0(binary_path, binary_name)
  }
  
  binary_path
}
#set_ascdata()
set_ascdata <- function(lat_lon_ll = c(-90.0, -180.0),
                        lat_lon_spacing = c(1.0, 1.0),
                        lat_lon_n = c(180, 360),
                        lu_category = 2,
                        roughness_l = 0.2,
                        data_dir = "'.'") {
  
  arg_names <- formals(set_ascdata) %>% names()
  arg_vals <- mget(arg_names)
  
  arg_vals$lat_lon_ll <- 
    paste0(arg_vals$lat_lon_ll[1], "  ", arg_vals$lat_lon_ll[2])
  
  arg_vals$lat_lon_spacing <- 
    paste0(arg_vals$lat_lon_spacing[1], "  ", arg_vals$lat_lon_spacing[2])
  
  arg_vals$lat_lon_n <- 
    paste0(arg_vals$lat_lon_n[1], "  ", arg_vals$lat_lon_n[2])
  
  arg_vals[!vapply(arg_vals, FUN = is.null, FUN.VALUE = logical(1))]
}
# write_config_list()
write_config_list <- function(config_list, dir) {
  
  paste0(
    "&SETUP\n",
    paste0(names(config_list), " = ", config_list, ",\n", collapse = ""),
    "/\n"
  ) %>%
    cat(file = file.path(dir, "SETUP.CFG"))
}
#write_ascdata_list()
write_ascdata_list <- function(ascdata_list, dir) {
  
  paste0(ascdata_list, "\n", collapse = "") %>%
    cat(file = file.path(dir, "ASCDATA.CFG"))
}
#download_met_files()
download_met_files <- function(met_type,
                               days,
                               duration,
                               direction,
                               met_dir) {
  
  if (met_type == "gdas1") {
    
    met_files <-
      get_met_gdas1(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  if (met_type == "gdas0.5") {
    
    met_files <-
      get_met_gdas0p5(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  if (met_type == "gfs0.25") {
    
    met_files <-
      get_met_gfs0p25(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  if (met_type == "reanalysis") {
    
    met_files <-
      get_met_reanalysis(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  if (met_type == "nam12") {
    
    met_files <-
      get_met_nam12(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  if (met_type == "narr") {
    
    met_files <-
      get_met_narr(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  if (met_type == "era5") {
    
    met_files <-
      get_met_era5(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  met_files
}
# get_receptor_values()
get_receptor_values <- function(receptors_tbl,
                                receptor_i) {
  
  receptors_tbl[receptor_i, ] %>% as.list()
}
#to_short_year()
to_short_year <- function(date) {
  
  date %>%
    lubridate::year() %>%
    as.character() %>%
    substr(3, 4)
}
#to_short_month()
to_short_month <- function(date) {
  
  formatC(
    date %>% lubridate::month(),
    width = 2, flag = "0"
  )
}
#to_short_day()
to_short_day <- function(date) {
  
  formatC(
    date %>% lubridate::day(),
    width = 2, flag = "0"
  )
}
#get_traj_output_filename()
get_traj_output_filename <- function(traj_name,
                                     site,
                                     direction,
                                     year,
                                     month,
                                     day,
                                     hour,
                                     lat,
                                     lon,
                                     height,
                                     duration) {
  
  paste0(
    "traj-",
    ifelse(is.null(traj_name), "", traj_name),
    "-",
    ifelse(direction == "backward", "bwd", "fwd"), "-",
    year, "-",
    month, "-",
    day, "-",
    hour, "-",
    site,
    "lat_", gsub("\\.", "p", as.character(lat)), "_",
    "lon_", gsub("\\.", "p", as.character(lon)), "-",
    "hgt_", height, "-",
    duration, "h"
  )
}
#write_traj_control_file()
write_traj_control_file <- function(start_year_GMT,
                                    start_month_GMT,
                                    start_day_GMT,
                                    start_hour_GMT,
                                    lat,
                                    lon,
                                    height,
                                    direction,
                                    duration,
                                    vert_motion,
                                    model_height,
                                    met_files,
                                    output_filename,
                                    system_type,
                                    met_dir,
                                    exec_dir) {
  
  paste0(
    start_year_GMT, " ", start_month_GMT, " ",
    start_day_GMT, " ", start_hour_GMT, "\n",
    "1\n",
    lat, " ", lon, " ", height, "\n",
    ifelse(direction == "backward", "-", ""), duration, "\n",
    vert_motion, "\n",
    model_height, "\n",
    length(met_files), "\n",
    paste0(met_dir, "/\n", met_files, collapse = "\n"), "\n",
    exec_dir, "/\n",
    output_filename, "\n"
  ) %>%
    cat(file = file.path(exec_dir, "CONTROL"), sep = "", append = FALSE)
}
#execute_on_system()
execute_on_system <- function(sys_cmd, system_type) {
  
  if (system_type %in% c("mac", "unix")) {
    system(sys_cmd)
  } else if (system_type == "win") {
    shell(sys_cmd)
  }
}
#to_null_dev()
to_null_dev <- function(system_type) {
  
  if (system_type %in% c("mac", "unix")) {
    null_dev <- ">> /dev/null 2>&1"
  } else if (system_type == "win") {
    null_dev <- "> NUL 2>&1"
  }
  
  null_dev
}
#tidy_grepl()
tidy_grepl <- function(x, pattern) {
  
  vapply(
    pattern,
    FUN = function(pattern) {
      grepl(pattern = pattern, x = x)
    },
    FUN.VALUE = logical(1),
    USE.NAMES = FALSE
  )
}
#get_os()
get_os <- function() {
  if (.Platform$OS.type == "windows") {
    return("win")
  } else if (Sys.info()["sysname"] == "Darwin") {
    return("mac")
  } else if (.Platform$OS.type == "unix") {
    return("unix")
  } else {
    stop("Unknown OS", call. = FALSE)
  }
}
#download_met_files
download_met_files <- function(met_type,
                               days,
                               duration,
                               direction,
                               met_dir) {
  
  if (met_type == "gdas1") {
    
    met_files <-
      get_met_gdas1(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  if (met_type == "gdas0.5") {
    
    met_files <-
      get_met_gdas0p5(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  if (met_type == "gfs0.25") {
    
    met_files <-
      get_met_gfs0p25(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  if (met_type == "reanalysis") {
    
    met_files <-
      get_met_reanalysis(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  if (met_type == "nam12") {
    
    met_files <-
      get_met_nam12(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  if (met_type == "narr") {
    
    met_files <-
      get_met_narr(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  if (met_type == "era5") {
    
    met_files <-
      get_met_era5(
        days = days,
        duration = duration,
        direction = direction,
        path_met_files = met_dir
      )
  }
  
  met_files
}
#get_met_gdas1
get_met_gdas1 <- function(days,
                          duration,
                          direction,
                          path_met_files) {
  
  # Determine the minimum date (as a `Date`) for the model run
  if (direction == "backward") {
    min_date <- 
      (lubridate::as_date(days[1]) - (duration / 24)) %>%
      lubridate::floor_date(unit = "day")
  } else if (direction == "forward") {
    min_date <- 
      (lubridate::as_date(days[1])) %>%
      lubridate::floor_date(unit = "day")
  }
  
  # Determine the maximum date (as a `Date`) for the model run
  if (direction == "backward") {
    max_date <- 
      (lubridate::as_date(days[length(days)])) %>%
      lubridate::floor_date(unit = "day")
  } else if (direction == "forward") {
    max_date <- 
      (lubridate::as_date(days[length(days)]) + (duration / 24)) %>%
      lubridate::floor_date(unit = "day")
  }
  
  met_days <- 
    seq(min_date, max_date, by = "1 day") %>% 
    lubridate::day()
  
  month_names <- 
    seq(min_date, max_date, by = "1 day") %>%
    lubridate::month(label = TRUE, abbr = TRUE, locale = Sys.getlocale("LC_TIME"))  %>%
    as.character() %>%
    tolower()
  
  #month_names <- 
  #  seq(min_date, max_date, by = "1 day") %>%
  #  lubridate::month(label = TRUE, abbr = TRUE, locale = "en_US.UTF-8")  %>%
  #  as.character() %>%
  #  tolower()
  
  
  met_years <- 
    seq(min_date, max_date, by = "1 day") %>%
    lubridate::year() %>% 
    substr(3, 4)
  
  # Only consider the weeks of the month we need:
  #.w1 - days 1-7
  #.w2 - days 8-14
  #.w3 - days 15-21
  #.w4 - days 22-28
  #.w5 - days 29 - rest of the month 
  
  met_week <- ceiling(met_days / 7)
  
  files <- paste0("gdas1.", month_names, met_years, ".w", met_week) %>% unique()
  
  get_met_files(
    files = files,
    path_met_files = path_met_files,
    ftp_dir = "ftp://arlftp.arlhq.noaa.gov/archives/gdas1"
  )
  
}
#get_met_files
get_met_files <- function(files, path_met_files, ftp_dir) {
  
  # Determine which met files are already locally available
  files_in_path <- list.files(path_met_files)
  
  # Download list of GFS0.25 met files by name
  if (!is.null(files)) {
    
    for (file in files) {
      
      if (!(file %in% files_in_path)) {
        
        downloader::download(
          url = file.path(ftp_dir, file),
          destfile = path.expand(file.path(path_met_files, file)),
          method = "auto",
          quiet = FALSE,
          mode = "wb",
          cacheOK = FALSE
        )
      }
    }
  }
  
  files
}



##### Updating trajectories to end of 2023 #####

## Section notes
# Updating this data in 2024 because tbh why didn't I run it to 2020 at least? 
# Runs now to go to end of 2023. 2023 was hanging for some reason, so I left it out.

## Directories
# met_dir_gdas <- "H:/DATA/HYSPLIT Files/GDAS1 Meteorological Data/"
met_dir_reanalysis <- "E:/DATA/HYSPLIT Files/Reanalysis Meteorological Data/" 
export_dir_years <- "C:/Users/matth/Desktop/University Files/mh phd 2020/grants and collaborations/JTibby via HCadd HYSPLIT/data/reanalysis/"


# Functions required (always)
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
Add_traj_identifier <- function(x, ntraj_1 = FALSE){
  # This function assumes that convert_openair() has been used on the dataset. 
  x_new <- as.data.frame(x)
  # Compute trajectory number. Starts at 1. 
  if(!isTRUE(ntraj_1)){
    x_new$start.time.minutes <- substr(x_new[,11],12,19)
    x_new$start.time.minutes <- 60*24*as.numeric(chron::times(x_new$start.time.minutes))
    x_new$trajectory <- cumsum(c(0, as.numeric(diff(x_new$start.time.minutes)) != 0)) + 1
  } else{
    x_new$diffs <- 1
    x_new$diffs[2:nrow(x_new)] <- diff(x_new$hour.inc) 
    x_new$trajectory <- cumsum(c(0,as.numeric(x_new$diffs[2:nrow(x_new)]) != -1)) + 1
  }
  #  x_new <<- as.data.frame(x)
  assign(paste0(deparse(substitute(x))), x_new, envir = parent.frame())
  #  rm(x_new, envir = parent.frame())
}
YSH_info <- function(){
  message("INFO ON FUNCTION: Yearly_Site_HYSPLIT()",
          "\n","------",
          "\n", "The Yearly_Site_HYSPLIT() function generates a yearly trajectory set for a given location somewhere in the world.",
          "\n","Read below for a rundown/reminder on the input parameters. You must have a working install of HYSPLIT.","\n","------")
  message("site_name      | name or identifier for the site/trajectory set. Will be present in output names. MUST BE ONE WORD in quotations, with less than 5 characters!!", "\n",
          "year           | the year for which you want to generate a set of trajectories.","\n",
          "site_lat       | the latitude of the target site.","\n",
          "site_lon       | the longitude of the target site.","\n",
          "traj_duration  | the duration, in hours, of each trajectory.","\n",
          "start_height   | the height of the trajectory release/endpoint, in metres. e.g. '50'","\n",
          "run_direction  | run model forwards or backwards? Forwards for standard trajectories, backwards for back-trajectories.","\n",
          "met_type       | what type of met data are you using? at the moment only gdas1 and reanalysis are recommended. Remove the associated if(){stop} statement to use other types.","\n",
          "ntraj_per_day  | number of trajectories per day. Must be a whole divisor of 24. They will be spread evenly across the day.", "\n",
          "met_dir        | path to the folder in which the met files are stored","\n",
          "output_folder  | path to the folder where the trajectories will be collated. Default is C:/hysplit/working/1/","\n","------", "\n",
          "The function requires the following packages: pacman, splitr, lubridate, tibble, dplyr, R.utils, rgdal, chron","\n","------")
}
# YSH_info()

# Here's the function. At the moment, the parts that read the outputs back into the workspace have been removed, pending further testing.
Yearly_Site_HYSPLIT_mod <- function(site_name, year, site_lat, site_lon, traj_duration, 
                                    start_height, direction, met_type, ntraj_per_day, met_dir, 
                                    output_folder = "C:/hysplit/working/1/", verbose = FALSE){
  library(pacman)
  pacman::p_load(splitr,lubridate,tibble,dplyr,R.utils,rgdal,chron)
  # Initial user start/stop prompt. Hash out if seeking to automate
  if(isTRUE(verbose)){
    PromptContinue <- if(interactive()){
      askYesNo("Generate yearly trajectories for the target site based on the supplied parameters? This could take a while!", 
               default = TRUE, prompts = getOption("askYesNo", gettext(c("Y/N/C"))))
    }
    if(!isTRUE(PromptContinue)){
      stop("User terminated function")}
  }
  # INPUT CHECKS
  #
  if(nchar(site_name) <= 5){
  } else{
    stop("Please shorten the site_name to 5 characters or less.")
  }
  if(direction == "forward" || direction == "backward"){
  } else{
    stop("Please enter a valid direction: either forward or backward")
  }
  if(met_type == "gdas1" || met_type == "reanalysis"){
  } else{
    stop("Please enter a valid met type: gdas1 or reanalysis")
  }
  if(ntraj_per_day == 1 || ntraj_per_day == 2 || ntraj_per_day == 3 || ntraj_per_day == 4 || ntraj_per_day == 6 || ntraj_per_day == 8 || ntraj_per_day == 10 || ntraj_per_day == 12){
  } else{
    stop("trajectories per day (ntraj_per_day) must be a whole divisor of 24")
  }
  if(ntraj_per_day <= 24){
  } else {
    stop("number of trajectories per day cannot be greater than 24!")
  }
  # Assign month vars. Function will iterate over these.
  month_init = c(1:12)
  days_in_months = c(31,28,31,30,31,30,31,31,30,31,30,31)
  if(year%%4 == 0 || year%%400 == 0){
    days_in_months[2] = 29} # leap year + century feb month adjustment
  # trajectories per day setting
  hours_inc <- seq(0,24,by = (24/ntraj_per_day))
  hours_increments <- hours_inc[1:(length(hours_inc)-1)]
  # Main loop! Runs HYSPLIT for every month.
  for(i in seq_along(month_init)){
    # from lubridate wrap sequence
    month_formatted <- formatC(month_init[i], width = 2, format = "d", flag = "0")
    start_date_day_formatted <- 1
    start_date_day_formatted <- formatC(start_date_day_formatted, width = 2, format = "d", flag = "0")
    start_date <- paste0(year,"-",month_formatted,"-",start_date_day_formatted)
    end_date <- paste0(year,"-",month_formatted,"-",days_in_months[i]) # to lubridate wrap sequence
    i_date <- Sys.Date() # iterative output name for each monthly trajectory bundle
    i_date <- as.character(as.Date(i_date, "%Y-%m-%d"), "%d%m%y")
    set_name <- paste0(site_name,"-",year,"-",i_date,"-YSH-",month_init[i])
    # HYSPLIT run
    hysplit_trajectory_mod(
      lat = site_lat,
      lon = site_lon,
      height = start_height,
      duration = traj_duration,
      days = seq(
        lubridate::ymd(start_date),
        lubridate::ymd(end_date),
        by = "1 day"
      ),
      daily_hours = hours_increments,
      direction = direction,
      met_type = met_type,
      extended_met = TRUE,
      clean_up = FALSE,
      met_dir = met_dir,
      traj_name = set_name
    )
    message(i,"/12")
  }
}

# This groups the trajectories by month, collates them, then exports as a .csv to the desigated output folder.
Export_YSH <- function(site_name, year, lon, lat, traj_duration, 
                       start_height, direction = "forwards", met_type = "gdas1", 
                       ntraj_per_day, met_dir, output_folder, save_dir, day = "today"){
  message("Make sure i_date is the correct day if trajectories were not run today!")
  # Lobotomised function
  month_init = c(1:12)
  days_in_months = c(31,28,31,30,31,30,31,31,30,31,30,31)
  if(year%%4 == 0 || year%%400 == 0){
    days_in_months[2] = 29} # leap year + century feb month adjustment
  # trajectories per day setting
  hours_inc <- seq(0,24,by = (24/ntraj_per_day))
  hours_increments <- hours_inc[1:(length(hours_inc)-1)]
  for(i in seq_along(month_init)){
    # from lubridate wrap sequence
    month_formatted <- formatC(month_init[i], width = 2, format = "d", flag = "0")
    start_date_day_formatted <- 1
    start_date_day_formatted <- formatC(start_date_day_formatted, width = 2, format = "d", flag = "0")
    start_date <- paste0(year,"-",month_formatted,"-",start_date_day_formatted)
    # to lubridate wrap sequence
    end_date <- paste0(year,"-",month_formatted,"-",days_in_months[i])
    # iterative output name for each monthly trajectory bundle
    i_date <- Sys.Date() # iterative output name for each monthly trajectory bundle
    i_date <- as.character(as.Date(i_date, "%Y-%m-%d"), "%d%m%y")
    set_name <- paste0(site_name,"-",year,"-",i_date,"-YSH-",month_init[i])
    # HYSPLIT run
    # Rename traj [i] to month 
    # assign(paste0("Trajset_", deparse(substitute(month_init[i]))), month_trajset)
    # Read trajectory output in from folder
    traj_reread <- trajectory_read(output_folder = paste0(output_folder,set_name))
    # Convert to desirable layout
    traj_converted <- convert_openair(traj_reread)
    assign(paste0("Trajset_converted_",month_init[i]), traj_converted, envir = parent.frame())
    message(i, "completed")
  }
  Yearly_trajset <- rbind(Trajset_converted_1,Trajset_converted_2,Trajset_converted_3,
                          Trajset_converted_4,Trajset_converted_5,Trajset_converted_6,
                          Trajset_converted_7,Trajset_converted_8,Trajset_converted_9,
                          Trajset_converted_10,Trajset_converted_11,Trajset_converted_12)
  # Formatting: added identifier, rename columns
  #
  if(ntraj_per_day == 1){
    traj1 = TRUE
  } else {
    traj1 = FALSE
  }
  Add_traj_identifier(Yearly_trajset,ntraj_1 = traj1)
  colnames(Yearly_trajset)[11]<- "date.inc"
  colnames(Yearly_trajset)[12] <- "date.start"
  # Export. THIS NEEDS WORK.
  save_dir = save_dir
  output_timestamp <- Sys.Date()
  file_output <- paste0(save_dir,site_name,"-",year,"-",output_timestamp,"_",traj_duration,"hr",ntraj_per_day,"TPD",start_height,"m.csv")
  write.csv(Yearly_trajset,file = file_output, row.names = FALSE)
}


## REANALYSIS: 1950:1999.
years <- seq(2000,2022,1)
# it_list <- vector(mode = "list", length = length(years))
# sites_lat <- c(SC_lat)
# sites_lon <- c(SC_lon)
# lat_list_qld <- vector(mode = "list", length = 1)
# lon_list_qld <- vector(mode = "list", length = 1)
# for(i in seq_along(lat_list_qld)){
#   lat_list_qld[[i]] <- sites_lat[i]
#   lon_list_qld[[i]] <- sites_lon[i]
# }
# names <- vector(mode = "list", length = length(1))
# names[[1]] <- "SC"

it_list <- vector(mode = "list", length = length(years))
sites_lat <- c(PL_lat,SC_lat)
sites_lon <- c(PL_lon,SC_lon)
lat_list_qld <- vector(mode = "list", length = 2)
lon_list_qld <- vector(mode = "list", length = 2)
for(i in seq_along(lat_list_qld)){
  lat_list_qld[[i]] <- sites_lat[i]
  lon_list_qld[[i]] <- sites_lon[i]
}
names <- vector(mode = "list", length = length(2))
names[[1]] <- "PL"
names[[2]] <- "SC"


# for(s in seq_along(lat_list_qld)){
  # Loop: years for site in init
  s = 2
  message("Starting site ",s,": ",names[[s]]," ",lat_list_qld[[s]],"S ",lon_list_qld[[s]],"W")
  for(y in seq_along(it_list)){
    # y = 23
    met_dir_it <- met_dir_reanalysis
    Yearly_Site_HYSPLIT_mod(site_name = paste0(names[[s]]),
                            year = years[[y]],
                            site_lat = lat_list_qld[[s]],
                            site_lon = lon_list_qld[[s]],
                            traj_duration = 72, 
                            start_height = 2000, 
                            direction = "backward", 
                            met_type = "reanalysis", 
                            ntraj_per_day = 1, 
                            met_dir = met_dir_it, 
                            output_folder = "C:/hysplit/working/1/")
    Export_YSH(site_name = paste0(names[[s]]), year = years[[y]], lat = lat_list_qld[[s]], lon = lon_list_qld[[s]], traj_duration = 72,
               start_height = 2000, direction = "backward", met_type = "reanalysis", 
               ntraj_per_day = 1,  met_dir = met_dir_it, output_folder = "C:/hysplit/working/1/",
               save_dir = export_dir_years)
    message(names[[s]]," ",years[[y]]," Complete")
  }
  message("Site ",s,"/",length(lat_list_qld)," complete.")
# }

# GDAS1: 2005-2019

##### OLD / PREV REARRANGED CODE BELOW THIS HEADER #####
##### SE QLD Trajectories #####

# The lat and lon for SC used in this portion was originally incorrect, hence the section
# afterwards that re-does the SC trajectories. Assuming the correct lat/lon is correctly input,
# this section below will work fine. 

# Do two loops: one for GDAS, one for reanalysis
# USING SEAGATE EXTERNAL FOR GDAS: PRE-2015
met_dir_gdas <- "H:/DATA/HYSPLIT Files/GDAS1 Meteorological Data/"
met_dir_reanalysis <- "H:/DATA/HYSPLIT Files/Reanalysis Meteorological Data/" 

# Site 1: Point Lookout Weather Station, North Stradbroke Island
PL_lat <- -27.44
PL_lon <- 153.55
# Site 2: Sandy Cape Weather Station, Fraser Island
SC_lat <- -24.73
SC_lon <- 153.21

# Functions required
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
Add_traj_identifier <- function(x, ntraj_1 = FALSE){
  # This function assumes that convert_openair() has been used on the dataset. 
  x_new <- as.data.frame(x)
  # Compute trajectory number. Starts at 1. 
  if(!isTRUE(ntraj_1)){
    x_new$start.time.minutes <- substr(x_new[,11],12,19)
    x_new$start.time.minutes <- 60*24*as.numeric(chron::times(x_new$start.time.minutes))
    x_new$trajectory <- cumsum(c(0, as.numeric(diff(x_new$start.time.minutes)) != 0)) + 1
  } else{
    x_new$diffs <- 1
    x_new$diffs[2:nrow(x_new)] <- diff(x_new$hour.inc) 
    x_new$trajectory <- cumsum(c(0,as.numeric(x_new$diffs[2:nrow(x_new)]) != 1)) + 1
  }
  #  x_new <<- as.data.frame(x)
  assign(paste0(deparse(substitute(x))), x_new, envir = parent.frame())
  #  rm(x_new, envir = parent.frame())
}
YSH_info <- function(){
  message("INFO ON FUNCTION: Yearly_Site_HYSPLIT()",
          "\n","------",
          "\n", "The Yearly_Site_HYSPLIT() function generates a yearly trajectory set for a given location somewhere in the world.",
          "\n","Read below for a rundown/reminder on the input parameters. You must have a working install of HYSPLIT.","\n","------")
  message("site_name      | name or identifier for the site/trajectory set. Will be present in output names. MUST BE ONE WORD in quotations, with less than 5 characters!!", "\n",
          "year           | the year for which you want to generate a set of trajectories.","\n",
          "site_lat       | the latitude of the target site.","\n",
          "site_lon       | the longitude of the target site.","\n",
          "traj_duration  | the duration, in hours, of each trajectory.","\n",
          "start_height   | the height of the trajectory release/endpoint, in metres. e.g. '50'","\n",
          "run_direction  | run model forwards or backwards? Forwards for standard trajectories, backwards for back-trajectories.","\n",
          "met_type       | what type of met data are you using? at the moment only gdas1 and reanalysis are recommended. Remove the associated if(){stop} statement to use other types.","\n",
          "ntraj_per_day  | number of trajectories per day. Must be a whole divisor of 24. They will be spread evenly across the day.", "\n",
          "met_dir        | path to the folder in which the met files are stored","\n",
          "output_folder  | path to the folder where the trajectories will be collated. Default is C:/hysplit/working/1/","\n","------", "\n",
          "The function requires the following packages: pacman, splitr, lubridate, tibble, dplyr, R.utils, rgdal, chron","\n","------")
}
YSH_info()

# Here's the function. At the moment, the parts that read the outputs back into the workspace have been removed, pending further testing.
Yearly_Site_HYSPLIT_mod <- function(site_name, year, site_lat, site_lon, traj_duration, 
                                    start_height, direction, met_type, ntraj_per_day, met_dir, 
                                    output_folder = "C:/hysplit/working/1/", verbose = FALSE){
  library(pacman)
  pacman::p_load(splitr,lubridate,tibble,dplyr,R.utils,rgdal,chron)
  # Initial user start/stop prompt. Hash out if seeking to automate
  if(isTRUE(verbose)){
    PromptContinue <- if(interactive()){
      askYesNo("Generate yearly trajectories for the target year using user specifications? This could take a while!", 
               default = TRUE, prompts = getOption("askYesNo", gettext(c("Y/N/C"))))
    }
    if(!isTRUE(PromptContinue)){
      stop("User terminated function")}
  }
  # INPUT CHECKS
  #
  if(nchar(site_name) <= 5){
  } else{
    stop("Please shorten the site_name to 5 characters or less.")
  }
  if(direction == "forward" || direction == "backward"){
  } else{
    stop("Please enter a valid direction: either forward or backward")
  }
  if(met_type == "gdas1" || met_type == "reanalysis"){
  } else{
    stop("Please enter a valid met type: gdas1 or reanalysis")
  }
  if(ntraj_per_day == 1 || ntraj_per_day == 2 || ntraj_per_day == 3 || ntraj_per_day == 4 || ntraj_per_day == 6 || ntraj_per_day == 8 || ntraj_per_day == 10 || ntraj_per_day == 12){
  } else{
    stop("trajectories per day (ntraj_per_day) must be a whole divisor of 24")
  }
  if(ntraj_per_day <= 24){
  } else {
    stop("number of trajectories per day cannot be greater than 24!")
  }
  # Assign month vars. Function will iterate over these.
  month_init = c(1:12)
  days_in_months = c(31,28,31,30,31,30,31,31,30,31,30,31)
  if(year%%4 == 0 || year%%400 == 0){
    days_in_months[2] = 29} # leap year + century feb month adjustment
  # trajectories per day setting
  hours_inc <- seq(0,24,by = (24/ntraj_per_day))
  hours_increments <- hours_inc[1:(length(hours_inc)-1)]
  # Main loop! Runs HYSPLIT for every month.
  for(i in seq_along(month_init)){
    # from lubridate wrap sequence
    month_formatted <- formatC(month_init[i], width = 2, format = "d", flag = "0")
    start_date_day_formatted <- 1
    start_date_day_formatted <- formatC(start_date_day_formatted, width = 2, format = "d", flag = "0")
    start_date <- paste0(year,"-",month_formatted,"-",start_date_day_formatted)
    end_date <- paste0(year,"-",month_formatted,"-",days_in_months[i]) # to lubridate wrap sequence
    i_date <- Sys.Date() # iterative output name for each monthly trajectory bundle
    i_date <- as.character(as.Date(i_date, "%Y-%m-%d"), "%d%m%y")
    set_name <- paste0(site_name,"-",year,"-",i_date,"-YSH-",month_init[i])
    # HYSPLIT run
    hysplit_trajectory_mod(
      lat = site_lat,
      lon = site_lon,
      height = start_height,
      duration = traj_duration,
      days = seq(
        lubridate::ymd(start_date),
        lubridate::ymd(end_date),
        by = "1 day"
      ),
      daily_hours = hours_increments,
      direction = direction,
      met_type = met_type,
      extended_met = TRUE,
      clean_up = FALSE,
      met_dir = met_dir,
      traj_name = set_name
    )
    message(i,"/12")
  }
}

# This groups the trajectories by month, collates them, then exports as a .csv to the desigated output folder.
Export_YSH <- function(site_name, year, lon, lat, traj_duration, 
                       start_height, direction = "forwards", met_type = "gdas1", 
                       ntraj_per_day, met_dir, output_folder, save_dir, day = "today"){
  message("Make sure i_date is the correct day if trajectories were not run today!")
  # Lobotomised function
  month_init = c(1:12)
  days_in_months = c(31,28,31,30,31,30,31,31,30,31,30,31)
  if(year%%4 == 0 || year%%400 == 0){
    days_in_months[2] = 29} # leap year + century feb month adjustment
  # trajectories per day setting
  hours_inc <- seq(0,24,by = (24/ntraj_per_day))
  hours_increments <- hours_inc[1:(length(hours_inc)-1)]
  for(i in seq_along(month_init)){
    # from lubridate wrap sequence
    month_formatted <- formatC(month_init[i], width = 2, format = "d", flag = "0")
    start_date_day_formatted <- 1
    start_date_day_formatted <- formatC(start_date_day_formatted, width = 2, format = "d", flag = "0")
    start_date <- paste0(year,"-",month_formatted,"-",start_date_day_formatted)
    # to lubridate wrap sequence
    end_date <- paste0(year,"-",month_formatted,"-",days_in_months[i])
    # iterative output name for each monthly trajectory bundle
    i_date <- Sys.Date() # iterative output name for each monthly trajectory bundle
    i_date <- as.character(as.Date(i_date, "%Y-%m-%d"), "%d%m%y")
    set_name <- paste0(site_name,"-",year,"-",i_date,"-YSH-",month_init[i])
    # HYSPLIT run
    # Rename traj [i] to month 
    # assign(paste0("Trajset_", deparse(substitute(month_init[i]))), month_trajset)
    # Read trajectory output in from folder
    traj_reread <- trajectory_read(output_folder = paste0(output_folder,set_name))
    # Convert to desirable layout
    traj_converted <- convert_openair(traj_reread)
    assign(paste0("Trajset_converted_",month_init[i]), traj_converted, envir = parent.frame())
    message(i, "completed")
  }
  Yearly_trajset <- rbind(Trajset_converted_1,Trajset_converted_2,Trajset_converted_3,
                          Trajset_converted_4,Trajset_converted_5,Trajset_converted_6,
                          Trajset_converted_7,Trajset_converted_8,Trajset_converted_9,
                          Trajset_converted_10,Trajset_converted_11,Trajset_converted_12)
  # Formatting: added identifier, rename columns
  #
  if(ntraj_per_day == 1){
    traj1 = TRUE
  } else {
    traj1 = FALSE
  }
  Add_traj_identifier(Yearly_trajset,ntraj_1 = traj1)
  colnames(Yearly_trajset)[11]<- "date.inc"
  colnames(Yearly_trajset)[12] <- "date.start"
  # Export. THIS NEEDS WORK.
  save_dir = save_dir
  output_timestamp <- Sys.Date()
  file_output <- paste0(save_dir,site_name,"-",year,"-",output_timestamp,"_",traj_duration,"hr",ntraj_per_day,"TPD",start_height,"m.csv")
  write.csv(Yearly_trajset,file = file_output, row.names = FALSE)
}

## GDAS1: 2011
years <- c(2011)
it_list <- vector(mode = "list", length = length(years))
sites_lat <- c(PL_lat,SC_lat)
sites_lon <- c(PL_lon,SC_lon)
lat_list_qld <- vector(mode = "list", length = 2)
lon_list_qld <- vector(mode = "list", length = 2)
for(i in seq_along(lat_list_qld)){
  lat_list_qld[[i]] <- sites_lat[i]
  lon_list_qld[[i]] <- sites_lon[i]
}
names <- vector(mode = "list", length = length(2))
names[[1]] <- "PL"
names[[2]] <- "SC"

for(s in seq_along(lat_list_qld)){
  # Loop: years for site in init
  message("Starting site ",s,": ",names[[s]]," ",lat_list_qld[[s]],"S ",lon_list_qld[[s]],"W")
  for(y in seq_along(it_list)){
    met_dir_it <- paste0("H:/DATA/HYSPLIT Files/GDAS1 Meteorological Data/",years[[y]],"/")
    Yearly_Site_HYSPLIT_mod(site_name = paste0(names[[s]]),
                            year = years[y],
                            site_lat = lat_list_qld[[s]],
                            site_lon = lon_list_qld[[s]],
                            traj_duration = 72, 
                            start_height = 2000, 
                            direction = "backward", 
                            met_type = "gdas1", 
                            ntraj_per_day = 1, 
                            met_dir = met_dir_it, 
                            output_folder = "C:/hysplit/working/1/")
    Export_YSH(site_name = paste0(names[[s]]), year = years[[y]], lat = lat_list_AR[[s]], lon = lon_list_AR[[s]], traj_duration = 72,
               start_height = 2000, direction = "backward", met_type = "gdas1", 
               ntraj_per_day = 1,  met_dir = met_dir_it, output_folder = "C:/hysplit/working/1/",
               save_dir = "C:/Users/Chris Turney/Desktop/Matt Harris 2020/2020 Stuck down under/software and coding/outputs/")
  }
  message("Site ",s,"/",length(lat_list_qld)," complete.")
}

## REANALYSIS: 1950:2004.
years <- seq(1950,2004,1)
it_list <- vector(mode = "list", length = length(years))
sites_lat <- c(PL_lat,SC_lat)
sites_lon <- c(PL_lon,SC_lon)
lat_list_qld <- vector(mode = "list", length = 2)
lon_list_qld <- vector(mode = "list", length = 2)
for(i in seq_along(lat_list_qld)){
  lat_list_qld[[i]] <- sites_lat[i]
  lon_list_qld[[i]] <- sites_lon[i]
}
names <- vector(mode = "list", length = length(2))
names[[1]] <- "PL"
names[[2]] <- "SC"
for(s in seq_along(lat_list_qld)){
  # Loop: years for site in init
  message("Starting site ",s,": ",names[[s]]," ",lat_list_qld[[s]],"S ",lon_list_qld[[s]],"W")
  for(y in seq_along(it_list)){
    met_dir_it <- met_dir_reanalysis
    Yearly_Site_HYSPLIT_mod(site_name = paste0(names[[s]]),
                            year = years[[y]],
                            site_lat = lat_list_qld[[s]],
                            site_lon = lon_list_qld[[s]],
                            traj_duration = 72, 
                            start_height = 2000, 
                            direction = "backward", 
                            met_type = "reanalysis", 
                            ntraj_per_day = 1, 
                            met_dir = met_dir_it, 
                            output_folder = "C:/hysplit/working/1/")
    Export_YSH(site_name = paste0(names[[s]]), year = years[[y]], lat = lat_list_AR[[s]], lon = lon_list_AR[[s]], traj_duration = 72,
               start_height = 2000, direction = "backward", met_type = "reanalysis", 
               ntraj_per_day = 1,  met_dir = met_dir_it, output_folder = "C:/hysplit/working/1/",
               save_dir = "C:/Users/Chris Turney/Desktop/Matt Harris 2020/2020 Stuck down under/software and coding/outputs/")
    message(names[[s]]," ",years[[y]]," Complete")
  }
  message("Site ",s,"/",length(lat_list_qld)," complete.")
}

# During the runs, 2000 onwards failed. I'm unsure why this is; just leaving it out for now.
# Now running for SC, 1950:1999.

## REANALYSIS 
years <- seq(1950,1999,1)
it_list <- vector(mode = "list", length = length(years))
sites_lat <- c(SC_lat)
sites_lon <- c(SC_lon)
lat_list_qld <- vector(mode = "list", length = 1)
lon_list_qld <- vector(mode = "list", length = 1)
for(i in seq_along(lat_list_qld)){
  lat_list_qld[[i]] <- sites_lat[i]
  lon_list_qld[[i]] <- sites_lon[i]
}
names <- vector(mode = "list", length = length(1))
names[[1]] <- "SC"
for(s in seq_along(lat_list_qld)){
  # Loop: years for site in init
  message("Starting site ",s,": ",names[[s]]," ",lat_list_qld[[s]],"S ",lon_list_qld[[s]],"W")
  for(y in seq_along(it_list)){
    met_dir_it <- met_dir_reanalysis
    Yearly_Site_HYSPLIT_mod(site_name = paste0(names[[s]]),
                            year = years[[y]],
                            site_lat = lat_list_qld[[s]],
                            site_lon = lon_list_qld[[s]],
                            traj_duration = 72, 
                            start_height = 2000, 
                            direction = "backward", 
                            met_type = "reanalysis", 
                            ntraj_per_day = 1, 
                            met_dir = met_dir_it, 
                            output_folder = "C:/hysplit/working/1/")
    Export_YSH(site_name = paste0(names[[s]]), year = years[[y]], lat = lat_list_AR[[s]], lon = lon_list_AR[[s]], traj_duration = 72,
               start_height = 2000, direction = "backward", met_type = "reanalysis", 
               ntraj_per_day = 1,  met_dir = met_dir_it, output_folder = "C:/hysplit/working/1/",
               save_dir = "C:/Users/Chris Turney/Desktop/Matt Harris 2020/2020 Stuck down under/software and coding/outputs/")
    message(names[[s]]," ",years[[y]]," Complete")
  }
  message("Site ",s,"/",length(lat_list_qld)," complete.")
}

# Why failing in 2000?
traj_traj <-
  trajectory_read(
    output_folder = "C:/hysplit4/working/1/PL-2000-301120-YSH-2/traj-PL-2000-301120-YSH-2-bwd-00-02-10-00-1lat_-27p44_lon_153p55-hgt_2000-72h")

hysplit_trajectory_mod <- function (lat = 49.263, lon = -123.25, height = 50, duration = 24, 
                                    days = NULL, daily_hours = 0, direction = "forward", 
                                    met_type = "reanalysis", vert_motion = 0, model_height = 20000, 
                                    extended_met = FALSE, config = NULL, ascdata = NULL, traj_name = NULL, 
                                    binary_path = NULL, met_dir = NULL, exec_dir = NULL, clean_up = FALSE) 
{
  #Source the two new functions
  if (is.null(exec_dir)) 
    exec_dir <- getwd()
  if (is.null(met_dir)) 
    met_dir <- getwd()
  binary_name <- "hyts_std"
  binary_path <-
    system.file(
      file.path("win", paste0(binary_name, ".exe")),
      package = "splitr"
    )
  system_type <- get_os()
  if (is.null(traj_name)) {
    folder_name <- paste0("traj-", format(Sys.time(), 
                                          "%Y-%m-%d-%H-%M-%S"))
  }
  else if (!is.null(traj_name)) {
    folder_name <- traj_name
  }
  if (is.null(config)) {
    config_list <- set_config()
  }
  else {
    config_list <- config
  }
  if (is.null(ascdata)) {
    ascdata_list <- set_ascdata()
  }
  else {
    ascdata_list <- ascdata
  }
  if (isTRUE(extended_met)) {
    tm_names <- config_list %>% names() %>% vapply(FUN.VALUE = logical(1), 
                                                   USE.NAMES = FALSE, FUN = function(y) y %>% tidy_grepl("^tm_")) %>% 
      which()
    config_list[tm_names] <- 1
  }
  config_list %>% write_config_list(dir = exec_dir)
  ascdata_list %>% write_ascdata_list(dir = exec_dir)
  if (length(lat) != length(lon)) {
    stop("The coordinate vectors are not the same length.", 
         call. = FALSE)
  }
  download_met_files_mod <- function(met_type,
                                     days,
                                     duration,
                                     direction,
                                     met_dir) {
    if (met_type == "gdas1") {
      get_met_gdas1_mod <- function(days,
                                    duration,
                                    direction,
                                    path_met_files) {
        
        # Determine the minimum date (as a `Date`) for the model run
        if (direction == "backward") {
          min_date <- 
            (lubridate::as_date(days[1]) - (duration / 24)) %>%
            lubridate::floor_date(unit = "day")
        } else if (direction == "forward") {
          min_date <- 
            (lubridate::as_date(days[1])) %>%
            lubridate::floor_date(unit = "day")
        }
        
        # Determine the maximum date (as a `Date`) for the model run
        if (direction == "backward") {
          max_date <- 
            (lubridate::as_date(days[length(days)])) %>%
            lubridate::floor_date(unit = "day")
        } else if (direction == "forward") {
          max_date <- 
            (lubridate::as_date(days[length(days)]) + (duration / 24)) %>%
            lubridate::floor_date(unit = "day")
        }
        
        met_days <- 
          seq(min_date, max_date, by = "1 day") %>% 
          lubridate::day()
        
        month_names <- 
          seq(min_date, max_date, by = "1 day") %>%
          lubridate::month(label = TRUE, abbr = TRUE, locale = Sys.getlocale("LC_TIME"))  %>%
          as.character() %>%
          tolower()
        
        # This was the previous version of the above block, and threw up an error. 
        #month_names <- 
        #  seq(min_date, max_date, by = "1 day") %>%
        #  lubridate::month(label = TRUE, abbr = TRUE, locale = "en_US.UTF-8")  %>%
        #  as.character() %>%
        #  tolower()
        
        met_years <- 
          seq(min_date, max_date, by = "1 day") %>%
          lubridate::year() %>% 
          substr(3, 4)
        
        # Only consider the weeks of the month we need:
        #.w1 - days 1-7
        #.w2 - days 8-14
        #.w3 - days 15-21
        #.w4 - days 22-28
        #.w5 - days 29 - rest of the month 
        
        met_week <- ceiling(met_days / 7)
        
        files <- paste0("gdas1.", month_names, met_years, ".w", met_week) %>% unique()
        
        get_met_files(
          files = files,
          path_met_files = path_met_files,
          ftp_dir = "ftp://arlftp.arlhq.noaa.gov/archives/gdas1"
        )
        
      }
      met_files <-
        get_met_gdas1_mod(
          days = days,
          duration = duration,
          direction = direction,
          path_met_files = met_dir
        )
    }
    
    if (met_type == "gdas0.5") {
      
      met_files <-
        get_met_gdas0p5(
          days = days,
          duration = duration,
          direction = direction,
          path_met_files = met_dir
        )
    }
    
    if (met_type == "gfs0.25") {
      
      met_files <-
        get_met_gfs0p25(
          days = days,
          duration = duration,
          direction = direction,
          path_met_files = met_dir
        )
    }
    
    if (met_type == "reanalysis") {
      
      met_files <-
        get_met_reanalysis(
          days = days,
          duration = duration,
          direction = direction,
          path_met_files = met_dir
        )
    }
    
    if (met_type == "nam12") {
      
      met_files <-
        get_met_nam12(
          days = days,
          duration = duration,
          direction = direction,
          path_met_files = met_dir
        )
    }
    
    if (met_type == "narr") {
      
      met_files <-
        get_met_narr(
          days = days,
          duration = duration,
          direction = direction,
          path_met_files = met_dir
        )
    }
    
    if (met_type == "era5") {
      
      met_files <-
        get_met_era5(
          days = days,
          duration = duration,
          direction = direction,
          path_met_files = met_dir
        )
    }
    
    met_files
  }
  met_files <- download_met_files_mod(met_type = met_type, days = days, 
                                      duration = duration, direction = direction, met_dir = met_dir)
  receptors_tbl <- dplyr::tibble(lat = lat, lon = lon) %>% 
    dplyr::group_by(lat, lon) %>% tidyr::expand(height = height) %>% 
    dplyr::ungroup() %>% dplyr::mutate(receptor = dplyr::row_number()) %>% 
    dplyr::select(receptor, dplyr::everything())
  receptors <- seq(nrow(receptors_tbl))
  ensemble_tbl <- dplyr::tibble()
  recep_file_path_stack <- c()
  for (receptor in receptors) {
    receptor_vals <- get_receptor_values(receptors_tbl = receptors_tbl, 
                                         receptor_i = receptor)
    receptor_i <- receptor_vals$receptor
    lat_i <- receptor_vals$lat
    lon_i <- receptor_vals$lon
    height_i <- receptor_vals$height
    list_run_days <- days %>% as.character()
    trajectory_files <- c()
    for (i in seq(list_run_days)) {
      start_year_GMT <- to_short_year(list_run_days[i])
      start_month_GMT <- to_short_month(list_run_days[i])
      start_day_GMT <- to_short_day(list_run_days[i])
      if (inherits(daily_hours, "numeric")) {
        daily_hours <- formatC(sort(daily_hours), width = 2, 
                               flag = 0)
      }
      for (j in daily_hours) {
        start_hour_GMT <- j
        if (start_year_GMT > 40) {
          full_year_GMT <- paste0("19", start_year_GMT)
        }
        else {
          full_year_GMT <- paste0("20", start_year_GMT)
        }
        output_filename <- get_traj_output_filename(traj_name = traj_name, 
                                                    site = receptor_i, direction = direction, year = start_year_GMT, 
                                                    month = start_month_GMT, day = start_day_GMT, 
                                                    hour = start_hour_GMT, lat = lat_i, lon = lon_i, 
                                                    height = height_i, duration = duration)
        trajectory_files <- c(trajectory_files, output_filename)
        write_traj_control_file(start_year_GMT = start_year_GMT, 
                                start_month_GMT = start_month_GMT, start_day_GMT = start_day_GMT, 
                                start_hour_GMT = start_hour_GMT, lat = lat_i, 
                                lon = lon_i, height = height_i, direction = direction, 
                                duration = duration, vert_motion = vert_motion, 
                                model_height = model_height, met_files = met_files, 
                                output_filename = output_filename, system_type = system_type, 
                                met_dir = met_dir, exec_dir = exec_dir)
        sys_cmd <- paste0("(cd \"", exec_dir, "\" && \"", 
                          binary_path, "\" ", to_null_dev(system_type = system_type), 
                          ")")
        execute_on_system(sys_cmd, system_type = system_type)
      }
    }
    recep_file_path <- file.path(exec_dir, receptor_i, folder_name)
    recep_file_path_stack <- c(recep_file_path_stack, file.path(exec_dir, 
                                                                receptor_i))
    if (!dir.exists(recep_file_path)) {
      dir.create(path = recep_file_path, recursive = TRUE)
    }
    file.copy(from = file.path(exec_dir, trajectory_files), 
              to = recep_file_path, copy.mode = TRUE)
    unlink(file.path(exec_dir, trajectory_files), force = TRUE)
    traj_tbl <- trajectory_read(output_folder = recep_file_path) %>% 
      dplyr::as_tibble() %>% dplyr::mutate(receptor = receptor_i, 
                                           lat_i = lat_i, lon_i = lon_i, height_i = height_i)
    ensemble_tbl <- ensemble_tbl %>% dplyr::bind_rows(traj_tbl)
  }
  if (clean_up) {
    unlink(file.path(exec_dir, traj_output_files()), force = TRUE)
    unlink(recep_file_path_stack, recursive = TRUE, force = TRUE)
  }
  ensemble_tbl <- ensemble_tbl %>% dplyr::select(-c(year, month, 
                                                    day, hour)) %>% dplyr::select(receptor, hour_along, traj_dt, 
                                                                                  lat, lon, height, traj_dt_i, lat_i, lon_i, height_i, 
                                                                                  dplyr::everything()) %>% dplyr::group_by(receptor, hour_along, 
                                                                                                                           traj_dt, traj_dt_i, lat_i, lon_i, height_i) %>% dplyr::slice(1) %>% 
    dplyr::ungroup()
  if (direction == "forward") {
    ensemble_tbl <- ensemble_tbl %>% dplyr::arrange(receptor, 
                                                    traj_dt_i)
  }
  else {
    ensemble_tbl <- ensemble_tbl %>% dplyr::arrange(receptor, 
                                                    traj_dt_i, dplyr::desc(hour_along))
  }
  ensemble_tbl %>% dplyr::right_join(ensemble_tbl %>% dplyr::select(receptor, 
                                                                    traj_dt_i, lat_i, lon_i, height_i) %>% dplyr::distinct() %>% 
                                       dplyr::mutate(run = dplyr::row_number()), by = c("receptor", 
                                                                                        "traj_dt_i", "lat_i", "lon_i", "height_i")) %>% 
    dplyr::select(run, dplyr::everything())
}

traj_test <-hysplit_trajectory_mod(lat = PL_lat, lon = PL_lon, height = 2000, duration = 72,
                                   days = seq("01/01/2"))


