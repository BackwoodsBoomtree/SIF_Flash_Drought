library(terra)


fd_dir       <- "G:/flash_drought/fix_extent" # Main directory
sub_dir_list <- list.dirs(path = fd_dir, full.names = TRUE, recursive = FALSE) # Subdirectories for each year
out_dir_n    <- "G:/flash_drought/analysis/n"
out_dir_doy  <- "G:/flash_drought/analysis/doy"

# Create output dirs
if (!dir.exists(out_dir_n)) {
  dir.create(out_dir_n, recursive = TRUE)
}

if (!dir.exists(out_dir_doy)) {
  dir.create(out_dir_doy, recursive = TRUE)
}

for (sub_dir in sub_dir_list) {

  # Get year
  year    <- basename(sub_dir)
  fd_list <- list.files(sub_dir, pattern = "*.nc$", full.names = TRUE, recursive = TRUE)
  
  # Calculate number of flash droughts in a year for each gridcell
  # Create a raster stack of start dates from the nc files
  for (i in 1:length(fd_list)) {
    if (i == 1) {
      annual_n <- sds(fd_list[i])[1]
    } else {
      annual_n <- c(annual_n,  sds(fd_list[i])[1])
    }
  }
  
  # Sum up the raster stack into a single raster
  annual_n <- sum(annual_n, na.rm = TRUE)
  plot(annual_n, main = paste0("Number of Flash Droughts ", year))
  writeRaster(annual_n, paste0(out_dir_n, "/n_flash_droughts_", year, ".tif"), overwrite = TRUE, NAflag = -9999)
  

  # Create a raster stack of start dates from the nc files,
  # where the values are the DOY
  for (i in 1:length(fd_list)) {
    if (i == 1) {
      annual_rast <- sds(fd_list[i])[1]
    } else {
      fd_day      <- sds(fd_list[i])[1]
      fd_day[fd_day == 1] <- i * 5 - 4
      annual_rast <- c(annual_rast, fd_day)
    }
  }
  
  # Create a date list for naming layers in raster stack
  date_list <- seq(as.Date(paste0(year, "/1/1")), as.Date(paste0(year, "/12/27")), by = 5)
  names(annual_rast) <- date_list
  
  # Mask out the gridcells with more than one flash drought
  annual_rast <-  mask(annual_rast, annual_n, inverse = TRUE, maskvalues = 1)
  
  # Combine raster stack by summing 
  annual_rast <- sum(annual_rast, na.rm = TRUE)
  
  # Plot and save
  plot(annual_rast, main = paste0("Start of Flash Drought (DOY) for Gridcells with One Flash Drought ", year))
  writeRaster(annual_rast, paste0(out_dir_doy, "/doy_flash_droughts_", year, ".tif"), overwrite = TRUE, NAflag = -9999)
  
}