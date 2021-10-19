library(terra)
library(viridis)
library(RColorBrewer)


fd_dir         <- "G:/flash_drought/fix_extent" # Main directory
sub_dir_list   <- list.dirs(path = fd_dir, full.names = TRUE, recursive = FALSE) # Subdirectories for each year
out_dir_n      <- "G:/flash_drought/analysis/n"
out_dir_start  <- "G:/flash_drought/analysis/start"
out_dir_length <- "G:/flash_drought/analysis/length"


# Create output dirs
if (!dir.exists(out_dir_n)) {
  dir.create(out_dir_n, recursive = TRUE)
}

if (!dir.exists(out_dir_start)) {
  dir.create(out_dir_start, recursive = TRUE)
}

if (!dir.exists(out_dir_length)) {
  dir.create(out_dir_length, recursive = TRUE)
}

for (sub_dir in sub_dir_list) {

  # Get year
  year    <- basename(sub_dir)
  fd_list <- list.files(sub_dir, pattern = "*.nc$", full.names = TRUE, recursive = TRUE)
  
  #### Create annual map of number of flash droughts
  
  # Calculate number of flash droughts in a year for each gridcell, which will be used in masking
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
  plot(annual_n, main = paste0("Number of Flash Droughts ", year), col = plasma(256))
  writeRaster(annual_n, paste0(out_dir_n, "/n_flash_droughts_", year, ".tif"), overwrite = TRUE, NAflag = -9999)
  
  #### Create annual maps of start date and length ####
  
  # Create a raster stack of start dates (DOY) and for length (pentads)
  for (i in 1:length(fd_list)) {
    if (i == 1) {
      annual_start  <- sds(fd_list[i])[1]
      annual_length <- sds(fd_list[i])[2]
    } else {
      fd_start                <- sds(fd_list[i])[1]
      fd_start[fd_start == 1] <- i * 5 - 4
      annual_start            <- c(annual_start, fd_start)
      
      fd_length                 <- sds(fd_list[i])[2]
      fd_length[fd_length == 1] <- i * 5 - 4
      annual_length             <- c(annual_length, fd_length)
    }
  }
  
  # Create a date list for naming layers in raster stack
  date_list            <- seq(as.Date(paste0(year, "/1/1")), as.Date(paste0(year, "/12/27")), by = 5)
  names(annual_start)  <- date_list
  names(annual_length) <- date_list
  
  # Mask out the gridcells with more than one flash drought
  annual_start  <- mask(annual_start, annual_n, inverse = TRUE, maskvalues = 1)
  annual_length <- mask(annual_length, annual_n, inverse = TRUE, maskvalues = 1)
  
  # Combine raster stack by summing 
  annual_start  <- sum(annual_start, na.rm = TRUE)
  annual_length <- sum(annual_length, na.rm = TRUE)
  
  # Plot and save
  plot(annual_start, main = paste0("Start of Flash Drought (DOY) for Gridcells with One Flash Drought ", year), col = brewer.pal(11, "RdYlBu"))
  plot(annual_length, main = paste0("Length of Flash Drought (pentads) for Gridcells with One Flash Drought ", year), col = viridis(7))
  
  writeRaster(annual_start, paste0(out_dir_start, "/doy_flash_droughts_", year, ".tif"), overwrite = TRUE, NAflag = -9999)
  writeRaster(annual_length, paste0(out_dir_length, "/length_flash_droughts_", year, ".tif"), overwrite = TRUE, NAflag = -9999)
  
}