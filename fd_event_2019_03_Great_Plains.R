library(terra)
library(viridis)
library(RColorBrewer)

# Import map of start dates for masking SIF and VI data
doy_tif         <- rast("G:/flash_drought/analysis/start/doy_flash_droughts_2019.tif") # Main directory
sif_esa_735     <- sds("G:/TROPOMI/esa/gridded/TROPOMI.SIF.201805-202108.0125deg.modisLike.esa.735nm.nc")
sif_esa_743     <- sds("G:/TROPOMI/esa/gridded/TROPOMI.SIF.201805-202108.0125deg.modisLike.esa.743nm.nc")
year            <- 2019
doy_filters     <- c(61, 66, 71, 76)    # DOYs to be used as a fd event
fd_event_coords <- c(-105, -91, 38, 43) # xmin, xmax, ymin, ymax for fd event

fd_event_mask   <- function(doy_tif, year, doy_filters, fd_event_coords) {
  # Create a 2 column matrix of is and becomes for creating mask
  m <- matrix(NA, nrow = 0, ncol = 2)
  for (i in seq.Date(as.Date(paste0(year, "/1/1")), as.Date(paste0(year, "/12/31")), by = "5 days")) {
    doy <- as.numeric(strftime(as.Date(i, origin = "1970-01-01"), format = "%j"))
  
    if (doy %in% doy_filters) {
      m <- rbind(m, c(doy, 1))
      # df[nrow(df) + 1,] = c(doy, 1)
    } else {
      m <- rbind(m, c(doy, NA))
      # df[nrow(df) + 1,] = c(doy, NA)
    }
  }
  
  # Crop mask to fd event, then extend extent to match other data
  doy_tif <- classify(doy_tif, m)
  doy_tif <- crop(doy_tif, fd_event_coords)
  doy_tif <- extend(doy_tif, ext(sif_esa_743))
  plot(doy_tif, main = paste0("Flash Drought Area ", year))
  
  return(doy_tif)
}

data_means      <- function(input, fd_mask) {
 
  df <- data.frame(matrix(NA, ncol = length(names(input))))
  
  for (i in 1:length(nlyr(input))) {
    
    colnames(df[i]) <- names(input)[i]
    
    for (j in 1:nlyr(input)[i]) {
      df[j, i] <- as.numeric(global(mask(input[i][[j]], fd_mask, inverse = TRUE, maskvalue = 1, updatevalue = NA), fun = mean, na.rm = TRUE))
    }
  }
  
  colnames(df) <- names(input)

  # Take a look! (max n of plots is 10, so generate more plots as needed)
  if (ncol(df) <= 10) {
    plot.ts(df, main = "Time Series of Data for Flash Drought Area")
  } else {
    for (i in 1:ceiling(ncol(df) / 10)) {
      last_pos <- i * 10
      if (last_pos > ncol(df)) {
        last_pos <- ncol(df)
      }
      plot.ts(df[, c((i * 10 - 9):last_pos)], main = "Time Series of Data for Flash Drought Area")
    }
  }
  return(df)
}


fd_mask <- fd_event_mask(doy_tif, year, doy_filters, fd_event_coords)

df_743  <- data_means(sif_esa_743, fd_mask)
df_735  <- data_means(sif_esa_735, fd_mask)



