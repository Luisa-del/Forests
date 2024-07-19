#'####################################################################################################################
# FUNCTIONS to generate base products for German Forests project
# 01.07.2024
# Luisa Pflumm
#'####################################################################################################################

#'####################################################################################################################
# SET CUSTOM PARAMETER ####
### Function to list mission folders and flight paths
list_all_flight_paths <- function(root_path) {
  # List all folders in root folder
  all_folders <- list.files(root_path, full.names = TRUE, recursive = FALSE)
  
  # Filter folders that match the pattern "yyyymmdd_name"
  pattern <- "^\\d{8}_.+"
  mission_folders <- all_folders[grepl(pattern, basename(all_folders))]
  
  # Check if the length of mission_folders is not equal to 13
  if (length(mission_folders) != 13) {
    stop("Please check if all 13 overall flight missions are stored in root-folder and modify code if this number is outdated.")
  } else {
    print("Correct number of mission folders found.")
  }
  
  # List all flight paths
  all_flight_folders <- list.files(file.path(mission_folders, "0_Flights"), full.names = TRUE, recursive = FALSE)
  all_flight_folders <- paste0(all_flight_folders, "/")
  
  return(all_flight_folders)
}



### Function to print flight paths matching mission
list_mission_flight_paths <- function(root_path, mission) {
  # List all folders in root folder
  all_folders <- list.files(root_path, full.names = TRUE, recursive = FALSE)
  
  # Filter folders that match the pattern "yyyymmdd_name" 
  pattern <- "^\\d{8}_.+"
  mission_folders <- all_folders[grepl(pattern, basename(all_folders))]
  
  # Check if the length of mission_folders is not equal to 13
  if (length(mission_folders) != 13) {
    stop("Please check if all 13 overall flight missions are stored in root-folder and modify code if this number is outdated.")
  } else {
    print("Correct number of mission folders found.")
  }
  
  # Initialize a vector to store matching flight paths
  matching_flight_paths <- character()
  
  # Iterate through each mission folder
  for (mission_folder in mission_folders) {
    # Get the last folder name
    last_folder <- basename(mission_folder)
    
    # Split the last folder name by "_"
    split_folder <- strsplit(last_folder, "_")[[1]]
    
    # Construct the mission part from the first two parts
    mission_part <- paste(split_folder[1], split_folder[2], sep = "_")
    
    # Check if the constructed mission part matches the given mission
    if (mission_part == mission) {
      # List flight paths for this mission folder
      flight_paths <- list.files(file.path(mission_folder, "0_Flights"), full.names = TRUE, recursive = FALSE)
      
      # Print the flight paths
      #cat("Flight paths for mission", mission_part, ":\n")
      #print(flight_paths)
      
      # Append matching flight paths to the vector
      matching_flight_paths <- c(matching_flight_paths, flight_paths)
      matching_flight_paths <- paste0(matching_flight_paths, "/")
      
    }
  }
  
  return(matching_flight_paths)
}



### Function to split path into tiles and select last folder (=Flightname)
select_flightname <- function(path) {
  split_result <- strsplit(path, "/")[[1]]
  return(split_result[length(split_result)])
}



# Function to return current time
get_current_time <- function() {
  current_datetime <- Sys.time()
  current_time <- format(current_datetime, "%H:%M:%S")
  return(current_time)
}



# Function to return current date
get_current_date <- function() {
  return(Sys.Date())
}



# Function to check if file exists
file_exists <- function(file_path) {
  file.exists(file_path)
}



# Function to handle user prompt
userPromt_outputTXTfile <- function(file_path) {
  # Check if file exists
  if (file_exists(file_path)) {
    # Prompt user for input
    user_input <- readline(prompt = "Output file already exists. Do you want to overwrite (o) already existing content, or append (a) new output to already existing file? (o/a): ")
    
    # Process user input
    if (tolower(user_input) == "o") {
      return(TRUE)  # Overwrite existing file
    } else if (tolower(user_input) == "a") {
      return(FALSE) # Create new file with a different name
    } else {
      cat("Invalid input. Please enter 'o' or 'a'.\n")
      userPromt_outputTXTfile(file_path)  # Recursive call to handle invalid input
    }
  } else {
    return(TRUE)  # File does not exist, proceed to write new file
  }
}



#'####################################################################################################################
# IMPORT FILES ####

### Function to check if raw output files are exsisting (.tif, .prj, .tfw and .las files for Lidar, .tif for all others) and import them
import_raw_output <- function(FlightPath, FlightName) {
  if (endsWith(FlightName, "DJIM300L1")) {
    # Construct the full paths
    base_path <- paste0(FlightPath, "4_RawOutput/", FlightName)
    tif_file <- paste0(base_path, "_DEM.tif")
    prj_file <- paste0(base_path, "_DEM.prj")
    tfw_file <- paste0(base_path, "_DEM.tfw")
    las_file <- (paste0(base_path, ".las"))    # -> what to select? select = "xyzi"
    #las_file <- system.file("extdata", "MixedConifer.laz", package ="lidR")
    # Check if the .tif file exists
    if (!file.exists(tif_file)) {
      stop(paste("No DEM was found in", base_path, "folder"))
    }
    
    # Check if the .prj and .tfw files exist and print a warning if they do not
    if (!file.exists(prj_file)) {
      warning(paste("Warning: .prj file does not exist in", base_path, "folder"))
    }
    if (!file.exists(tfw_file)) {
      warning(paste("Warning: .tfw file does not exist in", base_path, "folder"))
    }
    
    # Check if the .las file exists
    if (!file.exists(las_file)) {
      stop(paste("No Point Cloud was found in", base_path, "folder"))
    }
    
    # Load the .tif file
    raster_data <- rast(tif_file)
    
    # Load the .las file
    lidar_data <- readLAS(las_file)
    
    # Return the raster and las data in list
    return(list(dem = raster_data, pointcloud = lidar_data))
    
    } else {
      # Construct the full paths
      base_path <- paste0(FlightPath, "4_RawOutput/", FlightName)
      tif_file1 <- paste0(base_path, "_DSM.tif")
      tif_file2 <- paste0(base_path, "_OM.tif")
      
      # Check if the .tif files exists
      if (!file.exists(tif_file1)) {
        stop(paste("No DSM was found in", base_path, "folder"))
      }
      
      if (!file.exists(tif_file2)) {
        stop(paste("No OM was found in", base_path, "folder"))
      }
      
      # Load the .tif files
      raster_data1 <- rast(tif_file1)
      raster_data2 <- rast(tif_file2)
      
      # Return the raster data in list
      return(list(dsm = raster_data1, om = raster_data2))
    }
}



# Function to trim a raster
trim_raster <- function(raster) {
  return(trim(raster))
}



# Function to plot raster (if more than one bands plot first band)
plot_raster <- function(raster) {
  if (nlyr(raster) > 1) {
    # Extract and plot the first band
    first_band <- raster[[1]]
    #names <- strsplit(names(raster), "_")[[1]]
    plot(first_band, main = paste0(names(first_band), " - band 1"))
  } else {
    # Plot the entire raster
    plot(raster, main = paste0(names(raster)))
  }
  return(NULL)
}
# names <- strsplit(names(raster), "_")[[1]]
# #par(mfrow = c(1, 2))
# plot(raster, main = paste0(toupper(names(raw_output[1]))," from ", FlightName))
# plot(r2[[1]], main = paste0(toupper(names(raw_output[2]))," from ", FlightName, " - band 1"))
# #plotRGB(r2, r = 3, g = 2, b = 1, stretch = "lin")



# Function to check CRS and extent validity
check_raster_validity_info <- function(raster_path) {
  # Load the raster
  raster_data <- rast(raster_path)
  
  # Check if CRS is valid
  crs_valid <- !is.na(terra::crs(raster_data))
  crs_interpretable <- tryCatch({
    terra::crs(raster_data)
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  # Get the extent
  raster_extent <- ext(raster_data)
  
  # Check if extent values are finite
  extent_valid <- all(is.finite(c(raster_extent$xmin, raster_extent$xmax, raster_extent$ymin, raster_extent$ymax)))
  
  # Get the resolution of the raster
  r_res <- res(raster_data)
  
  # # Extract the first part of the resolution vector
  # first_part <- r_res[1]
  # 
  # # Check if the raster resolution is floating number and optionally replace '.' with '-'
  # if (grepl("\\.", as.character(first_part))) {
  #   # Replace '.' with '-'
  #   res <- gsub("\\.", "-", as.character(first_part))
  # } else {
  #   # Keep the first part as it is
  #   res <- as.character(first_part)
  # }
  
  # Get EPSG code of the raster
  r_epsg <- as.numeric(terra::crs(raster_data, describe = TRUE)$code)
  
  
  # Print results
  if (crs_valid && crs_interpretable) {
    cat(names(raster_path[[1]]), "\n")
    cat("CRS is valid and interpretable: ")
    cat(terra::crs(raster_data, proj = TRUE), "\n" )
  } else {
    cat("CRS is either invalid or not interpretable.\n")
  }

  if (extent_valid) {
    cat("EPSG code: ", r_epsg, "\n")
    cat("Pixel resolution: ", r_res, "\n")
    cat("Extent details:\n")
    cat("xmin:", raster_extent$xmin, "\n")
    cat("xmax:", raster_extent$xmax, "\n")
    cat("ymin:", raster_extent$ymin, "\n")
    cat("ymax:", raster_extent$ymax, "\n")
  } else {
    cat("Extent is invalid.\n")
    
  }
  
  # Prepare output in a list
  output <- list(
    raster_path = raster_path,
    crs_valid = crs_valid,
    crs_interpretable = crs_interpretable,
    extent_valid = extent_valid,
    resolution = r_res, 
    #resolution_x = res, 
    epsg = r_epsg,
    extent_details = raster_extent
  )
  
  # Return the results as a list
  return(output)
}



# Function to get the correct UTM EPSG code for a raster
get_utm_epsg <- function(raster) {
  
  # Get the CRS of the raster
  crs <- terra::crs(raster, describe=T)
  crs <- as.numeric(crs$code)
  
  # Check if the EPSG code is 4326
  if (crs == 4326) {
    # Get the extent of the raster
    extent <- ext(raster)
    
    # Calculate the center of the raster extent
    center_lon <- (extent$xmin + extent$xmax) / 2
    center_lat <- (extent$ymin + extent$ymax) / 2
    
    # Determine the UTM zone based on the center longitude
    utm_zone <- floor((center_lon + 180) / 6) + 1
    
    # Determine the EPSG code for the appropriate UTM zone
    epsg_code <- ifelse(center_lat >= 0, 32600 + utm_zone, 32700 + utm_zone)
    
    } else {
      # Return the current EPSG code of the raster's CRS
      epsg_code <- crs
  }
  
  return(epsg_code)
}



# Function to project rasters to correct UTM and return a list
project_raster_list <- function(raster_list) {
  # Internal function to project a single raster
  project_raster <- function(raster) {
    new_epsg <- get_utm_epsg(raster)
    projected_raster <- terra::project(raster, paste0("epsg:", new_epsg))
    return(projected_raster)
  }
  
  # Apply the projection function to each raster in the list
  projected_rasters <- lapply(raster_list, project_raster)
  
  return(projected_rasters)
}



# Function to determine adequate buffer that should be applied around AOI for raster clip to avoid edge effects in statistical analyses 
# -> the buffer should be ideally be 10 * pixel-resolution, if exceeding raster extent use factor 5. Otherwise no buffer will be applied to AOI.
apply_buffer_check <- function(aoi, raster) {
  # Get the pixel resolution (assuming square pixels)
  res <- res(raster)
  pixel_resolution <- min(res) # Using the minimum resolution as the pixel resolution

  # Define buffer sizes
  buffer_10x <- 10 * pixel_resolution
  buffer_5x <- 5 * pixel_resolution

  # Function to check if buffered AOI is within raster extent
  is_within_raster_extent <- function(buffered_aoi, raster) {
    raster_extent <- as.polygons(ext(raster), crs = terra::crs(raster))
    raster_extent_sf <- st_as_sf(raster_extent)
    st_within(buffered_aoi, raster_extent_sf, sparse = FALSE)
  }

  # Apply 10x buffer and check
  buffered_aoi_10x <- st_buffer(aoi, dist = buffer_10x)
  if (all(is_within_raster_extent(buffered_aoi_10x, raster))) {
    print(paste0(names(raster[[1]]), ": A buffer of factor 10 (", buffer_10x, "m) was applied to AOI."))
    return(list(aoiBuff_sf = buffered_aoi_10x, buff = buffer_10x))
  } else {
    # Apply 5x buffer and check
    buffered_aoi_5x <- st_buffer(aoi, dist = buffer_5x)
    if (all(is_within_raster_extent(buffered_aoi_5x, raster))) {
      print(paste0(names(raster[[1]]), "A buffer of factor 5 (", buffer_5x, "m) was applied to AOI."))
      return(list(aoiBuff_sf = buffered_aoi_5x, buff = buffer_5x))
    } else {
      # No buffer can be applied
      warning("No buffer can be applied to AOI; returned original AOI")
      return(NULL)
    }
  }
}

# Wrapper function to apply the `apply_buffer_check`-function to a list of rasters
apply_buffer_check_to_list <- function(aoi, raster_list) {
  lapply(raster_list, function(raster) {
    apply_buffer_check(aoi, raster)
  })
}



# Function to clip rasters in a list with corresponding AOIs from another list
clip_rasters_with_aoi_list <- function(utm_raster_list, aoiBuff_list) {
  # Initialize the output list
  clipped_raster_buff_list <- list()
  
  # Iterate over the names in the lists and perform the clipping
  for (name in names(utm_raster_list)) {
    # Retrieve the corresponding raster and AOI
    raster <- utm_raster_list[[name]]
    aoi <- aoiBuff_list[[name]]$aoiBuff_sf
    
    # Check if the AOI is not null
    if (!is.null(aoi)) {
      # Clip the raster with the AOI
      clipped_raster <- crop(raster, aoi)
      
      # Mask the raster with the AOI
      clipped_raster <- terra::mask(clipped_raster, aoi)
      
      # Store the clipped raster in the output list
      clipped_raster_buff_list[[name]] <- clipped_raster
    } else {
      warning(paste("No AOI found for", name, "; skipping this raster."))
    }
  }
  
  # Return the list of clipped rasters
  return(clipped_raster_buff_list)
}



# Function to clip rasters in a list with a single AOI
clip_rasters_with_aoi_sf <- function(utm_raster_list, aoi_utm) {
  # Initialize the output list
  clipped_raster_list <- list()
  
  # Iterate over the names in the utm_raster_list and perform the clipping
  for (name in names(utm_raster_list)) {
    # Retrieve the corresponding raster
    raster <- utm_raster_list[[name]]
    
    # Clip the raster with the AOI
    clipped_raster <- crop(raster, aoi_utm)
    
    # Mask the raster with the AOI
    clipped_raster <- terra::mask(clipped_raster, aoi_utm)
    
    # Store the clipped raster in the output list
    clipped_raster_list[[name]] <- clipped_raster
  }
  
  # Return the list of clipped rasters
  return(clipped_raster_list)
}



#'####################################################################################################################
# EXPORT FILES ####

# Function to define band names depending on System
system_bands <- function(FlightName){
  if (endsWith(FlightName, "DJIM300L1")){
    bands <- list(dem = "DEM")
    
  } else if (endsWith(FlightName, "DJIM300MXDual")){
    bands <- list(dsm = "DSM", 
                  om = c("CBlue-444", "Blue-475", "Green-531", "Green-560", "Red-650", "Red-668", "Red-edge-705", "Red-edge-717", "Red-edge-740", "NIR-840")) # band info from https://support.micasense.com/hc/en-us/articles/214878778-What-is-the-center-wavelength-and-bandwidth-of-each-filter-for-MicaSense-sensors & https://agisoft.freshdesk.com/support/solutions/articles/31000161029-how-to-add-micasense-rededge-mx-dual-data-properly
    
    # } else if (endsWith(FlightName, "DJIM300H20T")){
    #   bands <- list(dsm = "DSM", 
    #                 om = c("???"))
    
  } else if (endsWith(FlightName, "WingtraAltum")){
    bands <- list(dsm = "DSM", 
                  om = c("Blue", "Green", "Red", "Red-edge717", "NIR", "LWIR"))
    
  } else if (endsWith(FlightName, "WingtraRX1RII")){
    bands <- list(dsm = "DSM", 
                  om = c("Blue", "Green", "Red"))
    
  }
  return(bands)
}



# Function to round and format resolution
round_and_format <- function(resolution) {
    # Round the first part to three decimal places
  rounded <- round(as.numeric(resolution), digits = 3)
  
  # Format the rounded value
  formatted <- gsub("\\.", "-", as.character(rounded))
  
  return(formatted)
}



# Function to process raster_info list and generate rounded res export name
process_raster_info <- function(raster_info) {
  resolution_x_list <- lapply(raster_info, function(info) {
    resolution <- info$resolution[1]
    resolution_x <- round_and_format(resolution)
    return(resolution_x)
  })
  return(resolution_x_list)
}



# Function to rename bands of spatialRaster and add FlightName to each band name
rename_bands <- function(FlightName, raster_list, raster_info){
  # Get the system bands based on FlightName
  bands <- system_bands(FlightName)
  
  # Process raster_info to get the resolutions
  resolution_x_list <- process_raster_info(raster_info)
  
  # Use lapply to iterate over the items in raster_list and rename bands
  raster_list <- lapply(names(raster_list), function(name) {
    raster <- raster_list[[name]]
    if (!is.null(bands[[name]])) {
      # Add FlightName and resolution to each band name
      new_band_names <- paste0(FlightName, "_", bands[[name]], "_res", resolution_x_list[[name]])
      names(raster) <- new_band_names
    }
    return(raster)
  })
  
  # Preserve the names of the original raster_list
  names(raster_list) <- names(raster_info)
  
  return(raster_list)
}




# Function to generate export filename for buffered raster
generate_export_buff_path <- function(ExportPath, FlightName, type, resolution_x, buffer_x) {
  current_date <- format(Sys.Date(), "%Y%m%d")
  ExportPathBuff <- paste0(ExportPath, "0_Raster/bufferedRaster/")
  if (!dir.exists(ExportPathBuff)) {
    dir.create(ExportPathBuff, recursive = TRUE)
  }
  export_buff_name <- paste0(ExportPathBuff, FlightName, "_", toupper(type), "_res", resolution_x, "m_buff", buffer_x, "m_", current_date, ".tif")
  return(export_buff_name)
}



# Function to generate export filename for raster
generate_export_path <- function(ExportPath, FlightName, type, resolution_x) {
  current_date <- format(Sys.Date(), "%Y%m%d")
  export_name <- paste0(ExportPath, "0_Raster/", FlightName, "_", toupper(type), "_res", resolution_x, "m_", current_date, ".tif")
  return(export_name)
}



# Function to export raster as TIFF with a generated name
export_rasters <- function(clipped_rasters_buff_list, ExportPath, FlightName, raster_info, aoiBuff) {
  # Process raster_info to get the resolutions
  resolution_x_list <- process_raster_info(raster_info)
  
  # Iterate over the items in clipped_rasters_buff_list and export them to subfolder "bufferedRaster" in 0_Raster
  lapply(names(clipped_rasters_buff_list), function(name) {
    raster <- clipped_rasters_buff_list[[name]]
    resolution_x <- resolution_x_list[[name]]
    buffer <- aoiBuff[[name]][[2]]
    buffer_x <- round_and_format(buffer)
    export_path_buff <- generate_export_buff_path(ExportPath, FlightName, name, resolution_x, buffer_x)
    
    # Get band names for multi-band rasters
    band_names <- names(raster)
    
    writeRaster(raster, filename = export_path_buff, names = band_names, overwrite = TRUE)
    cat("Raster exported:", export_path_buff, "\n")
  })
  
  # Clip all buffered raster to aoi and export them to final 0_Raster folder
  clipped_rasters_list <- clip_rasters_with_aoi_sf(clipped_rasters_buff_list, aoi_utm)
  
  # Iterate over the items in clipped_rasters_list and export them
  lapply(names(clipped_rasters_list), function(name) {
    raster <- clipped_rasters_list[[name]]
    resolution_x <- resolution_x_list[[name]]
    export_path <- generate_export_path(ExportPath, FlightName, name, resolution_x)
    
    # Get band names for multi-band rasters
    band_names <- names(raster)
    
    writeRaster(raster, filename = export_path, names = band_names, overwrite = TRUE)
    cat("Raster exported:", export_path, "\n")
  })
}



# Function to generate export filename for buffered LAS file
generate_export_buff_path_las <- function(ExportPath, FlightName, buffer_x) {
  current_date <- format(Sys.Date(), "%Y%m%d")
  ExportPathBuff <- paste0(ExportPath, "1_PointClouds/bufferedPC/")
  if (!dir.exists(ExportPathBuff)) {
    dir.create(ExportPathBuff, recursive = TRUE)
  }
  export_buff_name <- paste0(ExportPathBuff, FlightName, "_PC_buff", buffer_x, "m_", current_date, ".las")
  return(export_buff_name)
}



# Function to generate export filename for LAS file
generate_export_path_las <- function(ExportPath, FlightName) {
  current_date <- format(Sys.Date(), "%Y%m%d")
  export_name <- paste0(ExportPath, "1_PointClouds/", FlightName, "_PC_" , current_date, ".las")
  return(export_name)
}



# Function to export pointcloud as LAS with a generated name
export_pointcloud <- function(pointcloud_list, ExportPath, FlightName, aoiBuff) {
  # Export buffered point cloud
  pc_buff <- pointcloud_list[[1]]
  pc <- pointcloud_list[[2]]
  
  buffer <- aoiBuff[[1]][[2]]
  buffer_x <- round_and_format(buffer)  # Assuming `round_and_format` is similar to `round`
  
  export_path_buff <- generate_export_buff_path_las(ExportPath, FlightName, buffer_x)
  export_path <- generate_export_path_las(ExportPath, FlightName)
  
  writeLAS(pc_buff, export_path_buff)
  cat("Point cloud exported:", export_path_buff, "\n")
  
  writeLAS(pc, export_path)
  cat("Point cloud exported:", export_path, "\n")
}


#'####################################################################################################################
# END
