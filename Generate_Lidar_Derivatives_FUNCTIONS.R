#'####################################################################################################################
# FUNCTIONS to generate base products for German Forests project
# 18.07.2024
# Luisa Pflumm
#'####################################################################################################################



# Function to check if file exists
file_exists <- function(file_path) {
  file.exists(file_path)
}



### Function to check if DJIM300L1 base products are exsisting and import them
import_base_products_l1_buff <- function(RootPath, mission, FlightName) {
  if (endsWith(FlightName, "DJIM300L1")) {
    # Construct the full paths
    raster_path <- paste0(RootPath, mission, "/2_Results/0_Raster/bufferedRaster/")
    las_path <- paste0(RootPath, mission, "/2_Results/1_PointClouds/bufferedPC/")
   
    # List all files in the directories without full path names
    tif_file <- list.files(raster_path, pattern = "\\.tif$", full.names = TRUE)
    las_file <- list.files(las_path, pattern = "\\.las$", full.names = TRUE)
    
    # Use grepl to filter files containing the flightname
    tif_file <- tif_file[grepl(FlightName, tif_file)]
    las_file <- las_file[grepl(FlightName, las_file)]
    
    # Check if the .tif and .las files exists
    if (!file.exists(tif_file)) {
      stop(paste("No DJIM300L1 DEM was found in", raster_path, "folder"))
    }
    
    if (!file.exists(las_file)) {
      stop(paste("No DJIM300L1 Point Cloud was found in", las_path, "folder"))
    }
    
    # Load the .tif file
    raster_data <- rast(tif_file)
    
    # Load the .las file
    lidar_data <- readLAS(las_file)
    
    # Return the l1 base products in list
    return(list(dem = raster_data, pointcloud = lidar_data))
    } else {
      print("Base products could not be found in folder. Check if they already were generated.")
  }
}



### Function to calculate different Canopy Height Models / Digital Surface Models
calculate_topALSreturns <- function(nlas, model, resolution) {
  # Convert model to uppercase
  model <- toupper(model)
  
  # Check if y is not "CHM" or "DSM"
  if (model != "CHM" && model != "DSM") {
    stop("Please specify 'CHM' or 'DSM'.")
  }
  
  # Point-to-raster methods
  mod_ptr_1 <- rasterize_canopy(nlas, res = resolution, p2r())
  mod_ptr_2 <- rasterize_canopy(nlas, res = resolution, algorithm = p2r(subcircle = 0.15))
  mod_ptr_3 <- rasterize_canopy(nlas, res = resolution, algorithm = p2r(0.2, na.fill = tin()))
  
  # Post-processing a point-to-raster CHM or DSM model
  fill.na <- function(x, i = 5) { if (is.na(x)[i]) { return(mean(x, na.rm = TRUE)) } else { return(x[i]) } }
  w <- matrix(1, 3, 3)
  
  filled1 <- terra::focal(mod_ptr_1, w, fun = fill.na)
  filled2 <- terra::focal(mod_ptr_2, w, fun = fill.na)
  filled3 <- terra::focal(mod_ptr_3, w, fun = fill.na)
  
  smoothed1 <- terra::focal(mod_ptr_1, w, fun = mean, na.rm = TRUE)
  smoothed2 <- terra::focal(mod_ptr_2, w, fun = mean, na.rm = TRUE)
  smoothed3 <- terra::focal(mod_ptr_3, w, fun = mean, na.rm = TRUE)
  
  mods_ptr_1 <- c(mod_ptr_1, filled1, smoothed1)
  mods_ptr_2 <- c(mod_ptr_2, filled2, smoothed2)
  mods_ptr_3 <- c(mod_ptr_3, filled3, smoothed3)
  
  names(mods_ptr_1) <- c("Z_Base", "Z_Filled", "Z_Smoothed")
  names(mods_ptr_2) <- c("Z_Base", "Z_Filled", "Z_Smoothed")
  names(mods_ptr_3) <- c("Z_Base", "Z_Filled", "Z_Smoothed")
  
  # Triangulation methods
  mod_tri_1 <- rasterize_canopy(nlas, res = resolution, algorithm = dsmtin())
  mod_tri_2 <- rasterize_canopy(nlas, res = resolution, algorithm = dsmtin(max_edge = 8))
  
  # Store all CHM or DSM objects in a list
  mod_list <- list(
    mod_ptr_1 = mods_ptr_1,
    mod_ptr_2 = mods_ptr_2,
    mod_ptr_3 = mods_ptr_3,
    mod_tri_1 = mod_tri_1,
    mod_tri_2 = mod_tri_2
  )
  
  # Rename the list elements
  mod <- tolower(model)
  names(mod_list) <- c(
    paste0(mod, "_ptr_1"),
    paste0(mod, "_ptr_2"),
    paste0(mod, "_ptr_3"),
    paste0(mod, "_tri_1"),
    paste0(mod, "_tri_2")
  )
  
  # Plot all CHM or DSM objects
  plot(mod_ptr_1, col = height.colors(25), main = paste0(model, " (ptr) v1"))
  plot(mod_ptr_2, col = height.colors(25), main = paste0(model, " (ptr) v2"))
  plot(mod_ptr_3, col = height.colors(25), main = paste0(model, " (ptr) v3"))
  
  plot(mods_ptr_1, col = height.colors(25))
  title(main = paste0(model, " (ptr) v1"),, outer=TRUE, line=-2)
  plot(mods_ptr_2, col = height.colors(25))
  title(main = paste0(model, " (ptr) v2"), outer=TRUE, line=-2)
  plot(mods_ptr_3, col = height.colors(25))
  title(main = paste0(model, " (ptr) v3"), outer=TRUE, line=-2)
  
  plot(mod_tri_1, col = height.colors(25), main = paste0(model, " (tri) v1"))
  plot(mod_tri_2, col = height.colors(25), main = paste0(model, " (tri) v2"))
  
  return(mod_list)
}


