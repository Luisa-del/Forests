#'####################################################################################################################
# FUNCTIONS to generate base products for German Forests project
# 18.07.2024
# Luisa Pflumm
#'####################################################################################################################



### Function to check if file exists
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



# # Function to plot a subsample of a LAS point cloud -> Fast and simple, useful when you want a quick overview with minimal computation
plot_las <- function(las_file, sample_fraction = 0.001, main_title = "Point Cloud") {  # 0.01 plots only 1% of the points
  # Randomly sample points
  set.seed(5)
  las_subsampled <- las_file@data[sample(1:nrow(las_file@data), size = floor(sample_fraction * nrow(las_file@data))), ]
  
  # Plot the subsampled points
  plot(las_subsampled$X, las_subsampled$Y, 
       xlab = "X", ylab = "Y", 
       main = paste0(main_title, " (", 100 * sample_fraction, " % of points)"),
       pch = 19, 
       col = rgb(0, 0, 0, 0.5))
}
# # Function to plot .las files
# plot_las <- function(las_file, main_title) {
#   # Extract x, y, z coordinates from the .las file
#   xyz <- las_file@data[, c("X", "Y", "Z")]
#   plot(xyz$X, xyz$Y, xlab = "X", ylab = "Y", main = main_title, pch = 19, col = rgb(0, 0, 0, 0.5))
# }



### Function to plot data (rasters or .las files)  from list
plot_list_output <- function(data_list, n_cols) {
  # Calculate the number of items
  num_items <- length(data_list)
  
  # Set up the plotting area with n_cols columns
  par(mfrow = c(ceiling(num_items / n_cols), n_cols))
  
  # Flag to track if we have plotted .las files
  las_plotted <- FALSE
  
  # Loop through the list and plot each item
  for (item_name in names(data_list)) {
    item <- data_list[[item_name]]
    
    if (inherits(item, "SpatRaster")) {
      # Plot raster
      plot(item, main = item_name)
      
    } else if (inherits(item, "LAS") || inherits(item, "LAScatalog")) {
      # Plot .las file (point cloud)
      plot_las(item, main_title = item_name)
      las_plotted <- TRUE
      
    } else {
      warning(paste("Item", item_name, "is neither a raster nor a .las file. Skipping."))
    }
  }
  
  # If we have .las files, plot the remaining rasters next to them
  if (las_plotted) {
    # Reset the plotting layout to default
    par(mfrow = c(1, 1))
    
    # You might want to implement further logic here if you need to separate the rasters and .las files.
    # This function assumes rasters are plotted before or after .las files if present.
  } else {
    # Reset the plotting layout to default
    par(mfrow = c(1, 1))
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



### Function to calculate slope
calculate_slope <- function(rast) {
  slope <- terra::terrain(rast, v = "slope", unit = "degrees", neighbors = 4)
  #plot(slope, main = "slope")
  return(slope)
}


### Function to calculate aspect with bilinear interpolation
calculate_aspect <- function(rast) {
  aspect <- terra::terrain(rast, v = "aspect", unit = "degrees", neighbors = 4)
  #plot(aspect, main = "aspect")
  return(aspect)
}


### Function to calculate the Terrain Ruggedness Index (TRI) -> mean of the absolute differences between the value of a cell and its 8 surrounding cells
calculate_tri <- function(rast) {
  tri <- terra::terrain(rast, v = "TRI")
  #plot(tri, main = "tri")
  return(tri)
}


### Function to calculate the Topographic Position Index (TPI) -> difference between the value of a cell and the mean value of its 8 surrounding cells
calculate_tpi <- function(rast) {
  tpi <- terra::terrain(rast, v = "TPI")
  #plot(tpi, main = "tpi")
  return(tpi)
}


### Function to calculate roughness -> difference between the maximum and the minimum value of a cell and its 8 surrounding cells
calculate_roughness <- function(rast) {
  rough <- terra::terrain(rast, v = "roughness")
  #plot(rough, main = "roughness")
  return(rough)
}



### Function to calculate global statistics (summarize values) for list of raster
calculate_global_stats <- function(raster_list) {
  
  stat_functions = list(mean = mean, 
                        min = min,
                        max = max, 
                        sd = sd#,
                        #isNA = isNA,
                        #notNA = notNA
  )
  
  # Initialize an empty list to store results
  results_list <- list()
  
  # Loop through each raster and calculate statistics
  for (i in seq_along(raster_list)) {
    raster <- raster_list[[i]]
    
    # Initialize an empty named vector to store statistics for this raster
    stats <- c()
    
    # Loop through each statistic function and calculate it
    for (stat_name in names(stat_functions)) {
      stat_value <- global(raster, stat_functions[[stat_name]], na.rm = TRUE)
      
      # Round the statistic value
      stats[stat_name] <- round(stat_value, 3)
    }
    
    # Store the results for this raster in the list
    results_list[[i]] <- stats
  }
  
  # Combine all results into a data frame
  results_df <- do.call(rbind, results_list)
  
  # Add a column for raster names if the raster_list has names, otherwise use indices
  raster_names <- if (!is.null(names(raster_list))) names(raster_list) else paste0("raster", seq_along(raster_list))
  results_df <- data.frame(raster = raster_names, results_df)
  
  return(results_df)
}



### Function to create the ggplot to visualise AOI & Patches
plot_aois <- function(aoi_utm, patches_utm) {
  plot <- ggplot() +
    geom_sf(data = aoi_utm, fill = NA, color = "black") +
    geom_sf(data = patches_utm, fill = NA, color = "black") +
    geom_sf_text(data = patches_utm, aes(label = Patch.Nr), size = 3, color = "black") +
    theme_minimal()
  
  # Print the plot
  print(plot)
}



### Function to clip data (raster or .las files) by each polygon and store in a list of lists
clip_data_by_polygons <- function(list_data, polygon_sf) {
  
  # Initialize the output list
  list_data_patches <- list()
  
  # Iterate over each spatial data in the list
  for (data_name in names(list_data)) {
    
    data_item <- list_data[[data_name]]
    
    # List to store clipped data for the current spatial data layer
    data_patches <- list()
    
    # Iterate over each polygon in the multipolygon sf object
    for (i in seq_along(st_geometry(polygon_sf))) {
      
      # Get the current polygon and its Patch.Nr
      polygon <- polygon_sf[i, ]
      patch_nr <- polygon$Patch.Nr
      
      # Check if the item is a SpatRaster (terra package)
      if (inherits(data_item, "SpatRaster")) {
        
        # Crop the raster by the current polygon
        clipped_data <- terra::crop(data_item, polygon)
        
        # Optionally, mask the raster to ensure it aligns with the polygon shape
        # clipped_data <- mask(clipped_data, polygon)
        
        # Check if the item is a LAS object (lidR package)
      } else if (inherits(data_item, "LAS")) {
        
        # Clip the LAS file by the polygon geometry
        clipped_data <- clip_roi(data_item, st_geometry(polygon))
        
      } else {
        warning(paste("Item", data_name, "is neither a SpatRaster nor a LAS file. Skipping."))
        next
      }
      
      # Add the clipped data to the list, using Patch.Nr as the name
      data_patches[[as.character(patch_nr)]] <- clipped_data
    }
    
    # Store the list of clipped data under the current spatial data name
    list_data_patches[[data_name]] <- data_patches
  }
  
  return(list_data_patches)
}



### Function to make 5 x 5 m grid on top of patches
make_grids <- function(list_patches, res) {
  # Create a grid of 5x5m covering the extent of the polygon
  grid <- st_make_grid(list_patches, cellsize = c(res, res), square = TRUE)
  # Convert to sf object
  grid <- st_sf(geometry = grid)
  # Assign the Patch.Nr to each grid cell; and ensure that only grid cells within/touching the patches are kept (`left = FALSE`)
  grid <- st_join(grid, list_patches["Patch.Nr"], left = FALSE) %>% mutate(cell.id = row_number())
  # Clip the grid with your polygon
  patches_grid <- st_intersection(grid, list_patches) %>% dplyr::select(Patch.Nr, cell.id) #%>% mutate(cell.id = row_number())
  # Keep only full grid cells within the polygon
  patches_grid_fullCell <- st_filter(grid, list_patches, .predicate = st_within)
  # Reset cell.id for each Patch.Nr
  patches_grid_fullCell <- patches_grid_fullCell %>%
    group_by(Patch.Nr) %>%
    mutate(cell.id = row_number()) %>%
    ungroup()
  
  # List grid-objects
  grid_list <- list(patches_grid = patches_grid, patches_grid_fullCell = patches_grid_fullCell)
  
  return(grid_list)
}

  
  
### Function to create the ggplot to visualise the original patches and the resulting grids
plot_grids <- function(grid_list, aoi_utm, patches_utm) {
  plot <- ggplot() +
  geom_sf(data = aoi_utm, fill = NA, color = "black") +
  geom_sf(data = patches_utm, fill = NA, color = "black") +
  geom_sf(data = grid_list$patches_grid, fill = "blue", alpha = 0.5) +
  geom_sf(data = grid_list$patches_grid_fullCell, fill = "red", alpha = 0.5) +
  geom_sf_text(data = patches_utm, aes(label = Patch.Nr), size = 3, color = "black", nudge_y = 50.5) +
  #geom_sf_text(data = patches_grid, aes(label = cell.id), size = 3, color = "black") +                          # show cell.ids
  #scale_color_manual(name = "Grid Type", values = c("Intersecting Cells" = "blue", "Full Cells" = "red")) +     # add legend -> not working yet
  theme_minimal()

# Print the plot
print(plot)
}



### Function to convert nested list to data frame
flatten_nested_list_to_df <- function(nested_list) {
  # Function to process each data frame in the list
  process_df <- function(df, type) {
    df %>%
      mutate(across(where(is.list), unlist)) %>%
      rename(Patch.Nr = raster) %>%
      mutate(raster = type) %>%
      dplyr::select(raster, everything())  # Ensure 'type' is the first column
  }
  
  # Process each data frame in the nested list
  combined_data <- map2(nested_list, names(nested_list), process_df)
  
  # Combine all processed data frames into one
  final_df <- bind_rows(combined_data)
  
  return(final_df)
}



### Function to plot partial pointcloud with transect
plot_las_with_transect <- function(las_data, aoi){
  
  # Call plot_las function
  plot_las(nlas, main_title = "Tansect over 2-D pointcloud plot")
  
  # Decimate the point cloud to 50% of the points
  nlas_decimated <- decimate_points(nlas, homogenize(2))  # Keeps every 2nd point (approx. 50%), performs a more aggressive reduction in both the points and the data structure, resulting in a significantly smaller file.
  #nlas_sampled <- filter_poi(nlas, runif(length(Z)) < 0.5)  # Randomly keeps 50% of points, does a selective filtering of points but retains much of the original LAS file's structure, resulting in a larger file size.
  
  # Get bbox (extent) of aoi
  bbox <- st_bbox(aoi)
  
  # Extract coordinates from bounding box
  top_left <- c(x = as.numeric(bbox["xmin"]), y = as.numeric(bbox["ymax"]))  # Top-left (Northwest)
  bottom_right <- c(x = as.numeric(bbox["xmax"]), y = as.numeric(bbox["ymin"]))  # Bottom-right (Southeast)
  
  # Create the data frame for transect points
  transect_points <- data.frame(
    X = c(top_left["x"], bottom_right["x"]),
    Y = c(top_left["y"], bottom_right["y"])
  )
  
  # Check if transect points are provided
  if (!is.null(transect_points) && nrow(transect_points) == 2) {
    # Add the transect as a purple line
    lines(transect_points$X, transect_points$Y, col = "purple", lwd = 2)
  }
  
  # Clip the transect using the decimated point cloud
  las_tr <- clip_transect(nlas_decimated, top_left, bottom_right, width = 5, xz = TRUE)
  
  # Plot the transect
  ggplot(payload(las_tr), aes(X,Z, color = Z)) + 
    geom_point(size = 0.5) + 
    coord_equal() + 
    theme_minimal() +
    scale_color_gradientn(colours = height.colors(50)) +
    ggtitle("Transect plot (50% of transect points)")

}



# ### Function to calculate BE_H indices
# calculate_be_h_stats <- function(las_data) {
#   # Extract heights (define the height column as Z)
#   heights <- las_data$Z
#   # # Replace negative height values with zero
#   # heights[heights < 0] <- 0
#   
#   # Function to calculate the indices
#   indices <- list(
#     BE_H_MIN = min(heights, na.rm = TRUE),
#     BE_H_MAX = max(heights, na.rm = TRUE),
#     BE_H_MEAN = mean(heights, na.rm = TRUE),
#     BE_H_MEDIAN = median(heights, na.rm = TRUE),
#     BE_H_SD = sd(heights, na.rm = TRUE),
#     BE_H_VAR = var(heights, na.rm = TRUE),
#     BE_H_VAR_COEF = sd(heights, na.rm = TRUE) / mean(heights, na.rm = TRUE),
#     BE_H_SKEW = e1071::skewness(heights, na.rm = TRUE),
#     BE_H_KURTOSIS = e1071::kurtosis(heights, na.rm = TRUE)
#   )
#   
#   # Percentiles
#   percentiles <- quantile(heights, probs = seq(0, 1, 0.1), na.rm = TRUE)
#   names(percentiles) <- c("BE_H_P0", paste0("BE_H_P", seq(10, 100, 10)))
#   indices <- c(indices, as.list(percentiles))
#   
#   return(indices)
# }

calculate_be_h_stats <- function(las_data) {
  # Extract heights (define the height column as Z)
  heights <- las_data$Z
  
  # Calculate percentiles (0%, 10%, 20%, ..., 100%)
  percentiles <- quantile(heights, probs = seq(0, 1, 0.1), na.rm = TRUE)
  names(percentiles) <- c("BE_H_P0", paste0("BE_H_P", seq(10, 100, 10)))
  
  # Initialize an empty list to store the data frames for each percentile group
  percentile_stats_list <- list()
  
  # Loop over each percentile and calculate statistics for the heights <= percentile value
  for (name in names(percentiles)) {
    # Filter heights that are less than or equal to the current percentile value
    filtered_heights <- heights[heights <= percentiles[[name]]]
    
    # Calculate statistics for the filtered heights
    stats <- data.frame(
      BE_H_MAX = max(filtered_heights, na.rm = TRUE),
      BE_H_MIN = min(filtered_heights, na.rm = TRUE),
      BE_H_MEAN = mean(filtered_heights, na.rm = TRUE),
      BE_H_MEDIAN = median(filtered_heights, na.rm = TRUE),
      BE_H_SD = sd(filtered_heights, na.rm = TRUE),
      BE_H_VAR = var(filtered_heights, na.rm = TRUE),
      BE_H_VAR_COEF = sd(filtered_heights, na.rm = TRUE) / mean(filtered_heights, na.rm = TRUE),
      BE_H_SKEW = e1071::skewness(filtered_heights, na.rm = TRUE),
      BE_H_KURTOSIS = e1071::kurtosis(filtered_heights, na.rm = TRUE)
    )
    
    # Add the percentile value to the statistics
    stats$Percentile_Height <- percentiles[[name]]
    
    # Store the data frame in the list with the name of the percentile
    percentile_stats_list[[name]] <- stats
  }
  
  return(percentile_stats_list)
}



### Function to calculate vegetation layer density by dividing the height into layers, counting points per layer, and then computing density.
#' Adjust the layer_height argument to define different layer heights (e.g., 0.5m, 2m).
#' Density can be normalized by dividing by the total number of points (for proportion) or by the height of the layer (for points per meter).
calculate_pointsStats_1m_layer 

calculate_layer_pointStats <- function(las_data, layer_height = 1) {
  # # Optional: Filter the data to exclude points with heights below 0
  # if (any(las_data$Z < 0)) {las_data <- las_data[las_data$Z >= 0, ]} else {las_data <- las_data}

  # Extract heights (assuming Z column contains the heights)
  heights <- las_data$Z
  heights <- na.omit(las_data$Z)

  # Define the layer boundaries (e.g., every 1 meter)
  max_height <- max(heights, na.rm = TRUE)
  layers <- seq(0, ceiling(max_height), by = layer_height)

  # Assign each height to a layer
  height_layers <- cut(heights, breaks = layers, include.lowest = TRUE, right = FALSE)

  # Count the number of points in each layer
  layer_counts <- table(height_layers)

  # Check that the total number of points matches
  length(height_layers) == sum(layer_counts)
  
  # Create a vector for the cumulative counts
  cumulative_counts <- cumsum(layer_counts)
  
  # Calculate penetration rate: layer_counts / cumulative_counts
  penetration_rate <- layer_counts / cumulative_counts

  # Calculate point density (as proportion of total points, %)
  Point_Density <- layer_counts / sum(layer_counts)   
  
  # Count points per height layer 
  if (layer_height == 1){
    Point_Count <- layer_counts / layer_height
  } else if (layer_height == 0.1){
    Point_Count <- (layer_counts / layer_height) / 10
  } else {
    stop("Layer height should be either 1 or 0.1.")
  }

  # Convert to data frames
  Point_Density <- as.data.frame(Point_Density)
  Point_Count <- as.data.frame(Point_Count)
  Penetration_Rate <- as.data.frame(penetration_rate)

  # Create a data frame with the results
  layer_density_df <- data.frame(
    Height_Layer_m = Point_Density$height_layers,     # Layer boundaries (e.g., "[0,1)", "[1,2)")
    Point_Density = Point_Density$Freq,               # Density as proportion of total points
    Point_Count = Point_Count$Freq,                  # Density as points per meter height
    Penetration_Rate = Penetration_Rate$Freq
  )

  return(layer_density_df)
}


sum(layer_density_df$Penetration_Rate)




### Same function as above, added penetration rate for 3 veg layers
calculate_veg_layer_pointStats <- function(las_data, grd = 2, reg = 8, und = 20) {
  # Specify meter bins
  layer_height = 0.1

  # Calculate the layer densities using the specified layer height (0.1 meter bins)
  layer_density_df <- calculate_layer_pointStats(las_data, layer_height)

  # Extract the lower and upper bounds of each bin from the Layer column using regex
  bin_bounds <- stringr::str_match(as.character(layer_density_df$Height_Layer_m), "\\[(\\d+(\\.\\d+)?), ?(\\d+(\\.\\d+)?)\\)")

  # Convert the extracted bounds to numeric
  lower_bounds <- as.numeric(bin_bounds[, 2])
  upper_bounds <- as.numeric(bin_bounds[, 4])

  # Define the indices for the layers based on your input thresholds
  ground_layer_indices <- which(lower_bounds >= 0 & upper_bounds <= grd)         # Ground layer: 0m to grd
  regeneration_layer_indices <- which(lower_bounds >= grd & upper_bounds <= reg) # Regeneration layer: grd to reg
  understory_layer_indices <- which(lower_bounds >= reg & upper_bounds <= und)   # Understory layer: reg to und
  canopy_layer_indices <- which(lower_bounds >= und)                             # Canopy layer: und to max

  # Extract the density and point count values for each bin
  point_density_per_bin <- layer_density_df$Point_Density
  point_count_per_bin <- layer_density_df$Point_Count

  # Calculate total point count
  total_point_count <- sum(point_count_per_bin, na.rm = TRUE)      # or nrow(las_data)
  
  if (total_point_count != nrow(las_data)){
    warning("total_point_count does not equal nrow(las_data)")
  }

  ### Sum the point (return?) densities for each layer
  BE_RD_GND <- sum(point_density_per_bin[ground_layer_indices], na.rm = TRUE)                   # ground_density
  BE_RD_REG <- sum(point_density_per_bin[regeneration_layer_indices], na.rm = TRUE)             # regeneration_density
  BE_RD_UND <- sum(point_density_per_bin[understory_layer_indices], na.rm = TRUE)               # understory_density
  BE_RD_CAN <- sum(point_density_per_bin[canopy_layer_indices], na.rm = TRUE)                   # canopy_density

  ### Sum the point counts for each layer
  ground_point_count <- sum(point_count_per_bin[ground_layer_indices], na.rm = TRUE)
  regeneration_point_count <- sum(point_count_per_bin[regeneration_layer_indices], na.rm = TRUE)
  understory_point_count <- sum(point_count_per_bin[understory_layer_indices], na.rm = TRUE)
  canopy_point_count <- sum(point_count_per_bin[canopy_layer_indices], na.rm = TRUE)

  # Cumulative point counts for penetration rate calculations
  regeneration_and_below_point_count <- sum(point_count_per_bin[c(ground_layer_indices, regeneration_layer_indices)], na.rm = TRUE)
  understory_and_below_point_count <- sum(point_count_per_bin[c(ground_layer_indices, regeneration_layer_indices, understory_layer_indices)], na.rm = TRUE)

  # Penetration Rates
  BE_PR_GRD <- ground_point_count / ground_point_count  # will be 1
  BE_PR_REG <- regeneration_point_count / regeneration_and_below_point_count
  BE_PR_UND <- understory_point_count / understory_and_below_point_count
  BE_PR_CAN <- canopy_point_count / total_point_count

  # Return the densities, point counts, and penetration rates as a list
  return(list(single_layer_stats = layer_density_df,
              Point_Count_per_veg_layer = data.frame(
                                             Ground_Points = ground_point_count,
                                             Regeneration_Points = regeneration_point_count,
                                             Understory_Points = understory_point_count,
                                             Canopy_Points = canopy_point_count),
              Point_Density_per_veg_layer = data.frame(
                                             BE_RD_GND = BE_RD_GND,
                                             BE_RD_REG = BE_RD_REG,
                                             BE_RD_UND = BE_RD_UND,
                                             BE_RD_CAN = BE_RD_CAN),
              Penetration_Rate_per_veg_layer = data.frame(
                                             BE_PR_GRD = BE_PR_GRD,
                                             BE_PR_REG = BE_PR_REG,
                                             BE_PR_UND = BE_PR_UND,
                                             BE_PR_CAN = BE_PR_CAN)
  ))
}
   
#   ### Calculate 1-Meter Layer Penetration Rates and Return Densities (BE_PR/RD_01, BE_PR/RD_02, ..., BE_PR/RD_maxHeight)
#   # Extract maximum point height
#   max_height = floor(max(las_data$Z, na.rm = TRUE))  # Floor ensures we handle full meter intervals
#   # Define empty container
#   one_meter_layer_pr <- c()
#   one_meter_layer_rd <- c()
#   # # Calculate for all layer
#   # for (i in 1:(max_height - 1)) {
#   #   layer_indices <- which(lower_bounds >= i & upper_bounds <= (i + 1))
#   #   layer_point_count <- sum(point_count_per_bin[layer_indices], na.rm = TRUE)
#   #   
#   #   # Penetration rate for this layer
#   #   penetration_rate <- layer_point_count / total_point_count
#   #   
#   #   # Return density for this layer (all points below included)
#   #   return_density <- sum(point_count_per_bin[which(lower_bounds < (i + 1))], na.rm = TRUE) / total_point_count
#   #   
#   #   # Store penetration rate and return density
#   #   one_meter_layer_pr <- c(one_meter_layer_pr, penetration_rate)
#   #   one_meter_layer_rd <- c(one_meter_layer_rd, return_density)
#   # }
#   # 
#   # # Name the elements for penetration rate and return density
#   # names(one_meter_layer_pr) <- paste0("BE_PR_", sprintf("%02d", 1:max_height))
#   # names(one_meter_layer_rd) <- paste0("BE_RD_", sprintf("%02d", 1:max_height))
#   # 
#   # # Convert to data frames
#   # one_meter_layer_pr <- data.frame(one_meter_layer_pr)
#   # one_meter_layer_rd <- data.frame(one_meter_layer_rd)
#   
#   # Calculate for all layers (starting from layer 1 to max_height)
#   for (i in 1:max_height) {
#     # Points in the current layer (from i-1 to i meters)
#     layer_indices <- which(las_data$Z >= (i-1) & las_data$Z < i)
#     layer_point_count <- length(layer_indices)
#     
#     # Points in the current layer and below
#     layer_and_below_indices <- which(las_data$Z < i)
#     layer_and_below_point_count <- length(layer_and_below_indices)
#     
#     # Penetration rate for this layer
#     penetration_rate <- layer_point_count / layer_and_below_point_count
#     
#     # Return density for this layer (since height_of_layer is 1 meter, it's simply layer_point_count)
#     return_density <- layer_point_count / 1  # or just layer_point_count
#     
#     # Store penetration rate and return density
#     one_meter_layer_pr <- c(one_meter_layer_pr, penetration_rate)
#     one_meter_layer_rd <- c(one_meter_layer_rd, return_density)
#   }
#   
#   # Name the elements for penetration rate and return density
#   names(one_meter_layer_pr) <- paste0("BE_PR_", sprintf("%02d", 1:max_height))
#   names(one_meter_layer_rd) <- paste0("BE_RD_", sprintf("%02d", 1:max_height))
#   
#   # Convert to data frames
#   one_meter_layer_pr <- data.frame(one_meter_layer_pr)
#   one_meter_layer_rd <- data.frame(one_meter_layer_rd)
#   
#   ### Return the densities, point counts, penetration rates, and 1-meter penetration rates as a list           
#   return(list(Point_Density_per_layer = layer_density_df,  
#               Point_Density_per_1m_layer = calculate_layer_pointStats(las_data, layer_height = 1), 
#               Return_Density_per_veg_layer = data.frame(BE_RD_GND = BE_RD_GND,
#                                              BE_RD_REG = BE_RD_REG,
#                                              BE_RD_UND = BE_RD_UND,
#                                              BE_RD_CAN = BE_RD_CAN),
#               Penetration_Rate_per_veg_layer = data.frame(BE_PR_REG = BE_PR_REG,  
#                                              BE_PR_UND = BE_PR_UND,
#                                              BE_PR_CAN = BE_PR_CAN),
#               Return_Density_per_1m_layer = one_meter_layer_rd,
#               Penetration_Rate_per_veg_layer = one_meter_layer_pr
#   ))
# }









#' calculate_vegetation_layer_pr <- function(las_data, grd = 2, reg = 8, und = 20) {
#' 
#'   #' Count points per 0.1 m layer ------------------------------------------------------------------------------------
#'   layer_height = 0.1
#' 
#'   # Calculate the layer densities using the specified layer height (0.1 meter bins)
#'   layer_density_df <- calculate_layer_pointStats(las_data, layer_height)
#' 
#'   # Extract the lower and upper bounds of each bin from the Layer column using regex
#'   bin_bounds <- stringr::str_match(as.character(layer_density_df$Layer), "\\[(\\d+(\\.\\d+)?), ?(\\d+(\\.\\d+)?)\\)")
#' 
#'   # Convert the extracted bounds to numeric
#'   lower_bounds <- as.numeric(bin_bounds[, 2])
#'   upper_bounds <- as.numeric(bin_bounds[, 4])
#' 
#'   # Define the indices for the layers based on your input thresholds
#'   ground_layer_indices <- which(lower_bounds >= 0 & upper_bounds <= grd)        # Ground layer: 0m to grd
#'   regeneration_layer_indices <- which(lower_bounds >= grd & upper_bounds <= reg) # Regeneration layer: grd to reg
#'   understory_layer_indices <- which(lower_bounds >= reg & upper_bounds <= und)   # Understory layer: reg to und
#'   canopy_layer_indices <- which(lower_bounds >= und)                            # Canopy layer: und to max
#' 
#'   # Extract the density values for each bin
#'   density_per_bin <- layer_density_df$Density
#' 
#'   # Sum the densities for each layer
#'   ground_density <- sum(density_per_bin[ground_layer_indices], na.rm = TRUE)
#'   regeneration_density <- sum(density_per_bin[regeneration_layer_indices], na.rm = TRUE)
#'   understory_density <- sum(density_per_bin[understory_layer_indices], na.rm = TRUE)
#'   canopy_density <- sum(density_per_bin[canopy_layer_indices], na.rm = TRUE)
#' 
#'   #' Count points per 0.1 m layer ------------------------------------------------------------------------------------
#' 
#'   # # Return the densities as a data frame              # -> this returns only densities, original version
#'   # return(data.frame(
#'   #   Ground_Density = ground_density,
#'   #   Regeneration_Density = regeneration_density,
#'   #   Understory_Density = understory_density,
#'   #   Canopy_Density = canopy_density
#'   # ))
#' 
#'   # Count the number of ground points in each layer              # -> this needs to be added to calculate penetration rates for different layers
#'   ground_points <- filter_poi(las_data, Classification == 2)
#'   total_ground_points <- npoints(ground_points)
#' 
#'   ground_layer_counts <- sapply(list(ground_layer_indices, regeneration_layer_indices, understory_layer_indices, canopy_layer_indices), function(indices) {
#'     sum(density_per_bin[indices] * layer_height, na.rm = TRUE) * total_ground_points
#'   })
#' 
#'   # Count the total number of points in each layer
#'   total_points <- npoints(las_data)
#' 
#'   # Count the number of points in each layer
#'   layer_counts <- sapply(list(ground_layer_indices, regeneration_layer_indices, understory_layer_indices, canopy_layer_indices), function(indices) {
#'     sum(density_per_bin[indices] * layer_height, na.rm = TRUE) * total_points
#'   })
#' 
#'   # Return the densities and point counts as a data frame
#'   return(data.frame(
#'     Ground_Density = ground_density,
#'     Regeneration_Density = regeneration_density,
#'     Understory_Density = understory_density,
#'     Canopy_Density = canopy_density,
#'     Count_Ground_Layer = layer_counts[1],
#'     Count_Regeneration_Layer = layer_counts[2],
#'     Count_Understory_Layer = layer_counts[3],
#'     Count_Canopy_Layer = layer_counts[4],
#'     Ground_Count_Ground_Layer = ground_layer_counts[1],
#'     Ground_Count_Regeneration_Layer = ground_layer_counts[2],
#'     Ground_Count_Understory_Layer = ground_layer_counts[3],
#'     Ground_Count_Canopy_Layer = ground_layer_counts[4]
#'   ))
#' 
#' }
#' 
#' 
#' 
#' 
#' 
#' calculate_penetration_rates <- function(las_data, grd = 2, reg = 8, und = 20) {
#' 
#'   # Calculate total number of points
#'   total_points <- npoints(las_data)
#' 
#'   # Calculate the density for layers of interest
#'   layer_density_df <- calculate_layer_pointStats(las_data, layer_height = 1)
#' 
#'   # Extract heights from layer density data frame
#'   bin_bounds <- stringr::str_match(as.character(layer_density_df$Layer), "\\[(\\d+(\\.\\d+)?), ?(\\d+(\\.\\d+)?)\\]")
#'   lower_bounds <- as.numeric(bin_bounds[, 2])
#'   upper_bounds <- as.numeric(bin_bounds[, 4])
#' 
#'   # Calculate penetration rates for 1-meter layers (BE_PR_01 to BE_PR_55)
#'   penetration_rates_1m_layers <- sapply(1:55, function(i) {
#'     indices <- which(lower_bounds >= i - 1 & upper_bounds <= i)
#'     layer_points <- sum(layer_density_df$Density[indices], na.rm = TRUE) * 1 # height = 1 meter
#'     ground_points <- sum(filter_poi(las_data, Classification == 2)$Z >= (i - 1) & filter_poi(las_data, Classification == 2)$Z < i, na.rm = TRUE)
#'     return(ground_points / layer_points)
#'   })
#' 
#'   # Penetration rate for canopy layer (BE_PR_CAN)
#'   canopy_layer_indices <- which(lower_bounds >= und) # Assuming canopy starts at `und`
#'   canopy_points <- sum(layer_density_df$Density[canopy_layer_indices], na.rm = TRUE)
#'   penetration_rate_canopy <- sum(filter_poi(las_data, Classification == 2)$Z >= und, na.rm = TRUE) / canopy_points
#' 
#'   # Penetration rates for height layers in 10% steps (BE_PR_H01 to BE_PR_H10)
#'   max_height <- max(las_data$Z, na.rm = TRUE)
#'   penetration_rates_height_layers <- sapply(1:10, function(i) {
#'     start_height <- (i - 1) * 0.1 * max_height
#'     end_height <- i * 0.1 * max_height
#'     indices <- which(lower_bounds >= start_height & upper_bounds < end_height)
#'     layer_points <- sum(layer_density_df$Density[indices], na.rm = TRUE) * 0.1 * max_height
#'     ground_points <- sum(filter_poi(las_data, Classification == 2)$Z >= start_height & filter_poi(las_data, Classification == 2)$Z < end_height, na.rm = TRUE)
#'     return(ground_points / layer_points)
#'   })
#' 
#'   # Penetration rate for regeneration layer (BE_PR_REG)
#'   regeneration_layer_indices <- which(lower_bounds >= grd & upper_bounds <= reg)
#'   regeneration_points <- sum(layer_density_df$Density[regeneration_layer_indices], na.rm = TRUE)
#'   total_regeneration_and_below_indices <- which(lower_bounds >= grd & upper_bounds <= und)
#'   regeneration_and_below_points <- sum(layer_density_df$Density[total_regeneration_and_below_indices], na.rm = TRUE)
#'   penetration_rate_regeneration <- regeneration_points / regeneration_and_below_points
#' 
#'   # Penetration rate for understory layer (BE_PR_UND)
#'   understory_layer_indices <- which(lower_bounds >= reg & upper_bounds <= und)
#'   understory_points <- sum(layer_density_df$Density[understory_layer_indices], na.rm = TRUE)
#'   total_understory_and_below_indices <- which(lower_bounds >= reg)
#'   understory_and_below_points <- sum(layer_density_df$Density[total_understory_and_below_indices], na.rm = TRUE)
#'   penetration_rate_understory <- understory_points / understory_and_below_points
#' 
#'   # Combine results into a data frame
#'   return(data.frame(
#'     BE_PR_01 = penetration_rates_1m_layers[1],
#'     BE_PR_02 = penetration_rates_1m_layers[2],
#'     # Add more BE_PR_x as needed
#'     BE_PR_CAN = penetration_rate_canopy,
#'     BE_PR_H01 = penetration_rates_height_layers[1],
#'     BE_PR_H02 = penetration_rates_height_layers[2],
#'     # Add more BE_PR_Hx as needed
#'     BE_PR_REG = penetration_rate_regeneration,
#'     BE_PR_UND = penetration_rate_understory
#'   ))
#' }
#' 





# ### Function to calculate penetration rates
# calculate_penetration_rates <- function(las_data) {
#   
#   # Total number of points in the LAS dataset
#   total_points <- npoints(las_data)
#   
#   # Filter ground points (classification == 2)
#   ground_points <- filter_poi(las_data, Classification == 2)
#   total_ground_points <- npoints(ground_points)
#   
#   # Penetration rate for all points
#   penetration_rate_all <- total_ground_points / total_points
#   
#   # Filter first returns (ReturnNumber == 1)
#   first_returns <- filter_poi(las_data, ReturnNumber == 1)
#   total_first_return_points <- npoints(first_returns)
#   ground_first_returns <- filter_poi(first_returns, Classification == 2)
#   total_ground_first_returns <- npoints(ground_first_returns)
#   
#   # Penetration rate for first returns
#   penetration_rate_first <- total_ground_first_returns / total_first_return_points
#   
#   # Filter last returns (ReturnNumber == NumberOfReturns)
#   last_returns <- filter_poi(las_data, ReturnNumber == NumberOfReturns)
#   total_last_return_points <- npoints(last_returns)
#   ground_last_returns <- filter_poi(last_returns, Classification == 2)
#   total_ground_last_returns <- npoints(ground_last_returns)
#   
#   # Penetration rate for last returns
#   penetration_rate_last <- total_ground_last_returns / total_last_return_points
#   
#   # # Return the penetration rates as a named list
#   # return(list(
#   #   Penetration_Rate_All = penetration_rate_all,
#   #   Penetration_Rate_First = penetration_rate_first,
#   #   Penetration_Rate_Last = penetration_rate_last
#   # ))
#   
#   # Create a data frame to store the results
#   penetration_df <- data.frame(
#     Penetration_Rate_All = penetration_rate_all,
#     Penetration_Rate_First = penetration_rate_first,
#     Penetration_Rate_Last = penetration_rate_last
#   )
#   
#   return(penetration_df)
#   
# }
# 
# 
# calculate_vegetation_penetration_rates <- function(las_data) {
#   
#   # Calculate vegetation layer densities and ground counts
#   vegetation_layer_density <- calculate_vegetation_layer_density(las_data)
#   
#   # Extract total number of points
#   total_points <- npoints(las_data)
#   
#   # Define a helper function to calculate penetration rates for a specific layer
#   calculate_penetration_rate <- function(layer_ground_count, layer_density) {
#     if (layer_density > 0) {
#       return(layer_ground_count / (layer_density * total_points))
#     } else {
#       return(NA)
#     }
#   }
#   
#   # Calculate penetration rates for each layer
#   penetration_rates <- data.frame(
#     Ground_Penetration_Rate = calculate_penetration_rate(vegetation_layer_density$Ground_Count_Ground_Layer, vegetation_layer_density$Ground_Density),
#     Regeneration_Penetration_Rate = calculate_penetration_rate(vegetation_layer_density$Ground_Count_Regeneration_Layer, vegetation_layer_density$Regeneration_Density),
#     Understory_Penetration_Rate = calculate_penetration_rate(vegetation_layer_density$Ground_Count_Understory_Layer, vegetation_layer_density$Understory_Density),
#     Canopy_Penetration_Rate = calculate_penetration_rate(vegetation_layer_density$Ground_Count_Canopy_Layer, vegetation_layer_density$Canopy_Density)
#   )
#   
#   return(penetration_rates)
# }




# # Function to calculate penetration rates for vegetation layers
# calculate_vegetation_penetration_rates <- function(las_data, grd = 2, reg = 8, und = 20) {
#   
#   # Calculate vegetation layer densities
#   vegetation_layer_density <- calculate_vegetation_layer_density(las_data, grd = grd, reg = reg, und = und)
#   
#   # Total number of points in the LAS dataset
#   total_points <- npoints(las_data)
#   
#   # Filter ground points (classification == 2)
#   ground_points <- filter_poi(las_data, Classification == 2)
#   total_ground_points <- npoints(ground_points)
#   
#   # Penetration rate for all points
#   penetration_rate_all <- total_ground_points / total_points
#   
#   # Penetration rate for specific vegetation layers
#   penetration_rate_ground <- vegetation_layer_density$Ground_Density
#   penetration_rate_regeneration <- vegetation_layer_density$Regeneration_Density
#   penetration_rate_understory <- vegetation_layer_density$Understory_Density
#   penetration_rate_canopy <- vegetation_layer_density$Canopy_Density
#   
#   # Create a data frame to store the results
#   penetration_df <- data.frame(
#     Penetration_Rate_All = penetration_rate_all,
#     Penetration_Rate_Ground = penetration_rate_ground,
#     Penetration_Rate_Regeneration = penetration_rate_regeneration,
#     Penetration_Rate_Understory = penetration_rate_understory,
#     Penetration_Rate_Canopy = penetration_rate_canopy
#   )
#   
#   return(penetration_df)
# }



















# # Function to calculate penetration rate
# calculate_penetration_rate <- function(las_data, layer_start_height, layer_end_height) {
#   point_heights <- las_data$Z
#   # layer_start_height <- 0
#   # layer_end_height <- floor(max(nlas$Z))
#   points_within_layer <- sum(point_heights >= layer_start_height & point_heights < layer_end_height)
#   total_points <- length(point_heights)
#   penetration_rate <- (points_within_layer / total_points) * 100
#   return(penetration_rate)
# }
# 
# # Define the function to calculate penetration rate for each 1-meter layer
# calculate_penetration_rates <- function(las_data) {
#   # Extract point heights from the LAS data
#   point_heights <- las_data$Z
#   # Create an empty data frame to store results
#   max_height <- floor(max(point_heights))
#   results <- data.frame(
#     LayerStart = numeric(),
#     PenetrationRate = numeric()
#   )
#   
#   # Iterate over each 1-meter layer
#   for (x in 1:max_height) {
#     layer_start_height <- x
#     layer_end_height <- x + 1
#     
#     # Calculate the penetration rate for the current layer
#     points_within_layer <- sum(point_heights >= layer_start_height & point_heights < layer_end_height)
#     total_points <- length(point_heights)
#     penetration_rate <- (points_within_layer / total_points) * 100
#     
#     # Append results to the data frame
#     results <- rbind(results, data.frame(
#       LayerStart = layer_start_height,
#       PenetrationRate = penetration_rate
#     ))
#   }
#   
#   return(results)
# }
# 
# 
# 
# # # Function to calculate penetration rate for each layer from 1 to 10 meters
# # calculate_pr <- function(las_data) {
# #   
# #   # # Classify ground points if not already classified
# #   # if (!"Classification" %in% colnames(las_data@data)) {
# #   #   las_data <- classify_ground(las_data, algorithm = csf())
# #   # }
# #   
# #    # Add height above ground as a new attribute
# #   ground_points <- filter_poi(las_data, Classification == 2)
# #   ground_height <- las_data$Z[las_data$Classification == 2]
# #   las_data <- add_attribute(las_data, (las_data$Z - ground_height[1]), "HeightAboveGround")
# #   
# #   # Total number of points
# #   total_points <- npoints(las_data)
# #   
# #   # Initialize list to store penetration rates for each layer
# #   penetration_rates <- list()
# #   
# #   # Loop through layers 1 to 10 meters
# #   for (layer in 1:10) {
# #     # Filter points within the current layer
# #     points_in_layer <- filter_poi(ground_points, HeightAboveGround >= (layer - 1) & HeightAboveGround < layer)
# #     n_points_in_layer <- npoints(points_in_layer)
# #     
# #     # Calculate penetration rate for the current layer
# #     penetration_rate <- n_points_in_layer / total_points
# #     
# #     # Store the result in the list
# #     penetration_rates[[paste0("BE_PR_", sprintf("%02d", layer))]] <- penetration_rate
# #   }
# #   
# #   return(penetration_rates)
# # }
