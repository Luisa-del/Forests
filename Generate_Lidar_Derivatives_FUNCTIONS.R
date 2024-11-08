#'####################################################################################################################
# FUNCTIONS to generate base products for German Forests project
# 18.07.2024
# Luisa Pflumm
#'####################################################################################################################

#'####################################################################################################################
# # SET CUSTOM PARAMETER ####

# -> All applied functions stored in : SourcePath1 <- "C:/Users/lup42bs/Documents/Projects/Forests/Scripts/R/Generate_Base_Products_FUNCTIONS.R"



#'####################################################################################################################
# # IMPORT FILES ####

# ### Function to check if DJIM300L1 base products are exsisting and import them
# import_base_products_l1_buff <- function(RootPath, mission, FlightName) {
#   if (endsWith(FlightName, "DJIM300L1")) {
#     # Construct the full paths
#     raster_path <- paste0(RootPath, mission, "/2_Results/0_Raster/bufferedRaster/")
#     las_path <- paste0(RootPath, mission, "/2_Results/1_PointClouds/bufferedPC/")
#    
#     # List all files in the directories without full path names
#     tif_file <- list.files(raster_path, pattern = "\\.tif$", full.names = TRUE)
#     las_file <- list.files(las_path, pattern = "\\.las$", full.names = TRUE)
#     
#     # Use grepl to filter files containing the flightname
#     tif_file <- tif_file[grepl(FlightName, tif_file)]
#     las_file <- las_file[grepl(FlightName, las_file)]
#     
#     # Check if the .tif and .las files exists
#     if (!file.exists(tif_file)) {
#       stop(paste("No DJIM300L1 DEM was found in", raster_path, "folder"))
#     }
#     
#     if (!file.exists(las_file)) {
#       stop(paste("No DJIM300L1 Point Cloud was found in", las_path, "folder"))
#     }
#     
#     # Load the .tif file
#     raster_data <- rast(tif_file)
#     
#     # Load the .las file
#     lidar_data <- readLAS(las_file)
#     
#     # Return the l1 base products in list
#     return(list(dem = raster_data, pointcloud = lidar_data))
#     } else {
#       print("Base products could not be found in folder. Check if they already were generated.")
#   }
# }
### Function to check if DJIM300L1 base products are exsisting and import them
import_base_products_l1_buff <- function(RootPath, mission, FlightName) {
  if (endsWith(FlightName, "DJIM300L1")) {
    # Construct the full paths
    raster_path <- paste0(RootPath, mission, "/2_Results/0_Raster/bufferedRaster/")
    las_path <- paste0(RootPath, mission, "/2_Results/1_PointClouds/bufferedPC/")
    
    # List all files in the directories with full path names
    tif_file <- list.files(raster_path, pattern = "\\.tif$", full.names = TRUE)
    las_file <- list.files(las_path, pattern = "\\.las$", full.names = TRUE)
    
    # Use grepl to filter files containing the flightname
    tif_file <- tif_file[grepl(FlightName, tif_file)]
    las_file <- las_file[grepl(FlightName, las_file)]
    
    # Initialize variables for DEM and LAS
    raster_data <- NULL
    lidar_data <- NULL
    
    # Check if the .tif file exists
    if (length(tif_file) == 0 || !file.exists(tif_file)) {
      warning(paste("No DJIM300L1 DEM was found in", raster_path, "folder. Returning only the LAS file."))
    } else {
      # Load the .tif file
      raster_data <- rast(tif_file)
    }
    
    # Check if the .las file exists
    if (length(las_file) == 0 || !file.exists(las_file)) {
      stop(paste("No DJIM300L1 Point Cloud was found in", las_path, "folder"))
    } else {
      # Load the .las file
      lidar_data <- readLAS(las_file)
    }
    
    # Return both DEM and LAS, with DEM being NULL if not found
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



#'####################################################################################################################
# # PROCESS FILES ####

# ### Function to calculate different Canopy Height Models / Digital Surface Models
# calculate_topALSreturns_all <- function(nlas, model, resolution) {
#   # Convert model to uppercase
#   model <- toupper(model)
#   
#   # Check if y is not "CHM" or "DSM"
#   if (model != "CHM" && model != "DSM") {
#     stop("Please specify 'CHM' or 'DSM'.")
#   }
#   
#   # Point-to-raster methods
#   mod_ptr_1 <- rasterize_canopy(nlas, res = resolution, p2r())
#   mod_ptr_2 <- rasterize_canopy(nlas, res = resolution, algorithm = p2r(subcircle = 0.15))
#   mod_ptr_3 <- rasterize_canopy(nlas, res = resolution, algorithm = p2r(0.2, na.fill = tin()))
#   
#   # Post-processing a point-to-raster CHM or DSM model
#   fill.na <- function(x, i = 5) { if (is.na(x)[i]) { return(mean(x, na.rm = TRUE)) } else { return(x[i]) } }
#   w <- matrix(1, 3, 3)
#   
#   filled1 <- terra::focal(mod_ptr_1, w, fun = fill.na)
#   filled2 <- terra::focal(mod_ptr_2, w, fun = fill.na)
#   filled3 <- terra::focal(mod_ptr_3, w, fun = fill.na)
#   
#   smoothed1 <- terra::focal(mod_ptr_1, w, fun = mean, na.rm = TRUE)
#   smoothed2 <- terra::focal(mod_ptr_2, w, fun = mean, na.rm = TRUE)
#   smoothed3 <- terra::focal(mod_ptr_3, w, fun = mean, na.rm = TRUE)
#   
#   mods_ptr_1 <- c(mod_ptr_1, filled1, smoothed1)
#   mods_ptr_2 <- c(mod_ptr_2, filled2, smoothed2)
#   mods_ptr_3 <- c(mod_ptr_3, filled3, smoothed3)
#   
#   names(mods_ptr_1) <- c("Z_Base", "Z_Filled", "Z_Smoothed")
#   names(mods_ptr_2) <- c("Z_Base", "Z_Filled", "Z_Smoothed")
#   names(mods_ptr_3) <- c("Z_Base", "Z_Filled", "Z_Smoothed")
#   
#   # Triangulation methods
#   mod_tri_1 <- rasterize_canopy(nlas, res = resolution, algorithm = dsmtin())
#   mod_tri_2 <- rasterize_canopy(nlas, res = resolution, algorithm = dsmtin(max_edge = 8))
#   
#   # Store all CHM or DSM objects in a list
#   mod_list <- list(
#     mod_ptr_1 = mods_ptr_1,
#     mod_ptr_2 = mods_ptr_2,
#     mod_ptr_3 = mods_ptr_3,
#     mod_tri_1 = mod_tri_1,
#     mod_tri_2 = mod_tri_2
#   )
#   
#   # Rename the list elements
#   mod <- tolower(model)
#   names(mod_list) <- c(
#     paste0(mod, "_ptr_1"),
#     paste0(mod, "_ptr_2"),
#     paste0(mod, "_ptr_3"),
#     paste0(mod, "_tri_1"),
#     paste0(mod, "_tri_2")
#   )
#   
#   # Plot all CHM or DSM objects
#   plot(mod_ptr_1, col = height.colors(25), main = paste0(model, " (ptr) v1"))
#   plot(mod_ptr_2, col = height.colors(25), main = paste0(model, " (ptr) v2"))
#   plot(mod_ptr_3, col = height.colors(25), main = paste0(model, " (ptr) v3"))
#   
#   plot(mods_ptr_1, col = height.colors(25))
#   title(main = paste0(model, " (ptr) v1"),, outer=TRUE, line=-2)
#   plot(mods_ptr_2, col = height.colors(25))
#   title(main = paste0(model, " (ptr) v2"), outer=TRUE, line=-2)
#   plot(mods_ptr_3, col = height.colors(25))
#   title(main = paste0(model, " (ptr) v3"), outer=TRUE, line=-2)
#   
#   plot(mod_tri_1, col = height.colors(25), main = paste0(model, " (tri) v1"))
#   plot(mod_tri_2, col = height.colors(25), main = paste0(model, " (tri) v2"))
#   
#   return(mod_list)
# }

### Function to calculate different Canopy Height Models / Digital Surface Models
calculate_topALSreturns <- function(nlas, model, resolution) {
  # Convert model to uppercase
  model <- toupper(model)

  # Check if y is not "CHM" or "DSM"
  if (model != "CHM" && model != "DSM") {
    stop("Please specify 'CHM' or 'DSM'.")
  }

  # Triangulation methods
  mod_tri_2 <- rasterize_canopy(nlas, res = resolution, algorithm = dsmtin(max_edge = 8))

  # Rename the list elements
  mod <- tolower(model)
  print(paste0("Calculating ", mod, " (triangulation-based method)"))

  # Plot all CHM or DSM objects
  plot(mod_tri_2, col = height.colors(25), main = model)

  return(mod_tri_2)
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
      stat_value <- terra::global(raster, stat_functions[[stat_name]], na.rm = TRUE)
      
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



### Function to calculate mean top-of-canopy height (TCH)    -> tch == chm_height_mean
calculate_tch <- function(chm_raster) {
  # Calculate the mean of CHM raster pixels
  #tch_mean <- cellStats(chm_raster, mean, na.rm = TRUE)  # raster::raster()
  tch_mean <- terra::global(chm_raster, "mean", na.rm = TRUE)        # terra::rast()
  return(tch_mean)
}

### Function to calculate AGB carbon based on TCH
calculate_agb_carbon <- function(tch) {
  # Use the formula from Stephan Getzin et al.
  agb_carbon <- 6.85 * tch^0.952
  return(agb_carbon)
}

### Function to calculate AGB based on AGB carbon
calculate_agb <- function(agb_carbon) {
  # Convert AGB carbon to AGB
  agb <- agb_carbon / 0.48
  return(agb)
}

### Function to calculate global statistics (summarize values) for list of raster
calculate_agb_stats <- function(raster) {
  
  # Extract mean top-of-canopy height (TCH)
  tch <- calculate_tch(raster)
  
  # Calculate AGB carbon (typically represents mass of carbon in the aboveground biomass, often measured in kilograms of carbon per square meter (kg C/m²) or megagrams of carbon per hectare (Mg C/ha))
  agb_carb <- calculate_agb_carbon(tch)
  
  # Calculate AGB (typically represents the total mass of the aboveground portion of the vegetation, often measured in kilograms per square meter (kg/m²) or megagrams per hectare (Mg/ha))
  agb <- calculate_agb(agb_carb)
  
  # Create df
  agb_df <- data.frame(tch = tch,
                       agb_carb = agb_carb,
                       agb = agb)
  
  colnames(agb_df) <- c("tch", "agb_carb", "agb")
  
  return(agb_df)
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
make_grids <- function(sf, res) {
  # Create a grid of 5x5m covering the extent of the polygon
  grid <- st_make_grid(sf, cellsize = c(res, res), square = TRUE)
  # Convert to sf object
  grid <- st_sf(geometry = grid)
  # Assign the Patch.Nr to each grid cell; and ensure that only grid cells within/touching the patches are kept (`left = FALSE`)
  grid <- st_join(grid, sf["Patch.Nr"], left = FALSE) %>% mutate(cell.id = row_number())
  # # Clip the grid with your polygon
  # clipped_grid <- st_intersection(grid, sf) %>% dplyr::select(Patch.Nr, cell.id) #%>% mutate(cell.id = row_number())
  # Keep only full grid cells within the polygon
  clipped_grid_fullCell <- st_filter(grid, sf, .predicate = st_within)
  # Reset cell.id for each Patch.Nr
  clipped_grid_fullCell <- clipped_grid_fullCell %>%
    group_by(Patch.Nr) %>%
    mutate(cell.id = row_number()) %>%
    ungroup()
  
  # List grid-objects
  grid_list <- list(patches_grid = grid, #clipped_grid, 
                    patches_grid_fullCell = clipped_grid_fullCell)
  
  return(grid_list)
}

  
  
### Function to create the ggplot to visualise the original patches and the resulting grids
plot_grids <- function(grid_list, aoi_utm, patches_utm) {
  plot <- ggplot() +
  geom_sf(data = aoi_utm, fill = NA, color = "black") +
  geom_sf(data = patches_utm, fill = NA, color = "red") +
  geom_sf(data = grid_list$patches_grid, fill = "blue", alpha = 0.5) +
  #geom_sf(data = grid_list$patches_grid_fullCell, fill = "red", alpha = 0.5) +
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


plot_transect <- function(las_data, aoi, rast){
  
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
  
  # Define line width
  wdt = 10

  # Clip the transect using the decimated point cloud
  las_tr <- clip_transect(las_data, top_left, bottom_right, width = wdt, xz = TRUE)
  
  # Create a buffer around the transect to simulate the 10m-wide transect area (create a polygon)
  transect_line <- st_sfc(st_linestring(matrix(c(top_left["x"], top_left["y"], bottom_right["x"], bottom_right["y"]), 
                                               ncol = 2, byrow = TRUE)), crs = st_crs(aoi))
  
  # Buffer the line to create the width of the transect area
  transect_buffer <- st_buffer(transect_line, dist = wdt, endCapStyle = "FLAT")
  
  # Convert the CHM raster to a data frame for ggplot
  raster_df <- terra::as.data.frame(rast, xy = TRUE, na.rm = TRUE)
  colnames(raster_df) <- c("X", "Y", "Height")  # Set column names for easy ggplot mapping
  
  # Plot 1: 2D map of the AOI with transect line
  map_plot <- ggplot() +
    geom_tile(data = raster_df, aes(x = X, y = Y, fill = Height)) +  # raster fill
    scale_fill_gradientn(colours = height.colors(50), name = "CHM (m)") +  # Color scale for CHM
    geom_sf(data = aoi, fill = NA, color = "black") +  # AOI boundary
    geom_line(data = transect_points, aes(x = X, y = Y), color = "purple", size = 1) +  # Transect line
    geom_sf(data = transect_buffer, fill = "purple", alpha = 0.4) +  # Transect area as a polygon
    coord_sf() +  # Set appropriate coordinate system
    theme_minimal() +
    ggtitle(paste0("CHM with ", wdt, " m transect line"))
  
  # Plot 2: Transect plot
  transect_plot <- ggplot(payload(las_tr), aes(X, Z, color = Z)) + 
    geom_point(size = 0.5) + 
    coord_equal() + 
    theme_minimal() +
    scale_color_gradientn(colours = height.colors(50)) +
    ggtitle(paste0("Vertical height profile of all points in ", wdt, " m transect line"))
  
  # Arrange both plots together
  library(gridExtra)
  grid.arrange(map_plot, transect_plot, ncol = 1)
  # map_plot
  # transect_plot
  
  # Generate 3D plot
  plot(las_tr, axis = TRUE, legend = TRUE, bg = "white", backend = "rgl")
  axes3d(edges = "bbox")  # Add axes around the bounding box
  title3d(xlab = "X (m)", ylab = "Y (m)", zlab = "Z (m)")
  
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



calculate_be_h_stats <- function(las_data) {
  # Extract heights (define the height column as Z)
  heights <- las_data$Z
  
  # # Calculate percentiles (0%, 10%, 20%, ..., 100%)
  # percentiles <- quantile(heights, probs = seq(0, 1, 0.1), na.rm = TRUE)
  # names(percentiles) <- c("BE_H_P0", paste0("BE_H_P", seq(10, 100, 10)))
  
  # Calculate percentiles (0%, 5%, 10%, ..., 100%)                                                                     # LP: 5% schritte
  percentiles <- quantile(heights, probs = seq(0, 1, 0.05), na.rm = TRUE)
  names(percentiles) <- c("BE_H_P0", paste0("BE_H_P", seq(5, 100, 5)))
  
  # Initialize an empty list to store the data frames for each percentile group
  percentile_stats_list <- list()
  
  # Loop over each percentile and calculate statistics for the heights <= percentile value
  for (name in names(percentiles)) {
    # Filter heights that are less than or equal to the current percentile value
    filtered_heights <- heights[heights <= percentiles[[name]]]
    
    # Calculate the required metrics
    stats <- data.frame(
      BE_H_MAX = max(filtered_heights, na.rm = TRUE),
      BE_H_MIN = min(filtered_heights, na.rm = TRUE),
      BE_H_MEAN = mean(filtered_heights, na.rm = TRUE),
      BE_H_MEDIAN = median(filtered_heights, na.rm = TRUE),
      BE_H_SD = sd(filtered_heights, na.rm = TRUE),
      BE_H_VAR = var(filtered_heights, na.rm = TRUE),
      BE_H_VAR_COEF = sd(filtered_heights, na.rm = TRUE) / mean(filtered_heights, na.rm = TRUE),
      BE_H_SKEW = e1071::skewness(filtered_heights, na.rm = TRUE),
      BE_H_KURTOSIS = e1071::kurtosis(filtered_heights, na.rm = TRUE),

      # # Calculate entropy (zentropy) for the percentile group
      # library(entropy)  # For entropy calculation (install if needed)
      # ZENTROPY = entropy::entropy(table(cut(filtered_heights, breaks = 10)), unit = "log2"),                          # LP: add ZENTROPY

      # Calculate pzabovezmean (percentage of points above the mean height for the percentile group)
      PZABOVEZMEAN = sum(filtered_heights > mean(filtered_heights, na.rm = TRUE)) / length(filtered_heights) * 100,   # LP: add PZABOVEZMEAN

      # Calculate pzabove2 (percentage of points above 2 meters for the percentile group)                             # LP: add PZABOVE2
      PZABOVE2 = sum(filtered_heights > 2) / length(filtered_heights) * 100
    )
    
    # Add the percentile value to the statistics
    stats$Percentile_Height <- percentiles[[name]]
    
    # Store the data frame in the list with the name of the percentile
    percentile_stats_list[[name]] <- stats
  }
  
  # Combine all percentile data frames into a single data frame
  percentile_stats_list <- do.call(rbind, percentile_stats_list)
  
  return(percentile_stats_list)
}

# calculate_be_h_stats <- function(las_data) {
#   # Extract heights (define the height column as Z)
#   heights <- las_data$Z
#   
#   # Calculate percentiles (0%, 10%, 20%, ..., 100%)
#   percentiles <- quantile(heights, probs = seq(0, 1, 0.1), na.rm = TRUE)
#   names(percentiles) <- c("BE_H_P0", paste0("BE_H_P", seq(10, 100, 10)))
#   
#   # Initialize an empty list to store the data frames for each percentile group
#   percentile_stats_list <- list()
#   
#   # Loop over each percentile and calculate statistics for the heights <= percentile value
#   for (name in names(percentiles)) {
#     # Filter heights that are less than or equal to the current percentile value
#     filtered_heights <- heights[heights <= percentiles[[name]]]
#     
#     # Calculate statistics for the filtered heights
#     stats <- data.frame(
#       BE_H_MAX = max(filtered_heights, na.rm = TRUE),
#       BE_H_MIN = min(filtered_heights, na.rm = TRUE),
#       BE_H_MEAN = mean(filtered_heights, na.rm = TRUE),
#       BE_H_MEDIAN = median(filtered_heights, na.rm = TRUE),
#       BE_H_SD = sd(filtered_heights, na.rm = TRUE),
#       BE_H_VAR = var(filtered_heights, na.rm = TRUE),
#       BE_H_VAR_COEF = sd(filtered_heights, na.rm = TRUE) / mean(filtered_heights, na.rm = TRUE),
#       BE_H_SKEW = e1071::skewness(filtered_heights, na.rm = TRUE),
#       BE_H_KURTOSIS = e1071::kurtosis(filtered_heights, na.rm = TRUE)
#     )
#     
#     # Add the percentile value to the statistics
#     stats$Percentile_Height <- percentiles[[name]]
#     
#     # Store the data frame in the list with the name of the percentile
#     percentile_stats_list[[name]] <- stats
#   }
#   
#   percentile_stats_list <- do.call(rbind, percentile_stats_list)
#   
#   return(percentile_stats_list)
# }



### Function to calculate point cloud statistics per layer (point density, point count, penetration rate) -> returns 
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
  
  layer_density_df <- layer_density_df %>%
    mutate(
      Cumulative_Point_Density = cumsum(Point_Density),  # Adds a cumulative density column
      Cumulative_Point_Count = cumsum(Point_Count)        # Adds a cumulative count column
    )

  return(layer_density_df)
}

### Function to calculate point cloud statistics (see above) per vegetation layer (defined by ground-, regeneration-, understory- and canopy-layer)
calculate_veg_layer_pointStats <- function(las_data, grd = 1.5, reg = 5, und = 7) {      #grd = 2, reg = 8, und = 20
  # Specify fine-scale meter bins
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

  ### Sum the point densities for each layer
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

  ### Penetration Rates
  BE_PR_GRD <- ground_point_count / ground_point_count  # will be 1
  BE_PR_REG <- regeneration_point_count / regeneration_and_below_point_count
  BE_PR_UND <- understory_point_count / understory_and_below_point_count
  BE_PR_CAN <- canopy_point_count / total_point_count
  
  # Create a data frame with 4 rows and 3 columns
  veg_layer_df <- data.frame(
    Layer = c("Ground", "Regeneration", "Understory", "Canopy"),
    Point_Count = c(ground_point_count, regeneration_point_count, understory_point_count, canopy_point_count),
    Point_Density = c(BE_RD_GND, BE_RD_REG, BE_RD_UND, BE_RD_CAN),
    Penetration_Rate = c(BE_PR_GRD, BE_PR_REG, BE_PR_UND, BE_PR_CAN)
  )

  # Return the densities, point counts, and penetration rates as a list
  return(list(single_layer_stats = layer_density_df,
              veg_layer_stats = veg_layer_df))
}
   


### Function to create the density - height plot
create_density_by_height_plot <- function(data, title) {
  ggplot(data, aes(x = Point_Density, y = Height_Layer_m)) +
    geom_bar(stat = "identity", fill = "lightblue", color = "lightblue", alpha = 0.7) +
    geom_point(color = "darkgreen", size = 3, alpha = 0.7) +
    geom_line(color = "darkgreen", group = 1, size = 1) +
    theme_minimal() +
    labs(title = title,     # "Point Density by Height Layer"
         x = "Point Density", 
         y = "Height Layer (m)") +
    xlim(0, 0.25) +  # Set x-axis limits to 0 and 0.25
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 5),
          panel.grid.major = element_line(color = "gray80"),
          panel.grid.minor = element_blank())
} 




