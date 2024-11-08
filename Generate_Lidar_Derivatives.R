#'####################################################################################################################
# Generate lidar derivatives for German Forests project
# 18.07.2024
# Luisa Pflumm
# Example area: BayWaldAOI8

# Load libraries
pacman::p_load(exifr, dplyr, leaflet,RColorBrewer, sf, parallel, viridis, zip, terra, lidR, ggplot2, purrr, rgl, patchwork)
options(scipen = 100, digits = 4) # avoid numbers with e

#'####################################################################################################################
# # SET CUSTOM PARAMETER ####
# USER: Set path to the corresponding function scripts
SourcePath1 <- "C:/Users/lup42bs/Documents/Projects/Forests/Scripts/R/Generate_Base_Products_FUNCTIONS.R"
source(SourcePath1)
SourcePath2 <- "C:/Users/lup42bs/Documents/Projects/Forests/Scripts/R/Generate_Lidar_Derivatives_FUNCTIONS.R"
source(SourcePath2)
# USER: Set path to root folder (where missions are stored)
RootPath <- "F:/ForestSinglePlots/"
# List all existing flight folder
list_all_flight_paths(RootPath)
# USER: Select mission (from table)
mission <- "20230823_BayWaldAOI8"
# List only flight folders of selected mission
(list_mission_flightpaths <- list_mission_flight_paths(RootPath, mission))
# USER: Select path to flight folder
FlightPath <- "F:/ForestSinglePlots/20230823_BayWaldAOI8/0_Flights/20230823_BayWaldAOI8_DJIM300L1/"

#'####################################################################################################################
# # PREPARE TXT OUTPUT ####
# Select flight name
FlightName <- select_flightname(FlightPath)
# Define export path
ExportPath <- paste0(RootPath, mission, "/2_Results/")
# Define current data
current_date <- get_current_date()
#'------------------------------------------------------------------------------------------------------------------#
#' # Create 2_Reports folder for txt output                                                                            #
#' ReportPath <- paste0(ExportPath, "2_Reports/")                                                                      #
#' if (!dir.exists(ReportPath)) {dir.create(ReportPath, recursive = TRUE)}                                             #
#' # Define output .txt file to save console log                                                                       #
#' outputTXTfile <- paste0(ReportPath, FlightName, "_console_output_processing_", current_date, ".txt")                #
#' # Check if file exists and decide: overwrite or append                                                              # 
#' user_decision_overwrite_file <- userPromt_outputTXTfile(outputTXTfile)                                              #
#' # Open a connection to the text file                                                                                #
#' sink(outputTXTfile, append = !user_decision_overwrite_file)                                                         #
#' #'------------------------------------------------------------------------------------------------------------------#
#' # Print executing codes                                 #
#' readLines(sub("_FUNCTIONS", "", SourcePath))            #
#' readLines(SourcePath)                                   #
#' cat("Console output: \n")                               #
#' # Print current date & time at start of processing      #
#' print(paste0("Current date: ", get_current_date()))     #
#' print(paste0("Current time: ", get_current_time()))     #
#' #'------------------------------------------------------#
#' # sink() 
#' # # Optionally, read the file to verify the content
#' # cat(readLines(outputTXTfile), sep = "\n")
#' # Print all custom parameter
#' cat("SourcePath:", SourcePath, "\n")
#' cat("RootPath:", RootPath, "\n")
#' cat("mission:", mission, "\n")
#' cat("FlightPath:", FlightPath, "\n")

#'####################################################################################################################
# # IMPORT FILES ####
# Load Vector Files
AOI <- st_read(paste0(RootPath,"0_BaseInfo/gpkg/beta4_aois.gpkg")) %>% st_zm()
Patches <- st_read(paste0(RootPath,"0_BaseInfo/gpkg/beta4_plots.gpkg"))
# Select required vector for AOI     -> Carefull! More aois per mission for Lübeck, Saarland and Passau!!
(aoi_name <- strsplit(mission, "_")[[1]][2])
aoi_wgs <- AOI[AOI$folder_name == aoi_name, ]
patches_wgs <- st_intersection(Patches, aoi_wgs)
# Load L1 base products (-> from buffered folder!)
(base_products_buff <- import_base_products_l1_buff(RootPath, mission, FlightName))
# Visualize base product raster
plot_list_output(base_products_buff, 2)  # -> PC vis might take a while



#'####################################################################################################################
# # PREPARE PROCESSING ####

# Validate point cloud
las <- base_products_buff$pointcloud
las_check(las)
las <- filter_duplicates(las)

# Apply point cloud normalization
nlas <- normalize_height(las, knnidw())
hist(filter_ground(nlas)$Z, main = "", xlab = "Elevation")
#nlas2 <- normalize_height(base_products_buff$pointcloud, knnidw(), dtm = base_products_buff$dem)  # -> hybrid method, can include negative numbers
#hist(filter_ground(nlas2)$Z, main = "", xlab = "Elevation")
# Optional: Filter the data to exclude points with heights below 0
if (any(nlas$Z < 0)) {nlas <- nlas[nlas$Z >= 0, ]
} else {nlas <- nlas}

# Get EPSG code from LAS file
(epsg <- sf::st_crs(lidR::crs(las))$epsg)

# Transform AOI & Patches: WGS to UTM
aoi_utm <- st_transform(aoi_wgs, epsg)
patches_utm <- st_transform(patches_wgs, epsg)

# Apply appropriate buffer to patches (-> to avoid edge effects in statistical analyses: consider pixel resolution, if not exceeding raster extent ideally use factor 10, or 5. Otherwise no buffer.)
patchesBuff <- apply_buffer(patches_utm, raster_deriv[[1]])

# Clip the normalized point cloud by each polygon and store them in a list
patches_pc <- clip_data_by_polygons(list(nlas = nlas), patches_utm)   # -> takes ~ 1.21 mins
patches_pc_buff <- clip_data_by_polygons(list(nlas = nlas), patchesBuff$aoiBuff_sf)    # -> takes ~ 1.3 mins

    
    
    
#'####################################################################################################################
# # PROCESS FILES ####
## RASTER derivatives  -----------------------------------------------------------------------------------------------
### Aoi - level ------------------------------------------------------------------------------------------------------

# Visualize aoi & patches
    plot_aois(aoi_utm, patches_utm)
# Visualize aoi & buffered patches
    plot_aois(aoi_utm, patchesBuff$aoiBuff_sf)


# Calculate variables from top-ALS returns: CHM (based on nlas) and DSM (based on las)
chm <- calculate_topALSreturns(nlas,"CHM", 0.5)
dsm <- calculate_topALSreturns(las,"dsm", 0.5)
    # chm_list <- calculate_topALSreturns_all(nlas, "CHM", 0.5)   # -> optional: compare different methods
    # dsm_list <- calculate_topALSreturns_all(las, "dsm", 0.5)    # -> optional: compare different methods

# Store suitable CHM & DSM in list
raster_deriv <- list(chm = chm, dsm = dsm)
# raster_deriv <- list(chm = chm_list$chm_tri_2, dsm = dsm_list$dsm_tri_2)   # -> optional: select chm/dsm version from list

# Check raster validity and get specific raster information
raster_deriv_info <- lapply(raster_deriv, check_raster_validity_info)

# ?->? Align CHM & DSM to DEM ????????
raster_deriv <- lapply(raster_deriv, function(raster) {resample(raster, base_products_buff$dem, method = "bilinear")})

# Calculate & list terrain variables: slope, aspect, terrain ruggedness index, topographic position index, roughness
raster_deriv[["dem"]] <- base_products_buff$dem
raster_deriv[["slo"]] <- calculate_slope(base_products_buff$dem)
raster_deriv[["asp"]] <- calculate_aspect(base_products_buff$dem)
raster_deriv[["tri"]] <- calculate_tri(base_products_buff$dem)
raster_deriv[["tpi"]] <- calculate_tpi(base_products_buff$dem)
raster_deriv[["rou"]] <- calculate_roughness(base_products_buff$dem)

# Visualize raster derivatives
    plot_list_output(raster_deriv, 3)

  
    
# Calculate global statistics for list of raster (mean, min, max, sd -> more?)  ### TABLE format
(raster_deriv_globalStats <- calculate_global_stats(raster_deriv))

# Make global stats raster from point cloud                                     ### RASTER format
f <- function(x) { list(mean = mean(x), sd = sd(x), min = min(x), max = max(x)) }
raster_deriv_globalStats_pixelmetrics <- pixel_metrics(nlas, ~f(Z), 0.5) # takes ~ 12 secs for resolution 5m, and ~ 27 secs for resolution 0.5m

# Visualize global stats raster
    plot(raster_deriv_globalStats_pixelmetrics, col = viridis(50)) #height.colors
  
    
    
# Calculate agb statistics based on chm (tch = mean top of canopy, agb carbon, agb)
agbStats <- calculate_agb_stats(raster_deriv$chm)
  





####   T e s t i n g   S t a r t   #################################################################################################################################################################


# # Transform to UTM and apply buffer
# aoi_utm <- st_transform(aoi_wgs, 32633)
# plot(aoi_utm$geom, add=T)
# aoi_buff <- st_buffer(aoi_utm, 5)
# plot(aoi_buff$geom, add=T)
# 
# # Calculate DEM from point cloud
# print(Sys.time())
# dtm_tin <- rasterize_terrain(las = las, res = 1, algorithm = tin(), shape = st_as_sfc(aoi_utm))  # -> takes 40 sec
# print(Sys.time())
# dtm_idw <- rasterize_terrain(las, algorithm = knnidw(k = 10L, p = 2), shape = st_as_sfc(aoi_utm))  # -> takes 8 sec
# print(Sys.time())
# dtm_kriging <- rasterize_terrain(las, algorithm = kriging(k = 40), shape = st_as_sfc(aoi_utm))  # -> very intensive computing, takes 1h 7min   (12h 10min without shape)
# print(Sys.time())
# 
# # Export DEMs to testing folder
# terra::writeRaster(dtm_tin, "F:/ForestSinglePlots/20230823_BayWaldAOI8/2_Results/0_Raster/bufferedRaster/DEM_testing/20230823_BayWaldAOI8_DJIM300L1_DEM_res1m_20240930_tin.tif")
# terra::writeRaster(dtm_idw, "F:/ForestSinglePlots/20230823_BayWaldAOI8/2_Results/0_Raster/bufferedRaster/DEM_testing/20230823_BayWaldAOI8_DJIM300L1_DEM_res1m_20240930_idw.tif")
# terra::writeRaster(dtm_kriging, "F:/ForestSinglePlots/20230823_BayWaldAOI8/2_Results/0_Raster/bufferedRaster/DEM_testing/20230823_BayWaldAOI8_DJIM300L1_DEM_res1m_20240930_kriging.tif")
# 
# 
# # Ground classification: 
# las_gc <- readLAS("F:/ForestSinglePlots/20230823_BayWaldAOI8/2_Results/1_PointClouds/bufferedPC/20230823_BayWaldAOI8_DJIM300L1_PC_buff5m_20240718.las", select = "xyzrn")
# # Progressive Morphological Filter (default algorithm)
# print(Sys.time())
# las_gc1 <- classify_ground(las_gc, algorithm = pmf(ws = 5, th = 3))   # -> takes 16h 15min
# print(Sys.time())
# # writeLAS(las_gc1, "F:/ForestSinglePlots/20230823_BayWaldAOI8/0_Flights/testing_LP/20230823_BayWaldAOI8_DJIM300L1/20230823_BayWaldAOI8_DJIM300L1_classifyGround_pmfDefault.las")
# # plot(las_gc)
# # plot(las)
# # plot(las, color = "Classification")
# # plot(las_gc1, color = "Classification")
# 
# # # Cloth Simulation Filter (CSF)
# # print(Sys.time())
# # las_gc2 <- classify_ground(las_gc, algorithm = csf())
# # print(Sys.time())
# 
# # Calculate DEM newly groudn classified point cloud
# print(Sys.time())
# dtm_tin <- rasterize_terrain(las = las_gc1, res = 1, algorithm = tin(), shape = st_as_sfc(aoi_utm))  # -> takes 40 sec
# print(Sys.time())
# dtm_idw <- rasterize_terrain(las_gc1, algorithm = knnidw(k = 10L, p = 2), shape = st_as_sfc(aoi_utm))  # -> takes 8 sec
# print(Sys.time())
# 
# 
# # Export DEMs to testing folder
# terra::writeRaster(dtm_tin, "F:/ForestSinglePlots/20230823_BayWaldAOI8/2_Results/0_Raster/bufferedRaster/DEM_testing/20230823_BayWaldAOI8_DJIM300L1_DEM_res1m_20241001_tin_classifyGround_pmfDefault.tif")
# terra::writeRaster(dtm_idw, "F:/ForestSinglePlots/20230823_BayWaldAOI8/2_Results/0_Raster/bufferedRaster/DEM_testing/20230823_BayWaldAOI8_DJIM300L1_DEM_res1m_20241001_idw_classifyGround_pmfDefault.tif")
#
# # ---> I don't like the DEM from classify_ground(las_gc, algorithm = pmf(ws = 5, th = 3)) !!


####   T e s t i n g   E n d   #####################################################################################################################################################################




### Patch - level -----------------------------------------------------------------------------------------------------

# Visualize aoi & patches
    plot_aois(aoi_utm, patches_utm)
# Visualize aoi & buffered patches
    plot_aois(aoi_utm, patchesBuff$aoiBuff_sf)
    
#///////////////////////////////////////////////////////////////////////////////
# Clip the raster by each polygon and store them in a list
patches_raster_deriv <- clip_data_by_polygons(raster_deriv, patches_utm)
patches_raster_deriv_buff <- clip_data_by_polygons(raster_deriv, patchesBuff$aoiBuff_sf)
#///////////////////////////////////////////////////////////////////////////////
# FROM OTHER SCRIPT; ATTEMPTS SIMILAR; ONE FUNCTION SHOULD BE ENOUGH; -> CLEAN!
# # Clip (and mask) raster with corresponding buffered aoi
# clipped_rasters_buff <- clip_rasters_with_aoi_list(utm_raster_list, aoiBuff) 
#///////////////////////////////////////////////////////////////////////////////

# Visualize raster derivatives per patch
    for (i in 1:length(patches_raster_deriv)){
      # Print the plot number and the name of the raster before apply plot function
      cat("Plot", i, ":", names(patches_raster_deriv[i]), "\n")
      plot_list_output(patches_raster_deriv[[i]], 3)
    }


# Calculate global statistics for list of raster                                ### TABLE format
patches_raster_deriv_buff_globalStats <- lapply(patches_raster_deriv_buff, calculate_global_stats)
(patches_raster_deriv_buff_globalStats_df <- flatten_nested_list_to_df(patches_raster_deriv_buff_globalStats))


# Make global stats raster from Point Cloud                                     ### RASTER format
print(f)
# Apply pixel_metrics to each LAS file in your list
raster_deriv_globalStats_pixelmetrics_list <- lapply(patches_pc_buff$nlas, function(las_file) {
  pixel_metrics(las_file, ~f(Z), 0.5)           # takes ~ 4 secs for resolution 0.5m
})


# Visualize global stats raster per patch
    for (i in 1:length(raster_deriv_globalStats_pixelmetrics_list)){
      # Print the plot number and the name of the raster before apply plot function
      cat("Plot", i, ":", names(raster_deriv_globalStats_pixelmetrics_list[i]), "\n")
      plot_list_output(raster_deriv_globalStats_pixelmetrics_list[[i]], 4)
    }



# Calculate agb statistics based on chm (tch = mean top of canopy, agb carbon, agb)
patches_agbStats <- lapply(patches_raster_deriv_buff$chm, calculate_agb_stats)





### Grid - level ------------------------------------------------------------------------------------------------------
# Create a grid of 5x5m covering the extent of all patches in AOI
grid_list <- make_grids(patches_utm, 5)
# Visualize aoi, patches & grids
plot_grids(grid_list, aoi_utm, patches_utm)







## POINT derivatives  ------------------------------------------------------------------------------------------------
### Aoi - level ------------------------------------------------------------------------------------------------------

# Visualize Transect
  print(Sys.time())
  plot_transect(nlas, aoi_utm, raster_deriv$chm)  # -> ~ 1 min 25 secs
  print(Sys.time())

# # Visualize base product raster
#     print(Sys.time())
#     plot_las_with_transect(nlas, aoi_utm)  # -> ~ 1 min 25 secs
#     print(Sys.time())
    
    # Visualize point cloud per patch
    print(Sys.time())
    plot(nlas, axis = TRUE, legend = TRUE, bg = "white", backend = "rgl")
    axes3d(edges = "bbox")  # Add axes around the bounding box
    title3d(xlab = "X (m)", ylab = "Y (m)", zlab = "Z (m)")
    print(Sys.time())
      
    
# 1a. Calculate lidR-defined cloud height statistics (zmax/zmean/zskew/zkurt/zentropy/pzabovezmean/pzabove2 for all points (== 100% quantile), and zmax of quantiles in 5% steps, and zpcum1-9 (???))
(cloud_stdmetrics_z <- cloud_metrics(nlas, func = .stdmetrics_z))  # -> ~ 35 secs
    
                # # Convert the list to a data frame
                # cloud_stdmetrics_df <- as.data.frame(t(as.data.frame(cloud_stdmetrics_z)))
                # # Set column names for clarity
                # colnames(cloud_stdmetrics_df) <- c("Value")
                # # Print the table
                # print(cloud_stdmetrics_df)♀
                # DT::datatable(round(cloud_stdmetrics_df,3))
                # # Rename `zq5` to `zq05`
                # rownames(cloud_stdmetrics_df)[rownames(cloud_stdmetrics_df) == "zq5"] <- "zq05"
                # # Reorder the rows by row name
                # zq_values <- zq_values[order(rownames(zq_values)), ]
                # # Extract only rows that start with "zq" and reverse the order
                # zq_values <- cloud_stdmetrics_df[grep("^zq", rownames(cloud_stdmetrics_df)), , drop = FALSE]
                # zq_values <- zq_values[rev(order(rownames(zq_values))), , drop = FALSE]
                # # Rename columns for clarity
                # colnames(zq_values) <- "Value"
                # # Print the table
                # print(zq_values)
                # DT::datatable(round(zq_values,3))

    
    
# 1b. Calculate self-defined cloud height statistics (BE_H_MAX/MIN/MEAN/MEDIAN/SD/VAR/VAR_COEF/SKEW/KURTOSIS)
#' -> based on different statistical metrics based on canopy heights; based on point height above ground
#' -> difference to 1a: calculates all statistics per quantiles and not for whole pc, but no zpcum1-9(???)
(cloud_be_h_stats <- calculate_be_h_stats(nlas))  # -> indices calc. might take a while (~3.61 mins)

                # Convert row names to a factor with correct order
                cloud_be_h_stats_new <- cloud_be_h_stats[order(factor(rownames(cloud_be_h_stats),
                                                                  levels = paste0("BE_H_P", c(100, 90:0)))), ]
                DT::datatable(round(cloud_be_h_stats, 3))




# 2. Calculate point statistics (BE_RD_CAN/UND/REG/GRD/_01/02/.../_maxHeight, BE_PR_CAN/UND/REG/GRD/_PR_01/02/.../_maxHeight) 
#' Point Density (Proportion of Total Points): number_points_in_layer / total_points_in_pointcloud
#' Point Count (Points per Unit Height):       number_points_in_layer / height_of_layer (e.g., 1 m)
#' Penetration Rate (Pass-Through Rate):       number_points_in_layer / number_points_in_layer_and_below
#' -> so far, all points below 0 are excluded. Clarify with others, if yes/no?!
#' -> clarify exact outer boundaries of bins in functions, if open/closed?!
#' -> there is a mini tiny slightly difference in point_density per meter: e.g 0-1m calculated with calculate_layer_pointStats: 0.1614687110, 0-1, sum up first ten 0.1m layer: 0.1615

# Option 1: Generate stats for single 1m layer
cloud_be_point_stats_1m_layer <- calculate_layer_pointStats(nlas, layer_height = 1)  # -> ~ 8 secs
View(cloud_be_point_stats_1m_layer)


# Option 2:  Generate stats for broader vegetation layers -> define ground-, regeneration- and understory-layer (canopy-layer is all above understory-layer)
cloud_be_point_stats_veg_layer <- calculate_veg_layer_pointStats(nlas, grd = 1.5, reg = 5, und = 7)   # -> ~ 10 secs
View(cloud_be_point_stats_veg_layer$veg_layer_stats)
View(cloud_be_point_stats_veg_layer$single_layer_stats)


                # # View output in reverse order
                # View(cloud_be_point_stats_1m_layer %>% arrange(desc(row_number())))  # Sort by Height_Layer_m in descending order
                # View(cloud_be_point_stats_veg_layer$veg_layer_stats %>% arrange(desc(row_number())))



# Visualize
    plot_height_density_cloud <- create_density_by_height_plot(cloud_be_point_stats_1m_layer, title = "Point Density by Height Layer")
# Display the plot
    print(plot_height_density_cloud)
# Save the plot (optional)
    #ggsave("C:/Users/lup42bs/Desktop/plot_height_density_cloud.png", plot = plot_height_density_cloud, width = 12, height = 16, dpi = 300)






####   T e s t i n g   S t a r t   #################################################################################################################################################################

    
# ### Cloud metrics
# print(Sys.time())
# print(Sys.time())
# 
# print(Sys.time())
# metrics_polygon <- polygon_metrics(nlas, .stdmetrics_z, patches_utm, radius = 0)  # -> ~ 18 secs
# print(Sys.time())
# 
# ### Pixel metrics
# print(Sys.time())
# hmean <- pixel_metrics(nlas, ~mean(Z), 10) # calculate mean at 10 m: ~ 10 secs 
# print(Sys.time())
# plot(hmean, col = height.colors(50))
# 
# # user-defined function
# f <- function(x) {
#   list(mean = mean(x), sd = sd(x))
# }
# 
# print(Sys.time())
# metrics_pixel <- pixel_metrics(nlas, ~f(Z), 10) # calculate grid metrics; ~ 10 secs
# print(Sys.time())
# plot(metrics_pixel, col = height.colors(50))
# 
# 
# 
# q <- cloud_metrics(nlas, ~quantile(Z, probs = c(0.1, 0.5, 0.9)))




####   T e s t i n g   E n d   #####################################################################################################################################################################








### Patch - level -----------------------------------------------------------------------------------------------------

# Visualize aoi & patches
    plot_aois(aoi_utm, patches_utm)
# Visualize aoi & buffered patches
    plot_aois(aoi_utm, patchesBuff$aoiBuff_sf)

    
# Visualize point cloud per patch
    for (i in 1:length(patches_pc$nlas)){
      cat("Plot", i, ":", names(patches_pc$nlas[i]), "\n")
      plot(patches_pc$nlas[[i]], axis = TRUE, legend = TRUE, bg = "white", backend = "rgl")
      axes3d(edges = "bbox")  # Add axes around the bounding box
      title3d(xlab = "X (m)", ylab = "Y (m)", zlab = "Z (m)")
    }

# # 2-D fast visualization, deprecated!
    # for (i in 1:length(patches_pc)){
    #   # Print the plot number and the name of the raster before apply plot function
    #   cat("Plot", i, ":", names(patches_pc[i]), "\n")
    #   plot_list_output(patches_pc$nlas, 3)
    # }
    
# Visualize Transect
    for (i in 2:length(patches_pc$nlas)){
    cat("Plot", i, ":", names(patches_pc$nlas[i]), "\n")
    print(Sys.time())
    #plot_las_with_transect(nlas, aoi_utm)  # -> ~ 17 secs (but plot vis in R takes more, cannot estimate)
    plot_transect(patches_pc_buff$nlas[[i]], patchesBuff$aoiBuff_sf[i,], patches_raster_deriv_buff$chm[[i]])  # -> ~ 1 min 25 secs
    print(Sys.time())
    }

    

# 1a. Calculate lidR-defined cloud height statistics (zmax/zmean/zskew/zkurt/zentropy/pzabovezmean/pzabove2 for all points (== 100% quantile), and zmax of quantiles in 5% steps, and zpcum1-9 (???))
(patches_stdmetrics_z <- polygon_metrics(nlas, func = .stdmetrics_z, geometry = patchesBuff$aoiBuff_sf))  # -> ~ 18 secs
patches_stdmetrics_z <-   sf::st_drop_geometry(patches_stdmetrics_z)   
View(t(round(patches_stdmetrics_z, 3)))
    

                # # Convert the list to a data frame
                # patches_stdmetrics_df <- as.data.frame(t(as.data.frame(patches_stdmetrics_z[9,])))
                # # Set column names for clarity
                # colnames(patches_stdmetrics_df) <- c("Value")
                # # Print the table
                # print(patches_stdmetrics_df)
                # DT::datatable(round(patches_stdmetrics_df,3))
                # # Rename `zq5` to `zq05`
                # rownames(patches_stdmetrics_df)[rownames(patches_stdmetrics_df) == "zq5"] <- "zq05"
                # # Reorder the rows by row name
                # zq_values <- zq_values[order(rownames(zq_values)), ]
                # # Extract only rows that start with "zq" and reverse the order
                # zq_values <- patches_stdmetrics_df[grep("^zq", rownames(patches_stdmetrics_df)), , drop = FALSE]
                # zq_values <- zq_values[rev(order(rownames(zq_values))), , drop = FALSE]
                # # Rename columns for clarity
                # colnames(zq_values) <- "Value"
                # # Print the table
                # print(zq_values)
                # View(round(zq_values,3))
                # 
                # 
                # # Extract only rows that start with "zq" and reverse the order
                # zp_values <- patches_stdmetrics_df[grep("^zp", rownames(patches_stdmetrics_df)), , drop = FALSE]
                # zp_values <- zp_values[rev(order(rownames(zp_values))), , drop = FALSE]
                # # Rename columns for clarity
                # colnames(zp_values) <- "Value"
                # # Print the table
                # print(zp_values)
                # View(round(zp_values,3))
                
    
    
    

# 1b. Calculate height statistics (BE_H_MAX/MIN/MEAN/MEDIAN/SD/VAR/VAR_COEF/SKEW/KURTOSIS)
#' -> based on different statistical metrics based on canopy heights; based on point height above ground
#' -> difference to 1a: calculates all statistics per quantiles and not for whole pc, but no zpcum1-9(???)

patches_be_h_stats <- lapply(patches_pc_buff$nlas, calculate_be_h_stats)  # -> ~ 16 secs
# print(Sys.time())
# (patches_be_h_stats <- polygon_metrics(nlas, func = ~calculate_be_h_stats(nlas), geometry = patches_utm))  # -> didn't run yet to find out if it works, takes too long right now, check later!
# print(Sys.time())



                # patches_be_h_stats_single <- patches_be_h_stats[[9]]
                # 
                # # Convert row names to a factor with correct order
                # patches_be_h_stats_single <- patches_be_h_stats_single[order(factor(rownames(patches_be_h_stats_single),
                #                                                   levels = paste0("BE_H_P", c(100, 90:0)))), ]
                # View(round(patches_be_h_stats_single,3))
                



# 2. Calculate point statistics (BE_RD_CAN/UND/REG/GRD/_01/02/.../_maxHeight, BE_PR_CAN/UND/REG/GRD/_PR_01/02/.../_maxHeight) 
#' Point Density (Proportion of Total Points): number_points_in_layer / total_points_in_pointcloud
#' Point Count (Points per Unit Height):       number_points_in_layer / height_of_layer (e.g., 1 m)
#' Penetration Rate (Pass-Through Rate):       number_points_in_layer / number_points_in_layer_and_below
#' -> so far, all points below 0 are excluded. Clarify with others, if yes/no?!
#' -> clarify exact outer boundaries of bins in functions, if open/closed?!
#' -> there is a mini tiny slightly difference in point_density per meter: e.g 0-1m calculated with calculate_layer_pointStats: 0.1614687110, 0-1, sum up first ten 0.1m layer: 0.1615


# Option 1: Generate stats for single 1m layer
patches_be_point_stats_1m_layer <- lapply(patches_pc_buff$nlas, calculate_layer_pointStats)  # -> ~ 1 sec
View(patches_be_point_stats_1m_layer)


# Option 2:  Generate stats for broader vegetation layers -> define ground-, regeneration- and understory-layer (canopy-layer is all above understory-layer)
patches_be_point_stats_veg_layer <- lapply(patches_pc_buff$nlas, calculate_veg_layer_pointStats)   # -> ~ 2 secs
View(patches_be_point_stats_veg_layer[[1]]$veg_layer_stats)
View(patches_be_point_stats_veg_layer[[2]]$veg_layer_stats)
View(patches_be_point_stats_veg_layer[[1]]$single_layer_stats)




                # # View output in reverse order
                # View(patches_be_point_stats_1m_layer[[9]] %>% arrange(desc(row_number())))  # Sort by Height_Layer_m in descending order
                # View(patches_be_point_stats_veg_layer[[9]]$veg_layer_stats %>% arrange(desc(row_number())))










# Visualize Option 1

# Generate a list of plots with titles based on list element names
    plots_height_density_patches <- mapply(function(df, name) {
      create_density_by_height_plot(df, paste("Plot id:", name))
    }, patches_be_point_stats_1m_layer, names(patches_be_point_stats_1m_layer), SIMPLIFY = FALSE)
    
# Arrange the plots in a 3-column grid
    combined_plots_height_density_patches <- wrap_plots(plots_height_density_patches, ncol = 3)
    
# Display the combined plot
    print(combined_plots_height_density_patches)
    
# Save the plot (optional)
    #ggsave("C:/Users/lup42bs/Desktop/combined_plots_height_density_patches.png", plot = combined_plots_height_density_patches, width = 12, height = 16, dpi = 300)








####   T e s t i n g   S t a r t   #################################################################################################################################################################



# Plot Point Density by Height Layer
#data <- patches_be_point_stats_veg_layer[[1]]$single_layer_stats
data <- patches_be_point_stats_1m_layer[[1]]
data$Height <- seq(1, nrow(data))



ggplot(data, aes(x = Point_Density)) +
  geom_density(fill = "steelblue", alpha = 0.5) +  # Density plot
  theme_minimal() +
  labs(title = "Density Plot of Point Density", x = "Point Density", y = "Density") + 
  coord_flip() 

ggplot(data, aes(x = Point_Count)) +
  geom_density(fill = "steelblue", alpha = 0.5) +  # Density plot
  theme_minimal() +
  labs(title = "Density Plot of Point Count", x = "Point Count", y = "Density") +
  coord_flip()  

ggplot(data, aes(x = Height_Layer_m, y = Point_Density)) +
  geom_bar(stat = "identity") +
  
  theme_minimal() +
  labs(title = "Point Density by Height Layer", x = "Height Layer (m)", y = "Point Density") +
  coord_flip()  

ggplot(data, aes(x = Point_Density, y = Height)) +
  geom_density_2d_filled() +  # Filled density contours
  theme_minimal() +
  labs(title = "Filled Density Plot of Height Numeric by Point Density", 
       x = "Point Density", 
       y = "Height (m)")






# metrics_polygon <- polygon_metrics(nlas, .stdmetrics_z, patches_utm)  # -> ~ 18 secs
# 
# x <- polygon_metrics(nlas, calculate_be_h_stats, patches_utm)
# 
# # user-defined function
# f <- function(x) {
#   list(mean = mean(x), sd = sd(x))
# }
# 
# print(Sys.time())
# metrics_pixel <- pixel_metrics(nlas, ~f(Z), 10) # calculate grid metrics; ~ 10 secs
# print(Sys.time())
# 
# 
# 
# 
# # # Apply the function to each LAS object in nlas2_patches
# # be_h_indices_list <- lapply(nlas, calculate_be_h_stats)
# # 
# # # Convert to a dataframe
# # be_h_indices_df <- do.call(rbind, lapply(seq_along(be_h_indices_list), function(i) {
# #   c(Patch.Nr = Patches$Patch.Nr[i], unlist(be_h_indices_list[[i]]))
# # }))



####   T e s t i n g   E n d   #####################################################################################################################################################################



### Grid - level ------------------------------------------------------------------------------------------------------
# Visualize aoi, patches & grids
plot_grids(grid_list, aoi_utm, patches_utm)
