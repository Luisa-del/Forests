#'####################################################################################################################
# Generate lidar derivatives for German Forests project
# 18.07.2024
# Luisa Pflumm
# Example area: BayWaldAOI8

# Load libraries
pacman::p_load(exifr, dplyr, leaflet,RColorBrewer, sf, parallel, viridis, zip, terra, lidR, ggplot2, purrr)
options(scipen = 100, digits = 4) # avoid numbers with e

#'####################################################################################################################
# # SET CUSTOM PARAMETER ####
# USER: Set path to the corresponding function scripts
SourcePath1 <- "C:/Users/lup42bs/Documents/Projects/Forests/Scripts/R/Generate_Lidar_Derivatives_FUNCTIONS.R"
source(SourcePath1)
SourcePath2 <- "C:/Users/lup42bs/Documents/Projects/Forests/Scripts/R/Generate_Base_Products_FUNCTIONS.R"
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
# # PROCESS FILES ####
## Raster derivatives  -----------------------------------------------------------------------------------------------
### AOI - level ------------------------------------------------------------------------------------------------------
# Apply point cloud normalization
las <- base_products_buff$pointcloud
nlas <- normalize_height(las, knnidw())
hist(filter_ground(nlas)$Z, main = "", xlab = "Elevation")
#nlas2 <- normalize_height(base_products_buff$pointcloud, knnidw(), dtm = base_products_buff$dem)  # -> hybrid method, can include negative numbers
#hist(filter_ground(nlas2)$Z, main = "", xlab = "Elevation")
# Optional: Filter the data to exclude points with heights below 0
if (any(nlas$Z < 0)) {nlas <- nlas[nlas$Z >= 0, ]
              } else {nlas <- nlas}

# Calculate & list variables from top-ALS returns: CHM (based on nlas) and DSM (based on las)
chm_list <- calculate_topALSreturns(nlas, "CHM", 0.5)
dsm_list <- calculate_topALSreturns(las, "dsm", 0.5)
# Store suitable CHM & DSM in list
raster_deriv <- list(chm = chm_list$chm_tri_2, dsm = dsm_list$dsm_tri_2) # select chnm/dsm version from list
# Check raster validity and get specific raster information
raster_deriv_info <- lapply(raster_deriv, check_raster_validity_info)
# ?->? Align CHM & DSM to DEM
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
# Calculate global statistics for list of raster (mean, min, max, sd -> more?)
(raster_deriv_globalStats <- calculate_global_stats(raster_deriv))

### Patch - level -----------------------------------------------------------------------------------------------------
# Transform AOI & Patches: WGS to UTM
aoi_utm <- st_transform(aoi_wgs, get_utm_epsg(raster_deriv[[1]]))
patches_utm <- st_transform(patches_wgs, get_utm_epsg(raster_deriv[[1]]))
# Visualize aoi & patches
plot_aois(aoi_utm, patches_utm)
# Apply appropriate buffer to patches (-> to avoid edge effects in statistical analyses: consider pixel resolution, if not exceeding raster extent ideally use factor 10, or 5. Otherwise no buffer.)
patchesBuff <- apply_buffer(patches_utm, raster_deriv[[1]])
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

# Calculate global statistics for list of raster
patches_raster_deriv_buff_globalStats <- lapply(patches_raster_deriv_buff, calculate_global_stats)
(patches_raster_deriv_buff_globalStats_df <- flatten_nested_list_to_df(patches_raster_deriv_buff_globalStats))


### Grid - level ------------------------------------------------------------------------------------------------------
# Create a grid of 5x5m covering the extent of all patches in AOI
grid_list <- make_grids(patches_utm, 5)
# Visualize aoi, patches & grids
plot_grids(grid_list, aoi_utm, patches_utm)







## Point derivatives  ------------------------------------------------------------------------------------------------
### AOI - level ------------------------------------------------------------------------------------------------------
# Visualize base product raster
plot_las_with_transect(nlas, aoi_utm)  # -> PC visual. might take a while

# 1. Calculate height statistics (BE_H_MAX/MIN/MEAN/MEDIAN/SD/VAR/VAR_COEF/SKEW/KURTOSIS)
#' -> based on different statistical metrics based on canopy heights; based on point height above ground
(be_h_stats <- calculate_be_h_stats(nlas))  # -> indices calc. might take a while (~3.61 mins)

# 2. Calculate point statistics (BE_RD_CAN/UND/REG/GRD, BE_RD_01/02/.../maxHeight, BE_PR_CAN/UND/REG/GRD, BE_PR_01/02/.../maxHeight) 
#' Point Density (Proportion of Total Points): number_points_in_layer / total_points_in_pointcloud
#' Point Count (Points per Unit Height):       number_points_in_layer / height_of_layer (e.g., 1 m)
#' Penetration Rate (Pass-Through Rate):       number_points_in_layer / number_points_in_layer_and_below
#' -> so far, all points below 0 are excluded. Clarify with others, if yes/no?!
#' -> clarify exact outer boundaries of bins in functions, if open/closed?!

# Option 1: Generate stats for single 1m layer
be_pr_rd_stats_1m_layer <- calculate_layer_pointStats(nlas, layer_height = 1)
View(be_pr_rd_stats_1m_layer)

# Option 2:  Generate stats for broader vegetation layers -> define ground-, regeneration- and understory-layer (canopy-layer is all above understory-layer)
be_pr_rd_stats_veg_layer <- calculate_veg_layer_pointStats(nlas, grd = 1.5, reg = 5, und = 7)
View(be_pr_rd_stats_veg_layer$single_layer_stats)
View(be_pr_rd_stats_veg_layer$Point_Count_per_veg_layer)
View(be_pr_rd_stats_veg_layer$Point_Density_per_veg_layer)
View(be_pr_rd_stats_veg_layer$Penetration_Rate_per_veg_layer)


#  -> HIER STEHEN GEBLIEBEN!! (unten alte versuche, ggfs weg)
















#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
# Calculate penetration rates
#extract ground points, first returns, and last returns.
#Calculate Penetration Rates: Compute the ratio of ground points to total points for all points, first returns, and last returns.
#'Penetration Rate All: ratio of the number of ground points (Classification == 2) to the total number of points in the LAS file; 
#->gives an overall penetration rate, considering all points in the dataset.
#'Penetration Rate First: ratio of the number of ground points that are also first returns (Classification == 2 & ReturnNumber == 1) to the total number of first returns (ReturnNumber == 1); 
#-> represents the penetration rate of ground points specifically among the first returns, which often correspond to the highest surface (e.g., canopy top).
#'Penetration Rate Last: ratio of the number of ground points that are also last returns (Classification == 2 & ReturnNumber == las_data$NumberOfReturns) to the total number of last returns;
#-> last returns are typically closer to the ground, so this rate gives insight into how many of the last returns are actually classified as ground points.

penetration_rates <- calculate_penetration_rates(nlas)
print(penetration_rates * 100)
penetration_rates_veg <- calculate_vegetation_penetration_rates(nlas)
print(penetration_rates_veg * 100)

# #pr <- calculate_penetration_rate(nlas, 0, 1)
# pr2 <- calculate_penetration_rates(nlas)
pr <- calculate_penetration_rates(nlas)










# # Function to calculate mean top-of-canopy height (TCH)
# calculate_tch <- function(chm_raster) {
#   # Calculate the mean of CHM raster pixels
#   tch_mean <- global(chm_raster, mean, na.rm = TRUE)
#   return(tch_mean)
# }
# # Calculate the mean height of the CHM
# tch <- chm_height_mean <- lapply(patches_raster_deriv$chm, calculate_tch)

#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////







### Patch - level -----------------------------------------------------------------------------------------------------
# Visualize aoi & patches
plot_aois(aoi_utm, patches_utm)
# Visualize aoi & buffered patches
plot_aois(aoi_utm, patchesBuff$aoiBuff_sf)
# Clip the normalized point cloud by each polygon and store them in a list
patches_pc <- clip_data_by_polygons(list(nlas = nlas), patches_utm)
patches_pc_buff <- clip_data_by_polygons(list(nlas = nlas), patchesBuff$aoiBuff_sf)
# Visualize point cloud per patch
for (i in 1:length(patches_pc)){
  # Print the plot number and the name of the raster before apply plot function
  cat("Plot", i, ":", names(patches_pc[i]), "\n")
  plot_list_output(patches_pc$nlas, 3)
}





# # Apply the function to each LAS object in nlas2_patches
# be_h_indices_list <- lapply(nlas, calculate_be_h_stats)
# 
# # Convert to a dataframe
# be_h_indices_df <- do.call(rbind, lapply(seq_along(be_h_indices_list), function(i) {
#   c(Patch.Nr = Patches$Patch.Nr[i], unlist(be_h_indices_list[[i]]))
# }))




### Grid - level ------------------------------------------------------------------------------------------------------
# Visualize aoi, patches & grids
plot_grids(grid_list, aoi_utm, patches_utm)
