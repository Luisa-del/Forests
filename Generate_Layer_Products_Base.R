#'####################################################################################################################
# Generate layer products for German Forests project
# 01.07.2024
# Luisa Pflumm
# Example area: BayWaldAOI1

# Load libraries
pacman::p_load(exifr, dplyr, leaflet,RColorBrewer, sf, parallel, viridis, zip, terra)

#'####################################################################################################################
#' System           || Mission               | Site                             | NOTE
#' ---------------- || --------------------- | -------------------------------- | ----------------
#' DJIM300L1        || 20230615_Passau       | AOI1, AOI2                       | L1: TF80 North, TF90 for South
#' DJIM300MXDual    || 20230620_Hunsrueck    |                                  |
#' DJIM300H20T      || 20230823_BayWaldAOI5  |                                  |
#' WingtraAltum     || 20230823_BayWaldAOI6  |                                  |
#' WingtraRX1RII    || 20230823_BayWaldAOI7  |                                  |
#'                  || 20230823_BayWaldAOI8  |                                  |
#'                  || 20230823_Luebeck      | North, South                     |
#'                  || 20230824_BayWaldAOI1  |                                  |
#'                  || 20230824_BayWaldAOI2  |                                  |
#'                  || 20230825_BayWaldAOI3  |                                  |
#'                  || 20230825_BayWaldAOI4  |                                  |
#'                  || 20230921_Saarland     | AOI1-2, AOI3 / AOI1, AOI2, AOI3  | WingtraAltum: AOI1, AOI2, AOI3; all others: AOI1-2, AOI3
#'                  || 20240625_BayWaldAOI9  |                                  |

#'####################################################################################################################
# SET CUSTOM PARAMETER ####
# USER: Set path to the corresponding function script
SourcePath <- "C:/Users/lup42bs/Documents/Projects/Forests/Scripts/R/Generate_Layer_Products_Base_FUNCTIONS.R"
source(SourcePath)
# USER: Set path to root folder (where missions are stored)
RootPath <- "F:/ForestSinglePlots/"
# List all existing flight folder
list_all_flight_paths(RootPath)
# USER: Select mission (from table)
mission <- "20230824_BayWaldAOI1"
# List only flight folders of selected mission
list_mission_flight_paths(RootPath, mission)
# USER: Select path to flight folder
FlightPath <- "F:/ForestSinglePlots/20230824_BayWaldAOI1/0_Flights/20230824_BayWaldAOI1_DJIM300L1/"
# USER: Set buffer (will be applied to aoi for raster clipping, default: 5m)
buff <- 5

#'####################################################################################################################
# PREPARE PROCESSING ####
# Select flight name
FlightName <- select_flightname(FlightPath)
# Define export path
ExportPath <- paste0(RootPath, mission, "/2_Results/0_Raster/")
#'-----------------------------------------------------------------------------------------------------#
# Define output .txt file to save console log                                                          #
outputTXTfile <- paste0(ExportPath, FlightName, "_console_output_processing_", current_date, ".txt")   #
# Check if file exists and decide: overwrite or append                                                 # 
user_decision_overwrite_file <- userPromt_outputTXTfile(outputTXTfile)                                 #
# Open a connection to the text file                                                                   #
sink(outputTXTfile, append = !user_decision_overwrite_file)                                            #
#'-----------------------------------------------------------------------------------------------------#
# Print current date & time at start of processing      #
print(paste0("Current date: ", get_current_date()))     #
print(paste0("Current time: ", get_current_time()))     #
#'------------------------------------------------------#
# sink() 
# # Optionally, read the file to verify the content
# cat(readLines(outputTXTfile), sep = "\n")
# Print all custom parameter
cat("SourcePath:", SourcePath, "\n")
cat("RootPath:", RootPath, "\n")
cat("mission:", mission, "\n")
cat("FlightPath:", FlightPath, "\n")
cat("Buffer (in m):", buff, "\n")

#'####################################################################################################################
# IMPORT FILES ####
# Load Vector Files
AOI_wgs <- st_read(paste0(RootPath,"0_BaseInfo/gpkg/beta4_aois.gpkg")) %>% st_zm()
Patches_wgs <- st_read(paste0(RootPath,"0_BaseInfo/gpkg/beta4_plots.gpkg"))
# Select required vector for AOI     -> Carefull! More aois per mission for Lübeck, Saarland and Passau!!
aoi_name <- strsplit(mission, "_")[[1]][2]
aoi_wgs <- AOI_wgs[AOI_wgs$folder_name == aoi_name, ]
# Load Raster Files
## L1 (Lidar) ####
r_l1 <- import_dem_l1(FlightPath, FlightName)
plot(r_l1, main = "DEM (L1)")

#'####################################################################################################################
# PROCESS FILES ####

# Check raster validity
validity_results <- check_raster_validity(r_l1)
# Transform vector to raster CRS
#aoi <- transform_sf_to_raster_crs(aoi_wgs, r_l1) # -> Function exists, but is commented out. Perform steps below.
r_epsg <- as.numeric(crs(r_l1, describe=T)$code)
r_extent <- ext(r_l1)
aoi <- st_transform(aoi_wgs, r_epsg)
plot(aoi$geom, add=T)
# Clip raster to to aoi
r_l1_clip_aoi <- crop(r_l1, aoi)
plot(r_l1_clip_aoi, main = paste0("DEM (L1) - clipped to aoi"))
plot(aoi$geom, add=T)

# # Apply buffer to aoi?
# aoiBuff <- st_buffer(aoi, buff)
# plot(aoiBuff$geom, add=T)
# # Clip raster to to aoi
# r_l1_clip_aoiBuff <- crop(r_l1, aoiBuff)
# plot(r_l1_clip_aoiBuff, main = paste0("DEM (L1) - clipped to ", buff, "m buffer around aoi"))
# plot(aoi$geom, add=T)
# plot(aoiBuff$geom, add=T)

#'####################################################################################################################
# EXPORT FILES ####
# Get resolution info of raster
(resolution_info <- get_resolution_info(r_l1))
# Rename bands
(current_bandname <- names(r_l1_clip_aoi))
(new_bandname <- paste0(FlightName, "_dem_res", resolution_info$res_exp, "m"))
names(r_l1_clip_aoi) <- new_bandname
# Define file path for export raster
ExportRasterPath <- paste0(ExportPath, FlightName, "_dem_res", resolution_info$res_exp, "m_", current_date, ".tif")
# Export raster
export_raster(r_l1_clip_aoi, ExportRasterPath)



#'------------------------------------------------------#
# Print current date & time at start of processing      #
print(paste0("Current date: ", get_current_date()))     #
print(paste0("Current time: ", get_current_time()))     #
#'-----------------------------------------------------------------------------------------------------#
# Close the connection to the text file                                                                #
sink()                                                                                                 #
# Optionally, read the file to verify the content                                                      #
#cat(readLines(outputTXTfile), sep = "\n")                                                             #
#'-----------------------------------------------------------------------------------------------------#






## MXDual (Multispectral 10b) ####


## H20T (Thermal) ####


## Altum (Multispectral 6b) ####


## RX1RII (RGB) ####

