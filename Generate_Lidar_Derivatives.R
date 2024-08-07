#'####################################################################################################################
# Generate lidar derivatives for German Forests project
# 18.07.2024
# Luisa Pflumm
# Example area: BayWaldAOI8

# Load libraries
pacman::p_load(exifr, dplyr, leaflet,RColorBrewer, sf, parallel, viridis, zip, terra, lidR)
options(scipen = 100, digits = 4) # avoid numbers with e

#'####################################################################################################################
# SET CUSTOM PARAMETER ####
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
# PREPARE TXT OUTPUT ####
# Select flight name
FlightName <- select_flightname(FlightPath)
# Define export path
ExportPath <- paste0(RootPath, mission, "/2_Results/")
# Define current data
current_date <- get_current_date()
#'------------------------------------------------------------------------------------------------------------------#
# Create 2_Reports folder for txt output                                                                            #
ReportPath <- paste0(ExportPath, "2_Reports/")                                                                      #
if (!dir.exists(ReportPath)) {dir.create(ReportPath, recursive = TRUE)}                                             #
# Define output .txt file to save console log                                                                       #
outputTXTfile <- paste0(ReportPath, FlightName, "_console_output_processing_", current_date, ".txt")                #
# Check if file exists and decide: overwrite or append                                                              # 
user_decision_overwrite_file <- userPromt_outputTXTfile(outputTXTfile)                                              #
# Open a connection to the text file                                                                                #
sink(outputTXTfile, append = !user_decision_overwrite_file)                                                         #
#'------------------------------------------------------------------------------------------------------------------#
# Print executing codes                                 #
readLines(sub("_FUNCTIONS", "", SourcePath))            #
readLines(SourcePath)                                   #
cat("Console output: \n")                               #
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

#'####################################################################################################################
# IMPORT FILES ####
# Load Vector Files
AOI <- st_read(paste0(RootPath,"0_BaseInfo/gpkg/beta4_aois.gpkg")) %>% st_zm()
Patches <- st_read(paste0(RootPath,"0_BaseInfo/gpkg/beta4_plots.gpkg"))
# Select required vector for AOI     -> Carefull! More aois per mission for Lübeck, Saarland and Passau!!
(aoi_name <- strsplit(mission, "_")[[1]][2])
aoi_wgs <- AOI[AOI$folder_name == aoi_name, ]
# Load L1 base products (-> from buffered folder!)
(base_products_buff <- import_base_products_l1_buff(RootPath, mission, FlightName))
# Apply point cloud normalization
las <- base_products_buff$pointcloud
nlas <- normalize_height(las, knnidw())
hist(filter_ground(nlas)$Z, main = "", xlab = "Elevation")
# nlas2 <- normalize_height(base_products_buff$pointcloud, knnidw(), dtm = base_products_buff$dem)  # -> hybrid method, can include negative numbers
# hist(filter_ground(nlas2)$Z, main = "", xlab = "Elevation")

#'####################################################################################################################
# PROCESS FILES ####
# Calculate CHM (based on nlas) and DSM (based on las)
chm_list <- calculate_topALSreturns(nlas, "CHM", 0.5)
dsm_list <- calculate_topALSreturns(las, "dsm", 0.5)
# Store suitable CHM & DSM in list
raster_list_deriv <- list(chm = chm_list$chm_tri_2, dsm = dsm_list$dsm_tri_2)
# Check raster validity and get specific raster information
raster_info_deriv <- lapply(raster_list_deriv, check_raster_validity_info)
# Align CHM & DSM to DEM?
raster_list_deriv_aligned <- lapply(raster_list_deriv, function(raster) {resample(raster, base_products_buff$dem, method = "bilinear")})
# Transform AOI: WGS to UTM
aoi_utm <- st_transform(aoi_wgs, get_utm_epsg(raster_list_deriv[[1]]))



