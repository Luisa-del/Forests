#'####################################################################################################################
# Generate base products for German Forests project
# 01.07.2024
# Luisa Pflumm
# Example area: BayWaldAOI8

# Load libraries
pacman::p_load(exifr, dplyr, leaflet,RColorBrewer, sf, parallel, viridis, zip, terra, lidR)
options(scipen = 100, digits = 4) # avoid numbers with e

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
SourcePath <- "C:/Users/lup42bs/Documents/Projects/Forests/Scripts/R/Generate_Base_Products_FUNCTIONS.R"
source(SourcePath)
# USER: Set path to root folder (where missions are stored)
RootPath <- "F:/ForestSinglePlots/"
# List all existing flight folder
list_all_flight_paths(RootPath)
# USER: Select mission (from table)
mission <- "20230823_BayWaldAOI8"
# List only flight folders of selected mission
(list_mission_flightpaths <- list_mission_flight_paths(RootPath, mission))
# USER: Select path to flight folder
#FlightPath <- "F:/ForestSinglePlots/20230823_BayWaldAOI8/0_Flights/20230823_BayWaldAOI8_DJIM300L1/"
#FlightPath <- "F:/ForestSinglePlots/20230823_BayWaldAOI8/0_Flights/20230823_BayWaldAOI8_DJIM300MXDual/"
#FlightPath <- "F:/ForestSinglePlots/20230823_BayWaldAOI8/0_Flights/20230823_BayWaldAOI8_DJIM300H20T/"   
#FlightPath <- "F:/ForestSinglePlots/20230823_BayWaldAOI8/0_Flights/20230823_BayWaldAOI8_WingtraAltum/" 
FlightPath <- "F:/ForestSinglePlots/20230823_BayWaldAOI8/0_Flights/20230823_BayWaldAOI8_WingtraRX1RII/"

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
# Load Raw Output
(raw_output <- import_raw_output(FlightPath, FlightName))
# Put files in list according to file type
if (any(grepl("pointcloud", names(raw_output)))) {     # if (endsWith(FlightName, "DJIM300L1")) {
  raster_list <- raw_output[1]
  pointcloud <- raw_output[[2]]
} else {
  raster_list <- raw_output
}
# Trim raster (remove NAs)
raster_list <- lapply(raster_list, trim_raster)
# # Plot raster
# invisible(lapply(raster_list, function(r) suppressMessages(suppressWarnings(plot_raster(r)))))    # this suppresses warnings, other than simple lapply(raster_list, plot_raster)

#'####################################################################################################################
# PROCESS FILES ####
# Check raster validity and get specific raster information
raster_info <- lapply(raster_list, check_raster_validity_info)
# Check if rasters have UTM projection, optionally get correct UTM EPSG code and reproject rasters
if (raster_info[[1]]$epsg == 4326){
  utm_raster_list <- project_raster_list(raster_list)
  raster_info <- lapply(utm_raster_list, check_raster_validity_info)
} else {
  utm_raster_list <- raster_list
}
# Transform AOI: WGS to UTM
aoi_utm <- st_transform(aoi_wgs, get_utm_epsg(raster_list[[1]]))
# Apply appropriate buffer to aoi (-> to avoid edge effects in statistical analyses: consider pixel resolution, if not exceeding raster extent ideally use factor 10, or 5. Otherwise no buffer.)
aoiBuff <- apply_buffer_check_to_list(aoi_utm, utm_raster_list)
# Clip (and mask) raster with corresponding buffered aoi
clipped_rasters_buff <- clip_rasters_with_aoi_list(utm_raster_list, aoiBuff)
# Plot raster
#invisible(lapply(clipped_rasters_buff, function(r) suppressMessages(suppressWarnings(plot_raster(r)))))

# Clip point cloud with corresponding buffered aoi and aoi
if (any(grepl("pointcloud", names(raw_output)))) {
  pointcloud_list <- list(clipped_pc_buff = clip_roi(pointcloud, aoiBuff[[1]][[1]]),
                          clipped_pc = clip_roi(clipped_pc_buff, aoi_utm))
}

#' #'####################################################################################################################
#' # PLOT RESULTS ####
#' plot(utm_raster_list$dem, main = "DEM")
#' plot(aoi_utm$geom, add=T)

#'####################################################################################################################
# EXPORT FILES ####
# Print current band names
cat("Current band names: \n")
lapply(clipped_rasters_buff, names)
# Rename bands to FlightName + bands + resolution
clipped_rasters_buff_renamed <- rename_bands(FlightName, clipped_rasters_buff, raster_info)
# Print new band names
cat("New band names: \n")
lapply(clipped_rasters_buff_renamed, names)

# Export the rasters
export_rasters(clipped_rasters_buff_renamed, ExportPath, FlightName, raster_info, aoiBuff)

# -> aktuell bei highest res RGB multiband werden band names noch nicht richtig exportiert !!!

# Export point clouds
if (any(grepl("pointcloud", names(raw_output)))) {
  export_pointcloud(pointcloud_list, ExportPath, FlightName, aoiBuff)
}

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

#'####################################################################################################################
# END
