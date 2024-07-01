################################################################################
# Load libraries
pacman::p_load(exifr, dplyr, leaflet,RColorBrewer, sf, parallel, viridis, zip)
################################################################################
################################################################################
################################################################################
# Add the flight path here
#FlightPath <- "F:/ForestSinglePlots/20240625_BayWaldAOI5_missingPlots/0_Flights/20240625_BayWaldAOI9_DJIM300L1/"
FlightPath <- "F:/ForestSinglePlots/20230823_Luebeck/0_Flights/20230823_LuebeckNorth_DJIM300L1/"

# Extract the flight name
FlightName <- basename(FlightPath)
# Extract flight name elements (LP)
FlightNameParts <- strsplit(FlightName, "_")[[1]]
# Define export folder
ExportMap <- paste0(FlightPath,"2_Reports/",FlightName,".html")
# Read the existing images in project
image_files <- list.files(paste0(FlightPath, "0_TerraFiles"), pattern = "*.JPG", full.names = TRUE, ignore.case = TRUE, recursive = T)
################################################################################
# Function to extract GPS data from a single image
extract_gps_data <- function(image_path) {
  exif_data <- read_exif(image_path)
  gps_data <- exif_data %>%
    select(SourceFile, GPSLatitude, GPSLongitude, GPSAltitude) %>%
    filter(!is.na(GPSLatitude) & !is.na(GPSLongitude))
  return(gps_data)
}
################################################################################
# Define the function to extract the last folder name from a path
extract_last_folder <- function(path) {
  # Normalize the path to remove any double slashes
  normalized_path <- gsub("//", "/", path)
  # Split the path into components
  path_components <- unlist(strsplit(normalized_path, "/"))
  # Extract the second to last component from the path, which is the last folder name
  last_folder <- tail(path_components, n = 2)[1]
  return(last_folder)
}
################################################################################
# Extract GPS data from all images in parallel
extract_gps_data_parallel <- function(image_files) {
  no_cores <- detectCores() - 1  # Leave one core free
  cl <- makeCluster(no_cores)
  # Export necessary variables and functions to the cluster
  clusterExport(cl, c("extract_gps_data", "read_exif"))
  clusterEvalQ(cl, library(dplyr))
  gps_data_list <- parLapply(cl, image_files, extract_gps_data)
  stopCluster(cl)
  gps_data <- bind_rows(gps_data_list)
  gps_data <- gps_data %>% mutate(last_folder = sapply(SourceFile, extract_last_folder))
  return(gps_data)
}
################################################################################
# Data root
VectorRoot <- "C:/Users/lup42bs/Documents/Projects/Forests/data/vector/gpkg/"

# Load Vector Files
AOI <- st_read(paste0(VectorRoot,"beta4_aois.gpkg")) %>% st_zm()
Patches <- st_read(paste0(VectorRoot,"beta4_plots.gpkg"))
################################################################################
# Extract GPS data from images to check coverage
gps_data <- extract_gps_data_parallel(image_files)
################################################################################
# Define a color palette based on the Name field for AOI
unique_names <- unique(AOI$name)
unique_folder_names <- unique(AOI$folder_name)
n_names <- length(unique_names)
name_palette <- colorFactor(
  palette = colorRampPalette(brewer.pal(12, "Paired"))(n_names), # Use Set3 palette for Name
  domain = AOI$name
)
# Define a color palette based on the Treatment field
unique_treatments <- unique(Patches$Treatment)
n_treatments <- length(unique_treatments)
treatment_palette <- colorFactor(
  palette = viridis(n_treatments),
  domain = Patches$Treatment
)
# Define a color palette based on the last folder name for points
unique_folders <- basename(gps_data$last_folder) %>% unique()
n_folders <- length(unique_folders)
folder_palette <- colorFactor(
  palette = colorRampPalette(brewer.pal(9, "Set1"))(n_folders), # Use Paired palette for last_folder
  domain = unique_folders
)
# Calculate the centroid of the Patches data
centroid <- st_centroid(st_union(Patches))
# # Calculate the centroid of the respective AOI data (LP)
# AOI2 <- AOI %>% filter(folder_name == FlightNameParts[2])
centroid <- st_centroid(st_union(AOI2))
# Extract the coordinates
home_lat <- st_coordinates(centroid)[2]
home_lng <- st_coordinates(centroid)[1]
home_zoom <- 14  # You can set this to your desired zoom level
# Create the map
map <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
  addTiles(group = "OpenStreetMap") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addProviderTiles(providers$CartoDB.DarkMatter, group = "Dark") %>%
  addScaleBar(position = "bottomleft") %>%
  addPolygons(data = AOI,
              fillColor = ~name_palette(name), # Set fill color based on Name
              fillOpacity = 0.2,    # Set fill opacity
              color = "black",      # Set border color
              stroke = TRUE,        # Show borders
              weight = 1,           # Set border weight
              popup = ~as.character(name)) %>%
  addPolygons(data = Patches,
              fillColor = ~treatment_palette(Treatment), # Set fill color based on Treatment
              fillOpacity = 0.5,    # Set fill opacity
              color = "black",      # Set border color
              stroke = TRUE,        # Show borders
              weight = 1,           # Set border weight
              popup = ~paste0("<strong>Region:</strong> ", Region, "<br>",
                              "<strong>RegionSite:</strong> ", RegionSite, "<br>",
                              "<strong>District:</strong> ", District, "<br>",
                              "<strong>Treatment:</strong> ", Treatment, "<br>",
                              "<strong>Patch.Nr:</strong> ", Patch.Nr, "<br>",
                              "<strong>PatchLabel:</strong> ", PatchLabel, "<br>",
                              "<strong>Label.alt:</strong> ", Label.alt)) %>%
  addCircleMarkers(data = gps_data,
                   lat = ~GPSLatitude,
                   lng = ~GPSLongitude,
                   color = ~folder_palette(last_folder),
                   radius = 5,
                   stroke = FALSE, # No border
                   fillOpacity = 0.8,
                   popup = ~paste0("<strong>Source:</strong> ", last_folder, "<br>",
                                   "<strong>Lat:</strong> ", GPSLatitude, "<br>",
                                   "<strong>Lon:</strong> ", GPSLongitude, "<br>",
                                   "<strong>H:</strong> ", GPSAltitude)) %>%
  addLayersControl(
    position = "topleft",
    baseGroups = c("OpenStreetMap", "Satellite", "Dark"),
    options = layersControlOptions(collapsed = TRUE) # Set collapsed to TRUE
  ) %>%
  addMiniMap(tiles = providers$Esri.WorldStreetMap, toggleDisplay = TRUE, position = "bottomright") %>%
  addEasyButton(easyButton(
    icon = "fa-home",
    title = "Zoom to Home",
    onClick = JS(sprintf("function(btn, map){ map.setView([%f, %f], %d); }", home_lat, home_lng, home_zoom))
  ))
# Print the map
map
################################################################################
# Export individual map
htmlwidgets::saveWidget(map, file=ExportMap, selfcontained = TRUE)
################################################################################
################################################################################
################################################################################
# # Function to extract the .las file from the zip archive
# extract_las_from_zip <- function(FlightPath) {
#   # Define the path to the zip file and output directory
#   ZipLoc <- paste0(FlightPath, "1_TerraResults") %>% list.files(pattern = ".zip", full.names = TRUE)
#   Zipout <- paste0(FlightPath, "4_RawOutput")
#   LasName <- paste0(basename(FlightPath),".las")
#   # Ensure the output directory exists
#   if (!dir.exists(Zipout)) {
#     dir.create(Zipout, recursive = TRUE)
#   }
#   # List contents of the zip file
#   zip_contents <- utils::unzip(ZipLoc, list = TRUE)
#   # Find the .las file path within the zip archive
#   las_file_path <- zip_contents$Name[grepl("lidars/terra_las/.*\\.las$", zip_contents$Name)]
#   if (length(las_file_path) == 0) {
#     stop("No .las file found in the specified directory within the zip file.")
#   }
#   # Extract the .las file
#   utils::unzip(ZipLoc, files = las_file_path, exdir = Zipout)
#   # Construct the full path to the extracted file
#   extracted_file_path <- file.path(Zipout, las_file_path)
#   # Remove the folder structure from the extracted file path
#   new_file_path <- file.path(Zipout, LasName)
#   file.rename(extracted_file_path, new_file_path)
#   # Clean up any empty directories created during the extraction
#   dir_to_clean <- dirname(extracted_file_path[1])
#   while (dir_to_clean != Zipout) {
#     unlink(dir_to_clean, recursive = TRUE)
#     dir_to_clean <- dirname(dir_to_clean)
#   }
#   # Print and return the path to the renamed .las file
#   message <- paste("Extracted .las file is located at:", new_file_path)
#   print(message)
#   return(new_file_path)
# }
# # Unzip Tool
# #FlightBase <- "/home/antonio/UASNAS/UniversityForest_AJCGPHD/1_UAVData/20230208_UniWaldML/0_Flights/" 
# #FlightBase <- "C:/Users/lup42bs/Documents/Projects/Forests/testing/UniversityForest_AJCGPHD/1_UAVData/20230208_North3_DJIM300L1/0_Flights/"
# #FlightBase <- "C:/Users/lup42bs/Documents/Projects/Forests/110624_Uniwald/0_Flights/"
# #FlightBase <- "D:/1_Projects/Uniwald/11062024_uniwald/0_Flights/" # WS1
# FlightBase <- "F:/ForestSinglePlots/20230824_BayWaldAOI2/0_Flights/"
# 
# 
# # Define a list of FlightPaths
# FlightPaths <- c(
#   #"20230208_South1_DJIM300L1",
#   "20230208_South2_DJIM300L1",
#   "20230208_South3_DJIM300L1")
# FlightPaths <- paste0(FlightBase,FlightPaths,"/")
# # Apply the extract_las_from_zip function to each FlightPath
# #purrr::map(FlightPaths, extract_las_from_zip) #LP: commented out
# purrr::map(FlightPath, extract_las_from_zip)   #LP: script work when "FlightPath" instead of "FlightPaths"
# ################################################################################
