#'####################################################################################################################
# FUNCTIONS to generate layer products for German Forests project
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
      handle_user_prompt(file_path)  # Recursive call to handle invalid input
    }
  } else {
    return(TRUE)  # File does not exist, proceed to write new file
  }
}






#'####################################################################################################################
# IMPORT FILES ####


### Function to check if .tif, .prj and .tfw files are existing in folder and load the raster file

import_dem_l1 <- function(FlightPath, FlightName) {
  # Construct the full paths
  base_path <- paste0(FlightPath, "4_RawOutput/", FlightName)
  tif_file <- paste0(base_path, ".tif")
  prj_file <- paste0(base_path, ".prj")
  tfw_file <- paste0(base_path, ".tfw")
  
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
  
  # Load the .tif file
  raster_data <- rast(tif_file)
  
  # Return the raster data
  return(raster_data)
}




# Function to check CRS and extent validity
check_raster_validity <- function(raster_path) {
  # Load the raster
  raster_data <- rast(raster_path)
  
  # Check if CRS is valid
  crs_valid <- !is.na(crs(raster_data))
  crs_interpretable <- tryCatch({
    crs(raster_data)
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  # Get the extent
  raster_extent <- ext(raster_data)
  
  # Check if extent values are finite
  extent_valid <- all(is.finite(c(raster_extent$xmin, raster_extent$xmax, raster_extent$ymin, raster_extent$ymax)))
  
  # Print results
  if (crs_valid && crs_interpretable) {
    cat("CRS is valid and interpretable: ")
    cat(crs(raster_data, proj = TRUE), "\n" )
  } else {
    cat("CRS is either invalid or not interpretable.\n")
  }
  
  if (extent_valid) {
    cat("Extent details:\n")
    cat("xmin:", raster_extent$xmin, "\n")
    cat("xmax:", raster_extent$xmax, "\n")
    cat("ymin:", raster_extent$ymin, "\n")
    cat("ymax:", raster_extent$ymax, "\n")
  } else {
    cat("Extent is invalid.\n")
  }
  
  # Return the results as a list
  return(list(crs_valid = crs_valid, crs_interpretable = crs_interpretable, extent_valid = extent_valid))
}



# # Function to transform CRS of aoi to CRS of raster (<< operator assigns values to global variables within the function; however, it's generally better practice to use list returns to avoid modifying the global environment)
# transform_sf_to_raster_crs <- function(sf_object, raster_path) {
#   # Load the raster
#   spatialRaster <- rast(raster_path)
#   
#   # Extract EPSG code from raster CRS
#   r_epsg <<- as.numeric(crs(spatialRaster, describe = TRUE)$code)
#   
#   # Extract the extent of the raster
#   r_extent <<- ext(spatialRaster)
#   
#   # Transform the sf object to the raster CRS
#   transformed_sf <<- st_transform(sf_object, r_epsg)
# }






get_resolution_info <- function(raster_path) {
  # Load the raster
  spatialRaster <- rast(raster_path)
  
  # Get the resolution of the raster
  r_res <- res(spatialRaster)
  
  # Extract the first part of the resolution vector
  first_part <- r_res[1]
  
  # Check if the raster resolution is floating number and optionally and replace '.' with '-'
  if (grepl("\\.", as.character(first_part))) {
    # Replace '.' with '-'
    res <- gsub("\\.", "-", as.character(first_part))
  } else {
    # Keep the first part as it is
    res <- as.character(first_part)
  }
  
  # Return both variables
  return(list(raster_res = r_res, res_exp = res))
}






#'####################################################################################################################
# EXPORT FILES ####

# Function to handle user prompt and export raster
userPromt_rasterExport <- function(raster, output_tif) {
  # Check if file exists
  if (file_exists(output_tif)) {
    # Prompt user for input
    user_input <- readline(prompt = "Output TIFF already exists. Do you want to overwrite it? (yes/no): ")
    
    # Process user input
    if (tolower(user_input) == "yes") {
      # Overwrite existing file
      writeRaster(raster, filename = output_tif, overwrite = TRUE)
      cat("Raster exported (overwritten):", output_tif, "\n")
    } else if (tolower(user_input) == "no") {
      # Export with a new filename
      new_filename <- paste0(tools::file_path_sans_ext(output_tif), "_new.tif")
      writeRaster(raster, filename = new_filename)
      cat("Raster exported (new file):", new_filename, "\n")
    } else {
      cat("Invalid input. Please enter 'yes' or 'no'.\n")
      handle_user_prompt_and_export(raster, output_tif)  # Recursive call to handle invalid input
    }
  } else {
    # File does not exist, proceed to export
    writeRaster(raster, filename = output_tif)
    cat("Raster exported:", output_tif, "\n")
  }
}

# # Check if file exists and decide: overwrite or add new raster?  (-> UNSOLVED ERROR)
# user_decision_export_raster <- userPromt_rasterExport(r_l1_clip_aoi, ExportRaster)
# # # UNSOLVED ISSUE: 
# # terra::writeRaster(r_l1_clip_aoi, filename = ExportRaster, overwrite = TRUE)
# # # -> Error [writeRaster] cannot overwrite existing file


# Function to export raster as TIFF
export_raster <- function(raster, output_tif) {
  if (file_exists(output_tif)) {
    # File exists, generate new filename with "_new.tif"
    new_filename <- paste0(tools::file_path_sans_ext(output_tif), "_new.tif")
    writeRaster(raster, filename = new_filename)
    cat("Raster exported (new file with _new.tif):", new_filename, "\n")
  } else {
    # File does not exist, proceed to export
    writeRaster(raster, filename = output_tif)
    cat("Raster exported:", output_tif, "\n")
  }
}

















#'####################################################################################################################
# ATTEMPTS (not finished) ####
#
# ### Automatically determine flight path based on custom Mission & System variable.
#
# ### Select custom parameter (from table)
# mission <- ""
# system <- ""
#
#'======================================================================================================================  
# # -> PART RIGHT NOW NOT WORKING for Saarland, Luebeck and Passau. For all others it does.
# # -> MAYBE ADJUST ANOTHER TIME; THEN DON'T FORGET TO REFER TO `mission_aoi` VARIABLE IN SUBSEQUENT CODE
#
# ### Select sub-site (if existing)
# 
# # Function to assign subsite to mission if existing
# assign_mission_subsite <- function(mission) {
#   if (mission == "20230823_Luebeck") {
#     aoi_specification <- readline(prompt = "Specify AOI for Lübeck mission ( 1 for AOI1 in the South, 2 for AOI2 in the North): ")
#     
#     # Validate the input and update the mission accordingly
#     if (aoi_specification == "1") {
#       mission_aoi <- "20230823_LuebeckSouth"
#       print("AOI1 in the South was selected.")
#     } else if (aoi_specification == "2") {
#       mission_aoi <- "20230823_LuebeckNorth"
#       print("AOI2 in the North was selected.")
#     } else {
#       stop("Invalid AOI specification")
#     }
#     
#   } else if (mission == "20230615_Passau") {
#     aoi_specification <- readline(prompt = "Specify AOI for Passau mission ( 1 for AOI1 in the North, 2 for AOI2 in the South): ")
#     
#     # Validate the input and update the mission accordingly
#     if (aoi_specification == "1") {
#       mission_aoi <- "20230615_PassauAOI1_TF80"
#       print("AOI1 in the North was selected, with TF 80 (terrain following at 80m height).")
#     } else if (aoi_specification == "2") {
#       mission_aoi <- "20230615_PassauAOI2_TF90"
#       print("AOI2 in the South  was selected, with TF 90 (terrain following at 90m height).")
#     } else {
#       stop("Invalid AOI specification")
#     }
#   
#   } else if (mission == "20230921_Saarland" && system == "WingtraAltum") {
#     aoi_specification <- readline(prompt = "Specify AOI for Saarland mission with WingtraAltum system ( 1 for AOI1 in the West, 2 for AOI2 in the Middle, 3 for AOI3 in the East): ")
#     
#     # Validate the input and update the mission accordingly
#     if (aoi_specification == "1") {
#       mission_aoi <- "20230921_SaarlandAOI1"
#       print("AOI1 in the West was selected.")
#     } else if (aoi_specification == "2") {
#       mission_aoi <- "20230921_SaarlandAOI2"
#       print("AOI2 in the Middle was selected.")
#     } else if (aoi_specification == "3") {
#       mission_aoi <- "20230921_SaarlandAOI3"
#       print("AOI3 in the East was selected.")
#     } else {
#       stop("Invalid AOI specification")
#     } 
#     
#   } else if (mission == "20230921_Saarland" && system != "WingtraAltum") {
#     aoi_specification <- readline(prompt = "Specify AOI for Saarland mission ( 1 for combined AOI1 & AOI2 in the West, 2 for AOI3 in the East): ")
#     
#     # Validate the input and update the mission accordingly
#     if (aoi_specification == "1") {
#       mission_aoi <- "20230921_SaarlandAOI1-2"
#       print("Combined AOI1 & AOI2 in the West was selected.")
#     } else if (aoi_specification == "2") {
#       mission_aoi <- "20230921_SaarlandAOI3"
#       print("AOI3 in the East was selected.")
#     } else {
#       stop("Invalid AOI specification")
#     } 
#     
#   } else {
#     mission_aoi <- mission  # Default assignment
#   }
#   
#   return(mission_aoi)
# }
# 
# # Example usage
# mission_aoi <- assign_mission_subsite(mission)
# print(mission_aoi)
#'======================================================================================================================  
# 
# ### Select required flight path
# 
# # Define the function
# select_flight_path <- function(paths, mission, system) {
#   # Check if mission is "yyy" or "xxx" for manual insertion
#   if (mission %in% c("20230921_Saarland", "20230615_Passau", "20230823_Luebeck")) {
#     FlightPath <- readline(prompt = paste("Specify required Flight path for", mission, "mission (use / and NO quotation marks): "))   #EXAMPLE: F:/ForestSinglePlots/20230823_Luebeck/0_Flights/20230823_LuebeckNorth_DJIM300L1
#     print(FlightPath)
#     return(FlightPath)
#   } else {
#     
#     # Iterate through each path
#     for (path in paths) {
#       # Split the path by "/"
#       split_path <- strsplit(path, "/")[[1]]
#       
#       # Get the last folder name
#       last_folder <- tail(split_path, n = 1)
#       
#       # Split the last folder name by "_"
#       split_folder <- strsplit(last_folder, "_")[[1]]
#       
#       # Check if the first two parts match the mission and the last part matches the system
#       if (length(split_folder) >= 3) {
#         # Construct the mission part from the first two parts
#         mission_part <- paste(split_folder[1], split_folder[2], sep = "_")
#         
#         # Check for match
#         if (mission_part == mission && split_folder[length(split_folder)] == system) {
#           return(path)
#           print(path)
#         }
#       } 
#     }
#     
#     # If no match is found, return NULL
#     stop("Execution stopped. Could not find flight path for selected mission and system.\n Check if required flight name contains date, aoi, and system. Alternatively check for spelling mistake in custom parameter.")
#   }
# }
# 
# # Call the function
# FlightPath <- select_flight_path(all_flight_folders, mission, system)
# 
# # Print the matching path
# print(FlightPath)










































#' 
#' 
#' 
#' #'####################################################################################################################
#' # DERIVE PATHS ####
#' 
#' ### List files
#' 
#' # List all folders in root folder
#' all_folders <- list.files(RootPath, full.names = TRUE, recursive = FALSE)
#' 
#' # Filter folders that match the pattern "yyyymmdd_name" 
#' pattern <- "^\\d{8}_.+"    # (\\d{8} matches exactly 8 digits; _ matches the underscore character; .+ matches one or more characters following the underscore)
#' mission_folders <- all_folders[grepl(pattern, basename(all_folders))]
#' 
#' # Check if the length of mission_folders is not equal to 13
#' if (length(mission_folders) != 13) {
#'   stop("Please check if all 13 overall flight missions are stored in root-folder and modify code if this number is outdated.")
#' } else {
#'   print("Correct number of mission folders found.")
#' }
#' 
#' # Print the mission folders
#' print(mission_folders)
#' #strsplit(mission_folders, "/")
#' 
#' # List all flight paths
#' all_flight_folders <- list.files(paste0(mission_folders, "/0_Flights/"), full.names = TRUE, recursive = FALSE)
#' print(all_flight_folders)
#' 
#' 
#' 
#' #'====================================================================================================================
#' # -> PART RIGHT NOW NOT WORKING; MAYBE ADJUST ANOTHER TIME; THEN DON'T FORGET TO REFER TO `mission_aoi` VARIABLE IN SUBSEQUENT CODE
#' # ### Select sub-site (if existing)
#' # 
#' # # Function to assign subsite to mission if existing
#' # assign_mission_subsite <- function(mission) {
#' #   if (mission == "20230823_Luebeck") {
#' #     aoi_specification <- readline(prompt = "Specify AOI for Lübeck mission ( 1 for AOI1 in the South, 2 for AOI2 in the North): ")
#' #     
#' #     # Validate the input and update the mission accordingly
#' #     if (aoi_specification == "1") {
#' #       mission_aoi <- "20230823_LuebeckSouth"
#' #       print("AOI1 in the South was selected.")
#' #     } else if (aoi_specification == "2") {
#' #       mission_aoi <- "20230823_LuebeckNorth"
#' #       print("AOI2 in the North was selected.")
#' #     } else {
#' #       stop("Invalid AOI specification")
#' #     }
#' #     
#' #   } else if (mission == "20230615_Passau") {
#' #     aoi_specification <- readline(prompt = "Specify AOI for Passau mission ( 1 for AOI1 in the North, 2 for AOI2 in the South): ")
#' #     
#' #     # Validate the input and update the mission accordingly
#' #     if (aoi_specification == "1") {
#' #       mission_aoi <- "20230615_PassauAOI1_TF80"
#' #       print("AOI1 in the North was selected, with TF 80 (terrain following at 80m height).")
#' #     } else if (aoi_specification == "2") {
#' #       mission_aoi <- "20230615_PassauAOI2_TF90"
#' #       print("AOI2 in the South  was selected, with TF 90 (terrain following at 90m height).")
#' #     } else {
#' #       stop("Invalid AOI specification")
#' #     }
#' #   
#' #   } else if (mission == "20230921_Saarland" && system == "WingtraAltum") {
#' #     aoi_specification <- readline(prompt = "Specify AOI for Saarland mission with WingtraAltum system ( 1 for AOI1 in the West, 2 for AOI2 in the Middle, 3 for AOI3 in the East): ")
#' #     
#' #     # Validate the input and update the mission accordingly
#' #     if (aoi_specification == "1") {
#' #       mission_aoi <- "20230921_SaarlandAOI1"
#' #       print("AOI1 in the West was selected.")
#' #     } else if (aoi_specification == "2") {
#' #       mission_aoi <- "20230921_SaarlandAOI2"
#' #       print("AOI2 in the Middle was selected.")
#' #     } else if (aoi_specification == "3") {
#' #       mission_aoi <- "20230921_SaarlandAOI3"
#' #       print("AOI3 in the East was selected.")
#' #     } else {
#' #       stop("Invalid AOI specification")
#' #     } 
#' #     
#' #   } else if (mission == "20230921_Saarland" && system != "WingtraAltum") {
#' #     aoi_specification <- readline(prompt = "Specify AOI for Saarland mission ( 1 for combined AOI1 & AOI2 in the West, 2 for AOI3 in the East): ")
#' #     
#' #     # Validate the input and update the mission accordingly
#' #     if (aoi_specification == "1") {
#' #       mission_aoi <- "20230921_SaarlandAOI1-2"
#' #       print("Combined AOI1 & AOI2 in the West was selected.")
#' #     } else if (aoi_specification == "2") {
#' #       mission_aoi <- "20230921_SaarlandAOI3"
#' #       print("AOI3 in the East was selected.")
#' #     } else {
#' #       stop("Invalid AOI specification")
#' #     } 
#' #     
#' #   } else {
#' #     mission_aoi <- mission  # Default assignment
#' #   }
#' #   
#' #   return(mission_aoi)
#' # }
#' # 
#' # # Example usage
#' # mission_aoi <- assign_mission_subsite(mission)
#' # print(mission_aoi)
#' #'====================================================================================================================
#' 
#' 
#' 
#' 
#' 
#' ### Select required flight path
#' 
#' # Define the function
#' select_flight_path <- function(paths, mission, system) {
#'   # Check if mission is "yyy" or "xxx" for manual insertion
#'   if (mission %in% c("20230921_Saarland", "20230615_Passau", "20230823_Luebeck")) {
#'     FlightPath <- readline(prompt = paste("Specify required Flight path for", mission, "mission (use / and NO quotation marks): "))   #EXAMPLE: F:/ForestSinglePlots/20230823_Luebeck/0_Flights/20230823_LuebeckNorth_DJIM300L1
#'     print(FlightPath)
#'     return(FlightPath)
#'   } else {
#'     
#'     # Iterate through each path
#'     for (path in paths) {
#'       # Split the path by "/"
#'       split_path <- strsplit(path, "/")[[1]]
#'       
#'       # Get the last folder name
#'       last_folder <- tail(split_path, n = 1)
#'       
#'       # Split the last folder name by "_"
#'       split_folder <- strsplit(last_folder, "_")[[1]]
#'       
#'       # Check if the first two parts match the mission and the last part matches the system
#'       if (length(split_folder) >= 3) {
#'         # Construct the mission part from the first two parts
#'         mission_part <- paste(split_folder[1], split_folder[2], sep = "_")
#'         
#'         # Check for match
#'         if (mission_part == mission && split_folder[length(split_folder)] == system) {
#'           return(path)
#'           print(path)
#'         }
#'       } 
#'     }
#'     
#'     # If no match is found, return NULL
#'     stop("Execution stopped. Could not find flight path for selected mission and system.\n Check if required flight name contains date, aoi, and system. Alternatively check for spelling mistake in custom parameter.")
#'   }
#' }
#' 
#' 
#' # Call the function
#' FlightPath <- select_flight_path(all_flight_folders, mission, system)
#' #FlightPath <- "F:/ForestSinglePlots/20230824_BayWaldAOI1/0_Flights/20230824_BayWaldAOI1_DJIM300L1/"
#' 
#' 
#' # Print the matching path
#' print(FlightPath)
