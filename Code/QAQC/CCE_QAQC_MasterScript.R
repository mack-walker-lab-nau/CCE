########################################
# Master QAQC Script for Cleaning CCE Data Sheets
# Author: Zach Madsen
# Contact: zachmadsen90@gmail.com
########################################

# IMPORTANT:
# This script is designed to be run interactively using source().
# Do NOT run by selecting all lines and pressing Ctrl+Enter.
# Instead, in RStudio:
#   1. Open this script in the script editor.
#   2. Click the "Source" button in the upper right corner of the editor.
#   3. Or run: source("Master_QAQC_Script.R", echo = FALSE)
########################################
library(dplyr)

# Helper function to type out messages like someone speaking
type_out <- function(text, delay = 0.03, pause = 0.5) {
  delay <- as.numeric(delay)
  pause <- as.numeric(pause)
  if(is.na(delay) || delay < 0) delay <- 0.01
  if(is.na(pause) || pause < 0) pause <- 0.2
  
  for(i in seq_len(nchar(text))) {
    cat(substr(text, i, i))
    flush.console()
    Sys.sleep(delay)
  }
  cat("\n")
  Sys.sleep(pause)
}

# Safety check: prevent running line-by-line
if (interactive() && sys.nframe() == 0) {
  type_out("===================================================", 0.01, 0.2)
  type_out("Hey! It looks like you're trying to run this script line-by-line.", 0.03, 0.5)
  type_out("This script is designed to be run using the 'Source' button in RStudio (upper right corner of the editor).", 0.03, 0.5)
  type_out("Running line-by-line (Ctrl+Enter) will break interactive prompts.", 0.03, 0.5)
  type_out("Please click 'Source' or use: source('Master_QAQC_Script.R', echo = FALSE).", 0.03, 0.5)
  type_out("The script will stop now to prevent errors.", 0.03, 0.5)
  type_out("===================================================", 0.01, 0.2)
  stop("Script stopped. Rerun using 'Source'.\n")
}

#----------------------------------------
# Opening messages
type_out("=========================================", 0.01, 0.2)
type_out("Welcome to the QAQC clean-up script!", 0.03, 0.5)
type_out("This script is fully automated and will guide you through checking and correcting your data.", 0.03, 0.5)
type_out("Before we start, make sure you have this script open using CCE.Rproj in the CCE folder.", 0.03, 0.5)

# Prompt for Rproj
rproj <- readline(prompt = "Did you open this script using CCE.Rproj in the CCE folder? y/n: ")
if(tolower(rproj) != "y") {
  type_out("\nPlease open the script using CCE.Rproj in the CCE folder or read the readme.txt file for instructions.", 0.03, 0.5)
  stop("Script ended. Rerun once ready.\n")
}

type_out("Great! Now, make sure you have created a folder for this year's data in QAQC_Notes and Processed_Data inside Field_Data.", 0.03, 0.5)

# Prompt for folders
fold <- readline(prompt = "Did you create the necessary folders? y/n: ")
if(tolower(fold) != "y") {
  type_out("\nPlease create the necessary folders or read the readme.txt file for instructions.", 0.03, 0.5)
  stop("Script ended. Rerun once ready.\n")
}

type_out("Perfect, let's get started!\n", 0.03, 0.5)
type_out("=========================================", 0.01, 0.2)

#----------------------------------------
#----------------------------------------
# Setup
data_year <- readline("Enter the year the data was collected: ")

# Paths
pathway     <- file.path("Field_Data", "Raw_CSVs", data_year)
outpathway  <- file.path("Field_Data", "Processed_Data", data_year)
qaqcpathway <- file.path("Field_Data", "QAQC_Notes", data_year)

# Expected dataset keywords
expected_keys <- c("spp", "browns", "cwd", "ground", "plant", 
                   "resprout", "seedling", "shrub", "site", "soils", 
                   "combustion", "disk", "dbh")

# Get all CSV files in folder
all_files <- list.files(pathway, pattern = "\\.csv$", full.names = TRUE)
all_files_short <- basename(all_files)

# Container for skipped files
skipped_files <- c()

# Loop through each expected keyword
for (key in expected_keys) {
  match_idx <- grep(key, all_files_short, ignore.case = TRUE)
  
  if (length(match_idx) == 0) {
    # File missing
    response <- readline(paste0("No file found for '", key, 
                                "'. Skip this dataset? (y/n): "))
    if (tolower(response) == "y") {
      skipped_files <- c(skipped_files, key)
      next
    } else {
      stop("Missing file for key: ", key, ". Please fix before continuing.")
    }
  } else if (length(match_idx) > 1) {
    stop("Multiple files found for key: ", key, ". Please fix filenames.")
  }
  
  # Load data into variable named after key
  file_path <- all_files[match_idx]
  df <- read.csv(file_path)
  assign(key, df, envir = .GlobalEnv)
  
  # Also save the short file name into a "_name" variable
  assign(paste0(key, "_name"), all_files_short[match_idx], envir = .GlobalEnv)
}

type_out(paste0("Found all files for ", data_year, ".\n"))
type_out("Beginning QAQC processing...")

#----------------------------------------
# Outlier detection and interactive QAQC function
#----------------------------------------
# Outlier detection function
outliers <- function(datasheet, filename) {
  type_out(paste0("Now processing ", filename, "\n"))
  type_out("Searching for outliers")
  
  numeric_columns <- names(datasheet)[sapply(datasheet, is.numeric)]
  
  log_df <- data.frame(
    row = integer(),
    column = character(),
    value = numeric(),
    issue = character(),
    direction = character(),
    action_taken = character(),
    note_value = character(),
    note_action = character(),
    stringsAsFactors = FALSE
  )
  
  # Prompt for type of outliers
  repeat {
    flag_type <- readline(prompt = "Flag [e]xtreme only or [m]ild and extreme outliers? (e/m): ")
    flag_type <- tolower(flag_type)
    if(flag_type %in% c("e", "m")) break
    cat("Invalid input. Enter 'e' for extreme only or 'm' for mild and extreme.\n")
  }
  
  for(col in numeric_columns){
    vec <- datasheet[[col]]
    zero_count <- sum(vec == 0, na.rm = TRUE)
    zero_flag_threshold <- 0.05 * length(vec)
    
    nonzero_vec <- vec[vec != 0 & !is.na(vec)]
    if(length(nonzero_vec) < 2) next
    
    q1 <- quantile(nonzero_vec, 0.25)
    q3 <- quantile(nonzero_vec, 0.75)
    iqr <- q3 - q1
    mild_lower <- q1 - 1.5 * iqr
    mild_upper <- q3 + 1.5 * iqr
    extreme_lower <- q1 - 3 * iqr
    extreme_upper <- q3 + 3 * iqr
    mean_val <- mean(nonzero_vec)
    sd_val <- sd(nonzero_vec)
    
    for(i in seq_along(vec)){
      val <- vec[i]
      if(is.na(val)) next
      
      issue <- ""
      direction <- ""
      note_value <- ""
      note_action <- ""
      threshold_msg <- ""
      
      # Determine issue
      if(val == 0 && zero_count < zero_flag_threshold){
        issue <- "Zero value (rare)"
        threshold_msg <- "Value is zero; few zeros in dataset"
      } else if(val == 0) {
        next
      } else if(val < extreme_lower){
        issue <- "Extreme low outlier"
        direction <- "low"
        threshold_msg <- paste0("Value <", round(extreme_lower,3), " (Q1 - 3×IQR)")
        if(flag_type == "m") threshold_msg <- paste0(threshold_msg, "; Mild lower bound = ", round(mild_lower,3))
      } else if(flag_type == "m" && val < mild_lower){
        issue <- "Mild low outlier"
        direction <- "low"
        threshold_msg <- paste0("Value <", round(mild_lower,3), " (Q1 - 1.5×IQR)")
      } else if(val > extreme_upper){
        issue <- "Extreme high outlier"
        direction <- "high"
        threshold_msg <- paste0("Value >", round(extreme_upper,3), " (Q3 + 3×IQR)")
        if(flag_type == "m") threshold_msg <- paste0(threshold_msg, "; Mild upper bound = ", round(mild_upper,3))
      } else if(flag_type == "m" && val > mild_upper){
        issue <- "Mild high outlier"
        direction <- "high"
        threshold_msg <- paste0("Value >", round(mild_upper,3), " (Q3 + 1.5×IQR)")
      } else {
        # safe value, log as valid
        log_df <- rbind(log_df, data.frame(
          row = i,
          column = col,
          value = val,
          issue = "Valid",
          direction = "",
          action_taken = "keep",
          note_value = "",
          note_action = "",
          stringsAsFactors = FALSE
        ))
        next
      }
      
      # Interactive prompt
      cat("\n-----------------------------------------\n")
      cat("Site:", datasheet$site[i], "\nColumn:", col, "\n", "\nRow:", i, "\nValue:", val, "\n")
      cat("Issue:", issue, "\nDirection:", direction, "\n")
      cat("Stats - Mean:", round(mean_val,3), "\nSD:", round(sd_val,3),
          "\nQ1:", round(q1,3), "\nQ3:", round(q3,3), "\n")
      cat("Thresholds:", threshold_msg, "\n")
      
      repeat{
        action <- readline(prompt = "Action? [k]eep / [c]orrect / [r]emove: ")
        action <- tolower(action)
        if(action %in% c("k","c","r")) break
        cat("Invalid input. Enter k, c, or r.\n")
      }
      
      if(action == "r") vec[i] <- NA
      if(action == "c"){
        repeat{
          newval <- readline(prompt = "Enter corrected value: ")
          if(!is.na(as.numeric(newval))) break
          cat("Invalid input. Enter a numeric value.\n")
        }
        vec[i] <- as.numeric(newval)
      }
      note_action <- paste0("Action taken: ", action)
      note_value <- readline(prompt = "Add a note about this value? (press Enter to skip): ")
      if(note_value == "") note_value <- "No note"
      
      log_df <- rbind(log_df, data.frame(
        row = i,
        column = col,
        value = vec[i],
        issue = issue,
        direction = direction,
        action_taken = action,
        note_value = note_value,
        note_action = note_action,
        stringsAsFactors = FALSE
      ))
    }
    
    datasheet[[col]] <- vec
  }
  
  # Save CSVs
  write.csv(datasheet, file.path(outpathway, filename), row.names = FALSE)
  write.csv(log_df, file.path(qaqcpathway, paste0("QAQC-Outlier_", filename)), row.names = FALSE)
  
  return(list(data = datasheet, log = log_df))
}




#----------------------------------------
# Ground and plant cover interactive QAQC
ground_plant_checks <- function(datasheet, filename, column) {
  type_out(paste0("Now processing ", filename, "\n"))
  type_out("Searching for cover values greater than 100,\nand less than one values not equal to 0.1 in cover data")
  
  
  # Log dataframe
  log_df <- data.frame(
    row = integer(),
    value = numeric(),
    issue = character(),
    action_taken = character(),
    notes = character(),
    stringsAsFactors = FALSE
  )
  
  # Loop over all columns
  vec <- datasheet[[column]]
  
  for(i in seq_along(vec)){
    val <- vec[i]
    if(is.na(val)) next
    
    # Flag conditions
    issue <- NULL
    if(val > 100){
      issue <- "Value > 100"
    } else if(val < 1 & val != 0.1){
      issue <- "Value < 1 (not 0.1)"
    } else if(val < 0){
      issue <- "Negative value"
    }
    
    if(!is.null(issue)){
      cat("\n-----------------------------------------\n")
      cat("\nRow:", i, "\nValue:", val, "\nIssue Detected:", issue, "\n")
      
      # Interactive action
      repeat{
        action <- readline(prompt = "Action? [k]eep / [c]orrect / [r]emove: ")
        action <- tolower(action)
        if(action %in% c("k","c","r")) break
        cat("Invalid input. Enter k, c, or r.\n")
      }
      
      if(action == "r"){
        vec[i] <- NA
      } else if(action == "c"){
        repeat{
          newval <- readline(prompt = "Enter corrected value: ")
          if(!is.na(as.numeric(newval))) break
          cat("Invalid input. Enter a numeric value.\n")
        }
        vec[i] <- as.numeric(newval)
      }
      
      note <- readline(prompt = "Add a note? (press Enter to skip): ")
      
      
      # Add to log
      log_df <- rbind(log_df, data.frame(
        site = datasheet$site[i],
        row = i,
        value = val,
        issue = issue,
        action_taken = action,
        notes = note,
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Update column in datasheet
  datasheet[[column]] <- vec
  
  # Save processed data and log
  write.csv(datasheet, file.path(outpathway, filename), row.names = FALSE)
  write.csv(log_df, file.path(qaqcpathway, paste0("QAQC-Outlier_", filename)), row.names = FALSE)
  return(list(data = datasheet, log = log_df))
}

#----------------------------------------
# Lat/Long function (row by row interactive)
latlong <- function(site, filename) {
  type_out(paste0("Now processing ", filename, "\n"))
  type_out("Searching for outliers")
  
  log.frame <- data.frame(
    Site = character(),
    Row = integer(),
    Switch.Lat.0 = character(),
    Switch.Lat.30 = character(),
    Switch.Long.0 = character(),
    Switch.Long.30 = character(),
    Long.West.0 = character(),
    Long.West.30 = character(),
    Elevation = character(),
    Elevation.Note = character(),
    Slope = character(),
    Slope.Note = character(),
    Aspect = character(),
    Aspect.Note = character(),
    Transect.Orientation = character(),
    Transect.Note = character(),
    Moisture.Class = character(),
    stringsAsFactors = FALSE
  )
  
  valid_moisture <- c("subhygric","mesic-subhygric","mesic","mesic-subxeric","subxeric","xeric")
  
  for(i in seq_len(nrow(site))){
    
    row_notes <- character()  # Initialize notes for this row
    
    # Ensure latitude is positive
    site$latitude_at_0m[i] <- abs(site$latitude_at_0m[i])
    site$latitude_at_30m[i] <- abs(site$latitude_at_30m[i])
    
    # --- Latitude / Longitude checks ---
    if(is.na(site$latitude_at_0m[i]) || site$latitude_at_0m[i] == 0){
      type_out(paste0("Site: ", site$site[i], " latitude at 0m is missing or is 0.\nYou can find the value in Fulcrum in your photo or a previous year file."))
      lat.0.prompt <- readline("Do you want to change in this script? [y]/[n]: ")
      repeat{
        if(tolower(lat.0.prompt) %in% c("y", "n")) break else cat("Please enter y or n.")
      }
      if(tolower(lat.0.prompt) == "y"){
        lat.val <- readline("Please enter the new value: ")
        repeat{
          if(!is.na(as.numeric(lat.val))) break else cat("Please enter a numeric value.") 
        }
        switch.0 <- paste0("Latitude at 0m changed from ", site$latitude_at_0m[i], " to", lat.val)
        site$latitude_at_0m[i] <- as.numeric(lat.val)
      } else{
        type_out("Please change later in excel.")
        switch.0 <- "Latitude at 0m missing or zero, change manually in excel."
      }
    } else if(floor(log10(abs(site$latitude_at_0m[i]))) + 1 == 3){
      temp <- site$latitude_at_0m[i]
      site$latitude_at_0m[i] <- site$longitude_at_0m[i]
      site$longitude_at_0m[i] <- temp
      switch.0 <- "Lat/Long at 0m swapped"
    } else { switch.0 <- "Data was good" }
    
    if(is.na(site$latitude_at_30m[i]) || site$latitude_at_30m[i] == 0){
      type_out(paste0("Site: ", site$site[i], " latitude at 30m is missing or is 0.\nYou can find the value in Fulcrum in your photo or a previous year file."))
      lat.30.prompt <- readline("Do you want to change in this script? [y]/[n]: ")
      repeat{
        if(tolower(lat.30.prompt) %in% c("y", "n")) break else cat("Please enter y or n.")
      }
      if(tolower(lat.30.prompt) == "y"){
        lat.val <- readline("Please enter the new value: ")
        repeat{
          if(!is.na(as.numeric(lat.val))) break else cat("Please enter a numeric value.") 
        }
        switch.30 <- paste0("Latitude at 30 m changed from ", site$latitude_at_30m[i], " to", lat.val)
        site$latitude_at_30m[i] <- as.numeric(lat.val)
      } else{
        type_out("Please change later in excel.")
        switch.30 <- "Latitude at 30m missing or zero, change manually in excel."
      }
    } else if(floor(log10(abs(site$latitude_at_30m[i]))) + 1 == 3){
      temp <- site$latitude_at_30m[i]
      site$latitude_at_30m[i] <- site$longitude_at_30m[i]
      site$longitude_at_30m[i] <- temp
      switch.30 <- "Lat/Long at 30m swapped"
    } else { switch.30 <- "Data was good" }
    
    # --- Longitude at 0m / 30m checks ---
    if(is.na(site$longitude_at_0m[i]) || site$longitude_at_0m[i] == 0){
      type_out(paste0("Site: ", site$site[i], " longitude at 0m is missing or is 0.\nYou can find the value in Fulcrum in your photo or a previous year file."))
      lon.0.prompt <- readline("Do you want to change in this script? [y]/[n]: ")
      repeat{
        if(tolower(lon.0.prompt) %in% c("y", "n")) break else cat("Please enter y or n.")
      }
      if(tolower(lon.0.prompt) == "y"){
        lon.val <- readline("Please enter the new value: ")
        repeat{
          if(!is.na(as.numeric(lon.val))) break else cat("Please enter a numeric value.") 
        }
        switch.lon.0 <- paste0("Longitude at 0m changed from ", site$longitude_at_0m[i], " to ", lon.val)
        site$longitude_at_0m[i] <- as.numeric(lon.val)
      } else{
        type_out("Please change later in excel.")
        switch.lon.0 <- "Longitude at 0m missing or zero, change manually in excel."
      }
    } else { switch.lon.0 <- "Data was good" }
    
    if(is.na(site$longitude_at_30m[i]) || site$longitude_at_30m[i] == 0){
      type_out(paste0("Site: ", site$site[i], " longitude at 30m is missing or is 0.\nYou can find the value in Fulcrum in your photo or a previous year file."))
      lon.30.prompt <- readline("Do you want to change in this script? [y]/[n]: ")
      repeat{
        if(tolower(lon.30.prompt) %in% c("y", "n")) break else cat("Please enter y or n.")
      }
      if(tolower(lon.30.prompt) == "y"){
        lon.val <- readline("Please enter the new value: ")
        repeat{
          if(!is.na(as.numeric(lon.val))) break else cat("Please enter a numeric value.") 
        }
        switch.lon.30 <- paste0("Longitude at 30m changed from ", site$longitude_at_30m[i], " to ", lon.val)
        site$longitude_at_30m[i] <- as.numeric(lon.val)
      } else{
        type_out("Please change later in excel.")
        switch.lon.30 <- "Longitude at 30m missing or zero, change manually in excel."
      }
    } else { switch.lon.30 <- "Data was good" }
    
    # --- Ensure western hemisphere longitudes ---
    if(!is.na(site$longitude_at_0m[i]) && site$longitude_at_0m[i] > 0) {
      site$longitude_at_0m[i] <- site$longitude_at_0m[i] * -1
      switch.0.west <- "Longitude 0 switched to western"
    } else if(site$longitude_at_0m[i] == 0){
      switch.0.west <- "Longitude at 0 m is equal to 0"
    } else{
      switch.0.west <- "Data was good"  
    }
    
    if(!is.na(site$longitude_at_30m[i]) && site$longitude_at_30m[i] > 0) {
      site$longitude_at_30m[i] <- site$longitude_at_30m[i] * -1
      switch.30.west <- "Longitude 30 switched to western"
    } else if(site$longitude_at_30m[i] == 0){
      switch.30.west <- "Longitude at 30 m is equal to 0"
    } else{
      switch.30.west <- "Data was good"  
    }
    
    # --- Elevation check ---
    if(is.na(site$elevation[i]) || site$elevation[i] == 0 || site$elevation[i] < 0  || (floor(log10(abs(site$elevation[i]))) + 1 > 4)) {
      repeat {
        ans <- readline(paste0("Attention:", site$site[i], " Elevation value (", site$elevation[i], ") seems unusual. Is it correct? [y/n]: "))
        if(tolower(ans) %in% c("y","n")) break
      }
      if(tolower(ans) == "y") {
        elev.check <- paste("Elevation confirmed:", site$elevation[i])
        elev.note <- 'Elevation is correct.'
      } else {
        repeat {
          change_ans <- readline("Manually enter new value? [y/n]: ")
          if(tolower(change_ans) %in% c("y","n")) break
        }
        if(tolower(change_ans) == "y") {
          repeat {
            newval <- readline("Enter corrected Elevation value: ")
            if(!is.na(as.numeric(newval))) break
          }
          elev.check <- paste("Elevation updated from", site$elevation[i], "to", newval)
          site$elevation[i] <- as.numeric(newval)
          elev.note <- readline("Enter a note? ([enter] to skip): ")
        } else {
          elev.check <- paste("Elevation left unchanged:", site$elevation[i])
          elev.note <- readline("Enter a note? ([enter] to skip): ")
        }
      }
    } else {
      elev.check <- "Data is valid"
      elev.note <- ""
    }
    
    # --- Slope check ---
    if(is.na(site$slope[i]) || site$slope[i] < 0 || floor(log10(abs(site$slope[i]))) + 1 > 2){
      repeat {
        if(is.na(site$slope[i])){
          type_out("The slope is missing. You can use the previous slope from previous year data.\n")
          ans <- "n"
        } else{
          ans <- readline(paste0("Attention:", site$site[i], " Slope value (", site$slope[i], ") seems incorrect. Is it correct? [y/n]: "))
          if(tolower(ans) %in% c("y","n")) break
        }
      }
      if(tolower(ans) == "y") {
        slope.check <- paste("Slope confirmed:", site$slope[i])
        slope.note <- readline("Enter a note? ([enter] to skip): ")
      } else {
        repeat {
          change_ans <- readline("Manually enter new value? [y/n]: ")
          if(tolower(change_ans) %in% c("y","n")) break else cat("Please press y or n.")
        }
        if(tolower(change_ans) == "y") {
          repeat {
            newval <- readline("Enter corrected Slope value: ")
            if(!is.na(as.numeric(newval))) break else cat("Invalid input. Try again.")
          }
          slope.check <- paste("Slope updated from", site$slope[i], "to", newval)
          site$slope[i] <- as.numeric(newval)
          slope.note <- readline("Enter a note? ([enter] to skip): ")
        } else {
          slope.check <- paste("Slope left unchanged:", site$slope[i])
          slope.note <- readline("Enter a note? ([enter] to skip): ")
        }
      }
    } else { slope.check <- "Data is valid"; slope.note <- "" }
    
    # --- Aspect check ---
    if(is.na(site$aspect[i]) || floor(log10(abs(site$aspect[i]))) + 1 > 3){
      repeat {
        if(is.na(site$aspect[i])){
          type_out("The aspect is missing. You can use the previous aspect from previous year data.\n")
          ans <- "n"
        } else{
          ans <- readline(paste0("Attention:", site$site[i], " Aspect value (", site$aspect[i], ") seems unusually high. Correct? [y/n]: "))
          if(tolower(ans) %in% c("y","n")) break else cat("Please press y or n.")
        }
      }
      if(tolower(ans) == "y") {
        aspect.check <- paste("Aspect confirmed:", site$aspect[i])
        aspect.note <- readline("Enter a note? ([enter] to skip): ")
      } else {
        repeat {
          change_ans <- readline("Manually enter new value? [y/n]: ")
          if(tolower(change_ans) %in% c("y","n")) break else cat("Please press y or n.")
        }
        if(tolower(change_ans) == "y") {
          repeat {
            newval <- readline("Enter corrected Aspect value: ")
            if(!is.na(as.numeric(newval))) break else cat("Invalid input. Try again.")
          }
          aspect.check <- paste("Aspect updated from", site$aspect[i], "to", newval)
          site$aspect[i] <- as.numeric(newval)
          aspect.note <- readline("Enter a note? ([enter] to skip): ")
        } else {
          aspect.check <- paste("Aspect left unchanged:", site$aspect[i])
          aspect.note <- readline("Enter a note? ([enter] to skip): ")
        }
      }
    } else { aspect.check <- "Data is valid"; aspect.note <- "" }
    
    # --- Transect Orientation check ---
    if(is.na(site$transect_orientation[i]) || site$transect_orientation[i] < 0 || floor(log10(abs(site$transect_orientation[i]))) + 1 > 3){
      repeat {
        if(is.na(site$transect_orientation[i])){
          type_out("The transect orientation is missing. You can use the previous aspect from previous year data.\n")
          ans <- "n" 
        }else{
          ans <- readline(paste0("Attention:", site$site[i], " Transect orientation value (", site$transect_orientation[i], ") seems incorrect. Is it correct? [y/n]: "))
          if(tolower(ans) %in% c("y","n")) break else cat("Please press y or n.")
        }
      }
      if(tolower(ans) == "y") {
        transect.check <- paste("Transect orientation confirmed:", site$transect_orientation[i])
        transect.note <- readline("Enter a note? ([enter] to skip): ")
      } else {
        repeat {
          change_ans <- readline("Manually enter new value? [y/n]: ")
          if(tolower(change_ans) %in% c("y","n")) break else cat("Please press y or n.")
        }
        if(tolower(change_ans) == "y") {
          repeat {
            newval <- readline("Enter corrected Transect orientation value: ")
            if(!is.na(as.numeric(newval))) break else cat("Invalid input. Try again.")
          }
          transect.check <- paste("Transect orientation updated from", site$transect_orientation[i], "to", newval)
          site$transect_orientation[i] <- as.numeric(newval)
          transect.note <- readline("Enter a note? ([enter] to skip): ")
        } else {
          transect.check <- paste("Transect orientation left unchanged:", site$transect_orientation[i])
          transect.note <- readline("Enter a note? ([enter] to skip): ")
        }
      }
    } else { transect.check <- "Data is valid"; transect.note <- "" }
    
    # --- Moisture class check with fuzzy matching ---
    if(!(site$moisture_class[i] %in% valid_moisture)){
      approx_match <- agrep(site$moisture_class[i], valid_moisture, max.distance = 0.2, value = TRUE)
      if(length(approx_match) == 1){
        site$moisture_class[i] <- approx_match
        moisture.check <- paste("Moisture class auto-corrected to:", approx_match)
      } else {
        repeat {
          newval <- readline(paste0(
            "Attention: Moisture class value ('", site$moisture_class[i], "') invalid.\n",
            "Enter a valid class (options: ", paste(valid_moisture, collapse = ", "), "): "
          ))
          if(newval %in% valid_moisture) break
          cat("Invalid entry. Must be one of: ", paste(valid_moisture, collapse = ", "), "\n")
        }
        site$moisture_class[i] <- newval
        moisture.check <- paste("Moisture class updated to:", newval)
      }
    } else {
      moisture.check <- "Data is valid"
    }
    
    # --- Log results ---
    log.frame <- rbind(
      log.frame,
      data.frame(
        Site = site$site[i],
        Row = i,
        Switch.Lat.0 = switch.0,
        Switch.Lat.30 = switch.30,
        Switch.Long.0 = switch.lon.0,
        Switch.Long.30 = switch.lon.30,
        Long.West.0 = switch.0.west,
        Long.West.30 = switch.30.west,
        Elevation = elev.check,
        Elevation.Note = elev.note,
        Slope = slope.check,
        Slope.Note = slope.note,
        Aspect = aspect.check,
        Aspect.Note = aspect.note,
        Transect.Orientation = transect.check,
        Transect.Note = transect.note,
        Moisture.Class = moisture.check,
        stringsAsFactors = FALSE
      )
    )
  }
  
  write.csv(site, file.path(outpathway, filename), row.names = FALSE)
  write.csv(log.frame, file.path(qaqcpathway, paste0("QAQC-Outlier_", filename)), row.names = FALSE)
  return(list(data = site, log = log.frame))
}

#----------------------------------------
# Site name check and correction function
site_name_check <- function(site_data, filename){
  type_out(paste0("Now processing ", filename, "\n"))
  type_out("Correcting site name abbreviations...")
  
  site_data$fire_scar[site_data$fire_scar == "Aggie"] <- "Aggie Creek"
  
  log.frame <- data.frame(
    Row = integer(),
    Original_Site = character(),
    Corrected_Site = character(),
    stringsAsFactors = FALSE
  )
  
  for(i in seq_len(nrow(site_data))){
    firescar <- site_data$fire_scar[i]
    original <- site_data$site[i]
    
    if(grepl(" ", firescar)){
      parts <- strsplit(firescar, " ")[[1]]
      correct <- paste0(substr(parts[1],1,2), substr(parts[2],1,2))
    } else {
      correct <- substr(firescar,1,4)
    }
    
    # Keep trailing number if present
    num_part <- gsub("[^0-9]", "", original)
    newname <- paste0(correct, num_part)
    if(newname == original){
      next
    }
    site_data$site[i] <- newname
    
    log.frame <- rbind(log.frame, data.frame(
      Row = i,
      Original_Site = original,
      Corrected_Site = site_data$site[i]
    ))
  }
  
  write.csv(log.frame, file.path(qaqcpathway, paste0("QAQC-SiteName_", filename)), row.names = FALSE)
  return(list(data = site_data, log = log.frame))
  
}

#----------------------------------------

# Helper function to print summary and pause
process_summary <- function(filename, log_df) {
  type_out("\n-----------------------------------------\n", 0.01, 0.2)
  type_out(paste0("File processed: ", filename, "\n"))
  if(nrow(log_df) == 0) {
    readline("There were no errors. Press enter to continue: ")
  } else repeat {
    view_log <- readline(prompt = "Do you want to see the full summary log? y/n: ")
    if(tolower(view_log) %in% c("y","n")) break
    cat("Invalid input. Enter 'y' or 'n'.\n")
  }
  if(tolower(view_log) == "y") {
    print(log_df)
    readline(prompt = "Press Enter to continue to the next file...")
  } else
    type_out("Moving on...")
}

#----------------------------
# File processing: Site name fixes only
#----------------------------
#Additional Species
if(!"spp" %in% skipped_files){
  spp_n.fix <- site_name_check(spp, filename = spp_name)
  spp <- spp_n.fix$data
  spp_n.log <- spp_n.fix$log
  process_summary(spp_name, spp_n.log)
  write.csv(spp, file.path(outpathway, spp_name), row.names = FALSE)
}
# Browns transect
if(!"browns" %in% skipped_files){
  browns_n.fix <- site_name_check(browns, filename = browns_name)
  browns <- browns_n.fix$data
  browns_n.log <- browns_n.fix$log
  process_summary(browns_name, browns_n.log)
}

# CWD
if(!"cwd" %in% skipped_files){
  cwd_n.fix <- site_name_check(cwd, filename = cwd_name)
  cwd <- cwd_n.fix$data
  cwd_n.log <- cwd_n.fix$log
  process_summary(cwd_name, cwd_n.log)
}

# Ground cover
if(!"ground" %in% skipped_files){
  ground_n.fix <- site_name_check(ground, filename = ground_name)
  ground <- ground_n.fix$data
  ground_n.log <- ground_n.fix$log
  process_summary(ground_name, ground_n.log)
}

# Plant cover
if(!"plant" %in% skipped_files){
  plant_n.fix <- site_name_check(plant, filename = plant_name)
  plant <- plant_n.fix$data
  plant_n.log <- plant_n.fix$log
  process_summary(plant_name, plant_n.log)
}

# Resprouts
if(!"resprout" %in% skipped_files){
  resprout_n.fix <- site_name_check(resprout, filename = resprout_name)
  resprout <- resprout_n.fix$data
  resprout_n.log <- resprout_n.fix$log
  process_summary(resprout_name, resprout_n.log)
}

# Seedlings
if(!"seedling" %in% skipped_files){
  seedling_n.fix <- site_name_check(seedling, filename = seedling_name)
  seedling <- seedling_n.fix$data
  seedling_n.log <- seedling_n.fix$log
  process_summary(seedling_name, seedling_n.log)
}

# Shrub regeneration
if(!"shrub" %in% skipped_files){
  shrub_n.fix <- site_name_check(shrub, filename = shrub_name)
  shrub <- shrub_n.fix$data
  shrub_n.log <- shrub_n.fix$log
  process_summary(shrub_name, shrub_n.log)
}

# Site level data
if(!"site" %in% skipped_files){
  site_n.fix <- site_name_check(site, filename = site_name)
  site <- site_n.fix$data
  site_n.log <- site_n.fix$log
  process_summary(site_name, site_n.log)
}

# Soils
if(!"soils" %in% skipped_files){
  soils_n.fix <- site_name_check(soils, filename = soils_name)
  soils <- soils_n.fix$data
  soils_n.log <- soils_n.fix$log
  process_summary(soils_name, soils_n.log)
}

# Tree combustion
if(!"combustion" %in% skipped_files){
  combustion_n.fix <- site_name_check(combustion, filename = combustion_name)
  combustion <- combustion_n.fix$data
  combustion_n.log <- combustion_n.fix$log
  process_summary(combustion_name, combustion_n.log)
}

# Tree disks
if(!"disk" %in% skipped_files){
  disk_n.fix <- site_name_check(disk, filename = disk_name)
  disk <- disk_n.fix$data
  disk_n.log <- disk_n.fix$log
  process_summary(disk_name, disk_n.log)
}

# Tree above DBH
if(!"dbh" %in% skipped_files){
  dbh_n.fix <- site_name_check(dbh, filename = dbh_name)
  dbh <- dbh_n.fix$data
  dbh_n.log <- dbh_n.fix$log
  process_summary(dbh_name, dbh_n.log)
  write.csv(dbh, file.path(outpathway, dbh_name), row.names = FALSE)
}

type_out("All site names have been corrected.\nMoving on to outliers.")
#----------------------------
# File processing: Outlier checks
#----------------------------

# Site data (latlong function)
if(!"site" %in% skipped_files){
  site_o.fix <- latlong(site, filename = site_name)
  site <- site_o.fix$data
  site_o.log <- site_o.fix$log
  process_summary(site_name, site_o.log)
}

# Browns transect
if(!"browns" %in% skipped_files){
  browns_o.fix <- outliers(browns, filename = browns_name)
  browns <- browns_o.fix$data
  browns_o.log <- browns_o.fix$log
  process_summary(browns_name, browns_o.log)
}

# CWD
if(!"cwd" %in% skipped_files){
  cwd_o.fix <- outliers(cwd, filename = cwd_name)
  cwd <- cwd_o.fix$data
  cwd_o.log <- cwd_o.fix$log
  process_summary(cwd_name, cwd_o.log)
}

# Ground cover
if(!"ground" %in% skipped_files){
  ground_o.fix <- ground_plant_checks(datasheet = ground, filename = ground_name, column = "ground_percent_cover")
  ground <- ground_o.fix$data
  ground_o.log <- ground_o.fix$log
  process_summary(ground_name, ground_o.log)
}

# Plant cover
if(!"plant" %in% skipped_files){
  plant_o.fix <- ground_plant_checks(datasheet = plant, filename = plant_name, column = "plant_percent_cover")
  plant <- plant_o.fix$data
  plant_o.log <- plant_o.fix$log
  process_summary(plant_name, plant_o.log)
}

# Resprouts
if(!"resprout" %in% skipped_files){
  resprout_o.fix <- outliers(resprout, filename = resprout_name)
  resprout <- resprout_o.fix$data
  resprout_o.log <- resprout_o.fix$log
  process_summary(resprout_name, resprout_o.log)
}

# Seedlings
if(!"seedling" %in% skipped_files){
  seedling_o.fix <- outliers(seedling, filename = seedling_name)
  seedling <- seedling_o.fix$data
  seedling_o.log <- seedling_o.fix$log
  process_summary(seedling_name, seedling_o.log)
}

# Shrub regeneration
if(!"shrub" %in% skipped_files){
  shrub_o.fix <- outliers(shrub, filename = shrub_name)
  shrub <- shrub_o.fix$data
  shrub_o.log <- shrub_o.fix$log
  process_summary(shrub_name, shrub_o.log)
}

# Soils
if(!"soils" %in% skipped_files){
  soils_o.fix <- outliers(soils, filename = soils_name)
  soils <- soils_o.fix$data
  soils_o.log <- soils_o.fix$log
  process_summary(soils_name, soils_o.log)
}

# Tree combustion
if(!"combustion" %in% skipped_files){
  combustion_o.fix <- outliers(combustion, filename = combustion_name)
  combustion <- combustion_o.fix$data
  combustion_o.log <- combustion_o.fix$log
  process_summary(combustion_name, combustion_o.log)
}

# Tree disks
if(!"disk" %in% skipped_files){
  disk_o.fix <- outliers(disk, filename = disk_name)
  disk <- disk_o.fix$data
  disk_o.log <- disk_o.fix$log
  process_summary(disk_name, disk_o.log)
}


# Closing messages
type_out("=========================================", 0.01, 0.2)
type_out("All files have been successfully processed.", 0.03, 0.5)
type_out("Processed data sheets are saved in the Processed_Data folder, and QAQC logs are saved in the QAQC folder.", 0.03, 0.5)
type_out("Now, check the site data in GIS to confirm that the coordinates match the expected locations.", 0.03, 0.5)
type_out("If you notice any lat/long errors, open Fulcrum, locate the photo (0m or 30m), update the CSV manually, and log it in a CSV in the QAQC_Notes folder.", 0.03, 0.5)
type_out("Once everything is verified, you can proceed to the next script to combine the data sheets.", 0.03, 0.5)
type_out("Thank you for completing QAQC for this year's dataset!", 0.03, 0.5)
type_out("=========================================", 0.01, 0.2)