library(dplyr)
library(lubridate)
library(openxlsx)
library(zoo)
library(purrr)
library(jsonlite)
library(svDialogs)  # For GUI input dialogs


# Function to securely load credentials from JSON
load_credentials <- function(credentials_file = "credentials.json") {
  fromJSON(credentials_file)
}
# Load all credentials
CREDENTIALS <- load_credentials()
# Access AQS credentials separately
SIGNIN_AQS <- CREDENTIALS$AQS
# Extract Envista credentials
SIGNIN_ENVISTA <- CREDENTIALS$Envista
# Extract Oregon API base URL (if needed)
baseurl <- CREDENTIALS$OregonAPI$baseurl

# set path & source files
# e.g., the path on my local system root_path <- "C:/Users/nkhosra/Oregon/DEQ - Air Data Team - OzoneDB/test_DB_PM2.5_summer_2024/"
#ask user to define the "root_path" based on the sharepoint address saved in the local system
root_path <- "C:/Users/nkhosra/Oregon/DEQ - Air Data Team - OzoneDB/test_DB_PM2.5_summer_2024/"

# Supporting tools: Load all R scripts from the specified directory
file.sources <- list.files('./src/r/data_processing/', pattern = "\\.R$", full.names = TRUE)

# Source the files only if they exist
sapply(file.sources, function(f) {
  if (file.exists(f)) source(f)
})

# needs edits after reorganizing folders/structure
source('./src/r/data_ingestion/get_envista_data.R') # get envista data
source('./src/r/data_ingestion/get_aqs_data.R')     # get aqs data
source('./src/r/data_ingestion/load_links.R')       # create cross tables
source('./src/r/utils/support_func.R')       # create cross tables

#***********************************************************************************************************************
# start working with metadata
##make a table of Envista meta data
# (iii) pre-loading envista 
aqm_monitors_Envista <- get_envita_stations() %>%
  build_envista_monitor_list() %>% flat_lat_long()

aqm_monitors_Envista <- aqm_monitors_Envista %>%
  mutate(across(where(is.list), ~ sapply(., unlist)))

aqm_monitors_Envista <- aqm_monitors_Envista %>%
  mutate(across(where(is.list), ~ sapply(., toString)))

# Update alias_type without altering monitor_type
aqm_monitors_Envista <- aqm_monitors_Envista %>%
  mutate(alias_type = if_else(
    monitor_type %in% c("sensor a pm2.5est", "sensor b pm2.5est"),
    "pm2.5 est sensor",
    alias_type
  ))

# # export Envista metadata 
# will uncomment for midnight to midnight run of the script
# write.csv(as.data.frame(aqm_monitors_Envista), 
#           file = paste0(root_path, "./SUpplemental_Data/aqm_monitors_Envista.csv", row.names = FALSE))


#extract name of all parameters and assign it to param_list
aqm_monitors_Envista$webname <- aqm_monitors_Envista$alias_type
missing_name <- is.na(aqm_monitors_Envista$webname)
aqm_monitors_Envista$webname[missing_name] <- aqm_monitors_Envista$monitor_type[missing_name]
aqm_monitors_Envista$webname <- aqm_monitors_Envista$webname %>% tolower() 
aqm_monitors_Envista$alias_type <- aqm_monitors_Envista$alias_type %>% tolower()

list <- aqm_monitors_Envista %>% select(alias_type) %>% unique()
# Filter rows where `alias_type` is NOT missing
# Create a list of parameters that will be used for API calls to retrieve data
param_list <- list[!is.na(list$alias_type),]
remove(list)

# Define categorized parameter lists
criteria_pollutants <- c(
  "nitric oxide", "nitrogen dioxide", "oxides of nitrogen", "carbon monoxide", "ozone",
  "pm2.5 estimate", "pm2.5l_bam1022", "pm2.5 est sensor", "nephelometer", "sulfur dioxide", "pm10(s)"
)

meteorological_data <- c(
  "wind direction", "wind speed", "ambient temperature", "solar radiation", "barometric pressure"
)

# Create a named list to store the categorized parameters
categorize_parameters <- function(param_list) {
  list(
    "Criteria Pollutants" = param_list[param_list %in% criteria_pollutants],
    "Meteorological Data" = param_list[param_list %in% meteorological_data]
  )
}

# Apply the function
categorized_params <- categorize_parameters(param_list)

# get information like address for sites using an excel sheet maintains updated manually in the local system
#this step might need maintenance if any changes happen in AQM
cross_tables <- loadXlink('./data/reference/')

# I edited the excel sheet Anthony mainatined to be able to merge this meta data with RAW measurements correctly
aqm_monitors_localsys <- cross_tables$aqm_sites # this is siteXepaid_crosstable.csv

# End of working with metadata
#***********************************************************************************************************************



#***********************************************************************************************************************
# Start API requests to retrieve HOURLY data from AQS and Envista
#make a table of Envista meta data
get_user_input <- function() {
  # Ask the user if they want a yearly or monthly export
  export_type <- dlg_list(c("Yearly", "Monthly"), title = "Select Export Type")$res
  
  if (export_type == "Yearly") {
    # Ask user for a year (from 2000 to the current year)
    current_year <- as.numeric(format(Sys.Date(), "%Y"))
    year_options <- as.character(seq(2000, current_year, by = 1))
    
    selected_year <- dlg_list(year_options, title = "Select Year")$res
    
    # Set the date range to the full year
    from_date <- as.Date(paste0(selected_year, "-01-01"))
    to_date <- as.Date(paste0(selected_year, "-12-31"))
    
  } else if (export_type == "Monthly") {
    # Ask for custom start and end dates
    from_date <- as.Date(dlg_input("Enter start date (YYYY-MM-DD):")$res, "%Y-%m-%d")
    to_date <- as.Date(dlg_input("Enter end date (YYYY-MM-DD):")$res, "%Y-%m-%d")
    
    # Validate date input
    if (is.na(from_date) | is.na(to_date)) {
      stop("Invalid date format. Please enter dates in YYYY-MM-DD format.")
    }
  } else {
    stop("No valid selection made.")
  }
  
  # ✅ Ask user if they want to select parameters from each category
  select_criteria <- dlg_message("Do you want to select Criteria Pollutants?", type = "yesno")$res
  select_meteorology <- dlg_message("Do you want to select Meteorological Data?", type = "yesno")$res
  
  # Initialize empty parameter lists
  selected_criteria_pollutants <- character(0)
  selected_meteorological_data <- character(0)
  
  # If user chooses to select criteria pollutants
  if (select_criteria == "yes") {
    selected_criteria_pollutants <- dlg_list(criteria_pollutants, multiple = TRUE, title = "Select Criteria Pollutants")$res
  }
  
  # If user chooses to select meteorological data
  if (select_meteorology == "yes") {
    selected_meteorological_data <- dlg_list(meteorological_data, multiple = TRUE, title = "Select Meteorological Data")$res
  }
  
  # Ensure at least one parameter is selected
  if (length(selected_criteria_pollutants) == 0 && length(selected_meteorological_data) == 0) {
    stop("You must select at least one parameter from either category.")
  }
  
  # Convert dates to year and month for checking HMS availability
  months_selected <- seq(from_date, to_date, by = "month") %>% month()
  selected_year <- format(from_date, "%Y")  # Extract year
  
  # ✅ Check if HMS data is needed (Only if Criteria Pollutants are selected & Date range includes June-October)
  if (length(selected_criteria_pollutants) > 0 && any(months_selected %in% 6:10)) {
    message("📌 User selected Criteria Pollutants & a date range that includes June to October. Retrieving HMS data...")
    
    # Try to load the HMS data if it exists
    hms_file_path <- paste0(root_path, "Supplemental_Data/HMS_daily_", selected_year, ".xlsx")
    
    if (file.exists(hms_file_path)) {
      HMS_list <- read.xlsx(hms_file_path, colNames = TRUE, detectDates = TRUE)
      message("✅ HMS data loaded from file: ", hms_file_path)
    } else {
      message("⚠️ No existing HMS file found. Generating HMS data...")
      HMS_list <- add_HMS_levels_daily(daily_pm25)
      
      # Save the generated HMS data
      write.xlsx(HMS_list, file = hms_file_path, rowNames = FALSE)
      message("✅ HMS data successfully saved to: ", hms_file_path)
    }
  } else {
    message("❌ HMS data is not required (No Criteria Pollutants selected OR Dates outside June-October).")
    HMS_list <- NULL
  }
  
  return(list(
    from_date = from_date, 
    to_date = to_date, 
    criteria_pollutants = selected_criteria_pollutants, 
    meteorological_data = selected_meteorological_data, 
    HMS_list = HMS_list
  ))
}


# Get user input via dialog boxes
# The user can choose more than one parameter
# User can request data for a full year, a specific month, or multiple months
user_input <- get_user_input()

# Modify the file names based on the selection
# Inform the user about the current date range
cat("Using date range:", user_input$from_date, "to", user_input$to_date, "\n")

# Initialize dat_out outside the loop to store all results
dat_out <- list(data = NULL, meta = NULL)  # Empty tibbles to avoid NULL issues

for (type in param_list) {
  if (type %in% user_input$parameters) {
    
    print(paste("Processing:", type))
    
    # Extract relevant monitor data
    sub_meta <- aqm_monitors_Envista %>%
      filter(alias_type == type) %>%
      distinct(shortName, .keep_all = TRUE)
    
    # Request data
    dat_request <- deq_dat(site = sub_meta$shortName, 
                           poll_name = type, 
                           from_date = user_input$from_date, 
                           to_date = user_input$to_date,
                           monitoring_list = sub_meta)
    
    # Compile data
    foc_data <- dat_request %>% compile_allXdat()
    
    # Append valid data only
    if (!is.null(foc_data$data) && nrow(foc_data$data) > 0) {
      dat_out$data <- bind_rows(dat_out$data, foc_data$data)
      print(paste("Added", nrow(foc_data$data), "rows to data"))
    } else {
      print("No data found for this type.")
    }
    
    # Append valid metadata only
    if (!is.null(foc_data$meta) && nrow(foc_data$meta) > 0) {
      dat_out$meta <- bind_rows(dat_out$meta, foc_data$meta)
      print(paste("Added", nrow(foc_data$meta), "rows to meta"))
    } else {
      print("No metadata found for this type.")
    }
  }
}

hourly_pm25 <- dat_out$data

# the user can get log info and asscoiated metadata for the call in dat_out$meta and dat_out$info
remove(dat_out)
# End of API requests to retrieve HOURLY data from AQS and Envista
#***********************************************************************************************************************



#***********************************************************************************************************************
# quick clean for bad data and datetime out of the request period
# I had qualifer 53 and since it was not labled in the "envista_api_qualifier_code.csv", it's simple_qual_best was 
# "character(0)" or NA or empty strings, I replaced it with "ok", ask Peter & Anthony
hourly_pm25$simple_qual_best[hourly_pm25$simple_qual_best == "character(0)" ] <- "ok"

#code reviewer needs to spend more time on the quick_clean function since it removes rows from the RAW DB.
# it works like a quick QA on the raw DB
hourly_pm25 <- quick_clean (hourly_pm25)

# Remove the minimum datetime rows as the script pulls a few hours from the previous day of the start date.
hourly_pm25$datetime <- as.POSIXct(hourly_pm25$datetime, format = "%Y-%m-%d %H:%M:%S")
min_date <- min (hourly_pm25$datetime)
hourly_pm25 <- hourly_pm25[hourly_pm25$datetime != min_date, ]


# flag 1hr data for wildfires (first approach is using moving avg over 3hr of pm2.5 concentration, needs to be velicated
# for sensors and neph with Anthony & Peter)

hourly_pm25$date     <- as.Date(hourly_pm25$datetime, format = "%Y-%m-%d", tz = 'Etc/GMT+8')
hourly_pm25$hour     <- hour(hourly_pm25$datetime)
#***********************************************************************************************************************
#flag hourly DB for wildfire imapcts
#****** step 1
# Assuming 'hourly_pm25' contains the 'pm25' column, calculate 3-hour moving average
hourly_pm25 <- hourly_pm25 %>%
  arrange(site, parameter_best, poc_best, date, hour) %>%  # Ensure correct time ordering
  group_by(site, parameter_best, poc_best) %>%
  mutate(pm25_3hr_avg = rollapplyr(sample_measurement_best, width = 3, FUN = mean, fill = NA, partial = TRUE)) %>%
  ungroup()


#****** step 2
hourly_pm25$fflag <- ifelse(hourly_pm25$pm25_3hr_avg > 15, 1, 0)

# DONE!

# Standardize PM2.5 concentrations to two decimal places for reporting consistency (Reference to 40 CFR)
hourly_pm25$pm25_3hr_avg <- trunc(hourly_pm25$pm25_3hr_avg*10, 2)/10

hourly_pm25 <- hourly_pm25 %>% select(-date, -hour)  # Removes 'pm2.5' and 'temperature'


# good function but since it replaces NA with -9999 before group_by() functions, it returns NA in daily pm2.5 
# hourly_pm25 <- hourly_pm25 %>%
#   mutate(poc_best = ifelse(poc_best == -9999, NA, poc_best))  # Replace -9999 with NA


# Before exporting, convert datetime to a character format to not miss "00:00":
hourly_pm25 <- hourly_pm25 %>%
  mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S"))

# Save the raw database as-is, without metadata expansion
# export both as csv and xlsx to not missing 00:00 in csv. 
# write.csv (hourly_pm25, file = paste0(root_path,"DB/Hourly/first_DB_PM2.5_hourlyRAW.csv"), row.names = FALSE)
write.xlsx(hourly_pm25, file = paste0(root_path,"DB/Hourly/", selected_year,"first_DB_PM2.5_hourlyRAW.xlsx"), rowNames = FALSE)

#export raw hourly pm2.5
#***********************************************************************************************************************

# to contibue the user can read the raw hourly data
# Step 1: Read the CSV file of raw hourly pm2.5 
# hourly_pm25 <- read.xlsx(paste0(root_path, "DB/Hourly/first_DB_PM2.5_hourlyRAW.xlsx"),
#                          colNames = TRUE,
#                          detectDates = TRUE)


# # Step 2: Convert the 'datetime' column to POSIXct
# hourly_pm25$datetime <- as.POSIXct(hourly_pm25$datetime, format = "%Y-%m-%d %H:%M:%S")

# Expanding the database to include additional metadata
hourly_pm25 <- add_time_intervals (hourly_pm25)


colnames(aqm_monitors_Envista)[colnames(aqm_monitors_Envista) == 'latitude']  <- 'latitude_envista'
colnames(aqm_monitors_Envista)[colnames(aqm_monitors_Envista) == 'longitude'] <- 'longitude_envista'

colnames(aqm_monitors_Envista)[colnames(aqm_monitors_Envista) == 'shortName']   <- 'site'
colnames(aqm_monitors_localsys)[colnames(aqm_monitors_localsys) == 'shortName'] <- 'site'

hourly_pm25 <- merge(hourly_pm25, 
               aqm_monitors_localsys[, c('StreetAddress', 'longName', 'site')], 
               by = 'site', 
               all.x = TRUE)  # Keeps all rows from hourly_pm25


hourly_pm25 <- left_join(hourly_pm25, 
                  aqm_monitors_Envista [, c('regions', 'latitude_envista', 'longitude_envista', 'census classifier', 
                                        'StationTarget', 'city', 'County', 'name', 'stationsTag', 'site')] %>% 
                                        distinct(site, .keep_all = TRUE), by = "site")


# Convert all column names to lowercase
colnames(hourly_pm25) <- tolower(colnames(hourly_pm25))


# *************************** Neda's Comment:
# This section of the code should be discussed in the code review group meeting to get the reviewers' opinions.

# to remove duplicate timestamp for each parameter & monitor type (probably there are duplicate because of poc_best)
# did not run this line
# dat_out$data %>% distinct(site, datetime, parameter_best, .keep_all=TRUE) ????????????????
# Before exporting, convert datetime to a character format:


# Before exporting, convert datetime to a character format to not miss "00:00":
hourly_pm25 <- hourly_pm25 %>%
  mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S"))

# write.csv(hourly_pm25, file = paste0(root_path, "DB/hourly/first_DB_PM2.5_hourly.csv"), row.names = FALSE)
write.xlsx(hourly_pm25,paste0(root_path, "DB/Hourly/", selected_year, "first_DB_PM2.5_hourly.xlsx"), rownames=FALSE)
#export hourly pm25 with expanded metadata
#end of generating hourly DB for PM2.5
#***********************************************************************************************************************


#***********************************************************************************************************************
# The user can create the hourly database by following the steps above or import an existing one.
# Step 1: Read the CSV file of raw hourly pm2.5 
hourly_pm25 <- read.xlsx(paste0(root_path, "DB/Hourly/", selected_year,"first_DB_PM2.5_hourlyRAW.xlsx"),
                         colNames = TRUE,
                         detectDates = TRUE)


# Step 2: Convert the 'datetime' column to POSIXct
hourly_pm25$datetime <- as.POSIXct(hourly_pm25$datetime, format = "%Y-%m-%d %H:%M:%S")

# Step 3: expand DB to include more metadata
hourly_pm25 <- add_time_intervals (hourly_pm25)

#create the daily DB from hourly DB
daily_pm25 <- calc_24hr_pm (hourly_pm25) 
daily_pm25 <- calc_aqi (daily_pm25)

daily_pm25$poc_best <- as.numeric(daily_pm25$poc_best)

# OR
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>   Daily PM2.5 DB    <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#read raw data
# Step 1: Read the CSV file of raw daily pm2.5 
# daily_pm25 <- read.csv(paste0(root_path, "DB/daily_DB/first_DB_PM2.5_dailyRAW.csv"))
daily_pm25 <- read.xlsx(paste0(root_path, ".DB/Daily/", selected_year,"first_DB_PM2.5_dailyRAW.xlsx"))

# Step 2: Convert the 'datetime' column to POSIXct
daily_pm25$date <- as.POSIXct(daily_pm25$date, format = "%Y-%m-%d")

daily_pm25 <- add_time_intervals_daily(daily_pm25)




# # >>>>>>>>>>>>>>>>>>>>>>>   the user can 
# HMS_list <- add_HMS_levels_daily (daily_pm25)
# 
# head(HMS_list)
# 
# save_HMS_to_excel <- function(HMS_list, root_path) {
#   # Ensure the directory exists
#   dir_path <- paste0(root_path, "/Supplemental_Data/")
#   
#   if (!dir.exists(dir_path)) {
#     dir.create(dir_path, recursive = TRUE)  # Create directory if missing
#     message("📂 Created missing directory: ", dir_path)
#   }
#   
#   # Define file path
#   file_path <- paste0(dir_path, "HMS_daily.xlsx")
#   
#   # Save the data to an Excel file
#   write.xlsx(HMS_list, file = file_path, rowNames = FALSE)
#   
#   message("✅ HMS_list successfully saved to: ", file_path)
# }


# OR

# to expedite the review process, HMS list for summer 2024 was generated & exported to this directory:
# C:\Users\nkhosra\Oregon\DEQ - Air Data Team - OzoneDB\test_DB_PM2.5_summer_2024\Supplemental_Data\

#the user can read HMS data for the wf season 2024 from the DataRepo located on the Air Data Team sharepoint
# HMS_list <- read.xlsx(paste0(root_path,"Supplemental_Data/HMS_daily_", selected_year,".xlsx"),
#                       colNames = TRUE,
#                       detectDates = TRUE)


#then HMS list should be merged with daily pm2.5
# Remove time zone offset (-08) and convert to Date
str(daily_pm25$date)
daily_pm25 <- daily_pm25 %>%
  mutate(date = as.Date(str_remove(date, " -08"), format = "%Y-%m-%d"))

# Since HMS_list$date is "2024-08-16 UTC", we remove UTC and convert it:
str(HMS_list$date)
HMS_list <- HMS_list %>%
  mutate(date = as.Date(str_remove(date, " UTC"), format = "%Y-%m-%d"))


# Left join while preserving duplicates in daily_pm25
daily_pm25 <- left_join (daily_pm25, 
                HMS_list, 
                by = c("site", "date"), 
                relationship = "many-to-many") 



daily_pm25 <- daily_pm25 %>%
  mutate(fflag = ifelse(pm25 >= 15, 1, 0))


# Save the raw database as-is, without metadata expansion
write.xlsx(daily_pm25, paste0(root_path, "DB/Daily/", selected_year,"first_DB_PM2.5_dailyRAW.xlsx"), rowNames = FALSE)
# End of creating of RAW daily pm2.5 
#***********************************************************************************************************************


#***********************************************************************************************************************
# Expanding the daily database to include additional metadata
daily_pm25 <- add_time_intervals_daily (daily_pm25)

colnames(aqm_monitors_Envista)[colnames(aqm_monitors_Envista) == 'latitude']  <- 'latitude_envista'
colnames(aqm_monitors_Envista)[colnames(aqm_monitors_Envista) == 'longitude'] <- 'longitude_envista'

colnames(aqm_monitors_Envista)[colnames(aqm_monitors_Envista) == 'shortName']   <- 'site'
colnames(aqm_monitors_localsys)[colnames(aqm_monitors_localsys) == 'shortName'] <- 'site'

daily_pm25 <- merge(daily_pm25, 
              aqm_monitors_localsys[, c('StreetAddress', 'longName', 'site')], 
              by = 'site', 
              all.x = TRUE)  # Keeps all rows from hourly_pm25

daily_pm25 <- left_join(daily_pm25, 
                             aqm_monitors_Envista [, c('regions', 'latitude_envista', 'longitude_envista', 'census classifier', 
                                                   'StationTarget', 'city', 'County', 'name', 'stationsTag', 'site')] %>% 
                                                    distinct(site, .keep_all = TRUE), by = "site")

# Convert all column names to lowercase
colnames(daily_pm25) <- tolower(colnames(daily_pm25))


write.xlsx(daily_pm25, file = paste0(root_path, "DB/Daily/", selected_year,"first_DB_PM2.5_daily.xlsx"), rowNames = FALSE)
#***********************************************************************************************************************
#End of exporting the daily DB
