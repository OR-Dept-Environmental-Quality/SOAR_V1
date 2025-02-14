library(dplyr)
library(lubridate)
library(openxlsx)
library(zoo)
library(purrr)

source('./pass/pass.R') 

# supporting tools
file.sources = list.files('./tools_functions/', pattern="*.R")
sapply(paste0('./tools_functions/',file.sources),source)

# needs edits after reorganizing folders/structure
source('./getdat_func/get_envista_data.R') # get envista data
source('./getdat_func/get_aqs_data.R') # get envista data
source('./siteXcode/load_links.R')
source('./figures_tools.R') # get envista data
source('./DATA_ACCESS_envista.R') # get envista metadata

# my path to read PM2.5 DB from DataRepo located on Air Data Team sharepoint 
root_path <- "C:/Users/nkhosra/Oregon/DEQ - Air Data Team - OzoneDB/test_DB_PM2.5_summer_2024/"


#make a table of Envista meta data
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

# export Envista metadata to double check later
# write.csv(as.data.frame(aqm_monitors_Envista), file = "./output_code_review/aqm_monitors_Envista.csv", row.names = FALSE)


#extract name of all parameters and assign it to param_list
aqm_monitors_Envista$webname <- aqm_monitors_Envista$alias_type
missing_name <- is.na(aqm_monitors_Envista$webname)
aqm_monitors_Envista$webname[missing_name] <- aqm_monitors_Envista$monitor_type[missing_name]
aqm_monitors_Envista$webname <- aqm_monitors_Envista$webname %>% tolower() 
aqm_monitors_Envista$alias_type <- aqm_monitors_Envista$alias_type %>% tolower()


list <- aqm_monitors_Envista %>% select(alias_type) %>% unique()
param_list <- list[!is.na(list$alias_type),]
remove(list)


# get information like address for sites using an excel sheet maintains updated manually in the local system
#this step might need maintanace if any changes happen in AQM
cross_tables <- loadXlink('./')

# I edited the excel sheet Anthony mainatined to be able to merge this meta data with RAW measurements correctly
aqm_monitors_localsys <- cross_tables$aqm_sites # this is siteXepaid_crosstable.csv

#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>  end of creating meta data   <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# # Prompt user for input dates
# cat("Please provide the 'from_date' (format: YYYY/MM/DD): ")
# from_date <- readline()
# 
# cat("Please provide the 'to_date' (format: YYYY/MM/DD): ")
# to_date <- readline()
# 
# # Validate user inputs (optional)
# if (!grepl("^\\d{4}/\\d{2}/\\d{2}$", from_date) || !grepl("^\\d{4}/\\d{2}/\\d{2}$", to_date)) {
#   stop("Invalid date format. Please use 'YYYY/MM/DD'.")
# }
# 
# # Ensure from_date is earlier than to_date
# if (as.Date(from_date) > as.Date(to_date)) {
#   stop("'from_date' cannot be later than 'to_date'.")
# }


#OR

# Define the date range variables 
from_date <- '2024/06/01'
to_date   <- '2024/10/31'

# Inform the user about the current date range
cat("Using date range:", from_date, "to", to_date, "\n")


# Initialize dat_out outside the loop to store all results
dat_out <- list(data = NULL, meta = NULL)  # Empty tibbles to avoid NULL issues

for (type in param_list) {
  if (type %in% c("pm2.5l_bam1022", "pm2.5 estimate", "pm2.5 est sensor")) {
    
    print(paste("Processing:", type))
    
    # Extract relevant monitor data
    sub_meta <- aqm_monitors_Envista %>%
      filter(alias_type == type) %>%
      distinct(shortName, .keep_all = TRUE)
    
    # Request data
    dat_request <- deq_dat(site = sub_meta$shortName, 
                           poll_name = type, 
                           from_date = from_date, 
                           to_date = to_date,
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

remove(dat_out)



# flag 1hr data for wildfires (first approach is using moving avg over 3hr of pm2.5 concentration, needs to be velicated
# for sensors and neph with Anthony & Peter)

hourly_pm25$date     <- as.Date(hourly_pm25$datetime, format = "%Y-%m-%d", tz = 'Etc/GMT+8')
hourly_pm25$hour     <- hour(hourly_pm25$datetime)


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
write.csv (hourly_pm25, file = paste0(root_path,"DB/hourly_DB/first_DB_PM2.5_hourlyRAW.csv"), row.names = FALSE)
write.xlsx(hourly_pm25, file = paste0(root_path,"DB/hourly_DB/first_DB_PM2.5_hourlyRAW.xlsx"), rowNames = FALSE)


#***********************************************************************************************************************
# Step 1: Read the CSV file of raw hourly pm2.5 
hourly_pm25 <- read.csv(file = paste0(root_path, "DB/hourly_DB/first_DB_PM2.5_hourlyRAW.csv"))

# Step 2: Convert the 'datetime' column to POSIXct
hourly_pm25$datetime <- as.POSIXct(hourly_pm25$datetime, format = "%Y-%m-%d %H:%M:%S")

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


write.csv(hourly_pm25, file = paste0(root_path, "DB/hourly_DB/first_DB_PM2.5_hourly.csv"), row.names = FALSE)
write.xlsx(hourly_pm25,paste0(root_path, "DB/hourly_DB/first_DB_PM2.5_hourly.xlsx"), rownames=FALSE)

#************************************************************************************************************************
#end of generating hourly DB for PM2.5


# The user can create the hourly database by following the steps above or import an existing one.

# Step 1: Read the CSV file of raw hourly pm2.5 
hourly_pm25 <- read.csv(file = paste0(root_path, "DB/hourly_DB/first_DB_PM2.5_hourlyRAW.csv"))
hourly_pm25$datetime <- as.POSIXct(hourly_pm25$datetime, format = "%Y-%m-%d %H:%M:%S")

#start expand DB to include more metadata
hourly_pm25 <- add_time_intervals (hourly_pm25)



#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>   Daily PM2.5 DB    <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#read raw data
# Step 1: Read the CSV file of raw daily pm2.5 
# daily_pm25 <- read.csv(paste0(root_path, "DB/daily_DB/first_DB_PM2.5_dailyRAW.csv"))
daily_pm25 <- read.csv(paste0(root_path, "DB/daily_DB/first_DB_PM2.5_dailyRAW.csv"))


# Step 2: Convert the 'datetime' column to POSIXct
# daily_pm25$date <- as.POSIXct(daily_pm25$date, format = "%Y-%m-%d")

# OR


#create the daily DB from hourly DB
daily_pm25 <- calc_24hr_pm (hourly_pm25) 
daily_pm25 <- calc_aqi (daily_pm25)

daily_pm25$poc_best <- as.numeric(daily_pm25$poc_best)


# Remove time zone offset (-08) and convert to Date
str(daily_pm25$date)
daily_pm25 <- daily_pm25 %>%
  mutate(date = as.Date(str_remove(date, " -08"), format = "%Y-%m-%d"))



HMS_list <- read.xlsx(paste0(root_path, "supplemental_data/HMS_daily.xlsx"),
                      colNames = TRUE,
                      detectDates = TRUE)

# Since HMS_list$date is "2024-08-16 UTC", we remove UTC and convert it:
str(HMS_list$date)
HMS_list <- HMS_list %>%
  mutate(date = as.Date(str_remove(date, " UTC"), format = "%Y-%m-%d"))


# Left join while preserving duplicates in daily_pm25
daily_pm25 <- left_join (daily_pm25, 
                HMS_list, 
                by = c("site", "date"), 
                relationship = "many-to-many") 


# daily_pm25$smoke_level[daily_pm25$smoke_level == ""] <- NA 
daily_pm25 <- daily_pm25 %>%
  mutate(fflag = ifelse(pm25 >= 15 & (is.na(smoke_level) | smoke_level == ""), 1, 0))



# Remove all rows where date is before from_date
hourly_pm25 <- hourly_pm25 %>%
  filter(date >= as.Date('2024/06/01', format = "%Y/%m/%d"))

View(daily_pm25)

# Save the raw database as-is, without metadata expansion
write.csv(daily_pm25, file = paste0(root_path, "DB/daily_DB/first_DB_PM2.5_dailyRAW.csv"), row.names = FALSE)
write.xlsx(daily_pm25, paste0(root_path, "DB/daily_DB/first_DB_PM2.5_dailyRAW.xlsx"), rowNames = FALSE)



# Expanding the database to include additional metadata
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

View(daily_pm25)

write.csv(daily_pm25, file = paste0(root_path, "DB/daily_DB/first_DB_PM2.5_daily.csv"), row.names = FALSE)
write.xlsx(daily_pm25, file = paste0(root_path, "DB/daily_DB/first_DB_PM2.5_daily.xlsx"), rowNames = FALSE)