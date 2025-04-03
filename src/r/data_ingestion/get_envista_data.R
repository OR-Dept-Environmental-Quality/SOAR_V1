#For code review
library(dplyr)
library(httr)
library(jsonlite)
library(lubridate)
library(stringr)



# Function to get Envista stations using stored credentials

get_envista_stations <- function() {
  
  q <- paste0(baseurl, "v1/envista/stations")
  
  resp <- GET(q, authenticate(SIGNIN_ENVISTA$username, SIGNIN_ENVISTA$password))
  # resp  ## need to check status code
  df <- fromJSON(content(resp, type = "text", encoding = "UTF-8"))
  
  regions <- get_regions()
  df <- left_join( df, regions [ , c('regionId', 'regions')], by = 'regionId' )
  
  colnames(df)[colnames(df) == 'address'] <- 'census classifier'
  
  return(df)}



get_aqi_param <- function(station, channel, from, to, time_base = 60) {
  
  q <- paste0(baseurl, "v1/envista/stations/", station, "/data/", channel,"?from=", from, "&to=", to, "&timebase=", time_base)
  #  https://oraqiapi.deq.state.or.us/v1/envista/stations/2/data/3?from=2018/12/07&to=2018/12/09&timebase=5
  
  resp <- GET(q, authenticate(SIGNIN_ENVISTA$username, SIGNIN_ENVISTA$password))
  df <- fromJSON(content(resp, type = "text", encoding = "UTF-8")) # was encoding = "ISO-8859-1" awf 20OCT2022
  
  return(df)}


# does the envista data call, and light api formatting for use 
# Check the Miro board. 'siteXpoll' is a metadata table created for the special quer by the user
get_envista_dat <- function(siteXpoll, i_site){

  # identify call
  site      <- siteXpoll$meta$site[i_site] 
  i_monitor <- siteXpoll$meta$poll_envista[i_site]
  # i_alias   <- siteXpoll$meta$poll_alias[i_site]
  from_time <- siteXpoll$meta$from_date[i_site]
  to_time   <- siteXpoll$meta$to_date[i_site]
  
  # if(siteXpoll$meta$by_date[i_site] == "five_min"){time_base = 5}
  if(siteXpoll$meta$by_date[i_site] == "hour"){time_base = 60}
  # if(siteXpoll$meta$by_date[i_site] == "day"){time_base = 1440}
  
  if(exists('dat_site_all')){rm(dat_site_all)}
  
  # get station - could replace with getting all envista stations at the start/initialization - awf
  metadat <- get_envista_stations() %>% build_envista_monitor_list() # opportunity to pull units
  metadat$shortName <- tolower(metadat$shortName)
  station <- metadat %>% filter(shortName == site) %>% filter(tolower(monitor_type) == tolower(i_monitor))
  stationId <- station %>% select(stationId)
  channelId <- station %>% select(channelId_type)
  
  # site_all_monitors <- station$monitors[[1]] # unpacking monitor list - move to initialization
  # channel <- site_all_monitors %>% filter(tolower(name) == tolower(i_monitor) | str_detect(tolower(site_all_monitors$alias), tolower(i_alias))) %>% select(channelId) # identify the monitor
  # channel <- as.numeric(channel); if(length(channel) != 1){stop('error: envista channels')}
          
  # do the call
  monitor_dat <- get_aqi_param(stationId, channelId, from_time, to_time, time_base) %>% unpack_envista_dat()

  # add metadata from the initialization deq_dat metadata request
  # add envista method code, parameter, and time from 
  monitor_dat$method_code      <- siteXpoll$meta$method_env[i_site]
  monitor_dat$parameter        <- siteXpoll$meta$poll_name[i_site]
  monitor_dat$by_date          <- siteXpoll$meta$by_date[i_site]
  monitor_dat$units_of_measure <- NA # siteXpoll$meta$envista_units[i_site]
  #pull lat and long 
  monitor_dat$latitude         <- siteXpoll$meta$latitude[i_site]
  monitor_dat$longitude        <- siteXpoll$meta$longitude[i_site]
  
  return(monitor_dat)}

# support get_envista_dat - envista api unpacking 
unpack_envista_dat <- function(envista_package){
  
  dat <- envista_package$data # extract data from envista package
  
  # now extract the data that was packaged by the envista api
  monitor_dat <- t(sapply(1:length(dat$channels),
                          function(ind){out <- c(
                            dat$datetime[[ind]],
                            dat$channels[[ind]]$id,
                            dat$channels[[ind]]$name,
                            dat$channels[[ind]]$alias,
                            dat$channels[[ind]]$value,
                            dat$channels[[ind]]$status,
                            dat$channels[[ind]]$valid,
                            dat$channels[[ind]]$description)
                          }))
  
  monitor_dat <- as.data.frame(monitor_dat, stringsAsFactors = FALSE)
  names(monitor_dat) <- c('datetime', 'id', 'name', 'alias', 
                          'value', 'status', 
                          'valid', 'description')
  
  # format data for R
  monitor_dat$datetime <- sub('T', ' ', monitor_dat$datetime)
  monitor_dat$datetime <- sub('-08:00', '', monitor_dat$datetime)
  monitor_dat$datetime <- sub('-07:00', '', monitor_dat$datetime)
  monitor_dat$datetime <- parse_date_time(monitor_dat$datetime, '%Y-%m-%d %H:%M:%S', tz = 'Etc/GMT+8')
  
  monitor_dat$value  <- as.numeric(monitor_dat$value)
  monitor_dat$status <- as.numeric(monitor_dat$status)
  
  return(monitor_dat)}

# wrangles envista data into a universal format
format_envista_universal <- function(data_envista){
  
  # envista time is the end of interval 
  # for comparison with aqs, we want it to be the start of the interval
  for(i_tme in c('five_min', 'hour', 'day')){
    if(i_tme == 'five_min'){offset <- minutes(5)}
    if(i_tme == 'hour'){    offset <- hours(1)}
    if(i_tme == 'day'){     offset <- days(1)}
    foc <- data_envista$by_date == i_tme
    data_envista$datetime[foc] <- data_envista$datetime[foc] - offset}
  # add lat and long
  # trim the dataset down to key variables
  data_envista <- data_envista %>% filter(!is.na(datetime)) %>% select(datetime, value, status, valid, parameter, method_code, units_of_measure, latitude, longitude)
  names(data_envista) <- c('datetime', 'sample_measurement', 'qualifier', 'valid', 'parameter', 'method_code', 'units_of_measure', 'latitude', 'longitude')
  
  # data_envista <- data_envista %>% filter(!is.na(datetime)) %>% select(datetime, value, status, valid, parameter, method_code, units_of_measure) 
  # names(data_envista) <- c('datetime', 'sample_measurement', 'qualifier', 'valid', 'parameter', 'method_code', 'units_of_measure')
  
  # may just be valid. we need to check
  data_envista$simple_qual <- data_envista$qualifier
  
  data_envista$dat_source  <- 'envista'
  
  data_envista$poc <- c(-9999) # poc is not identified in envista
    
  all_flags <- unique(data_envista$qualifier[!is.na(data_envista$qualifier)])
  
  for(i_flag in all_flags){
    i_simple_qual <- cross_tables$qualifierXcode %>% 
      filter(envista_qualifier_id == i_flag) %>% 
      select(simple_qualifier)
    
    foc_qual <- data_envista$qualifier == i_flag
    data_envista$simple_qual[foc_qual] <- i_simple_qual}
  
  # cast to type
  data_envista$sample_measurement <- as.numeric(data_envista$sample_measurement)
  data_envista$units_of_measure   <- as.character(data_envista$units_of_measure)
  data_envista$qualifier          <- as.character(data_envista$qualifier)
  data_envista$simple_qual        <- as.character(data_envista$simple_qual)
  data_envista$dat_source         <- as.character(data_envista$dat_source)
  data_envista$method_code        <- as.numeric(data_envista$method_code)
  data_envista$parameter          <- as.character(data_envista$parameter)
  data_envista$poc                <- as.numeric(data_envista$poc)
  # add lat and long
  data_envista$latitude           <- as.numeric(data_envista$latitude)
  data_envista$longitude          <- as.numeric(data_envista$longitude)

  return(data_envista)}