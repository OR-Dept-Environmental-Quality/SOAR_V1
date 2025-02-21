library(dplyr)
library(httr)
library(jsonlite)
library(lubridate)
library(stringr)

# basic api tools 
get_regions <- function (){
  
  q <- paste0(baseurl, 'v1/envista/regions')
  resp <- GET(q, authenticate(username, passwd))
  regions   <- fromJSON(content(resp, type = "text", encoding = "UTF-8")) # was encoding = "ISO-8859-1" awf 20OCT2022
  
  colnames(regions)[colnames(regions) == 'name'] <- 'regions'
  
  return(regions)
}

get_envita_stations <- function() {
  
  q <- paste0(baseurl, "v1/envista/stations")
  
  resp <- GET(q, authenticate(username, passwd))
  # resp  ## need to check status code
  df <- fromJSON(content(resp, type = "text", encoding = "UTF-8"))
  
  regions <- get_regions()
  df <- left_join( df, regions [ , c('regionId', 'regions')], by = 'regionId' )
  
  colnames(df)[colnames(df) == 'address'] <- 'census classifier'
  
  return(df)}

get_aqi_param <- function(station, channel, from, to, time_base = 60) {
  
  q <- paste0(baseurl, "v1/envista/stations/", station, "/data/", channel,"?from=", from, "&to=", to, "&timebase=", time_base)
  #  https://oraqiapi.deq.state.or.us/v1/envista/stations/2/data/3?from=2018/12/07&to=2018/12/09&timebase=5
  
  resp <- GET(q, authenticate(username, passwd))
  df <- fromJSON(content(resp, type = "text", encoding = "UTF-8"))
  
  return(df)}

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
  
  return(monitor_dat)}


# find all the unique monitors that are available

build_envista_monitor_list <- function(env_stations){
  
  all_monitors <- c()
  
  for(i_site in 1:dim(env_stations)[1]){
    
    # focus on a station
    env_stations_row  <- env_stations[i_site,]
    
    # get everything but the monitors
    env_row <- env_stations[i_site,] %>% select(names(env_stations_row[names(env_stations_row) != 'monitors']))
    
    env_monitors <- env_stations[i_site,]$monitors[[1]]
    
    # unpack the monitor by adding a new row for each monitor
    for(i_monitor in 1:dim(env_monitors)[1]){
      
      # channelId
      channelId_type <- as.data.frame(env_monitors$channelId[i_monitor])
      if(dim(channelId_type)[1] == 0){channelId_type <- as.data.frame(-9999)}
      
      names(channelId_type) <- 'channelId_type'
      
      # monitor name
      monitor_type <- as.data.frame(env_monitors$name[i_monitor])
      if(dim(monitor_type)[1] == 0){monitor_type <- as.data.frame('none')}
      
      names(monitor_type) <- 'monitor_type'
      
      # alias name
      alias_type <- as.data.frame(env_monitors$alias[i_monitor])
      if(dim(alias_type)[1] == 0){alias_type <- as.data.frame('none')}
      
      names(alias_type) <- 'alias_type'
      
      # typeID name
      typeID_type <- as.data.frame(env_monitors$typeId[i_monitor])
      if(dim(typeID_type)[1] == 0){typeID_type <- as.data.frame(-9999)}
      
      names(typeID_type) <- 'typeID_type'
      
      # pollutantId name
      pollutantID_type <- as.data.frame(env_monitors$pollutantId[i_monitor])
      if(dim(pollutantID_type)[1] == 0){pollutantID_type <- as.data.frame(-9999)}
      
      names(pollutantID_type) <- 'pollutantID_type'
      
      # units
      units_type <- as.data.frame(env_monitors$units[i_monitor])
      if(dim(units_type)[1] == 0){units_type <- as.data.frame('none')}
      
      names(units_type) <- 'units_type'
      
      # build a row 
      expanded_monitor <- bind_cols(env_row,
                                    channelId_type,
                                    monitor_type, 
                                    alias_type, 
                                    typeID_type, 
                                    pollutantID_type, 
                                    units_type)
      
      # add the row to all monitor list
      if(i_site == 1 & i_monitor == 1){all_monitors <- expanded_monitor
      } else {all_monitors <- bind_rows(all_monitors, expanded_monitor)}
      
    } # end i_monitor
    
  } # end i_site
  
  # make all names lower case
  all_monitors$shortName    <- tolower(all_monitors$shortName)
  all_monitors$monitor_type <- tolower(all_monitors$monitor_type)
  
  return(all_monitors)
  
} # end function

# unpack lat and long
# Check the Miro board to see how this function corrects the column names for latitude and longitude in the metadata
# returned by Envista API calls.
flat_lat_long <- function(envista_monitors) {
  df_flat <- envista_monitors
  df_flat$latitude <- df_flat$location$latitude
  df_flat$longitude <- df_flat$location$longitude
  df_flat <- df_flat %>% select(!location)
  return(df_flat)}



simplify_metadata <- function(metadata, envista_or_aqs = "envista"){
  if(envista_or_aqs == "envista"){
    
  } else {
    metadata$name <- metadata$local_site_name
    metadata$shortName <- "not tracked"
    metadata$stationsTag <- (10000000 * as.numeric(metadata$state_code)) +  
      (10000 * as.numeric(metadata$county_code)) + 
      as.numeric(metadata$site_number)
    metadata$latitude <- as.numeric(metadata$latitude)
    metadata$longitude <- as.numeric(metadata$longitude)
  }
  return(metadata)}