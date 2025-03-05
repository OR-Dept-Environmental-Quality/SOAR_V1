# initialize an object

# initiates a list (could change to object) that provides a consistent output
deq_dat <- function(site = 'none', 
                    poll_name = 'none', 
                    from_date = '01/01/01', 
                    to_date = '01/01/01', 
                    by_date = 'hour', 
                    tz_date = 'Etc/GMT+8',
                    analysis_level = 'not_provided',
                    monitoring_list = 'none'){

  # check input & control here
  if(site[1] == 'none'){stop('ERROR: set site with 3 letter code')}
  if(poll_name == 'none'){stop('ERROR: set pollutant type: pm25, ozone, nox, sensor, etc')}
  if(to_date == '01/01/01'){stop('ERROR: set time range')}
  if(monitoring_list[1,1] == 'none'){stop('ERROR: build envista monitor list before doing query. envista_monitors. See: get_envista_stations()')}
  
  site      <- tolower(site)
  poll_name <- tolower(poll_name)
  by_date   <- tolower(by_date)
  
  # initiate deq_dat parts
  meta <- init_metadat(site, poll_name, from_date, to_date, by_date, monitoring_list)
  data <- init_dat(from = from_date, to = to_date, poll = poll_name, by = by_date, tz_date = tz_date)
  # api_data <- list() ?
  info <- init_info(analysis_level)
  
  deq_dat <- list(meta, data, info)
  names(deq_dat) <- c('meta', 'data', 'info')
  
  class(deq_dat) <- "deq_dat"
  
  return(deq_dat)}

# compiles metadata for queries - init_deq_dat
init_metadat <- function(allsite, i_poll, from_date, to_date, by_date, monitoring_list){
  
  # metadata needed for the api call 
  # also provides the links between envista and aqs to support merge
  meta <- as.data.frame(matrix(ncol = 13, nrow = length(allsite)))
  names(meta) <- c('site', 'epa_id', 'latitude', 'longitude', 'poll_name', 'poll_envista', 'poll_aqs', 'poll_alias', 
                   'from_date', 'to_date', 'by_date',
                   'method_env', 'envista_units')
  
  i_poll <- tolower(i_poll)

  for(ind in 1:length(allsite)){
    
    i_site <- allsite[ind]
    
    # foc_siteXpoll <- monitoring_list %>% filter(shortName == i_site & monitor_type == i_poll)
    # foc_siteXpoll <- monitoring_list %>% filter(shortName == i_site)
    foc_siteXpoll <- monitoring_list %>% filter(shortName == i_site)
    
    try({
      meta$site[ind]         <- i_site
      # all cross table calls should be updated with envista monitoring list -awf
      # meta$epa_id[ind]           <- foc_siteXpoll$stationsTag # this is the epa id
      meta$epa_id[ind]            <- foc_siteXpoll$stationsTag # this is the epa id
      
      try({meta$latitude[ind]     <- foc_siteXpoll$latitude}, silent = TRUE)
      try({meta$longitude[ind]    <- foc_siteXpoll$longitude}, silent = TRUE)
      # try({meta$latitude[ind]   <- foc_siteXpoll$location$latitude}, silent = TRUE)
      # try({meta$longitude[ind]    <- foc_siteXpoll$location$longitude}, silent = TRUE)
      meta$poll_name[ind]    <- i_poll
      meta$poll_envista[ind] <- tolower(cross_tables$analyteXcode$EnvistaName[tolower(cross_tables$analyteXcode$name) == i_poll])
      meta$poll_aqs[ind]     <- tolower(cross_tables$analyteXcode$AqsName[tolower(cross_tables$analyteXcode$name) == i_poll])
      meta$poll_alias[ind]   <- tolower(cross_tables$analyteXcode$Alias[tolower(cross_tables$analyteXcode$name) == i_poll]) # consider removing
      meta$method_env[ind]   <- cross_tables$analyteXcode$method_code_filter[tolower(cross_tables$analyteXcode$name) == i_poll]
      print(paste0('heyyy',from_date))
      print(paste0('hoooooyyy',to_date))
      meta$from_date[ind]    <- from_date
      meta$to_date[ind]      <- to_date
      meta$by_date[ind]      <- by_date
      meta$envista_units[ind]<- foc_siteXpoll$units_type
      
      }, silent = TRUE)
    }
  
  return(meta)}

# initiates a time series for init_deq_dat
# -> all data will be merged to this master record, so it sets the range and time interval
init_dat <- function(from, to, poll, by, tz_date){

  if(by == "five_min"){by <- '5 min'}
  if(by == "hour"){by <- "hour"}
  if(by == "day"){by <- "day"}
  
  datetime <- seq(
    from = as.POSIXct(from, tryFormat = c("%Y/%m/%d %H:%M", "%Y/%m/%d"), tz = tz_date),
    to = as.POSIXct(to, tryFormat = c("%Y/%m/%d %H:%M", "%Y/%m/%d"), tz = tz_date),
    by = 'hour')  
  
  dat_all <- as.data.frame(datetime)
  
  names(dat_all) <- c('datetime')
  
  return(dat_all)}

# initiates session information
init_info <- function(analysis_level){
  
  details_header <- paste0('_____________ Data background _____________')
  odeq           <- 'This data was compiled by the Oregon Department of Environmental Quality'
  proc_date      <- paste0('Processed on: ', Sys.time())
  data_status    <- paste0('Data status: ', analysis_level)
  
  details_header <- paste0('_____________ system & session _____________')
  system_info    <- Sys.info()
  session_info   <- sessionInfo()
  details_end    <- paste0('_____________ end _____________')
  
  info <- list(odeq, proc_date, data_status, details_header, system_info, session_info, details_end)
  
  return(info)}

