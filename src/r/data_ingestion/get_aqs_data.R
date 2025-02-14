#For Code Review
# functions to support EPA API calls
library(httr)
library(jsonlite)

# basic api tools 
DoRequest <- function(http2get){
  resp <- GET(http2get) #get data
  jsonRespParsed <- content(resp,as="text") #parse data
  D2OUT = as.data.frame(fromJSON(jsonRespParsed)[[2]],stringsAsFactors = FALSE)  
  return(D2OUT)}

# query for monitors in Oregon
get_aqs_monitors <- function(state2get, params2get, StDate, EdDate, SIGNIN){
  paste0("https://aqs.epa.gov/data/api/",
         "monitors/byState?",
         "email=", SIGNIN[[1]],
         "&key=", SIGNIN[[2]],
         "&param=", params2get,
         "&bdate=", StDate,
         "&edate=", EdDate,
         "&state=", state2get)}


# does the aqs data call, and light api formatting for use 
get_aqs_dat <- function(siteXpoll, i_site){
  
  if(exists('d_aqs')){rm(d_aqs)}
     
  # location
  state  <- substr(siteXpoll$meta$epa_id[i_site],1,2)
  county <- substr(siteXpoll$meta$epa_id[i_site],3,5)
  site   <- substr(siteXpoll$meta$epa_id[i_site],6,9)
  
  # parameter
  param2foc <- siteXpoll$meta$poll_aqs[i_site]
  
  #test the error
  View(siteXpoll$meta)
  print(paste0('>>>>>>>>>>>>>>>>>>>>>>>>>>>',str(siteXpoll$meta$from_date[i_site])))
  print(paste0('***************************',str(siteXpoll$meta$to_date[i_site])))
  
  # time
  StDate <- siteXpoll$meta$from_date[i_site]
  EdDate <- siteXpoll$meta$to_date[i_site]
  
  aqs_request <- build_aqs_calls(state, county, site, param2foc, StDate, EdDate, SIGNIN)
  
  for(i_call in aqs_request){
    if(!exists('d_aqs')){d_aqs <- DoRequest(i_call)
    }else{d_aqs <- bind_rows(d_aqs, DoRequest(i_call))}
  }
  
  # add parameter & method id
  d_aqs$parameter        <- siteXpoll$meta$poll_name[i_site]
  d_aqs$requested_method <- siteXpoll$meta$method_env[i_site] # insert line awf 11NOV2020
  
  return(d_aqs)}

# support get_aqs_dat - formats api calls
build_aqs_calls <- function(state, county, site, param2foc, StDate, EdDate, SIGNIN){
  
  aqi_call <- list()
  all_year <- c(year(StDate):year(EdDate))
  
  for(ind in 1:length(all_year)){
    
    i_year <- all_year[ind]
    
    if(ind == 1){
      i_stDate <- gsub(ymd(StDate), pattern = '-', replacement = '', fixed = TRUE)
    }else{
      i_stDate <- paste0(toString(i_year),"01","01")}
    
    if(ind == length(all_year)){
      i_edDate <- gsub(ymd(EdDate), pattern = '-', replacement = '', fixed = TRUE)
    }else{
      i_edDate <- paste0(toString(i_year),"12","31")}
    
    i_aqi_call <- paste0("https://aqs.epa.gov/data/api/",
                         "sampleData/bySite?",
                         "email=",   SIGNIN[[1]],
                         "&key=",    SIGNIN[[2]],
                         "&param=",  param2foc,
                         "&bdate=",  i_stDate,
                         "&edate=",  i_edDate,
                         "&state=",  state,
                         "&county=", county,
                         "&site=",   site)
    
    aqi_call[ind] <- i_aqi_call}
  
  return(aqi_call)}

# wrangles aqs data into a universal format
format_aqs_universal <- function(data_aqs){
  
  data_aqs$datetime <- as.POSIXct(paste(data_aqs$date_local, data_aqs$time_local), 
                                  format = c("%Y-%m-%d %H:%M"),tz = 'Etc/GMT+8')
  data_aqs$dat_source <- 'aqs'
  
  data_aqs$simple_code <- sapply(data_aqs$qualifier, function(x){if(!is.na(x)){strsplit(x, " ")[[1]][1]}else{NA}})
  
  data_aqs$simple_qual <- data_aqs$simple_code
  all_ok <- is.na(data_aqs$qualifier)
  data_aqs$simple_qual[all_ok] <- 'ok'
  
  all_flags <- unique(data_aqs$simple_code[!is.na(data_aqs$simple_code)])
  
  aqs_qualifierXcode <- cross_tables$qualifierXcode %>% distinct(aqs_qualifier, .keep_all = TRUE) 
  
  for(i_flag in all_flags){
    i_qual <- aqs_qualifierXcode %>% 
      filter(aqs_qualifier == i_flag) %>% 
      select(simple_qualifier)
    
    foc_qual <- data_aqs$simple_code == i_flag
    data_aqs$simple_qual[foc_qual] <- i_qual}
  
  unit_check <- (data_aqs$parameter == "o3" | data_aqs$parameter == "ozone") & data_aqs$units_of_measure == 'Parts per million' & !is.na(data_aqs$units_of_measure)
  data_aqs$sample_measurement[unit_check] <- 1000 * data_aqs$sample_measurement[unit_check]
  data_aqs$units_of_measure[unit_check] <- 'Parts per billion'
  
  unit_check <- (data_aqs$parameter == "ws" | data_aqs$parameter == "wind speed") & data_aqs$units_of_measure == 'Knots' & !is.na(data_aqs$units_of_measure)
  data_aqs$sample_measurement[unit_check] <- 1.15078 * data_aqs$sample_measurement[unit_check]
  data_aqs$units_of_measure[unit_check] <- 'Miles per hour'
  
  # this is going to filter on the method as specified in the cross table - eg pm25 using an fem or frm
  # data_aqs <- data_aqs %>% select(datetime, sample_measurement, qualifier, simple_qual, dat_source, method_code, parameter) # changed line 11NOV2020
  data_aqs <- data_aqs %>% filter((requested_method == method_code) | (requested_method == -9999) | (is.na(requested_method))) %>%  
    select(datetime, sample_measurement, units_of_measure, qualifier, simple_qual, dat_source, method_code, parameter, poc, latitude, longitude, datum)# insert line awf 11NOV2020
  
  # cast to type
  data_aqs$sample_measurement <- as.numeric(data_aqs$sample_measurement)
  data_aqs$units_of_measure   <- as.character(data_aqs$units_of_measure)
  data_aqs$qualifier          <- as.character(data_aqs$qualifier)
  data_aqs$simple_qual        <- as.character(data_aqs$simple_qual)
  data_aqs$dat_source         <- as.character(data_aqs$dat_source)
  data_aqs$method_code        <- as.numeric(data_aqs$method_code)
  data_aqs$parameter          <- as.character(data_aqs$parameter)
  data_aqs$poc                <- as.numeric(data_aqs$poc)
  data_aqs$latitude           <- as.numeric(data_aqs$latitude)
  data_aqs$longitude          <- as.numeric(data_aqs$longitude)
  
  return(data_aqs)}