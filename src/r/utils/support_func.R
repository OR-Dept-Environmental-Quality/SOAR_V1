#############################################################
# query request (NK)
#############################################################
data_request <- function(variable,sites, str_date, end_dat, variable_list){

  dat_request <- deq_dat(site = sites, 
                 poll_name = variable, 
                 from_date = str_date, to_date = end_date,
                 monitoring_list = variable_list) 
  
  data <- dat_request %>% compile_allXdat() 
  #outfile_name <- paste("./input_data/", variable , ".R") 
  #saveRDS(data,outfile_name)
  
  print(paste('done for', variable))
  
  return(data)
  rm(data)

}
  
  # write the data to disk, switched to save RDS format to keep $data for being 
  #used by other functions
  #write.csv(data$data, outfile_name, row.names = FALSE)
#############################################################
# add typical time intervals (NK)
#############################################################
library(lubridate)

add_time_intervals_daily <- function(grab_dat){
 
 grab_dat$year         <- year(grab_dat$date)
 grab_dat$month        <- month(grab_dat$date)
 grab_dat$hour         <- hour(grab_dat$date)
 grab_dat$day2foc      <- day(grab_dat$date)
 grab_dat$doy          <- yday(grab_dat$date)
 grab_dat$week         <- epiweek(grab_dat$date)
 grab_dat$weekday_name <- weekdays(grab_dat$date, abbreviate = FALSE)
 grab_dat$weekend      <- grab_dat$weekday_name == "Saturday" | grab_dat$weekday_name == "Sunday"
 
 return(grab_dat)}

#############################################################
# add typical time intervals (NK)
#############################################################
library(lubridate)

add_time_intervals <- function(grab_dat){
  
  grab_dat$date         <- as.Date(grab_dat$datetime, format = "%Y-%m-%d", tz = 'Etc/GMT+8') #later this tz should be forced
  # grab_dat$date         <- as.Date(grab_dat$datetime, format = "%Y-%m-%d")
  grab_dat$year         <- year(grab_dat$datetime)
  grab_dat$month        <- month(grab_dat$datetime)
  grab_dat$hour         <- hour(grab_dat$datetime)
  grab_dat$day2foc      <- day(grab_dat$datetime)
  grab_dat$doy          <- yday(grab_dat$datetime)
  grab_dat$week         <- epiweek(grab_dat$datetime)
  grab_dat$weekday_name <- weekdays(grab_dat$datetime, abbreviate = FALSE)
  grab_dat$weekend      <- grab_dat$weekday_name == "Saturday" | grab_dat$weekday_name == "Sunday"
  
  return(grab_dat)}
#############################################################
# average of 24hr  (NK)
#############################################################
calc_24hr_pm <- function(df){
  
  # Ensure datetime remains in local time
  df <- df %>% 
    mutate(datetime = force_tz(datetime, "Etc/GMT+8"))  
  
  df_daily <- df  %>%
    mutate(date = floor_date(datetime, "day")) %>%
    group_by(date, site, parameter_best, poc_best) %>%
    dplyr::summarize(missing_obs    =    24 - n(),
                     startime       =    format(min(datetime) , format='%Y-%m-%dT%H:%M'),
                     endtime        =    format(max(datetime) , format='%Y-%m-%dT%H:%M'),
                     source_best    =    unique(source_best)[!is.na(unique(source_best))][1],  # Pick first non-NA source
                     parameter_best =    unique(parameter_best)[!is.na(unique(parameter_best))][1],
                     pm25           =    trunc(mean(sample_measurement_best,na.rm = TRUE)*10, 2)/10,
                     min_PM25       =    trunc(min(sample_measurement_best,na.rm = TRUE)*10, 2)/10,
                     max_PM25       =    trunc(max(sample_measurement_best,na.rm = TRUE)*10, 2)/10,
                     site           =    unique(site),
                     latitude       =    unique(latitude),
                     longitude      =    unique(longitude),
                     .groups        =    "drop")
  
  #   # days with < 18 valid hours are excluded.
  #   # However, we need to include days if there are < 18 observations but the weighted pm25 still exceeds the NAAQS
  #   too_few               <- df_daily$missing_obs < 18 
  #   df_daily$scaled_value <- (df_daily$missing_obs/24) * df_daily$pm25
  #   too_low               <- df_daily$scaled_value < 35 # 35 is the daily dv for pm25
  #   df_daily$scaled_value[too_low] <- NA
  #   
  #   
  #   df_daily$pm25_valid[too_few] <- df_daily$scaled_value[too_few]
  #   df_daily$pm25_valid[is.nan( df_daily$pm25_valid)] <- NA
  #   df_daily$pm25_valid <- trunc( df_daily$pm25_valid * 10,2)/10 
  #   df_daily <- df_daily %>% select(!scaled_value)
  #   
    print('Done! :)')
    return (df_daily)

}

#############################################################
# mops (NK)
#############################################################
quick_clean <- function(grab_dat){

  # good_data_filt1 <- grab_dat$data$simple_qual_best == 'ok'  |
  #                    grab_dat$data$simple_qual_best == 'fire'
  
  good_data_filt1 <- grab_dat$simple_qual_best == 'ok'
  
  grab_dat   <- grab_dat[good_data_filt1,]

  # good_data_filt2 <- ( grab_dat$sample_measurement_best > -99 &
  #                    !is.na(grab_dat$datetime)  &
  #                    grab_dat$poc_best == -9999 )  |
  #                    ( grab_dat$sample_measurement_best > -99 &
  #                     !is.na(grab_dat$datetime)  &
  #                      grab_dat$poc_best == 1 )

  # grab_dat[good_data_filt2,] -> grab_dat

  grab_dat %>% filter(if_any(everything(), ~ !is.na(.))) -> grab_dat
  
  grab_dat <- grab_dat[!is.na(grab_dat$datetime),]

  return(grab_dat)}