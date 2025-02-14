
# looks across sites to compile data
compile_allXdat <- function(all_siteXpoll){
  
  for(i_site in 1:length(all_siteXpoll$meta$site)){
    
    site_name <- all_siteXpoll$meta$site[i_site] # 3 letter site code
    
    print('---------------------')
    print(paste0('processing:', site_name))
    print(paste0('n = ', i_site, ' out of ', length(all_siteXpoll$meta$site)))
    print('---------------------')
    
    # initialization used to control for failed api calls
    data_aqs     <- as.data.frame(-9999)
    data_envista <- as.data.frame(-9999)
    data_merge   <- as.data.frame(-9999)
    
    # get the data - wrapped in try if query fails --> check below
    options(show.error.messages = TRUE)
    print("Try getting AQS data: line 22 in merge_records.R")
    try({data_aqs <- get_aqs_dat(all_siteXpoll, i_site)}, silent = FALSE)
    print("Try getting envista data: line 24 in merge_records.R")
    try({data_envista <- get_envista_dat(all_siteXpoll, i_site)}, silent = FALSE)
    # try({all_siteXpoll$api_data$data_aqs <- get_aqs_dat(all_siteXpoll, i_site)}, silent = TRUE)
    # try({all_siteXpoll$api_data$data_envista <- get_envista_dat(all_siteXpoll, i_site)}, silent = TRUE)
    # } end compile_allXdat ???
    # end compile dat here?
    
    # new funcition below
    # control flow: check if query returned results 
    if(exists('data_aqs')){
      if(dim(data_aqs)[2] > 1){
          do_aqs <- TRUE
        } else {
          do_aqs <- FALSE
          print("aqs has no data for:")
          print(all_siteXpoll$meta[i_site,])}
    } else {
      do_aqs <- FALSE
      print("aqs has no data for:")
      print(all_siteXpoll$meta[i_site,])}
    
    if(exists('data_envista')){
      if(dim(data_envista)[2] > 1){
        do_envista <- TRUE
      } else {
        do_envista <- FALSE
        print("envista has no data for:")
        print(all_siteXpoll$meta[i_site,])}
    } else {
      do_envista <- FALSE
      print("envista has no data for:")
      print(all_siteXpoll$meta[i_site,])}
    
    # data_merge is a df & is all or one dataset depending on queries
    if(do_aqs & do_envista){
      data_merge <- homogenize_datastreams(data_aqs, data_envista)
    } else if (do_aqs & !do_envista){
      data_merge <- choose_datastream(data_aqs, 'aqs_only')
    } else if (!do_aqs & do_envista){
      data_merge <- choose_datastream(data_envista, 'envista_only')
    } else {
      print(paste0('Some aqs and/or envista data were not available for: ', site_name))
      data_merge <- blank_datastream()} # end if(do_aqs & do_envista)
    
    # add the site name to the data
    data_merge$site <- site_name
    
    if(i_site == 1){
      dat_package <- data_merge
    } else {
      try({dat_package <- bind_rows(dat_package, data_merge)}, silent = TRUE)
      }
    
  }
  
  #now put the packaged data into deq_dat
  all_siteXpoll$data <- dat_package
    
  return(all_siteXpoll)}

# make one datastream from aqs and envista data
homogenize_datastreams <- function(data_aqs, data_envista){
  
  # wrangle data - should output consistent formats
  data_aqs     <- data_aqs %>% format_aqs_universal()         # aqs
  data_envista <- data_envista %>% format_envista_universal() # envista
  
  # merge records
  data_merge <- full_join(data_aqs, data_envista, by = 'datetime', na_matches = "never", suffix = c("_aqs", "_envista"))
  
  # remove records with no data
  no_dat <- is.na(data_merge$dat_source_envista) & is.na(data_merge$dat_source_aqs)
  data_merge <- data_merge[!no_dat,]
  
  # create best record
  data_merge$sample_measurement_best <- NA
  data_merge$units_of_measure_best   <- NA
  data_merge$qualifier_best          <- NA
  data_merge$simple_qual_best        <- NA
  data_merge$source_best             <- NA
  data_merge$method_code_best        <- NA
  data_merge$parameter_best          <- NA
  data_merge$poc_best                <- NA # awf 28JAN2021
  data_merge$latitude                <- NA
  data_merge$longitude               <- NA
  
  # fill best with aqs if available
  fill_with_aqs <- !is.na(data_merge$dat_source_aqs)
  
  data_merge$sample_measurement_best[fill_with_aqs] <- data_merge$sample_measurement_aqs[fill_with_aqs]
  data_merge$units_of_measure_best[fill_with_aqs]   <- data_merge$units_of_measure_aqs[fill_with_aqs]
  data_merge$qualifier_best[fill_with_aqs]          <- data_merge$qualifier_aqs[fill_with_aqs]
  data_merge$simple_qual_best[fill_with_aqs]        <- data_merge$simple_qual_aqs[fill_with_aqs]
  data_merge$source_best[fill_with_aqs]             <- data_merge$dat_source_aqs[fill_with_aqs]
  data_merge$method_code_best[fill_with_aqs]        <- data_merge$method_code_aqs[fill_with_aqs]
  data_merge$parameter_best[fill_with_aqs]          <- data_merge$parameter_aqs[fill_with_aqs]
  data_merge$poc_best[fill_with_aqs]                <- data_merge$poc_aqs[fill_with_aqs]
  data_merge$latitude[fill_with_aqs]                <- data_merge$latitude_aqs[fill_with_aqs]
  data_merge$longitude[fill_with_aqs]               <- data_merge$longitude_aqs[fill_with_aqs]
  
  # fill best with envista if available, but aqs is missing
  fill_with_envista <- !is.na(data_merge$dat_source_envista) & is.na(data_merge$dat_source_aqs) 
  
  data_merge$sample_measurement_best[fill_with_envista] <- data_merge$sample_measurement_envista[fill_with_envista]
  data_merge$units_of_measure_best[fill_with_envista]   <- data_merge$units_of_measure_envista[fill_with_envista]
  data_merge$qualifier_best[fill_with_envista]          <- data_merge$qualifier_envista[fill_with_envista]
  data_merge$simple_qual_best[fill_with_envista]        <- data_merge$simple_qual_envista[fill_with_envista]
  data_merge$source_best[fill_with_envista]             <- data_merge$dat_source_envista[fill_with_envista]
  data_merge$method_code_best[fill_with_envista]        <- data_merge$method_code_envista[fill_with_envista]
  #lat and long
  data_merge$parameter_best[fill_with_envista]          <- data_merge$parameter_envista[fill_with_envista]
  data_merge$poc_best[fill_with_envista]                <- data_merge$poc_envista[fill_with_envista]
  data_merge$latitude[fill_with_envista]                <- data_merge$latitude_envista[fill_with_envista]
  data_merge$longitude[fill_with_envista]               <- data_merge$longitude_envista[fill_with_envista]
 
 # add lat and long 
  data_merge <- data_merge %>% select(datetime, sample_measurement_best, units_of_measure_best, qualifier_best, simple_qual_best, source_best, poc_best,
                                     method_code_best, parameter_best, sample_measurement_aqs, sample_measurement_envista, latitude, longitude)
  
  # data_merge <- data_merge %>% select(datetime, sample_measurement_best, units_of_measure_best, qualifier_best, simple_qual_best, source_best, poc_best, 
  #                                     method_code_best, parameter_best, sample_measurement_aqs, sample_measurement_envista)
  
  # can we remove this - should have been done in universal format function & above
  # names(data_merge) <- c('datetime', 'sample_measurement_best', 'units_of_measure_best', 'qualifier_best', 'simple_qual_best', 'source_best', 'poc_best',
  #                                'method_code', 'parameter', 'sample_measurement_aqs', 'sample_measurement_envista')
  
  return(data_merge)}

# use either aqs or envista data
choose_datastream <- function(data, aqs_or_envista){
  
  # wrangle data & rename to parallel above
  if(aqs_or_envista == 'aqs_only'){data_merge <- data %>% format_aqs_universal()}  
  if(aqs_or_envista == 'envista_only'){data_merge <- data %>% format_envista_universal()} 
  
  # create best
  data_merge$sample_measurement_best <- data_merge$sample_measurement
  data_merge$units_of_measure_best   <- data_merge$units_of_measure
  data_merge$qualifier_best          <- data_merge$qualifier
  data_merge$simple_qual_best        <- data_merge$simple_qual
  data_merge$source_best             <- data_merge$dat_source
  data_merge$method_code_best        <- data_merge$method_code
  data_merge$parameter_best          <- data_merge$parameter
  data_merge$poc_best                <- data_merge$poc
  #add lat and long
  data_merge$latitude                <- data_merge$latitude
  data_merge$longitude               <- data_merge$longitude
  
  if(aqs_or_envista == 'aqs_only'){
    data_merge$sample_measurement_aqs           <- data_merge$sample_measurement
    try({data_merge$sample_measurement_envista  <- NA}, silent = TRUE)
  }
  
  if(aqs_or_envista == 'envista_only'){
    try({data_merge$sample_measurement_aqs <- NA}, silent = TRUE)
    data_merge$sample_measurement_envista  <- data_merge$sample_measurement
  }
  
  # add lat and long
  header <- c('datetime', 'sample_measurement_best', 'units_of_measure_best', 'qualifier_best', 'simple_qual_best', 'source_best', 'poc_best',
             'method_code', 'parameter', 'sample_measurement_aqs', 'sample_measurement_envista', 'latitude', 'longitude')
  
  # header <- c('datetime', 'sample_measurement_best', 'units_of_measure_best', 'qualifier_best', 'simple_qual_best', 'source_best', 'poc_best',
  #             'method_code', 'parameter', 'sample_measurement_aqs', 'sample_measurement_envista')

  if(dim(data_merge)[1] != 0){
    # add lat and long
    data_merge <- data_merge %>% select(datetime, sample_measurement_best, qualifier_best, simple_qual_best, source_best, poc_best,
                                        method_code_best, parameter_best, sample_measurement_aqs, sample_measurement_envista, latitude, longitude)
    
    # data_merge <- data_merge %>% select(datetime, sample_measurement_best, qualifier_best, simple_qual_best, source_best, poc_best,
    #                                     method_code_best, parameter_best, sample_measurement_aqs, sample_measurement_envista)
    } else { 
    data_merge <- as.data.frame(matrix(ncol = length(header), nrow = 1)) 
    }
  
  # names(data_merge) <- header
  
  return(data_merge)}


# if no data exists, write NA
blank_datastream <- function(){
  # add lat and long
  header <- c('datetime', 'sample_measurement_best', 'units_of_measure_best', 'qualifier_best', 'simple_qual_best', 'source_best', 'poc_best',
              'method_code', 'parameter', 'sample_measurement_aqs', 'sample_measurement_envista', 'latitude', 'longitude')
  
  # header <- c('datetime', 'sample_measurement_best', 'units_of_measure_best', 'qualifier_best', 'simple_qual_best', 'source_best', 'poc_best',
  #             'method_code', 'parameter', 'sample_measurement_aqs', 'sample_measurement_envista')
  
  data_merge <- as.data.frame(matrix(ncol = length(header), nrow = 1))
  
  names(data_merge) <- header
  
  return(data_merge)}