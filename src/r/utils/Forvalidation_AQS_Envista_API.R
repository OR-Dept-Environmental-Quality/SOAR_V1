# two methods to query Pm2.5 data from RAQSAPI
library(RAQSAPI)
library(jsonlite)


#***********************************************************************************************************************

"email"<- "Fellows.Aaron@deq.state.or.us"
"pass"<- "khakihawk79"

# AQS API query for validating of hourly and daily DB
aqs_credentials(email, pass)


# get the county code
county_list <- aqs_counties_by_state("41", return_header = FALSE)

# get ACS from the aqm_monitors_envista.csv located on the DataRepo on the Air Data Team Sharepoint, use the last three 
# digits of the stationsTag column for the target site

sData <- '20240101'
eDate <- '20240720'

# get hourly data
df <- aqs_sampledata_by_site('88101', as.Date(sData, format = '%Y%m%d'), as.Date(eDate, format = '%Y%m%d'), '41', '035', '0004')

# get daily data 
df <- aqs_dailysummary_by_site('88502', as.Date(sData, format = '%Y%m%d'), as.Date(eDate, format = '%Y%m%d'), '41', '043', '0009')


i_aqi_call <- paste0("https://aqs.epa.gov/data/api/",
                     "sampleData/bySite?",
                     "email=",   SIGNIN_AQS$email,
                     "&key=",    SIGNIN_AQS$api_key,
                     "&param=",  param2foc,
                     "&bdate=",  i_stDate,
                     "&edate=",  i_edDate,
                     "&state=",  state,
                     "&county=", county,
                     "&site=",   site)


i_aqi_call <- paste0("https://aqs.epa.gov/data/api/",
                     "sampleData/bySite?",
                     "email=",   email,
                     "&key=",    pass,
                     "&param=",  '88101',
                     "&bdate=",  format(as.Date(sData, format="%Y%m%d"), "%Y%m%d"),  # Correct format
                     "&edate=",  format(as.Date(eDate, format="%Y%m%d"), "%Y%m%d"),  # Correct format
                     "&state=",  '41',
                     "&county=", '035',
                     "&site=",   '0004')

resp <- GET(i_aqi_call) #get data
jsonRespParsed <- content(resp,as="text") #parse data
D2OUT = as.data.frame(fromJSON(jsonRespParsed)[[2]],stringsAsFactors = FALSE)  

#***********************************************************************************************************************
# AQS API query for validating of hourly and daily DB
# sampleData/bySite

# GET /v1/envista/stations/:id/data/:channelId
# - from <datetime>
#   - to <datetime>
#   - timebase <int> (optional)
# - limit <int> (optional)
# GET /v1/envista/stations/:id/data/: channelId?from=<datetime>&to=<datetime>[timebase=<int>&limit=<int>]

# the user can get stations ID, channel ID from he aqm_monitors_envista.csv located on the DataRepo on the Air Data Team Sharepoint,
#choose start and end date

q <- paste0(baseurl, 'v1/envista/stations/2/data/3?from=2018/12/07&to=2018/12/09&timebase=5')
resp <- GET(q, authenticate(username, passwd))
df   <- fromJSON(content(resp, type = "text", encoding = "UTF-8")) 


dat <- df$data

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
View(monitor_dat)

names(monitor_dat) <- c('datetime', 'id', 'name', 'alias', 
                        'value', 'status', 
                        'valid', 'description')
