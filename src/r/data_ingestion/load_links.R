#For Code Review
# provides cross tables that are needed to merge different data together
# these tables need to be updated 
loadXlink <- function(root_to_crosstab){
  # site
  # aqm_sites <- read.csv("siteXcode/AQMsites2020.csv") # removed 22SEP2022 awf
  aqm_sites <- read.csv(paste0(root_to_crosstab, "/siteXcode/siteXepaid_crosstable.csv"))
  # names(aqm_sites) <- tolower(names(aqm_sites))
  # try({aqm_sites$site <- tolower(aqm_sites$site)}, silent = TRUE)
  try({aqm_sites$shortName <- tolower(aqm_sites$shortName)}, silent = TRUE)
  
  analyteXcode <- read.csv(paste0(root_to_crosstab, "/siteXcode/analyteXcode.csv"))
  
  qualifierXcode <- read.csv(paste0(root_to_crosstab, "/siteXcode/envista_api_qualifier_code.csv"))
  
  i_loadXlink <- list(aqm_sites, analyteXcode, qualifierXcode)
  names(i_loadXlink) <- c('aqm_sites', 'analyteXcode', 'qualifierXcode')
  
  return(i_loadXlink)}