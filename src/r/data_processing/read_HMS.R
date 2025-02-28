library (sf)
library(maps)
library(dplyr)
library(ggplot2)
# 
# date1 <- as.Date('2024-07-20', format = '%Y-%m-%d')
# date2 <- as.Date('2024-08-09', format = '%Y-%m-%d')
# date3 <- as.Date('2024-06-01', format = '%Y-%m-%d')
# dates_filtered <- c(date1, date2)

# my path to read PM2.5 DB from DataRepo located on Air Data Team sharepoint 
root_path <- "C:/Users/nkhosra/Oregon/DEQ - Air Data Team - OzoneDB/test_DB_PM2.5_summer_2024/"

source('./src/r/utils/support_func.R')       # create cross tables



read_kml <- function(date, url, layer, state_sf_transformed, sub_dailypm25) {
  # url <- paste0("https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/KML/", 
  #               format(date, "%Y/%m/hms_smoke"), format(date, "%Y%m%d"), ".kml")
  
  tryCatch({
    kml_data <- sf::st_read(url, layer = layer, quiet = TRUE)
    kml_data_cleaned <- sf::st_make_valid(kml_data)
    
    state_sf_transformed_local <- sf::st_transform(state_sf_transformed, sf::st_crs(kml_data_cleaned))
    state_sf_transformed_local <- sf::st_make_valid(state_sf_transformed_local)
    
    # Rename columns to be consistent
    kml_data_cleaned           <- kml_data_cleaned %>% rename(geometry = geometry)
    state_sf_transformed_local <- state_sf_transformed_local %>% rename(geometry = geom)
    
    state_smoke <- tryCatch({
      sf::st_intersection(kml_data_cleaned, state_sf_transformed_local)
    }, error = function(e) {
      warning(paste("Error in state intersection:", e$message))
      tryCatch({
        buffered_state <- sf::st_buffer(state_sf_transformed_local, dist = 0.01)
        sf::st_intersection(kml_data_cleaned, buffered_state)
      }, error = function(e2) {
        warning(paste("Error in buffered state intersection:", e2$message))
        return(NULL)
      })
    })
    
    if (is.null(state_smoke) || nrow(state_smoke) == 0) return(NULL)
    
    sites_sf <- st_as_sf(sub_dailypm25, coords = c("longitude", "latitude"), crs = 4326)
    sites_sf_transformed <- sf::st_transform(sites_sf, sf::st_crs(state_smoke))
    
    sites_data <- tryCatch({
      sf::st_intersection(state_smoke, sites_sf_transformed)
    }, error = function(e) {
      warning(paste("Error in sites intersection:", e$message))
      return(NULL)
    })
    
    if (is.null(sites_data) || nrow(sites_data) == 0) {
      message(paste("No HMS Smoke data available for the selected area, dates, and smoke intensities."), type = "warning")
      return(NULL)
      
    } else  {
      
      # sites_data$date <- as.Date(date)
      sites_data <- sites_data %>% distinct(site, .keep_all = TRUE)
      
      return(sites_data)
    }
    
  }, error = function(e) {
    warning(paste("Failed to process layer", layer, "for date", date, ":", e$message))
    return(NULL)
  })
  
}




# read in supporting spatial information
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
state_sf_transformed <- states %>% filter(ID == "oregon")
states <- subset(states, !grepl("oregon", states$ID))

# layer_names <- c("Smoke (Light)", "Smoke (Medium)", "Smoke (Heavy)") 

# light_smoke  <- list ()  # Initialize column if not present
# medium_smoke <- list ()
# heavy_smoke  <- list ()


light_smoke  <- NULL  # Initialize to NULL or empty data frame
medium_smoke <- NULL
heavy_smoke  <- NULL


smoke_level <- c()  # Empty vector to store appended texts
# HMS_list <- data.frame(focdate = as.Date(character()), site = character(), smoke_level = character(), stringsAsFactors = FALSE)

#****************************  load daily_pm25 ****************
#*if the raw data was uploaded 
# daily_pm25 <- read.csv(paste0(root_path, "DB/first_DB_PM2.5_dailyRAW.csv"))


#*#start expand DB to include more metadata, then date and time variables needs to be extracted 
add_time_intervals_daily <- function(daily_pm25 = NULL) {
  if (is.null(daily_pm25)) {
    stop("Error: daily_pm25 argument is missing or incorrect")
  }
  
  # Filter dates between June and October 25
  date_index <- which(
    daily_pm25$month %in% 6:9 |  # June to September (all days)
      (daily_pm25$month == 10 & daily_pm25$day2foc <= 25)  # October 1-25
  )
  
  dates_filtered <- unique(daily_pm25$date[date_index])
  
  # Initialize storage for results
  HMS_list <- data.frame()
  
  for (index in seq_along(dates_filtered)) {
    
    focdate <- dates_filtered[index]
    print(paste("Processing date:", focdate))
    
    sub_dailypm25 <- daily_pm25 %>%
      filter(date == focdate) %>%
      select(date, site, longitude, latitude)
    
    url <- paste0("https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/KML/",
                  format(focdate, "%Y/%m/hms_smoke"), format(focdate, "%Y%m%d"), ".kml")
    
    # Step 1: Get KML layers safely
    layers_info <- tryCatch({
      st_layers(url)
    }, error = function(e) {
      message(paste("Error loading KML for date", focdate, ":", e))
      return(NULL)
    })
    
    if (!is.null(layers_info)) {  
      print("Layers found:")
      print(layers_info$name)
      
      # Initialize smoke category lists
      light_smoke <- data.frame()
      medium_smoke <- data.frame()
      heavy_smoke <- data.frame()
      
      for (layer in layers_info$name) {
        message(paste("Processing layer:", layer, "for", focdate))
        
        all_data <- tryCatch({
          read_kml(focdate, url, layer, state_sf_transformed, sub_dailypm25)
        }, error = function(e) {
          message(paste("Error reading KML for layer", layer, ":", e))
          return(NULL)
        })
        
        if (!is.null(all_data) && nrow(all_data) > 0) {
          if (layer == "Smoke (Light)") {
            light_smoke <- bind_rows(light_smoke, all_data)
          } else if (layer == "Smoke (Medium)") {
            medium_smoke <- bind_rows(medium_smoke, all_data)
          } else if (layer == "Smoke (Heavy)") {
            heavy_smoke <- bind_rows(heavy_smoke, all_data)
          }
        }
      }
    }
    
    # Process site-based smoke levels
    for (site in sub_dailypm25$site) {
      smoke_level <- c()
      
      if (site %in% light_smoke$site) {
        smoke_level <- c(smoke_level, "Smoke (Light)")
      }
      if (site %in% medium_smoke$site) {
        smoke_level <- c(smoke_level, "Smoke (Medium)")
      }
      if (site %in% heavy_smoke$site) {
        smoke_level <- c(smoke_level, "Smoke (Heavy)")
      }
      
      smoke_level <- paste(smoke_level, collapse = ", ")
      
      HMS <- data.frame(focdate = focdate, site = site, smoke_level = smoke_level)
      
      HMS_list <- bind_rows(HMS_list, HMS)
    }
  }
  
  colnames(HMS_list)[colnames(HMS_list) == 'focdate']  <- 'date'
  HMS_list$smoke_level[HMS_list$smoke_level == ""] <- NA
  
  HMS_list <- HMS_list %>% distinct(site, date, .keep_all = TRUE)
  
  return(HMS_list)
}



# write.xlsx(HMS_list, file = paste0(root_path, "Supplemental_Data/output_review/HMS_daily.xlsx", rowNames = FALSE))




# 



