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


read_kml <- function(date, url, layer, state_sf_transformed, sub_dailypm25) {
  tryCatch({
    # Load KML data safely
    kml_data <- tryCatch({
      sf::st_read(url, layer = layer, quiet = TRUE)
    }, error = function(e) {
      warning(paste("Error reading KML file for", date, ":", e$message))
      return(NULL)
    })
    
    if (is.null(kml_data) || nrow(kml_data) == 0) return(NULL)
    
    # Ensure valid geometries
    kml_data <- sf::st_make_valid(kml_data)
    
    # Transform CRS
    state_sf_transformed_local <- sf::st_transform(state_sf_transformed, sf::st_crs(kml_data))
    
    # Handle invalid geometries with buffering
    state_smoke <- tryCatch({
      sf::st_intersection(kml_data, state_sf_transformed_local)
    }, error = function(e) {
      warning(paste("Error in state intersection:", e$message))
      tryCatch({
        buffered_state <- sf::st_buffer(state_sf_transformed_local, dist = 0.01)
        sf::st_intersection(kml_data, buffered_state)
      }, error = function(e2) {
        warning(paste("Error in buffered state intersection:", e2$message))
        return(NULL)
      })
    })
    
    if (is.null(state_smoke) || nrow(state_smoke) == 0) return(NULL)
    
    # Convert monitoring site data to sf format
    sites_sf <- st_as_sf(sub_dailypm25, coords = c("longitude", "latitude"), crs = 4326)
    sites_sf_transformed <- sf::st_transform(sites_sf, sf::st_crs(state_smoke))
    
    # Perform intersection to get smoke levels for each site
    sites_data <- tryCatch({
      sf::st_intersection(state_smoke, sites_sf_transformed)
    }, error = function(e) {
      warning(paste("Error in sites intersection:", e$message))
      return(NULL)
    })
    
    if (is.null(sites_data) || nrow(sites_data) == 0) {
      message("No HMS Smoke data available for the selected area, dates, and smoke intensities.")
      return(NULL)
    } else {
      sites_data <- sites_data %>% distinct(site, .keep_all = TRUE)
      return(sites_data)
    }
    
  }, error = function(e) {
    warning(paste("Failed to process layer", layer, "for date", date, ":", e$message))
    return(NULL)
  })
}


add_HMS_levels_daily <- function(daily_pm25) {
  
  # Read in supporting spatial information
  states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
  state_sf_transformed <- states %>% filter(ID == "oregon")
  states <- subset(states, !grepl("oregon", states$ID))
  
  light_smoke  <- NULL  # Initialize to NULL or empty data frame
  medium_smoke <- NULL
  heavy_smoke  <- NULL
  
  smoke_level <- c()  # Empty vector to store appended texts
  
  date_index <- which(
    daily_pm25$month %in% 6:9 |  # June to September (all days)
      (daily_pm25$month == 10 & daily_pm25$day2foc <= 25)  # October 1-25
  )
  
  dates_filtered <- unique(daily_pm25$date[date_index]) 
  
  for (index in seq_along(dates_filtered)) {
    
    print(index)  
    focdate <- dates_filtered[index]
    print(focdate)
    
    sub_dailypm25 <- daily_pm25 %>% filter(date == focdate) %>% 
      select(c('date', 'site', 'longitude', 'latitude'))
    
    url <- paste0("https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/KML/",
                  format(focdate, "%Y/%m/hms_smoke"), format(focdate, "%Y%m%d"), ".kml")
    
    # Step 1: List all the layers in the KML file
    layers_info <- tryCatch({
      st_layers(url)
    }, error = function(e) {
      print(paste("Error loading KML for date", focdate, ":", e))
      return(NULL)
    })
    
    if (!is.null(layers_info)) {  
      print(layers_info)
      
      # Extract the layer names
      for (layer in layers_info$name) {
        
        all_data <- NULL
        all_data <- read_kml(focdate, url, layer, state_sf_transformed, sub_dailypm25)
        
        if (!is.null(all_data) && nrow(all_data) != 0) {
          
          if (layer == "Smoke (Light)" && is.null(light_smoke)) {
            light_smoke <- all_data
          } else if (layer == "Smoke (Light)" && !is.null(light_smoke)) {
            light_smoke <- bind_rows(all_data, light_smoke)
          }
          
          if (layer == "Smoke (Medium)" && is.null(medium_smoke)) {
            medium_smoke <- all_data
          } else if (layer == "Smoke (Medium)" && !is.null(medium_smoke)) {
            medium_smoke <- bind_rows(all_data, medium_smoke)
          }
          
          if (layer == "Smoke (Heavy)" && is.null(heavy_smoke)) {
            heavy_smoke <- all_data
          } else if (layer == "Smoke (Heavy)" && !is.null(heavy_smoke)) {
            heavy_smoke <- bind_rows(all_data, heavy_smoke)
          }
        }
      }
    }
    
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
      
      print(paste(site, smoke_level))
      
      HMS <- data.frame(focdate = focdate, site = site, smoke_level = smoke_level)
      
      if (!exists("HMS_list")) {
        HMS_list <- HMS 
      } else {
        HMS_list <- bind_rows(HMS, HMS_list)  
      }
    }
  }
  
  # ✅ Fix: Ensure final HMS_list `date` column is properly formatted
  colnames(HMS_list)[colnames(HMS_list) == 'focdate']  <- 'date'
  HMS_list$date <- as.Date(HMS_list$date)
  HMS_list$smoke_level[HMS_list$smoke_level == ""] <- NA
  HMS_list <- HMS_list %>% distinct(site, date, .keep_all = TRUE)
  
  return(HMS_list)  # Ensure the function returns the final dataset
}


#**********************************************************************************************************************
  
  # if the user interested in plotting HMS layers for each day and check the overlap with OR
  # 
  # library(ggplot2)
  # library(sf)
  # library(dplyr)
  # 
  # # Define the date
  # focdate <- as.Date('2024-06-08', format = '%Y-%m-%d')
  # 
  # # Construct the KML base URL
  # kml_base_url <- paste0("https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/KML/",
  #                        format(focdate, "%Y/%m/hms_smoke"), format(focdate, "%Y%m%d"), ".kml")
  # 
  # # Smoke intensity levels to loop through
  # smoke_layers <- c("Smoke (Light)", "Smoke (Medium)", "Smoke (Heavy)")
  # 
  # # Read Oregon boundary (ensure it is pre-loaded)
  # state_sf_transformed <- st_read("path_to_oregon_boundary.shp")  # Replace with correct file path
  # 
  # # Define color scheme
  # smoke_colors <- c("Smoke (Light)" = "green",  # Changed Light to green
  #                   "Smoke (Medium)" = "orange",
  #                   "Smoke (Heavy)" = "red")
  # 
  # # Loop through each smoke intensity level
  # for (layer in smoke_layers) {
  #   
  #   # Read KML file safely for each layer
  #   kml_data <- tryCatch({
  #     sf::st_read(kml_base_url, layer = layer, quiet = TRUE)
  #   }, error = function(e) {
  #     warning(paste("Error reading KML file for", focdate, ":", as.character(conditionMessage(e))))
  #     return(NULL)
  #   })
  #   
  #   # Skip if no data was loaded
  #   if (is.null(kml_data) || nrow(kml_data) == 0) {
  #     message(paste("⚠️ No data found for", layer))
  #     next
  #   }
  #   
  #   # Ensure proper CRS transformation
  #   kml_data <- st_transform(kml_data, crs = st_crs(state_sf_transformed))
  #   
  #   # Add a column for smoke intensity
  #   kml_data$smoke_level <- layer
  #   
  #   # Plot the data
  #   p <- ggplot() +
  #     geom_sf(data = state_sf_transformed, fill = "gray90", color = "black", alpha = 0.5) +  # Oregon state boundary
  #     geom_sf(data = kml_data, aes(color = smoke_level), size = 2, alpha = 0.7) +  # Smoke locations
  #     scale_color_manual(name = "Smoke Intensity", values = smoke_colors) +  # Apply color mapping
  #     theme_minimal() +
  #     labs(
  #       title = paste("HMS Smoke Plume Locations - June 10, 2024 (", layer, ")", sep = ""),
  #       subtitle = paste("Showing:", layer, "smoke layer"),
  #       x = "Longitude",
  #       y = "Latitude"
  #     ) +
  #     theme(legend.position = "right")  # Adjust legend position if needed
  #   
  #   print(p)  # Display plot for each layer
  # }
  # 
  