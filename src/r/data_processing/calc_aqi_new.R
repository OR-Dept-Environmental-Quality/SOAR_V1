# ************************************************************************************
# this version contains edits done by Ryan Porter
# ************************************************************************************

#######################################################################################
#
#            DEFINE AQI TABLEs anf functions
#
######################################################################################

## AQI breakpoints and category names (common across all pollutants)
# https://aqs.epa.gov/aqsweb/documents/codetables/aqi_breakpoints.html

ilo <- c(   0,  51, 101, 151, 201, 301, 401, 501)
ihi <- c(  50, 100, 150, 200, 300, 400, 500, 999)
hcat <- c("Good", "Moderate", "USG", "Unhealthy", "Very unhealthy","Hazardous", "Hazardous", "Hazardous")

hcatm <- data.frame(hcat = factor(c("Good", "Moderate", "USG", "Unhealthy", "Very Unhealthy", "Hazardous", "Missing"), 
                                  levels = c("Good", "Moderate", "USG", "Unhealthy", "Very Unhealthy", "Hazardous", "Missing"))) 


## AQI table for PM2.5
## AQI calculations based on 2016 EPA Technical doc
## conc   0.0 - 12.0   AQI   0 - 50
## conc  12.1 - 35.4   AQI  51 - 100
## conc  35.5 - 55.4   AQI 101 - 150
## conc  55.5 - 150.4  AQI 151 - 200
## conc 150.5 - 250.4  AQI 201 - 300
## conc 250.5 - 350.4  AQI 301 - 400
## conc 350.5 - 500.4  AQI 401 - 500
bplo <- c( 0.0, 9.1, 35.5,  55.5, 125.5, 225.5, 350.5, 500.5)
bphi <- c(9.0, 35.4, 55.4, 125.4, 225.4, 350.4, 500.4, 99999.9)
aqi_pm25 <- data.frame(bplo, bphi, ilo, ihi, hcat)


## AQI table for PM10
## AQI calculations based on 2016 EPA Technical doc
## conc   0.0 - 54.0   AQI   0 - 50
## conc     55 - 154   AQI  51 - 100
## conc    155 - 254   AQI 101 - 150
## conc    255 - 354   AQI 151 - 200
## conc    355 - 424   AQI 201 - 300
## conc    425 - 504   AQI 301 - 400
## conc    505 - 604   AQI 401 - 500
bplo <- c( 0.0,    55,   155,   255,   355,   425,   505,      605)
bphi <- c(54.9, 154.9, 254.9, 354.9, 424.9, 504.9,  604.9, 99999.9)
aqi_pm10 <- data.frame(bplo, bphi, ilo, ihi, hcat)


## AQI table for 8-hour ozone standard
## Ozone 8-hr standard
## conc  0.000 - 0.054   AQI   0 - 50
## conc  0.055 - 0.070   AQI  51 - 100
## conc  0.071 - 0.085   AQI 101 - 150
## conc  0.086 - 0.105   AQI 151 - 200
## conc  0.106 - 0.200   AQI 201 - 300
#ppm
bplo <- c( 0.000, 0.055, 0.071,  0.086, 0.106, NA, NA, NA )
bphi <- c( 0.054, 0.070, 0.085,  0.105, 0.200, NA, NA, NA )
#aqi_o38 <- data.frame(bplo, bphi, ilo, ihi, hcat)
#ppb - awf 31MAR2020
#bplo <- c( 0, 55, 71,  86, 106, NA, NA, NA)
#bphi <- c(54, 70, 85, 105, 200, NA, NA, NA)
aqi_o38 <- data.frame(bplo, bphi, ilo, ihi, hcat)


## Ozone 1-hr standard

## conc  0.125 - 0.164   AQI 101 - 150
## conc  0.165 - 0.204   AQI 151 - 200
## conc  0.205 - 0.404   AQI 201 - 300
## conc  0.405 - 0.504   AQI 301 - 400
## conc  0.505 - 0.604   AQI 401 - 500
## conc  0.605 - 99999   AQI 501 - 999

#ppm
#bplo <- c( 0.000, 0.055, 0.071,  0.086, 0.106, NA, NA )
#bphi <- c( 0.054, 0.070, 0.085,  0.105, 0.200, NA, NA )
#aqi_o38 <- data.frame(bplo, bphi, ilo, ihi, hcat)
#ppb - awf 31MAR2020
bplo <- c(NA, NA, 125,  165, 205, 405, 505,    605)
bphi <- c(NA, NA, 164,  204, 404, 504, 604, 999999)
aqi_o3_1hr <- data.frame(bplo, bphi, ilo, ihi, hcat)

## AQI table for 1-hr NO2 standard
## NO2 1-hr standard
## conc    0 -   53   AQI   0 - 50
## conc   54 -  100   AQI  51 - 100
## conc  101 -  360   AQI 101 - 150
## conc  361 -  649   AQI 151 - 200
## conc  650 - 1249   AQI 201 - 300
## conc 1250 - 1649   AQI 301 - 400
## conc 1650 - 2049   AQI 401 - 500
bplo <- c(  0,  54, 101,  361,  650, 1250, 1650, 2050)
bphi <- c( 53, 100, 360,  649, 1249, 1649, 2049, 99999)
aqi_no2d <- data.frame(bplo, bphi, ilo, ihi, hcat)


## AQI table for 24-hr SO2 standard
## SO2 1-hr standard (values in ppb) for AQI <= 150
## SO2 24 hr standard (values in ppb) for AQI > 150
## conc     0 -   35   AQI   0 - 50
## conc    36 -   75   AQI  51 - 100
## conc    76 -  185   AQI 101 - 150
## conc   186 -  304   AQI 151 - 200
## conc   305 -  604   AQI 201 - 300
## conc   605 -  804   AQI 301 - 400
## conc   805 - 1004   AQI 401 - 500
bplo <- c(  0, 36, 76,  186,  305, 605, 805,  1005)
bphi <- c( 35, 75, 185,  304, 604, 804, 1004, 99999)
aqi_so2d <- data.frame(bplo, bphi, ilo, ihi, hcat)


## AQI table for 8-hr CO standard
## CO standard (values in ppm) 
## conc      0 -  4.4   AQI   0 - 50
## conc    4.5 -  9.4   AQI  51 - 100
## conc    9.5 - 12.4   AQI 101 - 150
## conc   12.5 - 15.4   AQI 151 - 200
## conc   15.5 - 30.4   AQI 201 - 300
## conc   30.5 - 40.4   AQI 301 - 400
## conc   40.5 - 50.4   AQI 401 - 500
## ppb - awf 13APR2021
bplo <- c(    0,  4450,  9550,  12550,  15550, 30550, 40550, 50450)
bphi <- c( 4449,  9549, 12549,  15549,  30549, 40549, 50449, 99999)
aqi_co8 <- data.frame(bplo, bphi, ilo, ihi, hcat)

#for logical round; round() "rounds to even"
true_round <- function(number, digits) {
  posneg <- sign(number)
  number <- abs(number) * 10 ^ digits
  number <- number + 0.5 + sqrt(.Machine$double.eps)
  number <- trunc(number)
  number <- number / 10 ^ digits
  number * posneg
}

calc_aqi <- function(df, pollutant_column = "pm25", pollutant = "pm25", round = "r") {
  
  if (pollutant == "pm25_est" | pollutant == "filt" | pollutant == "pm25") { 
    aqi_table <- aqi_pm25
    
    pcol <- df[,which(colnames(df) == pollutant_column)]
    if (round == "r")  {df <- df %>% mutate(poll = pcol)  }
    else { df <- df %>% mutate(poll = trunc(pcol*10, 2)/10) }
    
  }
  if (pollutant == "o3" | pollutant == "ozone") { 
    aqi_table <- aqi_o38 
    pcol <- df[,which(colnames(df) == pollutant_column)]
    df <- df %>% mutate(poll = trunc(pcol*1000, 2)/1000)
    
  }
  if (pollutant == "o3_1hour") { 
    aqi_table <- aqi_o3_1hr 
    pcol <- df[,which(colnames(df) == pollutant_column)]
    df <- df %>% mutate(poll = trunc(pcol*1000, 2)/1000)
    
  }
  if (pollutant == "no2" ) { 
    aqi_table <- aqi_no2d 
    pcol <- df[,which(colnames(df) == pollutant_column)]
    df <- df %>% mutate(poll = trunc(pcol, 1))
  }
  if (pollutant == "co" ) { 
    aqi_table <- aqi_co8 
    pcol <- df[,which(colnames(df) == pollutant_column)]
    df <- df %>% mutate(poll = trunc(pcol, 1))
  }
  if (pollutant == "pm10" ) { 
    aqi_table <- aqi_pm10 
    pcol <- df[,which(colnames(df) == pollutant_column)]
    df <- df %>% mutate(poll = trunc(pcol, 1))
  }
  
  df$aqi <- NA
  df$hcat <- NA
  
  len <- length(aqi_table[,1])
  for ( i in 1:len) {
    if (!(is.na(aqi_table$bplo[i]))) {
      bpl <- aqi_table$bplo[i]
      bph <- aqi_table$bphi[i]
      il <- aqi_table$ilo[i]
      ih <- aqi_table$ihi[i]
      hc <- as.character(aqi_table$hcat[i])
      
      
      df$aqi[!is.na(df$poll) & df$poll >= bpl & df$poll <= bph] <- 
        true_round((((df$poll[!is.na(df$poll) & df$poll >= bpl & df$poll <= bph] - bpl) *((ih - il)/(bph - bpl))) + il),0)
      df$hcat[!is.na(df$poll) & df$poll >= bpl & df$poll <= bph] <- hc
      
    }
    
  }
  df$aqi <- true_round(df$aqi, 0)
  df <- df %>% dplyr::select(-poll)
  colnames(df)[which(colnames(df) == "aqi")] <- paste0("aqi_", pollutant)
  colnames(df)[which(colnames(df) == "hcat")] <- paste0("hcat_", pollutant)

  return(df)
}


calc_daily_aqi <- function(df){
  ## check if a column for the daily AQI (based on multiple pollutants) exists.
  ## If it does, delete it, to avoid recursion problems.
  if (length(grep("maqi", colnames(df), fixed=TRUE)) != 0) {
    df <- df[, -(grep("maqi", colnames(df), fixed=TRUE))]
  }
  
  ## calculate the daily AQI - the highest AQI for the day, based on all measured pollutants.
  df$maqi <- apply(df[, grep("aqi", colnames(df))], 1, max, na.rm=TRUE)
  df$maqi <- true_round(df$maqi)
  df$maqi[df$maqi == "-Inf"] <- NA
  
  ## associate a health category with the AQI
  
  df$hcat <- hcatm$hcat[7]
  
  df$hcat[df$maqi <= 50 & !(is.na(df$maqi))] <- hcatm$hcat[1]
  df$hcat[df$maqi > 50 & df$maqi <= 100 ] <- hcatm$hcat[2]
  df$hcat[df$maqi > 100 & df$maqi <= 150 ] <- hcatm$hcat[3]
  df$hcat[df$maqi > 150 & df$maqi <= 200 ] <- hcatm$hcat[4]
  df$hcat[df$maqi > 200 & df$maqi <= 300 ] <- hcatm$hcat[5]
  df$hcat[df$maqi > 300 & !(is.na(df$maqi))] <- hcatm$hcat[6]
  
  return(df)
  
}