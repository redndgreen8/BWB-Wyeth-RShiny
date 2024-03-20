
get_LatLong <- function(residence_str) {
  df <- read.csv(residence_str, stringsAsFactors = FALSE)
  
  # Standardizing location details
  names(df)[1] <- "ID"
  names(df) <- gsub("\\.$", "", names(df))
  df <- df %>% filter(!is.na(currentzip) & currentzip != "")
  df$currentzip <- substr(as.character(df$currentzip), 1, 5)
  df$location <- paste0(df$currentstaddr1, ", ", df$currentcity, ", ", df$currentstate, " ", df$currentzip)
  
  cache_file <- "geocode_cache_census.csv"
  if(file.exists(cache_file)) {
    cache <- read.csv(cache_file, stringsAsFactors = FALSE)
  } else {
    cache <- data.frame(location = character(), currentzip = character(), latitude = numeric(), longitude = numeric(), stringsAsFactors = FALSE)
  }
  
  #$currentzip <- substr(as.character(cache$currentzip), 1, 5)
  
  new_addresses <- setdiff(df$location, cache$location)
  
  if(length(new_addresses) > 0) {
    tryCatch({
      new_addresses_df <- data.frame(location = new_addresses, stringsAsFactors = FALSE)
    #view(new_addresses_df)
      new_geocoded <- tidygeocoder::geocode(new_addresses_df, address  = "location", method = 'census', lat = 'latitude', long = 'longitude')
    
#    new_geocoded$currentzip <- substr(as.character(new_geocoded$location), 1, 5)
      cache <- rbind(cache, new_geocoded)
      write.csv(cache, cache_file, row.names = FALSE)
    }, error = function(e) {
      message("Error during adress code geocoding: ", e$message)
    })
      
  }
  
  # Filter out rows with NA latitudes or longitudes
  lat_longs <- df %>%
    left_join(cache, by = "location")
  
  # Filter out rows with NA latitudes or longitudes
  incomplete_rows <- lat_longs %>%
    filter(is.na(latitude) | is.na(longitude))
  
  
  
  if(nrow(incomplete_rows) > 0) {
    #view(incomplete_rows)
    
    warning(paste0("Some addresses (", nrow(incomplete_rows), ") could not be geocoded. Attempting to geocode with ZIP code only."))
    tryCatch({
      incomplete_rows$location <- as.character(incomplete_rows$currentzip)
      
      new_geocoded_zip <- tidygeocoder::geocode(incomplete_rows, address = "location", method = 'census', lat = 'latitude', long = 'longitude')
      # need to remove lat long to avooid duplicates
      
      
        
      # Filter successful geocoding results
      #view(new_geocoded_zip)
      new_geocoded_zip <- new_geocoded_zip %>% filter(!is.na(lat) & !is.na(long))
      
      # Update cache with new geocoding results
      if(nrow(new_geocoded_zip) > 0) {
        cache <- anti_join(cache, new_geocoded_zip, by = "location")
        cache <- rbind(cache, new_geocoded_zip)
        write.csv(cache, cache_file, row.names = FALSE)
        }
      }, error = function(e) {
        message("Error during ZIP code geocoding: ", e$message)
    })
    
  }
  # Merge the updated cache back into the main data frame
  lat_longs <- df %>%
    left_join(cache, by = "location")
  

  # Handling entries with no successful geocoding
  final_incomplete <- lat_longs %>% filter(is.na(latitude) | is.na(longitude))
  if(nrow(final_incomplete) > 0) {
    warning(paste0("There are ", nrow(final_incomplete), " addresses that could not be geocoded with either method."))
  }
  lat_longs <- lat_longs %>% filter(!is.na(latitude) & !is.na(longitude))
  
  return(lat_longs)
}



#ll <- get_LatLong("HS2100716BodourSalhi-ResidenceHistory_DATA_2024-02-08_1653.csv")
#qmplot(longitude, latitude, data = ll, colour = I("red"), maptype = "toner-lite", zoom = 5)
#qmplot(longitude, latitude, data = ll, colour = I("red"), source = "osm", zoom = 5)
#m_full <- leaflet(data = ll) %>%
#  addTiles() %>%  # This uses OpenStreetMap by default
#  addCircleMarkers(~longitude, ~latitude, color = "red", radius = 3) %>%
#  setView(lng = mean(ll$longitude, na.rm = TRUE), 
#          lat = mean(ll$latitude, na.rm = TRUE), zoom = 5)
#m_full

