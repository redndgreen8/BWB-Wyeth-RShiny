

#df <- read.csv(list.files("residence/", full.names = T))
get_LatLong <- function(residence_str){
  
  df <- read.csv(residence_str, stringsAsFactors = FALSE)
  
  names(df)[1] <- "ID"
  names(df) <- gsub("\\.$", "", names(df))
  df$location <- paste0(df$currentstaddr1, ", ", df$currentcity, ", ", df$currentstate, " ", df$currentzip)
  #df$location <- paste0(df$Current.Street.Address.1, ", ", df$Current.City, ", ", df$Current.State, " ", df$Current.Zip)
  
  cache_file <- "geocode_cache.csv"
  if(file.exists(cache_file)) {
    cache <- read.csv(cache_file, stringsAsFactors = FALSE)
  } else {
    cache <- data.frame(location = character(), latitude = numeric(), longitude = numeric(), stringsAsFactors = FALSE)
  }
  
  # Identify new addresses that are not in the cache
  new_addresses <- setdiff(df$location, cache$location)
  
  # If there are new addresses to geocode, create a new data frame to hold them
  if(length(new_addresses) > 0) {
    # Create a data frame from new addresses
    new_addresses_df <- data.frame(location = new_addresses, stringsAsFactors = FALSE)
    
    # Geocode new addresses
    new_geocoded <- new_addresses_df |>
      tidygeocoder::geocode(location , method = 'osm', lat = 'latitude', long = 'longitude')
    
    # Append new geocoded results to the cache and save it
    cache <- rbind(cache, new_geocoded)
    write.csv(cache, cache_file, row.names = FALSE)
  }
  
  # Combine the original dataframe with the cache to add latitude and longitude
  lat_longs <- df %>%
    left_join(cache, by = "location")
  
  # Check for completeness and errors
  # Here you would check the structure and content of lat_longs to ensure it's as expected
  # Example check: Are there any NAs in latitude or longitude that shouldn't be there?
  incomplete_rows <- lat_longs %>%
    filter(is.na(latitude) | is.na(longitude))
  
  if(nrow(incomplete_rows) > 0) {
    warning(paste0("Some addresses (", nrow(incomplete_rows),") could not be geocoded and have NA for latitude or longitude."))
  }
  # If incomplete_rows is not empty, there were some addresses that could not be geocoded
  
  # Return or print the final dataframe
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

