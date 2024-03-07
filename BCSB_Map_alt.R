
get_LatLong <- function(residence_str) {
  df <- read.csv(residence_str, stringsAsFactors = FALSE)
  
  # Standardizing location details
  names(df)[1] <- "ID"
  names(df) <- gsub("\\.$", "", names(df))
  df <- df %>% filter(!is.na(currentzip) & currentzip != "")
  df$currentzip <- substr(as.character(df$currentzip), 1, 5)
  df$location <- paste0(df$currentstaddr1, ", ", df$currentcity, ", ", df$currentstate, " ", df$currentzip)
  
  # Geocode using full addresses
  df_full_geocoded <- tryCatch({
    tidygeocoder::geocode(df, address = "location", method = 'census', lat = 'latitude', long = 'longitude')
  }, error = function(e) {
    message("Error during full address geocoding: ", e$message)
    return(df)  # Return original df on error
  })
  
  # Filter out rows with NA latitudes or longitudes for re-geocoding using ZIP codes
  incomplete_rows <- df_full_geocoded %>% filter(is.na(latitude) | is.na(longitude))
  
  if(nrow(incomplete_rows) > 0) {
    message(paste0("Some addresses (", nrow(incomplete_rows), ") could not be geocoded. Attempting to geocode with ZIP code only."))
    incomplete_rows$location <- as.character(incomplete_rows$currentzip)
    
    tryCatch({
      new_geocoded_zip <- tidygeocoder::geocode(incomplete_rows, address = "location", method = 'census', lat = 'latitude', long = 'longitude')
      
      # Replace the NA geocode results in df_full_geocoded with new ZIP-based results
      for (i in 1:nrow(new_geocoded_zip)) {
        if (!is.na(new_geocoded_zip$latitude[i]) & !is.na(new_geocoded_zip$longitude[i])) {
          df_full_geocoded[df_full_geocoded$ID == new_geocoded_zip$ID[i], c("latitude", "longitude")] <- new_geocoded_zip[i, c("latitude", "longitude")]
        }
      }
    }, error = function(e) {
      message("Error during ZIP code geocoding: ", e$message)
    })
  }
  
  # Final filtering to remove any entries that couldn't be geocoded
  df_final <- df_full_geocoded %>% filter(!is.na(latitude) & !is.na(longitude))
  
  if(nrow(df_final) < nrow(df)) {
    message("Some addresses could not be geocoded even with ZIP code fallback.")
  }
  
  return(df_final)
}




ll <- get_LatLong("HS2100716BodourSalhi-ResidenceHistory_DATA_2024-02-08_1653.csv")
#qmplot(longitude, latitude, data = ll, colour = I("red"), maptype = "toner-lite", zoom = 5)
#qmplot(longitude, latitude, data = ll, colour = I("red"), source = "osm", zoom = 5)
#m_full <- leaflet(data = ll) %>%
#  addTiles() %>%  # This uses OpenStreetMap by default
#  addCircleMarkers(~longitude, ~latitude, color = "red", radius = 3) %>%
#  setView(lng = mean(ll$longitude, na.rm = TRUE), 
#          lat = mean(ll$latitude, na.rm = TRUE), zoom = 5)
#m_full

