source("str_list.R")

get_LatLong <- function(residence_str) {
  df <- read.csv(residence_str, stringsAsFactors = FALSE)
  
  # Standardizing location details
  names(df)[1] <- "ID"
  names(df) <- gsub("\\.$", "", names(df))
  df <- df %>% filter(!is.na(currentzip) & currentzip != "")
  df$currentzip <- substr(as.character(df$currentzip), 1, 5)
  df$location <- paste0(df$currentstaddr1, ", ", df$currentcity, ", ", df$currentstate, " ", df$currentzip)
  
  cache_file <- "geocode_cache_gis.csv"
  if(file.exists(cache_file)) {
    cache <- read.csv(cache_file, stringsAsFactors = FALSE)
  } else {
    cache <- data.frame(location = character(), currentzip = character(), latitude = numeric(), longitude = numeric(), stringsAsFactors = FALSE)
  }
  

  new_addresses <- setdiff(df$location, cache$location)
  
  if(length(new_addresses) > 0) {
    tryCatch({
      new_addresses_df <- data.frame(location = new_addresses, stringsAsFactors = FALSE)
    #view(new_addresses_df)
      new_geocoded <- tidygeocoder::geocode(new_addresses_df, address  = "location", method = 'arcgis', lat = 'latitude', long = 'longitude')
    
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



ll <- get_LatLong(dx_str)


# Function to check if a coordinate is in Southern California
is_in_socal <- function(lat, lon) {
  # Define the bounding box for Southern California
  # These are approximate values and can be adjusted as needed
  socal_north <- 35.5  # Approximate northern border
  socal_south <- 32  # Approximate southern border (US-Mexico border)
  socal_east <- -114.1 # Approximate eastern border
  socal_west <- -120 # Pacific Ocean
  
  return(lat >= socal_south && lat <= socal_north &&
           lon <= socal_east && lon >= socal_west)
}

# Function to count coordinates in Southern California
count_socal_coordinates <- function(latitudes, longitudes) {
  if (length(latitudes) != length(longitudes)) {
    stop("The number of latitudes and longitudes must be the same.")
  }
  
  socal_count <- sum(mapply(is_in_socal, latitudes, longitudes))
  
  return(socal_count)
}

# Example usage:
#latitudes <- c(34.0522, 37.7749, 32.7157, 36.1699, 33.8121)
#longitudes <- c(-118.2437, -122.4194, -117.1611, -115.1398, -117.9190)

result <- count_socal_coordinates(ll$latitude, ll$longitude)
print(paste("Number of coordinates in Southern California:", result))

