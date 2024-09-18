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


# Function to check if a coordinate is within a specified region
is_in_region <- function(lat, lon, north, south, east, west) {
  return(lat >= south && lat <= north &&
           lon >= west && lon <= east)
}

# Function to count coordinates in a specified region
count_coordinates <- function(latitudes, longitudes, region) {
  if (length(latitudes) != length(longitudes)) {
    stop("The number of latitudes and longitudes must be the same.")
  }
  
  count <- sum(mapply(is_in_region, latitudes, longitudes,
                      MoreArgs = list(north = region$north,
                                      south = region$south,
                                      east = region$east,
                                      west = region$west)))
  
  return(count)
}

# Define regions
regions <- list(
  "Southern California" = list(north = 35.5, south = 32, east = -114.1, west = -120),
  "Los Angeles" = list(north = 34.3373, south = 33.7036, east = -118.1553, west = -118.6682)#,
#  "Northern California" = list(north = 42, south = 35.5, east = -114.1, west = -124.4),
#  "Whole California" = list(north = 42, south = 32.5, east = -114.1, west = -124.4),
#  "Outside California (US)" = list(north = 49, south = 24.5, east = -66.9, west = -124.4),
#  "Canada" = list(north = 83, south = 41.7, east = -52.6, west = -141)
)

# Function to count coordinates for all defined regions
count_all_regions <- function(latitudes, longitudes) {
  results <- sapply(names(regions), function(region_name) {
    count_coordinates(latitudes, longitudes, regions[[region_name]])
  })
  
  data.frame(Region = names(regions), Count = results)
}

# Example usage:
# Assuming ll is your dataframe with latitude and longitude columns
# ll <- data.frame(latitude = c(34.0522, 37.7749, 32.7157, 36.1699, 33.8121),
#                  longitude = c(-118.2437, -122.4194, -117.1611, -115.1398, -117.9190))

results_table <- count_all_regions(ll$latitude, ll$longitude)

# Print the results table
print(results_table)

#If you want a more formatted output, you can use knitr::kable
 if (require(knitr)) {
   print(kable(results_table, format = "pipe", caption = "Coordinate Counts by Region"))
 }

count_la_zips <- sum(grepl("^9(00[0-9]{2}|01[0-9]{2}|02[0-9]{2}|03[0-5]{2}|04[0-9]{2}|05[0-9]{2}|06[0-6]{2}|07[0-3]{2}|08[0-8]{2}|10[0-9]{2}|11[0-9]{2}|12[0-9]{2}|13[0-9]{2}|14[0-9]{2}|15[0-9]{2}|16[0-9]{2})$", ll$currentzip))
count_la_zips

