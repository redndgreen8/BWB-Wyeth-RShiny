

df <- read.csv(list.files("residence/", full.names = T))
names(df)[1] <- "ID"
names(df) <- gsub("\\.$", "", names(df))
# df$location <- paste0(df$currentstaddr1, ", ", df$currentcity, ", ", df$currentstate, " ", df$currentzip)
df$location <- paste0(df$Current.Street.Address.1, ", ", df$Current.City, ", ", df$Current.State, " ", df$Current.Zip)



lat_longs <- df |> 
  tidygeocoder::geocode(location, method = 'osm', lat = latitude, long = longitude)

qmplot(longitude, latitude, data = lat_longs, colour = I("red"), maptype = "toner-lite", zoom = 5)
ggsave("plots/BCSB_locations.png", height = 6, width = 6, dpi = 600)

lat_longs_CUS <- lat_longs |> 
  filter(!Current.State %in% c("AK", "HI", "Alaska", "Hawaii"))
qmplot(longitude, latitude, data = lat_longs_CUS, colour = I("red"), maptype = "toner-lite", zoom = 5)
ggsave("plots/BCSB_locations_CUS.png", height = 6, width = 6, dpi = 600)

lat_longs_CA <- lat_longs[grepl("^ca", ignore.case = T, lat_longs$Current.State), ]
qmplot(longitude, latitude, data = lat_longs_CA, colour = I("red"), maptype = "toner-lite", zoom = 8)
ggsave("plots/BCSB_locations_CA.png", height = 6, width = 6, dpi = 600)

lat_longs_LA <- lat_longs[lat_longs$Current.Zip %in% 91001:93000, ]
qmplot(longitude, latitude, data = lat_longs_LA, colour = I("red"), maptype = "toner-lite", zoom = 10)
ggsave("plots/BCSB_locations_LA.png", height = 6, width = 6, dpi = 600)
