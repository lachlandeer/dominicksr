# tidy up the raw store location data

library(magrittr)

raw_data <- readr::read_csv("data-raw/store_locations.csv")
open_stores <- raw_data[!grepl("closed", raw_data$address),]

# prepare address string for a geocode search
open_stores <-  open_stores %>%
                dplyr::mutate(
                            address = stringr::str_replace_all(address, "[[:punct:]]", ""),
                            full_address = stringr::str_c(zip, address, city, "IL" ,sep = " ")
                                )

# initialize columns of NAs to fill with geo information
open_stores <- open_stores %>%
    dplyr::mutate(
                lat = NA,
                lon = NA,
                geoAddress = NA
    )

# geocode using ggmap

# Loop through the addresses to get the latitude and longitude of each address and add it to the
for(i in 1:nrow(open_stores)) {
    # ggmap::geocode returns missings, keep trying until it fills up a row
    while (is.na(open_stores$lon[i])) {
        result <- tryCatch(ggmap::geocode(open_stores$full_address[i], output = "latlona", source = "google"),
                           warning = function(w) data.frame(lon = NA, lat = NA, address = NA))
        open_stores$lon[i] <- as.numeric(result[1])
        open_stores$lat[i] <- as.numeric(result[2])
        open_stores$geoAddress[i] <- as.character(result[3])
    } # end while
} # end for


# tidy up
store_locations <- open_stores %>%
        dplyr::select(-full_address) %>%
        dplyr::rename(store_id = store,
                      geo_address = geoAddress,
                      latitude = lat,
                      longtitude = lon)

# save
readr::write_csv(store_locations, "data-raw/store_locations.csv")
devtools::use_data(store_locations, overwrite = TRUE, compress = 'xz')
