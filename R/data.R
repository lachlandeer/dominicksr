#' Locations of Dominick's Finer Food stores
#'
#' A dataset containing address and price tier data of
#' the 86 stores that contain complete address information
#' from Kilt's Dominick's Finer Foods data.
#'
#' @format A data frame with 86 rows and 9 variables:
#' \describe{
#'   \item{store_id}{Store identifier to link to sales data}
#'   \item{city}{City in which store is located}
#'   \item{price_tier}{Store category, within category prices are
#'   meant to be uniform; categories: cub_fighter, low, medium, and high}
#'   \item{zone}{Pricing zone. When the Dominick's data was collected, DFF priced products by 16 zones.
#'   Within each zone, there was supposed to be a uniform regular price (promoted prices are the same, chainwide).
#'   Some stores in the data do not have a zone assignment as these stores came on-line after 1992 when the zone information was obtained}
#'   \item{zip}{Store's zip code}
#'   \item{address}{Street name and number for the store}
#'   \item{latitude}{Latitude of store location, from google maps}
#'   \item{longitude}{Longitude of store location, from google maps}
#'   \item{geo_address}{Address of the store based on latitude and longitude}
#' }
#' @source pages 14--19 of \url{https://research.chicagobooth.edu/-/media/faculty/docs/dominicks-manual-and-codebook_kiltscenter2013.pdf?la=en&hash=955F55CE727ED5790C6D7B4F53E40A3DFA3C99DD}
"store_locations"
