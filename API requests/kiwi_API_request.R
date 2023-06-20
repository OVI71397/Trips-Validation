library(httr)
library(tidyverse)


headers = c('accept' = 'application/json', 
            'apikey' = Sys.getenv("kiwi_key"))
api_call <- function(url) {
  query <- GET(url, add_headers(.headers=headers))
  resp <- jsonlite::fromJSON(rawToChar(query$content))
  resp_df <- resp$data
  tibble(flyFrom = resp_df$flyFrom,
         flyTo = resp_df$flyTo,
         cityFrom = resp_df$cityFrom,
         cityTo = resp_df$cityTo,
         countryFrom = resp_df$countryFrom$name,
         countryTo = resp_df$countryTo$name,
         distance = resp_df$distance,
         duration = resp_df$duration$departure,
         price = resp_df$price,
         price_EUR = resp_df$conversion$EUR)
}


flight_routes <- read_csv("flight_routes_europe.csv") %>%
  select(from_code, to_code) %>%
  distinct()


links_to_call <- paste0("https://api.tequila.kiwi.com/v2/search?fly_from=", flight_routes$from_code, "&fly_to=", flight_routes$to_code, "&date_from=01%2F06%2F2023&date_to=30%2F11%2F2023&flight_type=oneway&adults=1&partner_market=en&curr=EUR&vehicle_type=aircraft&limit=2")
length(links_to_call)
dataset_of_flights <- tibble()
for (i in 1:length(links_to_call)) {
  data <- api_call(links_to_call[i])
  Sys.sleep(2.0)
  dataset_of_flights <- rbind(dataset_of_flights, data)
}

write_csv(dataset_of_flights, "kiwi_flights_07_06.csv")



# Example of the queries for buses/trains
url3 <- paste0("https://api.tequila.kiwi.com/v2/search?fly_from=madrid_es&fly_to=lisbon_pt&date_from=01%2F06%2F2023&date_to=30%2F11%2F2023&curr=EUR&vehicle_type=train")
url3 <- paste0("https://api.tequila.kiwi.com/v2/search?fly_from=amsterdam_nl&fly_to=barcelona_es&date_from=01%2F06%2F2023&date_to=30%2F11%2F2023&curr=EUR&vehicle_type=bus")
