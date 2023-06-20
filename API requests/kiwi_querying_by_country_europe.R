library(tidyverse)
library(httr)

# Get Country Codes
c_code <- GET("http://api.travelpayouts.com/data/en/countries.json")
c_code_resp <- jsonlite::fromJSON(rawToChar(c_code$content))
country_code_europe <- c_code_resp %>%
  select(code:name) %>%
  filter(name %in% c("Germany", "United Kingdom", "France", "Italy", "Spain",
                     "Ukraine", "Poland", "Romania", "Netherlands", "Belgium",
                     "Czech Republic", "Greece", "Portugal", "Sweden", "Hungary",
                     "Austria", "Serbia", "Switzerland", "Bulgaria", "Denmark",
                     "Finland", "Slovakia", "Norway", "Ireland", "Croatia", 
                     "Moldova", "Bosnia and Herzegovina", "Albania", "Lithuania", "Slovenia",
                     "North Macedonia", "Latvia", "Estonia", "Montenegro", "Luxembourg",
                     "Malta", "Iceland", "Andorra", "Monaco", "Liechtenstein",
                     "San Marino", "Russia", "Belarus"))
europe_routes <- tibble(from_code = country_code_europe$code,
                        from_country = country_code_europe$name,
                        to_code = country_code_europe$code,
                        to_country = country_code_europe$name)
e1 <- europe_routes %>% select(from_code, from_country)
e2 <- europe_routes %>% select(to_code, to_country)
generated_routes <- expand.grid(europe_routes$from_code, europe_routes$to_code)
generated_routes <- generated_routes %>%
  rename(from_code = Var1,
         to_code = Var2)
routes_df_europe <- inner_join(generated_routes, e1, by = "from_code")  
routes_df_europe <- inner_join(routes_df_europe, e2, by = "to_code")


# Kiwi Requests
headers <- c('accept' = 'application/json', 
            'apikey' = Sys.getenv("kiwi_key"))
links_plane <- paste0("https://api.tequila.kiwi.com/v2/search?fly_from=", routes_df_europe$from_code, 
                      "&fly_to=", routes_df_europe$to_code, 
                      "&date_from=30%2F06%2F2023&date_to=30%2F12%2F2023&curr=EUR&vehicle_type=aircraft&limit=1000") 
links_bus <- paste0("https://api.tequila.kiwi.com/v2/search?fly_from=", routes_df_europe$from_code, 
                    "&fly_to=", routes_df_europe$to_code, 
                    "&date_from=30%2F06%2F2023&date_to=30%2F12%2F2023&curr=EUR&vehicle_type=bus&limit=1000") 
links_train <- paste0("https://api.tequila.kiwi.com/v2/search?fly_from=", routes_df_europe$from_code, 
                      "&fly_to=", routes_df_europe$to_code, 
                      "&date_from=30%2F06%2F2023&date_to=30%2F12%2F2023&curr=EUR&vehicle_type=train&limit=1000") 

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
         countryFromCode = resp_df$countryFrom$code,
         countryToCode = resp_df$countryTo$code,
         nightsInDest = resp_df$nightsInDest,
         distance = resp_df$distance,
         durationDepatrure = resp_df$duration$departure,
         durationReturn = resp_df$duration$return,
         price = resp_df$price,
         price_EUR = resp_df$conversion$EUR,
         oneBagPrice = resp_df$bags_price$`1`,
         bagLimitHeight = resp_df$baglimit$hand_height,
         bagLimitLength = resp_df$baglimit$hand_length,
         bagLimitWidth = resp_df$baglimit$hand_width,
         bagLimitWeight = resp_df$baglimit$hand_weight,
         airlines = resp_df$airlines,
         UTCarrival = resp_df$utc_arrival,
         UTCdeparture = resp_df$utc_departure,
         throwAwayTicketing = resp_df$throw_away_ticketing,
         hiddenCityTicketing = resp_df$hidden_city_ticketing,
         virtualInterlining = resp_df$virtual_interlining,
         pnrCount = resp_df$pnr_count,
         hasAirportChange = resp_df$has_airport_change)
}

api_call_vital <- function(url) {
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
         durationDepatrure = resp_df$duration$departure,
         durationReturn = resp_df$duration$return,
         price_EUR = resp_df$conversion$EUR,
         airlines = resp_df$airlines,
         UTCarrival = resp_df$utc_arrival,
         UTCdeparture = resp_df$utc_departure,
         bookLink = resp_df$deep_link)
}



# FLIGHTS
dataset_of_flights <- tibble()
for (i in 1:length(links_plane)) {
  data <- api_call_vital(links_plane[i])
  Sys.sleep(2.0)
  dataset_of_flights <- rbind(dataset_of_flights, data)
}

call_flights <- dataset_of_flights
tail(dataset_of_flights)
write_csv(dataset_of_flights, "kiwi_europe_country_flights.csv")


# BUS
dataset_of_bus <- tibble()
for (i in 1:length(links_bus)) {
  data <- api_call_vital(links_bus[i])
  Sys.sleep(2.0)
  dataset_of_bus <- rbind(dataset_of_bus, data)
}

write_csv(dataset_of_bus, "kiwi_europe_country_bus.csv")

# TRAIN
dataset_of_train <- tibble()
for (i in 1:length(links_train)) {
  data <- api_call_vital(links_train[i])
  Sys.sleep(2.0)
  dataset_of_train <- rbind(dataset_of_train, data)
}

write_csv(dataset_of_train, "kiwi_europe_country_train.csv")


