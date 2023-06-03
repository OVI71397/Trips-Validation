library(tidyverse)
library(rvest)

train_routes <- read_csv("Datasets/train_routes_all.csv") %>%
  select(from_city,
         to_city) %>%
  distinct()
#TRAINLINE
train_routes_loop <- train_routes
train_routes_loop$from_city <- tolower(train_routes_loop$from_city) 
train_routes_loop$from_city <- str_replace_all(train_routes_loop$from_city, " ", "-")
train_routes_loop$to_city <- tolower(train_routes_loop$to_city)
train_routes_loop$to_city <- str_replace_all(train_routes_loop$to_city, " ", "-")

url <- paste0("https://www.thetrainline.com/en/train-times/", train_routes_loop$from_city, "-to-", train_routes_loop$to_city)
train_outcome_prices <- tibble()
for (i in 1:length(url)){
  page <- read_html(url[i])
  text <- page %>% 
    html_nodes("#pageHeader .price-tag-body") %>%
    html_text() 
  df <- tibble(url = url[i],
               price = text)
  train_outcome_prices <- rbind(train_outcome_prices, df)
}
which(train_routes$from_city == "Doncaster" & train_routes$to_city == "Birmingham")
tail(train_outcome_prices)
run1 <- train_outcome_prices
train_routes <- cbind(train_routes, url)
run1_df <- left_join(train_routes, run1, by = "url")

write_csv(run1_df, "Datasets/train_prices_from_trainline.csv")


################################################################################
# Scrapping continuation
scrapped_trains <- read_csv("Datasets/train_prices_from_trainline.csv")
scrapped_trains_sub <- scrapped_trains %>%
  filter(price %in% NA)
url2 <- scrapped_trains_sub$url
train_outcome_prices <- tibble()
for (i in 1:length(url2)){
  page <- read_html(url2[i])
  text <- page %>% 
    html_nodes("#pageHeader .price-tag-body") %>%
    html_text() 
  df <- tibble(url = url2[i],
               price = text)
  train_outcome_prices <- rbind(train_outcome_prices, df)
}

tail(train_outcome_prices)

run2 <- train_outcome_prices
scrapped_trains_to_join <- scrapped_trains %>%
  select(from_city:url)
run2_named <- left_join(run2, scrapped_trains_to_join, by = "url")
run2_named <- run2_named %>%
  select(from_city, to_city, url, price)

full_price_df <- rbind(scrapped_trains, run2_named) %>%
  filter(!(price %in% NA))
full_price_df$price <- as.numeric(gsub("from €", "", full_price_df$price))

################################################################################
#Validation
train2 <- read_csv("Datasets/all_direct_routes_2_run.csv") %>%
  filter(transport_id == 3)
train3 <- read_csv("Datasets/all_direct_routes_3_run.csv") %>%
  filter(transport_id == 3)
train4 <- read_csv("Datasets/all_direct_routes_for_validation_4run.csv") %>%
  filter(transport_id == 3) %>%
  select(-distance_km,
         -frequency_tpw,
         -num_transfers)
train_comb <- rbind(train2, train3, train4) %>%
  distinct()

location <- readxl::read_excel("Datasets/locations_with_id.xlsx", 
                                range = "A1:H775", col_names = FALSE) %>%
  rename(id_city = ...1, 
         city = ...2, 
         id_country = ...3, 
         country = ...8,
         latitude = ...4,
         longitude = ...5) %>%
  select(id_city, city, id_country, country, latitude, longitude)
locations_from <- location %>%
  rename(from_id = id_city, from_city = city,
         from_country_id = id_country, from_country = country,
         from_latitude = latitude, from_longitude = longitude)
locations_to <- location %>%
  rename(to_id = id_city, to_city = city,
         to_country_id = id_country, to_country = country,
         to_latitude = latitude, to_longitude = longitude)
connected_from <- left_join(train_comb, locations_from, by = "from_id")
connected_to <- left_join(connected_from, locations_to, by = "to_id")
train_to_valid <- left_join(connected_to, full_price_df, by = c("from_city" = "from_city",
                                                                "to_city" = "to_city"))

train_to_valid <- train_to_valid %>%
  mutate(valid = price_min_EUR >= price)
train_to_valid %>%
  select(price_min_EUR,
         price,
         valid) 
train_suspicious <- train_to_valid %>%
  filter(valid == FALSE) %>%
  mutate(percent_decline = abs(price_min_EUR-price)/price) %>%
  mutate(invalid = (percent_decline > 0.15))

train_suspicious %>%
  filter(invalid == TRUE)%>%
  count()
train_suspicious %>%
  filter(invalid == TRUE) %>%
  select(price_min_EUR, price) %>%
  View()

invalid_correction <- train_suspicious %>%
  filter(invalid == TRUE) %>%
  select(-price_min_EUR) %>%
  mutate(price_min_EUR = round(price,0)) %>%
  select(from_id:transport_id, price_min_EUR, duration_min:url)
valid_part1 <- train_to_valid %>%
  filter(valid == TRUE) %>%
  select(-price,
         -valid)
valid_part2 <- train_suspicious %>%
  filter(invalid == FALSE) %>%
  select(-percent_decline,
         -invalid,
         -price,
         -valid)
unknown_valid <- train_to_valid %>%
  filter(valid %in% NA) 
train_validated_dataset <- rbind(valid_part1, valid_part2, invalid_correction)

train_europe_valid <- train_validated_dataset %>%
  filter((from_id %in% c(100:386)) & (to_id %in% c(100:386)))

write_csv(train_europe_valid, "Datasets/train_europe_validated.csv")



