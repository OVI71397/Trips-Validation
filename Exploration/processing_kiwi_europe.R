library(tidyverse)

flights <- read_csv("kiwi_europe_country_flights.csv")
flights_cheap <- flights %>%
  group_by(cityFrom, cityTo) %>%
  slice(which.min(price_EUR))
flights_cheap %>%
  group_by(cityFrom, cityTo) %>%
  count() %>%
  arrange(desc(n))
write_csv(flights_cheap, "kiwi_europe_country_flights_processed.csv")


bus <- read_csv("kiwi_europe_country_bus.csv")
bus_cheap <- bus %>%
  group_by(cityFrom, cityTo) %>%
  slice(which.min(price_EUR))
bus_cheap %>%
  group_by(cityFrom, cityTo) %>%
  count() %>%
  arrange(desc(n))
write_csv(bus_cheap, "kiwi_europe_country_bus_processed.csv")


train <- read_csv("kiwi_europe_country_train.csv")
train_cheap <- train %>%
  group_by(cityFrom, cityTo) %>%
  slice(which.min(price_EUR))
train_cheap %>%
  group_by(cityFrom, cityTo) %>%
  count() %>%
  arrange(desc(n))
write_csv(train_cheap, "kiwi_europe_country_train_processed.csv")


# Europe list of cities
routs <- rbind(flights_cheap, bus_cheap, train_cheap) %>%
  select(cityFrom, cityTo)
cities1 <- tibble(city = routs$cityFrom)
cities2 <- tibble(city = routs$cityTo)
cities <- rbind(cities1, cities2) %>%
  distinct()
