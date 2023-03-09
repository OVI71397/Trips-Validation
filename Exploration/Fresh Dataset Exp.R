library(tidyverse)
library(readxl)
library(dbscan)
library(caret)
library(GGally)
library(corrplot)
library(ClusterR)
library(cluster)


#Exploration
trips_2 <- read_csv("all_direct_routes_validation.csv")
sum(is.na(trips_2))
summary(trips_2)
trips_2 <- trips_2 %>%
  distinct(.keep_all = FALSE) %>%
  filter(frequency_tpw != -1)
#-----------------------------------------------------------------------------
Full_list_with_countries <- read_excel("locations_with_id.xlsx", 
                                       range = "A1:H775", col_names = FALSE)
location <- Full_list_with_countries
location <- location %>%
  rename(id_city = ...1, city = ...2, id_country = ...3, country = ...8) %>%
  select(id_city, city, id_country, country)
locations_from <- location %>%
  rename(from_id = id_city, from_city = city,
         from_country_id = id_country, from_country = country)
locations_to <- location %>%
  rename(to_id = id_city, to_city = city,
         to_country_id = id_country, to_country = country)
#-----------------------------------------------------------------------------
connected_from_2 <- left_join(trips_2, locations_from, by = "from_id")
connected_to_2 <- left_join(connected_from_2, locations_to, by = "to_id")
trips_general_2 <- connected_to_2
#-----------------------------------------------------------------------------
trips_general_2 <- trips_general_2 %>%
  distinct(.keep_all = FALSE)
trips_general_2$transport_id <- as.factor(trips_general_2$transport_id)
summary(trips_general_2)  
#-----------------------------------------------------------------------------
flight <- trips_general_2 %>%
  filter(transport_id == 1)
ggplot(flight) +
  geom_point(aes(x = duration_min, y = price_min_EUR))
flight <- trips_general_2 %>%
  filter(price_min_EUR < 20000)
ggplot(flight) +
  geom_point(aes(x = duration_min, y = price_min_EUR))
flights_subset <- flight %>%
  filter((from_id %in% c(100:386)) & (to_id %in% c(100:386)))
ggplot(flights_subset) +
  geom_point(aes(x = duration_min, y = price_min_EUR))
summary(flights_subset)
#-----------------------------------------------------------------------------
bus <- trips_general_2 %>%
  filter(transport_id == 2) %>%
  filter((from_id %in% c(100:386)) & (to_id %in% c(100:386)))
ggplot(bus) +
  geom_point(aes(x = duration_min, y = price_min_EUR))
ggplot(bus) +
  geom_point(aes(x = distance_km, y = price_min_EUR))
ggplot(bus) +
  geom_point(aes(x = frequency_tpw, y = price_min_EUR))
ggplot(bus) +
  geom_histogram(aes(x = frequency_tpw))
ggplot(bus) +
  geom_histogram(aes(x = distance_km))
ggplot(bus) +
  geom_histogram(aes(x = duration_min))
ggplot(bus) +
  geom_histogram(aes(x = price_min_EUR))

#EDA
df_bus <- bus %>%
  select(price_min_EUR,
         duration_min,
         distance_km,
         frequency_tpw)
summary(df_bus)
abnormal_frequency <- df_bus %>%
  filter(frequency_tpw > 20)

ggplot(abnormal_frequency) +
  geom_point(aes(x = duration_min, y = price_min_EUR))
ggpairs(df_bus)
ggpairs(abnormal_frequency)
col <- colorRampPalette(c("black", "darkgray", "gray", "yellow"))
corrplot(cor(df_bus), 
         method = "ellipse", 
         col = col(100), 
         addCoef.col = "black", 
         tl.col = "black")
corrplot(cor(abnormal_frequency), 
         method = "ellipse", 
         col = col(100), 
         addCoef.col = "black", 
         tl.col = "black")

#Density clustering
scale <- preProcess(df_bus, method = "range")
df_bus <- predict(scale, df_bus)
set.seed(876)
hdb <- hdbscan(df_bus[, 1:4], 
               minPts = 10)
hullplot(df_bus[, 1:4], hdb$cluster)
df_bus <- df_bus %>%
  mutate(cluster_hdb = hdb$cluster)

withou_noise <- df_bus %>%
  filter(cluster_hdb != 0)
ggplot(df_bus) +
  geom_point(aes(x = duration_min, y = price_min_EUR, color = factor(cluster_hdb)))

kNNdistplot(df_bus[,1:4], k=5)
abline(h=0.05, col="red")
set.seed(174)
db <- dbscan(df_bus[,1:4], 
             eps = 0.05, 
             minPts = 10,
             borderPoints = TRUE)
hullplot(df_bus[,1:4], db$cluster)
df_bus <- df_bus %>%
  mutate(cluster_db = db$cluster)
withou_noise2 <- df_bus %>%
  filter(cluster_db != 0)
ggplot(withou_noise2) +
  geom_point(aes(x = duration_min, y = price_min_EUR, color = factor(cluster_db)))

#GMM
gmm_model <- GMM(df_bus[,1:4],
                 gaussian_comps = 50,
                 dist_mode = "maha_dist",
                 km_iter = 10,
                 em_iter = 10)
pred_gmm <- predict(gmm_model, df_bus[,1:4])
df_bus <- df_bus %>%
  mutate(cluster_gmm = pred_gmm)
ggplot(df_bus) +
  geom_point(aes(x = duration_min, y = price_min_EUR, color = factor(cluster_gmm)))

opt_gmm <- Optimal_Clusters_GMM(df_bus[,1:4], 
                                max_clusters = 50, 
                                criterion = "BIC", 
                                dist_mode = "maha_dist",
                                km_iter = 10, 
                                em_iter = 10, 
                                plot_data = T)
#-----------------------------------------------------------------------------
train <- trips_general_2 %>%
  filter(transport_id == 3)
ggplot(train) +
  geom_point(aes(x = duration_min, y = price_min_EUR))
