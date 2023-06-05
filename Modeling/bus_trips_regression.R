library(tidyverse)
library(caret)
library(readxl)
library(forcats)
library(doParallel)
library(DataExplorer)
library(GGally)


#-------------------------------------------------------------------------------
bus_valid <- read_excel("Datasets/buses_valid_trips.xlsx")
bus_invalid <- read_excel("Datasets/buses_invalid_trips.xlsx")
#bus_min_price <- bus_valid %>%
#  group_by(from_id, to_id) %>%
#  summarise(price_min_EUR = min(price_min_EUR)) %>%
#  ungroup()
#bus_valid_min <- inner_join(bus_min_price, bus_valid,
#                           by=c("from_id", "to_id", "price_min_EUR")) 
#bus_valid %>%
#  filter(from_id == 100 & to_id == 172) %>%
#  View()

bus <- rbind(bus_valid, bus_invalid)
countries <- read_excel("Datasets/locations_with_id.xlsx", 
                        range = "A1:H775", 
                        col_names = FALSE) %>%
  rename(id_city = ...1, 
         city = ...2, 
         id_country = ...3, 
         country = ...8,
         latitude = ...4,
         longitude = ...5) %>%
  select(id_city, 
         city, 
         id_country, 
         country, 
         latitude, 
         longitude)
locations_from <- countries %>%
  rename(from_id = id_city, from_city = city,
         from_country_id = id_country, from_country = country,
         from_latitude = latitude, from_longitude = longitude)
locations_to <- countries %>%
  rename(to_id = id_city, to_city = city,
         to_country_id = id_country, to_country = country,
         to_latitude = latitude, to_longitude = longitude)
connection_from <- left_join(bus, locations_from, by = c("from_id" = "from_id"))
connection_to <- left_join(connection_from, locations_to, by = c("to_id" = "to_id"))
bus <- connection_to %>%
  select(price_min_EUR,
         duration_min,
         distance_km,
         from_latitude,
         from_longitude,
         to_latitude,
         to_longitude,
         Predicted_Outcome)
bus_for_modeling <- bus
summary(bus_for_modeling)

bus_for_modeling <- bus_for_modeling %>%
  mutate(log_price = log(price_min_EUR),
         log_duration = log(duration_min),
         log_dist = log(distance_km)) %>%
  select(-price_min_EUR,
         -duration_min,
         -distance_km)
summary(bus_for_modeling)


lat <- preProcess(bus_for_modeling[,c(1,3)], method = "range",
                  rangeBounds = c(0.49, 1.01))
bus_for_modeling[,c(1,3)] <- predict(lat, newdata = bus_for_modeling[,c(1,3)])
long <- preProcess(bus_for_modeling[,c(2,4)], method = "range",
                   rangeBounds = c(-0.15, 0.66))
bus_for_modeling[,c(2,4)] <- predict(long, newdata = bus_for_modeling[,c(2,4)])


valid <- bus_for_modeling %>%
  filter(Predicted_Outcome == 0) %>%
  select(-Predicted_Outcome)
invalid <- bus_for_modeling %>%
  filter(Predicted_Outcome == 1) %>%
  select(-Predicted_Outcome)

#-------------------------------------------------------------------------------
#Split
set.seed(147)
i <- createDataPartition(valid$log_price, p = .8, list = FALSE)
train <- valid[ i,]
test  <- valid[-i,]
#Scale
scale_logs <- preProcess(train[, c(6,7)], method = c("center", "scale"))
train[,c(6,7)] <- predict(scale_logs, newdata = train[,c(6,7)])
test[,c(6,7)] <- predict(scale_logs, newdata = test[,c(6,7)])
invalid[,c(6,7)] <- predict(scale_logs, newdata = invalid[,c(6,7)])
#-------------------------------------------------------------------------------
#EDA
plot_histogram(train)
plot_density(train)
plot_correlation(train)
ggpairs(train[, c(1:7)])
ggplot(data = train, aes(x = log_duration, y = log_price)) +
  geom_point()
ggplot(data = train, aes(x = log_dist, y = log_price)) +
  geom_point()
#-------------------------------------------------------------------------------
#Modeling
control <- trainControl(method = "cv", 
                        number = 10,
                        verboseIter = TRUE,
                        allowParallel = TRUE)


model_gam_loess <- train(log_price ~ ., 
                     data = train, 
                     method = "gamLoess",
                     tuneLength = 100,
                     trControl = control, 
                     metric = "MAE")
model_gam_loess$bestTune
model_gam_loess$results
forecast::accuracy(as.vector(exp(predict(model_gam_loess, test))), exp(test$log_price))
forecast::accuracy(as.vector(predict(model_gam_loess, test)), test$log_price)


model_gam_spline <- train(log_price ~ ., 
                         data = train, 
                         method = "bam",
                         tuneLength = 10,
                         trControl = control, 
                         metric = "MAE")
model_gam_spline$bestTune
model_gam_spline$results
forecast::accuracy(as.vector(exp(predict(model_gam_spline, test))), exp(test$log_price))


knn_grid <- expand.grid(kmax = c(1:60),
                        distance = 2,
                        kernel = "optimal")
model_knn <- train(log_price ~ ., 
                   data = train, 
                   method = "kknn",
                   tuneGrid = knn_grid,
                   trControl = control, 
                   metric = "MAE")
model_knn$bestTune
model_knn$results
forecast::accuracy(as.vector(exp(predict(model_knn, test))), exp(test$log_price))


model_rand_forest <- train(log_price ~ ., 
                           data = train, 
                           method = "RRFglobal",
                           tuneGrid = forest_grid,
                           tuneLength = 5,
                           trControl = control, 
                           metric = "MAE")
model_rand_forest$bestTune
model_rand_forest$results
forecast::accuracy(as.vector(exp(predict(model_rand_forest, test))), exp(test$log_price))
forecast::accuracy(as.vector(predict(model_rand_forest, test)), test$log_price)
#-------------------------------------------------------------------------------



library(mlr)
listLearners("regr")[c("class", "package")]
getParamSet("regr.kknn")


task <- makeRegrTask(data = train,
                     target = "log_price")
test_task <- makeRegrTask(data = test,
                          target = "log_price")

knn <- makeLearner("regr.kknn")

control_knn <- makeResampleDesc(method = "CV",
                                iters = 10L)

knn_parans <- makeParamSet(makeDiscreteParam("kernel", values = c("rectangular",
                                                                  "triangular",
                                                                  "epanechnikov",
                                                                  "biweight",
                                                                  "cos",
                                                                  "gaussian",
                                                                  "rank")),
                           makeNumericParam("distance", lower = 2, upper = 20),
                           makeIntegerParam("k", lower = 2, upper = 60))
tune_knn_grid <- makeTuneControlGrid()


set.seed(134)
tune <- tuneParams(knn,
                   task,
                   resampling = control_knn,
                   par.set = knn_parans,
                   control = tune_knn_grid,
                   measures = list(mae))


knn_hyper <- setHyperPars(knn, 
                          par.vals = tune$x)
model_knn <- train(knn_hyper, task)
getLearnerModel(model_knn)
pred_knn <- predict(model_knn, test_task)
pred_knn$data$response
forecast::accuracy(as.vector(exp(pred_knn$data$response)), exp(test$log_price))





