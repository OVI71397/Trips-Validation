library(tidyverse)
library(readxl)
library(caret)
library(REdaS)
library(doParallel)
library(GGally)
library(caretEnsemble)
library(scatterplot3d)
library(corrplot)
library(isotree)


#-----------------------------------------------------------------------------
plane <- read_excel("flights.xlsx")
plane <- plane %>%
  select(from_id, 
         to_id,
         transport_id,
         price_EUR,
         duration_min,
         Outcome)
plane$from_id <- as.numeric(plane$from_id)
plane$to_id <- as.numeric(plane$to_id)
#-----------------------------------------------------------------------------
countries <- read_excel("locations_with_id.xlsx", 
                                       range = "A1:H775", col_names = FALSE)
location <- countries
location <- location %>%
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
#-----------------------------------------------------------------------------
connected_from <- left_join(plane, locations_from, by = "from_id")
connected_to <- left_join(connected_from, locations_to, by = "to_id")
plane_extended <- connected_to
plane_extended <- plane_extended %>%
  filter((from_id %in% c(100:386)) & (to_id %in% c(100:386)))
summary(plane_extended)
#-----------------------------------------------------------------------------
plane_for_modeling <- plane_extended %>%
  select(price_EUR,
         duration_min,
         from_latitude,
         from_longitude,
         to_latitude,
         to_longitude,
         Outcome)

summary(factor(plane_for_modeling$Outcome))
#-----------------------------------------------------------------------------
ggplot(data = remove_missing(plane_for_modeling, na.rm = TRUE), 
       aes(x = duration_min, y = price_EUR, color = factor(Outcome))) +
  geom_point()
#-----------------------------------------------------------------------------
#Compute distance
trip_distance <- function(from_latitude, 
                          from_longitude, 
                          to_latitude, 
                          to_longitude) {
  delta_latitude <- deg2rad(to_latitude) - deg2rad(from_latitude)
  delta_longitude <- deg2rad(to_longitude) - deg2rad(from_longitude)
  a <- ((sin(delta_latitude/2))^2) + 
    (cos(deg2rad(from_latitude)) * 
       cos(deg2rad(to_latitude)) *
       ((sin(delta_longitude/2))^2))
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  d <- 6371 * c
}

plane_for_modeling <- plane_for_modeling %>%
  mutate(distance = trip_distance(plane_for_modeling$from_latitude,
                                  plane_for_modeling$from_longitude,
                                  plane_for_modeling$to_latitude,
                                  plane_for_modeling$to_longitude)) %>%
  select(price_EUR,
         duration_min,
         distance,
         from_latitude,
         from_longitude,
         to_latitude,
         to_longitude,
         Outcome)
plane_for_modeling$Outcome <- as.factor(plane_for_modeling$Outcome)
data_to_predict <- plane_for_modeling %>%
  filter(is.na(Outcome))
plane_for_modeling <- plane_for_modeling %>%
  filter(!is.na(Outcome))
plane_for_modeling$Outcome <- as.factor(plane_for_modeling$Outcome)
summary(plane_for_modeling)
#-----------------------------------------------------------------------------
#EDA
featurePlot(x = plane_for_modeling[, 1:3], 
            y = plane_for_modeling$Outcome, 
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))

featurePlot(x = plane_for_modeling[, 1:3], 
            y = plane_for_modeling$Outcome, 
            plot = "density",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))

ggpairs(plane_for_modeling[, c(1:3, 8)])

df_to_investig <- plane_for_modeling %>%
  mutate(log_price = log(price_EUR),
         log_dist = log(distance),
         lod_dur = log(duration_min)) %>%
  select(-price_EUR,
         -duration_min,
         -distance) %>%
  select(Outcome, everything())
ggpairs(df_to_investig[, c(1:8)])
#-----------------------------------------------------------------------------
set.seed(17)
plane_for_modeling_shuffled <- plane_for_modeling[sample(1:nrow(plane_for_modeling)), ]
#-----------------------------------------------------------------------------
set.seed(198)
ind <- createDataPartition(plane_for_modeling_shuffled$Outcome, p = .8, 
                                  list = FALSE)
train <- plane_for_modeling_shuffled[ ind,]
test  <- plane_for_modeling_shuffled[-ind,]
#-----------------------------------------------------------------------------
ggplot(remove_missing(train)) +
  geom_point(aes(x = log(duration_min), y = log(price_EUR), color = factor(Outcome)))
#-----------------------------------------------------------------------------
train$Outcome <- as.factor(train$Outcome)
test$Outcome <- as.factor(test$Outcome)
train$Outcome <- ifelse(train$Outcome == "1", "I", "V")
train$Outcome <- as.factor(train$Outcome)
summary(train)
#-----------------------------------------------------------------------------
# SplitBal step
negative_cases <- train %>%
  filter(Outcome == "V")
positive_cases <- train %>%
  filter(Outcome == "I")
set.seed(177111)
i1 <- sample(seq_len(nrow(negative_cases)), size = 163)
df1 <- negative_cases[i1, ]
rest <- negative_cases[-i1, ]
i2 <- sample(seq_len(nrow(rest)), size = 163)
df2 <- rest[i2, ]
rest <- rest[-i2, ]
i3 <- sample(seq_len(nrow(rest)), size = 164)
df3 <- rest[i3, ]
df4 <- rest[-i3, ]

train1 <- rbind(df1, positive_cases)
train2 <- rbind(df2, positive_cases)
train3 <- rbind(df3, positive_cases)
train4 <- rbind(df4, positive_cases)

#-----------------------------------------------------------------------------
ggplot(train1) +
  geom_point(aes(x = duration_min, y = price_EUR, color = factor(Outcome)))
#-----------------------------------------------------------------------------
clust <- makePSOCKcluster(6)
registerDoParallel(clust)
#-----------------------------------------------------------------------------
#Test1 Random Forest
control <- trainControl(method = "cv", 
                        number = 3,
                        classProbs = TRUE,
                        verboseIter = TRUE,
                        savePredictions = TRUE,
                        allowParallel = TRUE)
tune_rf <- expand.grid(mtry = seq(2, 4, len = 3))
set.seed(123)
model_forest1 <- train(Outcome ~ ., 
                       data = train1, 
                       method = "rf", 
                       tuneGrid = tune_rf,
                       trControl = control, 
                       metric = "F")
pred_rf1 <- predict(model_forest1, test, type = "prob")
pred_class_rf1 <- ifelse(pred_rf1$I >= 0.6, 1, 0)
#-----------------------------------------------------------------------------
#Test2 Random Forest
set.seed(321)
model_forest2 <- train(Outcome ~ ., 
                       data = train2, 
                       method = "rf", 
                       tuneGrid = tune_rf,
                       trControl = control, 
                       metric = "F")
pred_rf2 <- predict(model_forest2, test, type = "prob")
pred_class_rf2 <- ifelse(pred_rf2$I >= 0.6, 1, 0)
#-----------------------------------------------------------------------------
set.seed(444)
model_forest3 <- train(Outcome ~ ., 
                       data = train3, 
                       method = "rf", 
                       tuneGrid = tune_rf,
                       trControl = control, 
                       metric = "F")
pred_rf3 <- predict(model_forest3, test, type = "prob")
pred_class_rf3 <- ifelse(pred_rf3$I >= 0.6, 1, 0)
#-----------------------------------------------------------------------------
set.seed(1414)
model_forest4 <- train(Outcome ~ ., 
                       data = train4, 
                       method = "rf", 
                       tuneGrid = tune_rf,
                       trControl = control, 
                       metric = "F")
pred_rf4 <- predict(model_forest4, test, type = "prob")
pred_class_rf4 <- ifelse(pred_rf4$I >= 0.6, 1, 0)
#-----------------------------------------------------------------------------
#without division
CTL <- trainControl(method = "repeatedcv", 
                    number = 5,
                    repeats = 5,
                    classProbs = TRUE,
                    verboseIter = TRUE,
                    summaryFunction = prSummary,
                    savePredictions = TRUE,
                    allowParallel = TRUE)
set.seed(123)
model_forest <- train(Outcome ~ ., 
                      data = train, 
                      method = "rf", 
                      tuneGrid = tune_rf,
                      trControl = CTL, 
                      metric = "Precision")
model_forest$bestTune
model_forest$finalModel
pred_rf <- predict(model_forest, test, type = "prob")
pred_class_rf <- ifelse(pred_rf$I >= 0.6, 1, 0)
confusionMatrix(factor(pred_class_rf), test$Outcome, positive = "1")$byClass
#-----------------------------------------------------------------------------
#ENSAMBLING
prediction_gathering <- tibble(p1 = pred_rf1$I,
                               p2 = pred_rf2$I,
                               p3 = pred_rf3$I,
                               p4 = pred_rf4$I,
                               Outcome = test$Outcome)
max_prob <- matrixStats::rowMaxs(as.matrix((prediction_gathering[,c(1:4)])))
prediction_gathering <- prediction_gathering %>%
  mutate(max_prob = max_prob,
         sum = p1+p2+p3+p4,
         prod = round(p1*p2*p3*p4, 5),
         w_sum = p1+p2+p3+p4)
pred_class <- ifelse(prediction_gathering$max_prob >= 0.7, 1, 0)
confusionMatrix(factor(pred_class), test$Outcome, positive = "1")$byClass
#-----------------------------------------------------------------------------
#XGBoost
set.seed(537)
model_xgboost <- train(Outcome ~ ., 
                       data = train, 
                       method = "xgbTree", 
                       tuneLength = 5,
                       trControl = CTL, 
                       metric = "Precision")
model_xgboost$bestTune
model_xgboost$finalModel
pred_xgb <- predict(model_xgboost, test, type = "prob")
pred_class_xgb <- ifelse(pred_xgb$I >= 0.5, 1, 0)
confusionMatrix(factor(pred_class_xgb), test$Outcome, positive = "1")$byClass
#-----------------------------------------------------------------------------
set.seed(5377)
model_xgboost1 <- train(Outcome ~ ., 
                       data = train1, 
                       method = "xgbTree", 
                       tuneLength = 5,
                       trControl = control, 
                       metric = "F")
model_xgboost1$bestTune
pred_xgb1 <- predict(model_xgboost1, test, type = "prob")
pred_class_xgb1 <- ifelse(pred_xgb1$I >= 0.5, 1, 0)

set.seed(5377)
model_xgboost2 <- train(Outcome ~ ., 
                        data = train2, 
                        method = "xgbTree", 
                        tuneLength = 5,
                        trControl = control, 
                        metric = "F")
model_xgboost2$bestTune
pred_xgb2 <- predict(model_xgboost2, test, type = "prob")
pred_class_xgb2 <- ifelse(pred_xgb2$I >= 0.5, 1, 0)

set.seed(5377)
model_xgboost3 <- train(Outcome ~ ., 
                        data = train3, 
                        method = "xgbTree", 
                        tuneLength = 5,
                        trControl = control, 
                        metric = "F")
model_xgboost3$bestTune
pred_xgb3 <- predict(model_xgboost3, test, type = "prob")
pred_class_xgb3 <- ifelse(pred_xgb3$I >= 0.5, 1, 0)

set.seed(5377)
model_xgboost4 <- train(Outcome ~ ., 
                        data = train4, 
                        method = "xgbTree", 
                        tuneLength = 5,
                        trControl = control, 
                        metric = "F")
model_xgboost4$bestTune
pred_xgb4 <- predict(model_xgboost4, test, type = "prob")
pred_class_xgb4 <- ifelse(pred_xgb4$I >= 0.5, 1, 0)


prediction_gathering_boost <- tibble(p1 = pred_xgb1$I,
                                     p2 = pred_xgb2$I,
                                     p3 = pred_xgb3$I,
                                     p4 = pred_xgb4$I)
max_prob_boost <- matrixStats::rowMaxs(as.matrix((prediction_gathering_boost[,c(1:4)])))
prediction_gathering_boost <- prediction_gathering_boost %>%
  mutate(max_prob = max_prob_boost)
pred_class_boost <- ifelse(prediction_gathering_boost$max_prob >= 0.6, 1, 0)
confusionMatrix(factor(pred_class_boost), test$Outcome, positive = "1")$byClass
#-----------------------------------------------------------------------------


#-----------------------------------------------------------------------------
#Use model to make prediction for Europe
#-----------------------------------------------------------------------------
new_plane_europe <- read_csv("4run.csv") %>%
  filter(transport_id == 1) %>%
  filter(!(to_id %in% c(1:99) | from_id %in% c(1:99))) %>%
  filter(!(to_id %in% c(547:797) | from_id %in% c(547:797)))
f <- left_join(new_plane_europe, locations_from, by = c("from_id" = "from_id"))
t <- left_join(f, locations_to, by = c("to_id" = "to_id"))
df_plane_predict <- t %>%
  mutate(distance = trip_distance(t$from_latitude,
                                  t$from_longitude,
                                  t$to_latitude,
                                  t$to_longitude)) %>%
  select(price_min_EUR,
         duration_min,
         distance,
         from_latitude,
         from_longitude,
         to_latitude,
         to_longitude) %>%
  rename(price_EUR = price_min_EUR)
prob_pred <- predict(model_xgboost, df_plane_predict,
                     type = "prob")
class_pred <- ifelse(prob_pred$I >= 0.5, 1, 0)
summary(factor(class_pred))
new_plane_europe <- new_plane_europe %>%
  mutate(Outcome = class_pred,
         Probability_of_invalidity = prob_pred$I) %>%
  filter(Outcome == 1)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
#GaussianDA
#Data preprocess
alter_train <- train %>%
  mutate(log_price = log(price_EUR),
         log_duration = log(duration_min),
         log_dist = log(distance)) %>%
  select(-price_EUR,
         -duration_min,
         -distance) %>%
  select(Outcome,
         everything())
alter_test <- test %>%
  mutate(log_price = log(price_EUR),
         log_duration = log(duration_min),
         log_dist = log(distance)) %>%
  select(-price_EUR,
         -duration_min,
         -distance) %>%
  select(Outcome,
         everything())
summary(alter_train)
summary(alter_test)
scale <- preProcess(alter_train[,c(6:8)], method = c("center", "scale"))
alter_train[,c(6:8)] <- predict(scale, newdata = alter_train[,c(6:8)])
alter_test[,c(6:8)] <- predict(scale, newdata = alter_test[,c(6:8)])


scale_lat_tr <- preProcess(alter_train[,c(2, 4)], method = "range",
                           rangeBounds = c(0.47, 1.12))
alter_train[,c(2, 4)] <- predict(scale_lat_tr, newdata = alter_train[,c(2, 4)])
scale_lat_tst_from <- preProcess(alter_test[,2], method = "range",
                                 rangeBounds = c(0.47, 1.12))
alter_test[,2] <- predict(scale_lat_tst_from, newdata = alter_test[,2])
scale_lat_tst_to <- preProcess(alter_test[,4], method = "range",
                               rangeBounds = c(0.47, 1.07))
alter_test[,4] <- predict(scale_lat_tst_to, newdata = alter_test[,4])


scale_long_tr <- preProcess(alter_train[,c(3, 5)], method = "range",
                            rangeBounds = c(-0.45, 0.66))
alter_train[,c(3, 5)] <- predict(scale_long_tr, newdata = alter_train[,c(3, 5)])
scale_long_tst_from <- preProcess(alter_test[,3], method = "range",
                                  rangeBounds = c(-0.37, 0.66))
alter_test[,3] <- predict(scale_long_tst_from, newdata = alter_test[,3])
scale_long_tst_to <- preProcess(alter_test[,5], method = "range",
                                  rangeBounds = c(-0.45, 0.66))
alter_test[,5] <- predict(scale_long_tst_to, newdata = alter_test[,5])


#EDA
ggplot(alter_train, aes(x=log_price, y=log_duration, color = Outcome))+
  geom_point()
colors <- c("#E69F00", "#56B4E9")
colors <- colors[as.numeric(alter_train$Outcome)]
scatterplot3d(alter_train$log_dist,
              alter_train$log_duration,
              alter_train$log_price,
              color = colors,
              angle = 300)
cor_mat1 <- alter_train %>%
  filter(Outcome == "I") %>%
  select(-Outcome)
cor_mat0 <- alter_train %>%
  filter(Outcome == "V") %>%
  select(-Outcome)
col <- colorRampPalette(c("black", "darkgray", "gray", "yellow"))
corrplot(cor(cor_mat0), 
         method = "ellipse", 
         col = col(100), 
         addCoef.col = "black", 
         tl.col = "black")
corrplot(cor(cor_mat1), 
         method = "ellipse", 
         col = col(100), 
         addCoef.col = "black", 
         tl.col = "black")
#Outliers
iso_forest <- isolation.forest(alter_train[,2:8], 
                               ndim=3, 
                               ntrees=100, 
                               nthreads=1,
                               sample_size=250,
                               standardize_data = FALSE,
                               scoring_metric = "depth")

iso_scores <- predict(iso_forest, alter_train[,2:8])
summary(iso_scores)
ggplot(alter_train, 
       aes(x = log_dist, y = log_price, color = iso_scores)) +
  geom_point()
#Modeling
ctl_spec <- trainControl(method = "repeatedcv", 
                         number = 5,
                         repeats = 3,
                         classProbs = TRUE,
                         verboseIter = TRUE,
                         savePredictions = TRUE,
                         summaryFunction = prSummary,
                         allowParallel = TRUE)
set.seed(123)
model_da <- train(Outcome ~.,
                  data = alter_train, 
                  method = "lda2", 
                  tuneLength = 100,
                  trControl = ctl_spec, 
                  metric = "Precision")
model_da$bestTune
pred_da <- predict(model_da, alter_test, type = "prob")
tune_thresh <- thresholder(model_da, threshold = seq(0.1, 0.8, by = 0.02))
ggplot(tune_thresh) + 
  geom_point(aes(x = prob_threshold, y = Precision), color = "blue") +
  geom_point(aes(x = prob_threshold, y = Recall), color = "green") +
  geom_point(aes(x = prob_threshold, y = F1), color = "red") +
  ylab("measure")
pred_class_da <- ifelse(pred_da$I >= 0.58, 1, 0)
confusionMatrix(factor(pred_class_da), alter_test$Outcome, positive = "1")$byClass



negative_cases <- alter_train %>%
  filter(Outcome == "V")
positive_cases <- alter_train %>%
  filter(Outcome == "I")
set.seed(177111)
i1 <- sample(seq_len(nrow(negative_cases)), size = 163)
df1 <- negative_cases[i1, ]
rest <- negative_cases[-i1, ]
i2 <- sample(seq_len(nrow(rest)), size = 163)
df2 <- rest[i2, ]
rest <- rest[-i2, ]
i3 <- sample(seq_len(nrow(rest)), size = 164)
df3 <- rest[i3, ]
df4 <- rest[-i3, ]
alt_train1 <- rbind(df1, positive_cases)
alt_train2 <- rbind(df2, positive_cases)
alt_train3 <- rbind(df3, positive_cases)
alt_train4 <- rbind(df4, positive_cases)


control_spec <- trainControl(method = "repeatedcv", 
                             number = 3,
                             repeats = 3,
                             classProbs = TRUE,
                             verboseIter = TRUE,
                             summaryFunction = prSummary,
                             allowParallel = TRUE)
model_da1 <- train(Outcome ~.,
                   data = alt_train1, 
                   method = "lda2", 
                   tuneLength = 100,
                   trControl = control_spec, 
                   metric = "F")
pred_da1 <- predict(model_da1, alter_test, type = "prob")
model_da2 <- train(Outcome ~.,
                   data = alt_train2, 
                   method = "lda2", 
                   tuneLength = 100,
                   trControl = control_spec, 
                   metric = "F")
pred_da2 <- predict(model_da2, alter_test, type = "prob")
model_da3 <- train(Outcome ~.,
                   data = alt_train3, 
                   method = "lda2", 
                   tuneLength = 100,
                   trControl = control_spec, 
                   metric = "F")
pred_da3 <- predict(model_da3, alter_test, type = "prob")
model_da4 <- train(Outcome ~.,
                   data = alt_train4, 
                   method = "lda2", 
                   tuneLength = 100,
                   trControl = control_spec, 
                   metric = "F")
pred_da4 <- predict(model_da4, alter_test, type = "prob")
prediction_gathering_lda <- tibble(p1 = pred_da1$I,
                                   p2 = pred_da2$I,
                                   p3 = pred_da3$I,
                                   p4 = pred_da4$I)
max_prob_lda <- matrixStats::rowMaxs(as.matrix((prediction_gathering_lda[,c(1:4)])))
prediction_gathering_lda <- prediction_gathering_lda %>%
  mutate(max_prob = max_prob_lda)
pred_class_lda <- ifelse(prediction_gathering_lda$max_prob >= 0.8, 1, 0)
confusionMatrix(factor(pred_class_lda), alter_test$Outcome, positive = "1")$byClass
#-----------------------------------------------------------------------------
stopCluster(clust)





#-----------------------------------------------------------------------------
#Use model to make prediction for Europe by LDA
#-----------------------------------------------------------------------------
plane_to_pred <- read_csv("4run.csv") %>%
  filter(transport_id == 1) %>%
  filter(!(to_id %in% c(1:99) | from_id %in% c(1:99))) %>%
  filter(!(to_id %in% c(547:797) | from_id %in% c(547:797)))
f1 <- left_join(plane_to_pred, locations_from, by = c("from_id" = "from_id"))
t1 <- left_join(f1, locations_to, by = c("to_id" = "to_id"))
df_plane_predict1 <- t1 %>%
  mutate(distance = trip_distance(t1$from_latitude,
                                  t1$from_longitude,
                                  t1$to_latitude,
                                  t1$to_longitude)) %>%
  select(price_min_EUR,
         duration_min,
         distance,
         from_latitude,
         from_longitude,
         to_latitude,
         to_longitude) %>%
  mutate(log_price = log(price_min_EUR),
         log_duration = log(duration_min),
         log_dist = log(distance)) %>%
  select(-price_min_EUR,
         -duration_min,
         -distance)


df_plane_predict1[,c(5:7)] <- predict(scale, newdata = df_plane_predict1[,c(5:7)])
df_plane_predict1[,c(1, 3)] <- predict(scale_lat_tr, newdata = df_plane_predict1[,c(1, 3)])
df_plane_predict1[,c(2, 4)] <- predict(scale_long_tr, newdata = df_plane_predict1[,c(2, 4)])
summary(df_plane_predict1)

p1 <- predict(model_da1, df_plane_predict1, type = "prob")
p2 <- predict(model_da2, df_plane_predict1, type = "prob")
p3 <- predict(model_da3, df_plane_predict1, type = "prob")
p4 <- predict(model_da4, df_plane_predict1, type = "prob")

pred_lda <- tibble(p1 = p1$I,
                   p2 = p2$I,
                   p3 = p3$I,
                   p4 = p4$I)

max_prob_lda_new <- matrixStats::rowMaxs(as.matrix((pred_lda[,c(1:4)])))
pred_lda <- pred_lda %>%
  mutate(max_prob = max_prob_lda_new)
pred_class_lda_new <- ifelse(pred_lda$max_prob >= 0.8, 1, 0)
summary(factor(pred_class_lda_new))

plane_to_pred <- plane_to_pred %>%
  mutate(Outcome = pred_class_lda_new,
         Probability_of_invalidity = max_prob_lda_new) %>%
  filter(Outcome == 1)
#-----------------------------------------------------------------------------