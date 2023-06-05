library(tidyverse)
library(readxl)
library(caret)
library(mlr)
library(doParallel)
library(parallelMap)
library(GGally)
library(DataExplorer)
library(corrplot)
library(isotree)


#-----------------------------------------------------------------------------
bus <- read_excel("Datasets/bus_for_labeling.xlsx") %>%
  select(from_id,
         to_id,
         price_min_EUR,
         duration_min,
         distance_km,
         frequency_tpw,
         Outcome) %>%
  filter(!is.na(Outcome))
summary(bus)
#-----------------------------------------------------------------------------
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
#-----------------------------------------------------------------------------
connection_from <- left_join(bus, locations_from, by = c("from_id" = "from_id"))
connection_to <- left_join(connection_from, locations_to, by = c("to_id" = "to_id"))
#-----------------------------------------------------------------------------
bus_for_modeling <- connection_to %>%
  select(price_min_EUR,
         duration_min,
         distance_km,
         frequency_tpw,
         from_latitude,
         from_longitude,
         to_latitude,
         to_longitude,
         Outcome)
bus_for_modeling$Outcome <- as.factor(bus_for_modeling$Outcome)
summary(bus_for_modeling)
#-----------------------------------------------------------------------------
scale_lat_from <- preProcess(bus_for_modeling[,5], method = "range",
                        rangeBounds = c(0.61, 1.01))
bus_for_modeling[,5] <- predict(scale_lat_from, newdata = bus_for_modeling[,5])
scale_lat_to <- preProcess(bus_for_modeling[,7], method = "range",
                             rangeBounds = c(0.61, 1.09))
bus_for_modeling[,7] <- predict(scale_lat_to, newdata = bus_for_modeling[,7])
scale_long_from <- preProcess(bus_for_modeling[,6], method = "range",
                             rangeBounds = c(-0.15, 0.48))
bus_for_modeling[,6] <- predict(scale_long_from, newdata = bus_for_modeling[,6])
scale_long_to <- preProcess(bus_for_modeling[,8], method = "range",
                              rangeBounds = c(-0.15, 0.46))
bus_for_modeling[,8] <- predict(scale_long_to, newdata = bus_for_modeling[,8])
#-----------------------------------------------------------------------------
set.seed(333)
ind <- createDataPartition(bus_for_modeling$Outcome, p = .8, list = FALSE)
train <- bus_for_modeling[ ind,]
test  <- bus_for_modeling[-ind,]
train_alter <- train
test_alter <- test
#-----------------------------------------------------------------------------
#EDA
plot_histogram(train)
plot_density(train)
plot_bar(train)
plot_correlation(train)
plot_boxplot(train, by = "Outcome")
ggplot(train, aes(x = log(price_min_EUR))) +
  geom_histogram(fill = "orange", col = "gray")
ggplot(train, aes(x = log(duration_min))) +
  geom_histogram(fill = "orange", col = "gray")
ggplot(train, aes(x = log(distance_km))) +
  geom_histogram(fill = "orange", col = "gray")
ggplot(train, aes(x = log(frequency_tpw))) +
  geom_histogram(fill = "orange", col = "gray")
featurePlot(x = train[, 1:4], 
            y = train$Outcome, 
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))
featurePlot(x = train[, 1:4], 
            y = train$Outcome, 
            plot = "density",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))
ggpairs(train[, c(1:4, 9)])
ggplot(data = train, 
       aes(x = log(duration_min), y = log(price_min_EUR), color = Outcome)) +
  geom_point()
ggplot(data = train, 
       aes(x = log(distance_km), y = log(price_min_EUR), color = Outcome)) +
  geom_point()
ggplot(data = train, 
       aes(x = frequency_tpw, y = price_min_EUR, color = Outcome)) +
  geom_point()
#-------------------------------------------------------------------------------
scale <- preProcess(train[,c(1:4)], method = c("center", "scale"))
train[,c(1:4)] <- predict(scale, newdata = train[,c(1:4)])
test[,c(1:4)] <- predict(scale, newdata = test[,c(1:4)])
#-------------------------------------------------------------------------------
summary(train)
levels(train$Outcome) <- c("V", "I")
#-------------------------------------------------------------------------------
control <- trainControl(method = "repeatedcv", 
                        number = 3,
                        repeats = 5,
                        classProbs = TRUE,
                        savePredictions = TRUE,
                        verboseIter = TRUE,
                        summaryFunction = prSummary,
                        allowParallel = TRUE)
tune_param_forest <- expand.grid(mtry = seq(2, 4, len = 3))
set.seed(135)
forest <- caret::train(Outcome ~ ., 
                data = train, 
                method = "rf", 
                tuneGrid = tune_param_forest,
                trControl = control, 
                metric = "F")
varImp(forest)
tune_thresh <- thresholder(forest, threshold = seq(0.1, 0.8, by = 0.02))
ggplot(tune_thresh) + 
  geom_point(aes(x = prob_threshold, y = Precision), color = "blue") +
  geom_point(aes(x = prob_threshold, y = Recall), color = "green") +
  geom_point(aes(x = prob_threshold, y = F1), color = "red") +
  ylab("measure")
pred <- predict(forest, newdata = test, type = "prob")
cls <- ifelse(pred$I >= 0.52, "1", "0")
confusionMatrix(factor(cls), test$Outcome, positive = "1")$byClass
#-------------------------------------------------------------------------------
train <- train %>%
  select(-frequency_tpw)
levels(train$Outcome) <- c(0, 1)
test <- test %>%
  select(-frequency_tpw)
#-------------------------------------------------------------------------------
#library(pROC)
#roc_obj <- roc(Outcome ~ ., train)
#plot(roc_obj, print.thres = "best")
#-------------------------------------------------------------------------------
#Data modification
train_alter <- train_alter %>%
  mutate(log_price = log(price_min_EUR),
         log_duration = log(duration_min),
         log_dist = log(distance_km)) %>%
  select(-price_min_EUR,
         -duration_min,
         -distance_km,
         -frequency_tpw) %>%
  select(Outcome, everything())
test_alter <- test_alter %>%
  mutate(log_price = log(price_min_EUR),
         log_duration = log(duration_min),
         log_dist = log(distance_km)) %>%
  select(-price_min_EUR,
         -duration_min,
         -distance_km,
         -frequency_tpw) %>%
  select(Outcome, everything())

cor_mat1 <- train_alter %>%
  filter(Outcome == 1) %>%
  select(-Outcome)
cor_mat0 <- train_alter %>%
  filter(Outcome == 0) %>%
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

#Scale
scale2 <- preProcess(train_alter[,c(6:8)], method = c("center", "scale"))
train_alter[,c(6:8)] <- predict(scale2, newdata = train_alter[,c(6:8)])
test_alter[,c(6:8)] <- predict(scale2, newdata = test_alter[,c(6:8)])
summary(train_alter)
# Check for outliers
iso_forest <- isolation.forest(train_alter[,2:8], 
                               ndim=2, 
                               ntrees=100, 
                               nthreads=1,
                               sample_size=250,
                               standardize_data = FALSE,
                               scoring_metric = "depth")

iso_scores <- predict(iso_forest, train_alter[,2:8])
summary(iso_scores)
ggplot(data = train_alter, 
       aes(x = log_duration, y = log_price, color = iso_scores)) +
  geom_point()
#-------------------------------------------------------------------------------
levels(train_alter$Outcome) <- c("V", "I")
set.seed(333)
lda_caret_tresholding <- caret::train(Outcome ~ ., 
                                      data = train_alter, 
                                      method = "lda", 
                                      trControl = control, 
                                      metric = "F")
tune_thresh <- thresholder(lda_caret_tresholding, threshold = seq(0.1, 0.8, by = 0.02))
ggplot(tune_thresh) + 
  geom_point(aes(x = prob_threshold, y = Precision), color = "blue") +
  geom_point(aes(x = prob_threshold, y = Recall), color = "green") +
  geom_point(aes(x = prob_threshold, y = F1), color = "red") +
  ylab("measure")
pred <- predict(lda_caret_tresholding, newdata = test_alter, type = "prob")
cls <- ifelse(pred$I >= 0.5, "1", "0")
confusionMatrix(factor(cls), test_alter$Outcome, positive = "1")$byClass
levels(train_alter$Outcome) <- c(0, 1)
#-------------------------------------------------------------------------------
#Cost-Sensitive Classification with mlr
listLearners("classif")[c("class", "package")]
# observation weights
listLearners("classif", properties = "weights")[c("class", "package")]
# class weights
listLearners("classif", properties = "class.weights")[c("class", "package")]
#-------------------------------------------------------------------------------
#SVM
#-------------------------------------------------------------------------------
summary(train$Outcome)
task <- makeClassifTask(data = train_alter,
                        target = "Outcome",
                        positive = "1")

costs <- matrix(c(0, 1, 4.5, 0), 2)
colnames(costs) = rownames(costs) <- getTaskClassLevels(task)
th <- costs[2,1]/(costs[2,1] + costs[1,2])
w <- (1 - th)/th

svm <- makeLearner("classif.svm",
                   predict.type = "prob")
svm <- makeWeightedClassesWrapper(svm,
                                  wcw.weight = w)

control_svm <- makeResampleDesc(method = "CV",
                                predict = "both",
                                iters = 4L,
                                stratify = TRUE)
getParamSet("classif.svm")

kernels <- c("polynomial", "radial", "sigmoid")
svm_parans <- makeParamSet(makeDiscreteParam("kernel", values = kernels),
                           makeIntegerParam("degree", lower = 1, upper = 4),
                           makeNumericParam("cost", lower = 0.1, upper = 2000),
                           makeNumericParam("gamma", lower = 0.1, 10))

tune_svm_grid <- makeTuneControlRandom(maxit = 500L)

parallelMap::parallelStart()
set.seed(134)
svm_tune <- tuneParams(svm,
                       task,
                       resampling = control_svm,
                       par.set = svm_parans,
                       control = tune_svm_grid,
                       measures = list(f1))
parallelStop()

svm_hyper <- setHyperPars(svm, 
                          par.vals = svm_tune$x)
model_svm <- train(svm_hyper, task)
getLearnerModel(model_svm)
test_task <- makeClassifTask(data = test,
                             target = "Outcome",
                             positive = "1")
pred_svm <- predict(model_svm, test_task)
class_svm <- ifelse(pred_svm$data$prob.1 >= 0.5, 1, 0)
confusionMatrix(factor(class_svm), test$Outcome, positive = "1")$byClass
#===============================================================================
svm <- makeLearner("classif.ksvm",
                   predict.type = "prob")
svm <- makeWeightedClassesWrapper(svm,
                                  wcw.weight = w)

control_svm <- makeResampleDesc(method = "CV", 
                                iters = 4L,
                                stratify = TRUE)
getParamSet("classif.ksvm")

svm_parans <- makeParamSet(makeLogicalParam("scaled", default = FALSE, special.vals = list(scaled = FALSE)),
                           makeDiscreteParam("kernel", values = c("besseldot")),
                           #makeIntegerParam("degree", lower = 1, upper = 4),
                           makeNumericParam("sigma", lower = 0.1, upper = 20),
                           makeNumericParam("C", lower = 0.1, upper = 2000))
tune_svm_grid <- makeTuneControlRandom(maxit = 500L)

parallelMap::parallelStart()
svm_tune <- tuneParams(svm,
                       task,
                       resampling = control_svm,
                       par.set = svm_parans,
                       control = tune_svm_grid,
                       measures = list(f1))
parallelStop()

svm_hyper <- setHyperPars(svm, 
                          par.vals = svm_tune$x)
model_svm <- train(svm_hyper, task)
getLearnerModel(model_svm)
test_task <- makeClassifTask(data = test_alter,
                             target = "Outcome",
                             positive = "1")
pred_svm <- predict(model_svm, test_task)
class_svm <- ifelse(pred_svm$data$prob.1 >= 0.5, 1, 0)
confusionMatrix(factor(class_svm), test_alter$Outcome, positive = "1")$byClass
#-------------------------------------------------------------------------------
#Random Forest
#-------------------------------------------------------------------------------
forest <- makeLearner("classif.randomForest",
                       predict.type = "prob")
forest <- makeWeightedClassesWrapper(forest,
                                     wcw.weight = w)

control_forest <- makeResampleDesc(method = "CV", 
                                   iters = 4L,
                                   stratify = TRUE)
getParamSet("classif.randomForest")
forest_parans <- makeParamSet(makeIntegerParam("ntree", lower = 400, upper = 700),
                              makeIntegerParam("mtry", lower = 2, upper = 4),
                              makeIntegerParam("sampsize", lower = 100, upper = 200),
                              makeIntegerParam("nodesize", lower = 5, upper = 100))

tune_forest_rule <- makeTuneControlRandom(maxit = 3000L)    

parallelMap::parallelStart()
forest_tune <- tuneParams(forest,
                          task,
                          resampling = control_forest,
                          par.set = forest_parans,
                          control = tune_forest_rule,
                          measures = list(f1))  
parallelStop()
forest_tune$x
forest_hyper <- setHyperPars(forest, 
                             par.vals = forest_tune$x)
model_forest <- train(forest_hyper, task)
getLearnerModel(model_forest)
test_task <- makeClassifTask(data = test_alter,
                             target = "Outcome",
                             positive = "1")
pred_forest <- predict(model_forest, test_task)
class_forest <- ifelse(pred_forest$data$prob.1 >= 0.5, 1, 0)
confusionMatrix(factor(class_forest), test$Outcome, positive = "1")$byClass
#-------------------------------------------------------------------------------
#Gaussian DA
#-------------------------------------------------------------------------------
#Model LDA
task_lda <- makeClassifTask(data = train_alter,
                            target = "Outcome",
                            positive = "1")
control_lda <- makeResampleDesc(method = "CV", 
                                iters = 4L,
                                stratify = TRUE)
lda <- makeLearner("classif.lda",
                   predict.type = "prob")

getParamSet("classif.lda")
lda_param <- makeParamSet(makeDiscreteParam("method", values = c("moment", "mle",
                                                                 "mve","t")),
                          makeNumericParam("nu", lower = 2.5, upper = 100))

tune_lda_grid <- makeTuneControlGrid()  

lda_tune <- makeTuneWrapper(lda,
                            resampling = control_lda,
                            par.set = lda_param,
                            control = tune_lda_grid,
                            measures = list(f1))

parallelMap::parallelStart()
set.seed(777)
resamp_lda <- resample(lda_tune, 
                       task_lda, 
                       resampling = control_lda,
                       extract = getTuneResult,
                       measures = list(f1))
parallelStop()
print(resamp_lda$extract)


d <- generateThreshVsPerfData(resamp_lda, measures = list(f1))
plotThreshVsPerf(d, mark.th = th)
tune_th <- tuneThreshold(resamp_lda$pred)



parallelMap::parallelStart()
set.seed(345)
lda_best_hyp_search <- tuneParams(lda,
                                  task,
                                  resampling = control_lda,
                                  par.set = lda_param,
                                  control = tune_lda_grid,
                                  measures = list(f1))  
parallelStop()
lda_best_hyp_search$x


lda_hyper <- setHyperPars(lda, 
                          par.vals = list(method="mve"))
set.seed(665)
model_lda <- train(lda_hyper, task_lda)
getLearnerModel(model_lda)
test_for_lda_task <- makeClassifTask(data = test_alter,
                                     target = "Outcome",
                                     positive = "1")
pred_lda <- predict(model_lda, test_for_lda_task)
pred_lda <- setThreshold(pred_lda, tune_th$th)
confusionMatrix(factor(pred_lda$data$response), test_alter$Outcome, positive = "1")$byClass
#===============================================================================
task_qda <- makeClassifTask(data = train_alter,
                            target = "Outcome",
                            positive = "1")
control_qda <- makeResampleDesc(method = "CV", 
                                iters = 4L,
                                stratify = TRUE)
qda <- makeLearner("classif.qda",
                   predict.type = "prob")
getParamSet("classif.qda")
qda_param <- makeParamSet(makeDiscreteParam("method", values = c("moment", "mle",
                                                                 "mve","t")),
                          makeNumericParam("nu", lower = 3, upper = 300))

tune_qda_grid <- makeTuneControlGrid()  

qda_tune <- makeTuneWrapper(qda,
                            resampling = control_qda,
                            par.set = qda_param,
                            control = tune_qda_grid,
                            measures = list(f1))

set.seed(177)
resamp_qda <- resample(qda_tune, 
                       task_qda, 
                       resampling = control_qda,
                       extract = getTuneResult,
                       measures = list(f1))
print(resamp_qda$extract)


d2 <- generateThreshVsPerfData(resamp_qda, measures = list(f1))
plotThreshVsPerf(d2, mark.th = th)
tune_th2 <- tuneThreshold(resamp_qda$pred)


parallelMap::parallelStart()
set.seed(977)
qda_h_search <- tuneParams(qda,
                       task_qda,
                       resampling = control_qda,
                       par.set = qda_param,
                       control = tune_qda_grid,
                       measures = list(f1))
parallelStop()
qda_h_search$x

qda_hyper <- setHyperPars(qda, 
                          par.vals = qda_h_search$x)
model_qda <- train(qda_hyper, task_qda)
getLearnerModel(model_qda)
test_for_lda_task <- makeClassifTask(data = test_alter,
                                     target = "Outcome",
                                     positive = "1")
pred_qda <- predict(model_qda, test_for_lda_task)
pred_qda <- setThreshold(pred_qda, tune_th2$th)
confusionMatrix(factor(pred_qda$data$response), test_alter$Outcome, positive = "1")$byClass
#-------------------------------------------------------------------------------
