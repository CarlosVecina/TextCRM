
source("functions/LoadRequiredPackages.R")
LoadRequiredPackages(c("ggplot2","ggrepel","scales","plotly","stringr","tidyverse","tidytext","SnowballC",
                       "minqa","e1071","caret","tm","lime"))

todasReviewsTidy <- readRDS("data/todasReviewsTidy.RDS")
todasReviewsTidyFiltrado <- readRDS("data/todasReviewsTidyFiltrado.RDS")

dfm <- todasReviewsTidyFiltrado %>%
    count(Banco, Comentario,Palabra, sort=T) %>% 
    # filter(Banco == "EVO") %>% 
    cast_dfm(Comentario, Palabra, n)
matriz <- dfm %>% as.tibble() %>% mutate(document=as.numeric(document)) %>% 
    left_join(todasReviewsTidyFiltrado %>% select(Valoracion,Comentario),by=c("document"="Comentario")) %>% select(Valoracion,everything())

longitudTrain <- 0.8*nrow(matriz)
longitudTest <- nrow(matriz)-longitudTrain
trainSet <- matriz %>% mutate(Valoracion=as.factor(Valoracion)) %>% sample_n(longitudTrain) %>% unique()
testSet <- matriz %>% mutate(Valoracion=as.factor(Valoracion)) %>% sample_n(longitudTest) %>% unique()
testSet <- setdiff(testSet,trainSet)

# Train.
train_control <- trainControl(method="repeatedcv", number=10, repeats=2)
fitLogit <- train(Valoracion ~ ., data = trainSet, method = 'LogitBoost')

predictLogit <- predict(fitLogit, newdata = testSet)
predictLogit

library(randomForest)
library(ROCR)
trainSet$`100` <- NULL
trainSet$`10` <- NULL
testSet$`10` <- NULL
testSet$`100` <- NULL
fitRf <-randomForest(Valoracion~.,data=trainSet, mtry=8, ntree=50,     
                     keep.forest=TRUE, importance=TRUE)
predictRf = predict(fitRf,type="prob",newdata=testSet)
valoracionesPredict <- colnames(predictRf)[apply(predictRf,1,which.max)] %>% as.tibble() %>% set_names("Predict")

predVSreal <- testSet %>% select(Valoracion) %>% bind_cols(valoracionesPredict)
table(predVSreal) %>% prop.table()
library(ROCR)
perf <- ROCR::performance(predVSreal$Valoracion,"prec","rec")
confusionMatrix(predVSreal$Valoracion,predVSreal$Predict)
rf.pred = prediction(predictRf, testSet$Valoracion)  # Con las probabilidades quiero que me prediga income.
rf.perf = performance(rf.pred,"tpr","fpr")
plot(rf.perf,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")

# LIME tratar de explicar los resultados
model_df <- model_rf <- caret::train(Valoracion ~ .,
                                     data = trainSet[,-2],
                                     method = "gbm") # random forest
                                     y# trControl = trainControl(method = "repeatedcv", 
                                     #                          number = 1, 
                                     #                          repeats = 1, 
                                     #                          verboseIter = FALSE))

explainer <- explainer <- lime(trainSet %>% select(-Valoracion,-document), model_rf, n_bins = 5, quantile_bins = TRUE)
explanation_df <- lime::explain(testSet %>% select(-Valoracion,-document), explainer, n_labels = 1, n_features = 10, n_permutations = 100, feature_select = "forward_selection")

plot_features(explanation_df[9, ], ncol = 1)

