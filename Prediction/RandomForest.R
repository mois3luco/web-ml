#
# Random Forest
#

## VALIDATION SET (STRATIFIED SAMPLING)

require(sampling)

validation_ids <- strata(data=data, size=round(table(data$long_url)/nrow(data)*2500)
                    , method="srswor", description=FALSE, stratanames=c("long_url"))
validation_set <- data[validation_ids$ID_unit,]
real <- validation_set$long_url
rm(validation_ids)

## NTREE (CROSS-VALIDATION)
require(randomForest)

# Fijo componente aleatoria para obtener resultados reproducibles
set.seed(415) 

# Valores de ntree a probar
ntree_tests <- c(10,20,50,100,250,500,1000,2000,3000,5000)

results_rf <- list()

for(nt in ntree_tests){
  
  # Entreno el modelo
  rf <- randomForest(long_url ~ referring_url + country_code + ua_profiles + known_user +
                       midweek + time, data=train_set, importance=TRUE, ntree=nt)
  # Obtengo predicciones
  prediction <- predict(rf, validation_set)
  
  # Almaceno modelos y calculo accuracy "media"
  results_rf$model[[length(results_rf$model)+1]] <- rf
  results_rf$accuracy[[length(results_rf$accuracy)+1]] <- sum(prediction == real)/length(real)
  
}

ggplot()+geom_line(aes(x=ntree_tests,y=results_rf$accuracy),colour="RED")+
geom_point(aes(x=ntree_tests,y=results_rf$accuracy),colour="RED",shape=15)+
labs(x = "Nº de árboles", y="Accuracy")+coord_cartesian(ylim=c(0.45,0.6))+
geom_vline(xintercept=500, linetype="dashed")

## TRAINING SET SIZE (CROSS-VALIDATION)

size_tests <- c(1000,2500,5000,10000,25000,50000)

results_rf2 <- list()

for(s in size_tests){
  # Training set extraction
  train_ids <- strata(data=data, size=round(table(data$long_url)/nrow(data)*s)
                      , method="srswor", description=FALSE, stratanames=c("long_url"))
  train_set <- data[train_ids$ID_unit,]
  rm(train_ids)
  
  # Training
  rf <- randomForest(long_url ~ referring_url + country_code + ua_profiles + known_user +
                       midweek + time, data=train_set, importance=TRUE, ntree=1000)
  # Predict
  prediction <- predict(rf, validation_set)
  
  # Store model & mean accuracy 
  results_rf2$model[[length(results_rf2$model)+1]] <- rf
  results_rf2$accuracy[[length(results_rf2$accuracy)+1]] <- sum(prediction == real)/length(real)
  
}

# Select best model
selected_rf <- results_rf2$model[[6]
varImpPlot(selected_rf)

## TEST
test_ids <- strata(data, size=round(table(data$long_url)/nrow(data)*2000)
                    , method="srswor", description=FALSE, stratanames=c("long_url"))
test_set <- data[test_ids$ID_unit,]
real <- test_set$long_url

prediction <- predict(selected_rf, test_set)
sum(prediction == real)/length(real)

## PLOT TREE
require(rpart)
tree <- rpart(long_url ~ referring_url + country_code + ua_profiles + known_user +
           midweek + time, data=train_set, method="class")

require(rattle)
require(rpart.plot)
require(RColorBrewer)

fancyRpartPlot(tree,sub="")

## BALANCED TRAINING

proportions <- as.numeric(table(data$long_url)/nrow(data))
proportions[19] <- 0.1 # Lower NASA proportion manually

plot(as.numeric(table(train_set$long_url)/nrow(train_set)))

train_ids <- strata(data=data, size=round(proportions*55000)
                    ,method="srswor", description=FALSE, stratanames=c("long_url"))

train_set <- data[train_ids$ID_unit,]

rm(train_ids)

# Now train again


