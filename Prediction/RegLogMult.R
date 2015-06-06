#
# Multinomial logistic regression
#

require(sampling)
require(ggplot2)
require(glmnet)
require(caret)


## VALIDATION SET

validation_ids <- strata(data=data, size=round(table(data$long_url)/nrow(data)*2500)
                         , method="srswor", description=TRUE, stratanames=c("long_url"))
validation_set <- data[validation_ids$ID_unit,]
real <- validation_set$long_url
f <- as.formula(long_url ~ referring_url + country_code + ua_profiles + known_user + midweek + time)
validation_matrix <- model.matrix(f, validation_set)

# TRAIN SET SIZE (CROSS-VALIDATION)

trainsize_tests <- c(250, 500, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 
                     8000, 9000, 10000, 20000, 30000, 40000, 50000)

results <- list()

for(s in trainsize_tests){
  
  # Train set extraction
  train_ids <- strata(data=data, size=round(table(data$long_url)/nrow(data)*s)
                , method="srswor", description=FALSE, stratanames=c("long_url"))

  train_set <- data[train_ids$ID_unit,]

  x <- model.matrix(f, train_set)
  y <- as.matrix(train_set$long_url, ncol=1)
  
  # Train
  model <- cv.glmnet(x, y, family = "multinomial", alpha=0.95) # fijo alpha a casi lasso

  # Predict
  prediction <- predict(model, s="lambda.min", validation_matrix, type="class")
  prediction <- as.factor(prediction)

  # Both factors with same levels
  prediction <- factor(prediction,levels=levels(real))
  
  # Store model and mean accuracy
  results$model[[length(results$model)+1]] <- model
  results$accuracy[[length(results$accuracy)+1]] <- sum(prediction == real)/length(real)
  
  rm(x,y,model,train_ids,train_set,prediction)
}

ggplot()+geom_line(aes(x=trainsize_tests,y=as.numeric(results$accuracy)),colour="RED")+
geom_point(aes(x=trainsize_tests,y=as.numeric(results$accuracy)),colour="RED",shape=15)+
labs(x = "Nº de instancias del conjunto de entrenamiento", y="Accuracy")


# ALPHA (CROSS-VALIDATION)

alpha_tests <- c(0, 0.05, 0.2, 0.4, 0.6, 0.8, 0.95, 1)

results2 <- list()

train_ids <- strata(data=data, size=round(table(data$long_url)/nrow(data)*10000)
                    , method="srswor", description=FALSE, stratanames=c("long_url"))

train_set <- data[train_ids$ID_unit,]

x <- model.matrix(f, train_set)
y <- as.matrix(train_set$long_url, ncol=1)

for(a in alpha_tests){
  
  model <- cv.glmnet(x, y, family = "multinomial", alpha = a) 
  
  prediction <- predict(model, s="lambda.min", validation_matrix, type="class")
  prediction <- as.factor(prediction)
  
  # Both factors with same levels
  prediction <- factor(prediction,levels=levels(real))
  
  # Store model and mean accuracy
  results2$model[[length(results2$model)+1]] <- model
  results2$accuracy[[length(results2$accuracy)+1]] <- sum(prediction == real)/length(real)

}

ggplot()+geom_line(aes(x=alpha_tests,y=as.numeric(results2$accuracy)),colour="RED")+
geom_point(aes(x=alpha_tests,y=as.numeric(results2$accuracy)),colour="RED",shape=15)+
labs(x = expression(paste("Valor de ",alpha)), y="Accuracy")+coord_cartesian(ylim=c(0.3,0.7))

## PERFORMANCE EVALUATION

# Select best moedl
model <- results2$model[[8]]

# Extract test set
test_ids <- strata(data=data, size=round(table(data$long_url)/nrow(data)*2500)
                         , method="srswor", description=FALSE, stratanames=c("long_url"))
test_set <- data[test_ids$ID_unit,]
real <- test_set$long_url
f <- as.formula(long_url ~ referring_url + country_code + ua_profiles + known_user + midweek + time)
test_matrix <- model.matrix(f, test_set)

# Predict
prediction <- factor(predict(model, s="lambda.min", test_matrix, type="class"),levels=levels(real))

# Confusion matrix
confmx <- confusionMatrix(prediction, real, positive = NULL, dnn = c("Prediction", "Real"), prevalence = NULL)

# General metrics

as.numeric(round(confmx$overall[1]*100,2)) # Mean accuracy
as.numeric(round(confmx$overall[5]*100,2)) # No information rate

# Metrics by class (Plot)
ggplot() + geom_bar(aes(x=levels(real),y=confmx$byClass[,1]),
           stat="identity",fill="#56B4E9") + labs(x = "Positive Rate por clase", y="")+
           theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

ggplot() + geom_bar(aes(x=levels(real),y=confmx$byClass[,2]),
           stat="identity",fill="#009E73") + labs(x = "Negative Rate por clase", y="")+
           theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

ggplot() + geom_bar(aes(x=levels(real),y=confmx$byClass[,8]),
           stat="identity",fill="#D55E00") + labs(x = "Balanced Accuracy por clase", y="")+
           theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

ggplot() + geom_bar(aes(x=levels(real),y=as.numeric(table(data$long_url)/nrow(data))),
           stat="identity",fill="#56B4E9") +geom_line(aes(x=1:24,y=confmx$byClass[,6]),
           colour="RED")+ geom_point(aes(x=1:24,y=confmx$byClass[,6]),
           colour="RED",shape=15)+labs(x = "% Total aparición (Barra) VS Accuracy (Línea)", y="")+
           theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
