#
# Logistic Regression "One VS All"
#

require(sampling)
require(glmnet)
require(caret)

## TRAIN SET EXTRACTION (STRATIFIED SAMPLING)
train_ids <- strata(data=data, size=round(table(data$long_url)/nrow(data)*10000)
                    ,method="srswor", description=FALSE, stratanames=c("long_url"))

train_set <- data[train_ids$ID_unit,]

rest <- data[-train_ids$ID_unit,]

f <- as.formula(long_url ~ referring_url + country_code + ua_profiles + known_user)

## TRAIN
binary_models <- list()

for(level in levels(data$long_url)){

    train_set[[level]] <- train_set$long_url == level

    x <- model.matrix(f, train_set)
    y <- as.matrix(train_set[[level]], ncol=1)
  
    binary_models[[length(binary_models)+1]] <- cv.glmnet(x, y, family = "binomial", alpha=0.95)
    
}

## TEST
test_ids <- strata(data=rest, size=round(table(data$long_url)/nrow(data)*3000)
                   , method="srswor", description=FALSE, stratanames=c("long_url"))
test_set <- rest[test_ids$ID_unit,]
rm(test_ids)

prediction <- factor(predict_onevsall(test_set, binary_models, f),levels=levels(real))

## PERFORMANCE EVALUATION

# Mean Accuracy
real <- test_set$long_url
sum(prediction == real)/length(real)

# Confusion matrix
confmx <- confusionMatrix(prediction, real, positive = NULL, dnn = c("Prediction", "Real"), prevalence = NULL)
confmx$overall


## PREDICTION FUNCTION ONE-VS-ALL DEFINITION

predict_onevsall <- function(test_data, binary_models, formula){
   
   m <- matrix(nrow=nrow(test_data),ncol=length(binary_models))  
   colnames(m) <- levels(test_data$long_url)  
   
   test_matrix <- model.matrix(f, test_data)

   for(i in c(1:length(binary_models)))
      m[,i] <- predict(binary_models[[i]], s="lambda.min", test_matrix, type="response")
   
   prediction <- colnames(m)[max.col(m)]
   prediction <- factor(prediction)
   return(prediction)

}
