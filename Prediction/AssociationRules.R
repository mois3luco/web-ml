#
# Association Rules
#

require(arules) # http://cran.r-project.org/web/packages/arules/vignettes/arules.pdf

## NUMBER OF RULES

support <- c(0.01,0.05,0.1)
confidence <- c(0.75,0.85,0.95)

num_rules <- numeric()

for(sp in support){
  for(cf in confidence){
    num_rules[length(num_rules)+1] <- length(apriori(transactions, 
                                      parameter =list(support = sp, confidence=cf)))
  }
}

## RULES MINING

# "Transactions" format (similar to sparse matrix)
transactions <- as(data, "transactions")

# Get rules
rules <- apriori(transactions, parameter =list(support = 0.1, confidence=0.75))

# Plot rules
require(arulesViz)

plot(rules) # Scatter plot
plot(rules, method="matrix", measure=c("lift", "confidence")) # Matrix-based plot
plot(rules, method="matrix3d", measure="lift") # Matrix-based 3D plot
plot(rules,method="grouped") # Grouped matrix plot


# Extra measures
measures <- interestMeasure(rules,
                            method=c("coverage", "fishersExactTest"),
                            transactions=transactions)

# Filter rules X -> Destination URL

long_urls <- colnames(transactions)[67:90]

not_long_urls <- colnames(transactions)[c(1:66,91:95)]

rules_url <- apriori(transactions,
                 parameter =list(support = 0.001, confidence=0.5),
                 appearance=list(rhs=long_urls, lhs=not_long_urls))

## PREDICT

test_ids <- strata(data=data, size=round(table(data$long_url)/nrow(data)*3000)
                   , method="srswor", description=FALSE, stratanames=c("long_url"))
test_set <- rest[test_ids$ID_unit,]
rm(test_ids)

prediction <- recommend(rules_url, test_set)

## PREDICTION FUNCTION DEFINITION

recommend <- function(rules, test_data){
  
  transactions <- as(test_data, "transactions")
  results <- logical() 
  
  for(i in 1:nrow(transactions)){
    
    row <- transactions[i,]
    # Select suitable rules (transaction includes X)
    suitableRules <- is.subset(rules@lhs, as(row, "transactions"))

    if(sum(suitableRules) == 0){
      
      results[length(results)+1] <- F
      
    }else{
      # Select most voted Y (rule X->Y)
      recommendation <- names(sort(table(as.character(LIST(rules[suitableRules]@rhs))),decreasing=T)[1])
      results[length(results)+1] <- as.logical(as(row, "matrix")[which(colnames(row)==recommendation)])
      
    }
 
  }
  
  return(results)
  
} 





