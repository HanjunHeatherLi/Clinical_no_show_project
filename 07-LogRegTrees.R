library(randomForest)
if (!require(caret)) install.packages("caret")
library(caret)
library(dplyr)
library(tidyr)
if (!require(aod)) install.packages("aod")
library(aod)

######## Preprocess ########
# Read data set
setwd('E:/Courses/GT_OMSA/2021-Spring/ISYE-6740/CDA_project/CDA_DataAnalysis')
Xtrain = read.csv('Xtrain_r.csv')
ytrain = read.csv('ytrain_r.csv')
Xtest = read.csv('Xtest_r.csv')
ytest = read.csv('ytest_r.csv')

# Convert to factors
col_factor = colnames(Xtrain)[-c(2,9)]
Xtrain[col_factor] = lapply(Xtrain[col_factor], factor)
Xtest[col_factor] = lapply(Xtest[col_factor], factor)

ytrain$Noshow = as.factor(ytrain$Noshow)
ytest$No.show = as.factor(ytest$No.show)
train = cbind(Xtrain, ytrain)
test = cbind(Xtest, ytest)

logreg = glm(Noshow ~ ., data = train, family = "binomial")
summary(logreg)
# Test if overall DetailedLocation is significant
wald.test(b = coef(logreg), Sigma = vcov(logreg), Terms = 16:22)

# Odds ratio
exp(coef(logreg))

test$pred = predict(logreg, newdata = test[, !names(train) %in% c('No.show')], type = 'response')
test = test %>% mutate(predlabel = ifelse(pred <= 0.5, 'No', 'Yes'))
table(test$predlabel, test$No.show)


###### Decision tree #####
library(rpart)
if (!require(rpart.plot)) install.packages("rpart.plot")
library(rpart.plot)
dtc = rpart(Noshow ~ ., data = train, method = 'class', minsplit = 20, minbucket = 20, maxdepth = 15)
pred_tree = predict(logreg, Xtest, type = 'response')
pred_tree = as.data.frame(pred_tree)
pred_tree = pred_tree %>% mutate(predlabel = ifelse(pred_tree <= 0.5, 'No', 'Yes'))
table(pred_tree$predlabel, ytest$No.show)
rpart.plot(dtc, type = 4, extra = 1, box.palette="GnBu", branch.lty=3)


##### RF #####
rf_opt = randomForest(
  Noshow ~., data = train, ntree = 500, 
  mtry = 8, importance = TRUE, type = 'classification'
)

pred_tree = predict(rf_opt, Xtest, type = 'response')
pred_tree = as.data.frame(pred_tree)
pred_tree = pred_tree %>% mutate(predlabel = ifelse(pred_tree <= 0.5, 'No', 'Yes'))
table(pred_tree$predlabel, ytest$No.show)

# Retrieve the importance of each feature
rf_imp = importance(rf_opt)
rf_imp = as.data.frame(rf_imp)
rf_imp = cbind(Variable = rownames(rf_imp), rf_imp)
# Output feature importance
# write.csv(rf_imp, 'rf_imp.csv', row.names = FALSE)

# Plot top 10 importance
varImpPlot(
  rf_opt, type = 1, n.var = 10, 
  main = "Top 10 most important variables", col = 'blue'
)
