#remove existing variables:
rm(list = ls())
library(ggplot2) #visualizations
library(caret)
library(DMwR)#knnImputation
library(corrgram)#for correlation calculaltion
library(rpart)#decision tree alg
library(rpart.plot)
library(randomForest)#for random forest algorithm
#set working directory 
setwd("E:/EDW/Projects/Bike Rental")

## Read the data
day_data = read.csv("day.csv", header = T)

#names(day_data)
#[1] "instant"    "dteday"     "season"     "yr"         "mnth"       "holiday"    "weekday"   
#[8] "workingday" "weathersit" "temp"       "atemp"      "hum"        "windspeed"  "casual"    
#[15] "registered" "cnt"

##################################################################################################

#1. Missing Value Identification
missing_value = data.frame(apply(day_data, 2, function(x){sum(is.na(x))}))

#2. Outlier analysis
numeric_index = sapply(day_data,is.numeric) #selecting only numeric
numeric_data = day_data[,numeric_index]
cnames = colnames(numeric_data)

for (i in 1:length(cnames))
{
   assign(paste0("boxplot",i), ggplot(aes_string(y = (cnames[i]), x = "season"), data = subset(day_data))+
            stat_boxplot(geom = "errorbar", width = 0.5) +
            geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                         outlier.size=1, notch=FALSE) +
            theme(legend.position="bottom")+
            labs(y=cnames[i],x="season")+
            ggtitle(paste("Box plot of season vs",cnames[i])))
}

# Plotting box plots together for dependant variables
#gridExtra::grid.arrange(boxplot2, boxplot4, ncol=2)
#gridExtra::grid.arrange(boxplot5, boxplot8,ncol=2)

df = day_data
day_data_wo = day_data
#day_data = df

# #Replace all outliers(windspeed and temp) with NA and impute
val_w = day_data_wo$windspeed[day_data_wo$windspeed %in% boxplot.stats(day_data_wo$windspeed)$out]
day_data_wo$windspeed[day_data_wo$windspeed %in% val_w] = NA

#Imputation
day_data_wo = knnImputation(day_data_wo, k = 2)

##3. Feature Selection - Correlation Plot 
corrgram(day_data[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
##atemp and temp are highly correlated and atemp can be removed
##casual and registered are correlated to cnt, hence cnt can be considered dependant variable
day_data = subset(day_data, select= -c(instant, dteday, yr, atemp, casual, registered))
##4.Feature Scaling is already done by normalising temp, hum, windspeed variables

######################################################################################################

#1. Decision tree algorith for regression
#Divide the data into train and test

train_index = sample(1:nrow(day_data), 0.9 * nrow(day_data))
train = day_data[train_index,]
test = day_data[-train_index,]
##Regression algorithm - anova
regression_result = rpart(cnt ~ ., data = train, method = "anova")

#prediction of test values
predict_DT = predict(regression_result, test[,-10])
rpart.plot(regression_result, type = 1)
#MAPE
#calculate MAPE
mape = function(y, yhat){
  mean(abs((y - yhat)/y))*100
}
accuracy_DT = 100 - (mape(test[,10], predict_DT))
#alternate method to find error metrics for regression.
regr.eval(test[,10], predict_DT, stats = c('mae', 'rmse', 'mape', 'mse'))


##2. Random Forest algorithm
RF_model = randomForest(cnt ~ ., train, importance = TRUE, ntree = 50)
#Predict test data using random forest model
RF_Predictions = predict(RF_model, test[,-10])
accuracy_RF = 100 - (mape(test[,10], RF_Predictions))


##3.Linear regression
#check multicollearity
library(usdm)
vif(day_data[,-11])
vifcor(day_data[,-11], th = 0.9)

#run regression model
lm_model = lm(cnt ~., data = train)

#Summary of the model
summary(lm_model)

#Predict
predictions_LR = predict(lm_model, test[,1:9])
accuracy_LR = 100 - (mape(test[,10], predictions_LR))

##############################################################################################
##############################################################################################
#1. Decision tree algorith for regression - removing outliers
#Divide the data into train and test
day_data_wo = subset(day_data_wo, select= -c(instant, dteday, yr, atemp, casual, registered))
train_index_wo = sample(1:nrow(day_data_wo), 0.9 * nrow(day_data_wo))
train_wo = day_data_wo[train_index_wo,]
test_wo = day_data_wo[-train_index_wo,]
##Regression algorithm - anova
regression_result_wo = rpart(cnt ~ ., data = train_wo, method = "anova")

#prediction of test values
predict_DT_wo = predict(regression_result_wo, test_wo[,-10])
rpart.plot(regression_result_wo, type = 1)
#MAPE
#calculate MAPE
mape = function(y, yhat){
  mean(abs((y - yhat)/y))*100
}
accuracy_DT_wo = 100 - (mape(test_wo[,10], predict_DT_wo))
#alternate method to find error metrics for regression.
regr.eval(test_wo[,10], predict_DT_wo, stats = c('mae', 'rmse', 'mape', 'mse'))


##2. Random Forest algorithm
RF_model_wo = randomForest(cnt ~ ., train_wo, importance = TRUE, ntree = 50)
#Predict test data using random forest model
RF_Predictions_wo = predict(RF_model_wo, test_wo[,-10])
accuracy_RF_wo = 100 - (mape(test_wo[,10], RF_Predictions_wo))


##3.Linear regression
#check multicollearity
library(usdm)
vif(day_data_wo[,-11])
vifcor(day_data_wo[,-11], th = 0.9)

#run regression model
lm_model = lm(cnt ~., data = train_wo)

#Summary of the model
summary(lm_model)

#Predict
predictions_LR_wo = predict(lm_model, test_wo[,1:9])
accuracy_LR_wo = 100 - (mape(test_wo[,10], predictions_LR_wo))

print("Accuracy of Decision tree with Outliers : ")
accuracy_DT
print("Accuracy of Decision tree without Outliers : ")
accuracy_DT_wo
print("Accuracy of Linear Regression Model with Outliers : ")
accuracy_LR
print("Accuracy of Linear Regression Model without Outliers : ")
accuracy_LR_wo
print("Accuracy of Random Forest with Outliers : ")
accuracy_RF
print("Accuracy of Random Forest without Outliers : ")
accuracy_RF_wo