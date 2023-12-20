
###Instaling and calling all the required packaged##
library(tinytex)
library(latexpdf)
library(kernlab)
library("readxl")
library("lmtest")
library(ggplot2)
library("MASS")

###Reading Data set###
setwd("C:/Users/ptoram3/Downloads")
data = read.csv("clean_data.csv")
data =subset(data,select = -Country)
data =subset(data,select = -X)
head(data)
# Display basic information about the dataset
str(data)
# Summary statistics
summary(data)
# Check for missing values
colSums(is.na(data))
cols_to_remove=c("Country","Status","Population")
cols_to_apply=setdiff(colnames(data),cols_to_remove)

###Scaling the data with minmax scaler
standardize <- function(x) {
  return((x - min(x,na.rm=TRUE)) / (max(x,na.rm=TRUE) - min(x,na.rm=TRUE)))
}
data_scaled <- as.data.frame(lapply(data[,cols_to_apply], function(x) if(is.numeric(x)) 
  standardize(x) else x))
finaldata= data_scaled

#finaldata$Year=as.factor(data$Year)
finaldata$Status=as.factor(data$Status)

####Spilitting train(80%) and test(20%) data
set.seed(9)
sample_size = floor(0.8*nrow(data))
picked = sample(seq_len(nrow(finaldata)),size = sample_size)

# Remove irrelevant columns from training data
train = finaldata[picked,]
test =  finaldata[-picked,]

###MODEL1 MLR with all variables
model1 <- lm(Life_expectancy ~ ., data = train)
summary(model1)
 
###Model performance metrics and 
resids = stdres(model1)
predictions = predict(model1,newdata = test)
actual_values <- test$Life_expectancy
residuals_test <- actual_values - predictions
# Show the residuals for the test data
print(residuals_test)
R_squared <- 1 - (sum(residuals_test^2) / sum((actual_values - mean(actual_values))^2))
# Print R-squared on test data
print(paste("R-squared on test data:", R_squared))
mean_actual <- mean(actual_values)
# Calculate the total sum of squares (TSS)
TSS <- sum((actual_values - mean_actual)^2)
# Calculate residual sum of squares (RSS)
RSS <- sum(residuals_test^2)

# Calculate degrees of freedom for residuals (n - p - 1)
n <- length(actual_values)  # Number of observations
p <- length(coefficients(model1)) - 1  # Number of predictors (excluding intercept)
df_residual <- n - p - 1
# Calculate adjusted R-squared
adjusted_R_squared <- 1 - (RSS / df_residual) / (TSS / (n - 1))
# Print adjusted R-squared on test data
print(paste("Adjusted R-squared on test data:", adjusted_R_squared))
# Mean Squared Prediction Error (MSPE)
MSPE = mean((predictions-actual_values)^2)
# Mean Absolute Prediction Error (MAE)
MAE = mean(abs(predictions-actual_values))
# Mean Absolute Percentage Error (MAPE)
MAPE=mean(abs(predictions-actual_values)/actual_values)
# Precision Measure (PM)
PM = sum((predictions-actual_values)^2)/sum((actual_values-mean(actual_values))^2)

print(paste("Adjusted R-squared on test data:", adjusted_R_squared))
print(paste("MSPE:", MSPE))
print(paste("MAE:", MAE))
print(paste("PM:", PM))



####Residual Plot
par(mfrow =c(3,3))
par(mar=c(1,1,1,1))
par(mgp=c(0,0,0))

for (i in seq(2,18)){
#print(i)
plot(train[,i],resids,xlab = "", ylab = "",main = paste("Residuals vs", colnames(train)[i]))
abline(0,0,col="red")
}


par(mgp=c(3,1,0))
library(car)
fits = model1$fitted
cook = cooks.distance(model1)
par(mfrow =c(2,2))
plot(fits, resids, xlab="Fitted Values",ylab="Residuals")
abline(0,0,col="red")
qqPlot(resids, ylab="Residuals", main = "")
hist(resids, xlab="Residuals", main = "",nclass=10,col="orange")
plot(cook,type="h",lwd=3,col="red", ylab = "Cook's Distance")

# Get influential points (observations with high Cook's distance)
influential_points <- which(cook > (4 / length(cook)))

# Remove influential points from the training data
clean_training_data <- train[-influential_points, ]

# Retrain the model without influential points
updated_model <- lm(Life_expectancy ~ ., data = clean_training_data)

# Summary of the updated model
summary(updated_model)
library(car)
# Calculate VIF for each predictor variable in the model
vif_values <- car::vif(updated_model)
# Display VIF values
print(vif_values)
1/(1-summary(model)$r.squared)


car::vif(model2)
### Second Model
columns_to_remove <- c("infant_deaths")
# Remove specified columns from the dataset
data_subset <- clean_training_data[, !names(clean_training_data) %in% columns_to_remove]
write.csv(data_subset, file = "VIF_outlier_train.csv", row.names = FALSE)
test_subset = test[, !names(clean_training_data) %in% columns_to_remove]
write.csv(test_subset, file = "VIF_test.csv", row.names = FALSE)
## Box Cox transformation
bc <- boxcox(updated_model)
lambda <- bc$x[which(bc$y==max(bc$y))]
lambda


##########Model 2 - VIF and outlier #########
colnames(clean_training_data)
model2 <- lm(Life_expectancy ~ ., data = data_subset)
summary(model2)

test_subset
resids = stdres(model2)
predictions = predict(model2,newdata = test_subset)
actual_values <- test_subset$Life_expectancy
residuals_test <- actual_values - predictions

# Show the residuals for the test data
print(residuals_test)
R_squared <- 1 - (sum(residuals_test^2) / sum((actual_values - mean(actual_values))^2))

# Print R-squared on test data
print(paste("R-squared on test data:", R_squared))
mean_actual <- mean(actual_values)
# Calculate the total sum of squares (TSS)
TSS <- sum((actual_values - mean_actual)^2)
# Calculate residual sum of squares (RSS)
RSS <- sum(residuals_test^2)
# Calculate degrees of freedom for residuals (n - p - 1)
n <- length(actual_values)  # Number of observations
p <- length(coefficients(model)) - 1  # Number of predictors (excluding intercept)
df_residual <- n - p - 1

# Calculate adjusted R-squared
adjusted_R_squared <- 1 - (RSS / df_residual) / (TSS / (n - 1))

# Print adjusted R-squared on test data
print(paste("Adjusted R-squared on test data:", adjusted_R_squared))


##Residual Plots
par(mfrow =c(4,4))
par(mar=c(1,1,1,1))
par(mgp=c(0,0,0))

for (i in seq(2,17)){
  #print(i)
  plot(data_subset[,i],resids,xlab = "", ylab = "",main = paste("Residuals vs", colnames(data_subset)[i]))
  abline(0,0,col="red")
}

par(mgp=c(3,1,0))
library(car)
library("MASS")
resids = stdres(model2)
fits = model2$fitted
cook = cooks.distance(model2)
par(mfrow =c(2,2))
plot(fits, resids, xlab="Fitted Values",ylab="Residuals")
abline(0,0,col="red")
qqPlot(resids, ylab="Residuals", main = "")
hist(resids, xlab="Residuals", main = "",nclass=10,col="orange")
plot(cook,type="h",lwd=3,col="red", ylab = "Cook's Distance")



## Model 3
# Forward Stepwise Regression
#forward_stepwise = step(lm(Life_expectancy~1,data = data_subset), scope=formula(model2), direction="forward")
# Backward Stepwise Regression
backward_stepwise <- stepAIC(model2, direction = "backward")

# Print the results
#summary(forward_stepwise)
summary(backward_stepwise)

test_subset
resids = stdres(backward_stepwise)
predictions = predict(backward_stepwise,newdata = test_subset)
actual_values <- test_subset$Life_expectancy
residuals_test <- actual_values - predictions

# Show the residuals for the test data
print(residuals_test)

R_squared <- 1 - (sum(residuals_test^2) / sum((actual_values - mean(actual_values))^2))

# Print R-squared on test data
print(paste("R-squared on test data:", R_squared))

mean_actual <- mean(actual_values)

# Calculate the total sum of squares (TSS)
TSS <- sum((actual_values - mean_actual)^2)

# Calculate residual sum of squares (RSS)
RSS <- sum(residuals_test^2)

# Calculate degrees of freedom for residuals (n - p - 1)
n <- length(actual_values)  # Number of observations
p <- length(coefficients(model)) - 1  # Number of predictors (excluding intercept)
df_residual <- n - p - 1

# Calculate adjusted R-squared
adjusted_R_squared <- 1 - (RSS / df_residual) / (TSS / (n - 1))

# Print adjusted R-squared on test data
print(paste("Adjusted R-squared on test data:", adjusted_R_squared))

par(mfrow =c(4,5))
par(mar=c(1,1,1,1))

for (i in seq(2,18)){
  #print(i)
  plot(data_subset[,i],resids,xlab=colnames(data_subset)[i],ylab="Residuals")
  abline(0,0,col="red")
}

par(mfrow =c(3,3))
par(mar=c(1,1,1,1))
par(mgp=c(0,0,0))

for (i in seq(2,18)){
  #print(i)
  plot(data_subset[,i],resids,xlab = "", ylab = "",main = paste("Residuals vs", colnames(data_subset)[i]))
  abline(0,0,col="red")
}

par(mgp=c(3,1,0))
library(car)
library("MASS")
resids = stdres(backward_stepwise)
fits = backward_stepwise$fitted
cook = cooks.distance(backward_stepwise)
par(mfrow =c(2,2))
plot(fits, resids, xlab="Fitted Values",ylab="Residuals")
abline(0,0,col="red")
qqPlot(resids, ylab="Residuals", main = "")
hist(resids, xlab="Residuals", main = "",nclass=10,col="orange")
plot(cook,type="h",lwd=3,col="red", ylab = "Cook's Distance")


###MODEL 4##

# Lasso regression to get variable importance
install.packages("glmnet")
# library(glmnet)

lasso_model <- cv.glmnet(X_train, y_train, alpha = 1)
lasso_coefs <- coef(lasso_model, s = "lambda.min")
lasso_coefs=as.matrix(lasso_coefs)
lasso_variable_importance <- data.frame(variable = rownames(lasso_coefs), importance = abs(lasso_coefs))
lasso_variable_importance=lasso_variable_importance[order(lasso_variable_importance$s1,decreasing = TRUE),]

lasso_variable_importance <- lasso_variable_importance[lasso_variable_importance$s1 > 0.1, ]
#col_in_model4=c("infant_deaths","HIV_AIDS","Schooling","Adult_Mortality","Income_composition_of_resources","percentage_expenditure","Life_expectancy")

train_model4 = train[,colnames(train) %in% lasso_variable_importance ]
test_model4 = test[,colnames(test) %in% lasso_variable_importance ]

model4 <- lm(Life_expectancy ~ ., data = train_model4)
summary(model4)

#Model performance of Model4(including variables given by Lasso regression
resids = stdres(model4)
predictions = predict(model4,newdata = test_model4)
actual_values <- test_model4$Life_expectancy
residuals_test <- actual_values - predictions

# Show the residuals for the test data
print(residuals_test)
R_squared <- 1 - (sum(residuals_test^2) / sum((actual_values - mean(actual_values))^2))
# Print R-squared on test data
print(paste("R-squared on test data:", R_squared))
mean_actual <- mean(actual_values)
# Calculate the total sum of squares (TSS)
TSS <- sum((actual_values - mean_actual)^2)
# Calculate residual sum of squares (RSS)
RSS <- sum(residuals_test^2)
# Calculate degrees of freedom for residuals (n - p - 1)
n <- length(actual_values)  # Number of observations
p <- length(coefficients(model)) - 1  # Number of predictors (excluding intercept)
df_residual <- n - p - 1
# Calculate adjusted R-squared
adjusted_R_squared <- 1 - (RSS/ df_residual) / (TSS / (n - 1))

# Print adjusted R-squared on test data
print(paste("Adjusted R-squared on test data:", adjusted_R_squared))

###Residual Plots and cooks plot

par(mfrow =c(2,3))
par(mar=c(1,1,1,1))
par(mgp=c(0,0,0))

for (i in seq(2,17)){
  #print(i)
  plot(train_model4[,i],resids,xlab = "", ylab = "",main = paste(colnames(train_model4)[i]))
  abline(0,0,col="red")
}

par(mgp=c(3,1,0))
library(car)
library("MASS")
resids = stdres(model4)
fits = model4$fitted
cook = cooks.distance(model4)
par(mfrow =c(2,2))
plot(fits, resids, xlab="Fitted Values",ylab="Residuals")
abline(0,0,col="red")
qqPlot(resids, ylab="Residuals", main = "")
hist(resids, xlab="Residuals", main = "",nclass=10,col="orange")
plot(cook,type="h",lwd=3,col="red", ylab = "Cook's Distance")


