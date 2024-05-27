# EDA for statistical learning project------------

# remove everything from memory

rm(list = ls())

# Import data------------------


library(readxl)
library(dplyr) # for dataframe manipulation
library(car) # for vif
library(lmtest) # for breusch pagan test
library(zoo)

# change here for current data path
data <- read_excel("C:/Users/danie/Documents/GitHub/unipd_sl_24/data/data.xlsx",sheet = "dataframe_col")
View(data)

# view data types---------------

str(data)

# all variables that are not datetype are numerical


# Clean data-------------------
# Counting NA values in each column
na_counts <- apply(data, 2, function(x) sum(is.na(x)))

# Print the counts of NA values per column
print(na_counts)

# Construction and Industrial prod have nans, 
# they are at the extremes so interpolation is not so nice

# Interpolate finished_constructions
# Interpolate NA values in the finished_constructions column
# Interpolate internal NA values in the finished_constructions column
data$finished_constructions <- na.approx(data$finished_constructions, na.rm = FALSE)

# Fill leading NA values with the first non-NA value
data$finished_constructions <- na.locf(data$finished_constructions, na.rm = FALSE, fromLast = FALSE)

# Fill trailing NA values with the last non-NA value
data$finished_constructions <- na.locf(data$finished_constructions, na.rm = FALSE, fromLast = TRUE)

# Do lag of the dependent variable bc should be included as regressor
data <- data %>%
  mutate(y_1 = lag(y, n = 1))




# Removing rows with any NA values
clean_data <- na.omit(data)

# Print the cleaned dataframe
View(clean_data)

# View description --------------
# Get the summary of the dataframe
summary_stats <- summary(clean_data)

# Print the summary statistics
print(summary_stats)

# Drop columns ------------

data_reduced <- subset(clean_data, select = -c(MY, date))

View(data_reduced)


# Outliers---------------

# Hacer interquartil range
# see outliers based on z scores

df_with_z_scores <- data_reduced %>%
  mutate(across(where(is.numeric), ~ scale(.), .names = "z_{col}"))

# Identify outliers
outlier_threshold <- 3
df_with_outliers <- df_with_z_scores %>%
  mutate(across(starts_with("z_"), ~ abs(.) > outlier_threshold, .names = "outlier_{col}"))

# Print data frame with Z-scores and outliers identified
df_with_outliers

outlier_counts <- df_with_outliers %>%
  summarise(across(starts_with("outlier_"), ~ sum(. == TRUE), .names = "count_{col}"))

# Print the counts
outlier_counts

# print the row that has outliers
# Filter the data frame where "outlier_z_energy" is TRUE
filtered_df <- df_with_outliers %>%
  filter(outlier_z_energy == TRUE | outlier_z_shipping == TRUE)

head(filtered_df)

# Filter original df to see when it was
filtered_df_energy <- clean_data %>%
  filter(energy > 370)

filtered_df_energy

# see high values of shipping when happend
filtered_df_shipping <- clean_data %>%
  filter(shipping > 468)

filtered_df_shipping

# It was aug 2022-dec 2022, peak inflation wave, war on Ukraine and,
# preocupation of winter in Europe, makes sense to leave it

# Create a pairs plot excluding columns C and D
# pairs(data_reduced) # does not say much

# Variable Transformations----------------


# Create Differences (Inflation)
# Assuming your dataframe is named 'data'
colnames(data)

# Select subset of variables you want to difference
variables_to_calculate <- c("y", "industrial_inputs", "metals",
                            "energy", "shipping", "fx", "industrial_prod",
                            "construction_licences_area", "finished_constructions",
                            "google_trends")


# Create a function to calculate the 12-month percentual variation
percentual_variation_12_months <- function(x) {
  return((x / dplyr::lag(x, n = 12) - 1))
}

# Apply the function to the subset of variables
data_percentual_variation <- data %>%
  mutate(across(all_of(variables_to_calculate), percentual_variation_12_months, .names = "pct_var_{col}"))

View(data_percentual_variation)




# Create new log-transformed variables
data_reduced <- data_reduced %>%
  mutate(across(c(y,y_1, industrial_inputs, metals, energy, shipping, fx,
                  industrial_prod, construction_licences_area,finished_constructions,
                  google_trends),
                list(log = ~log(.)), .names = "{.col}_log"))

# Subset the dataframe by selecting specific columns
data_final <- data_reduced %>%
  select(y_log, industrial_inputs_log, metals_log, energy_log,
         shipping_log, fx_log,industrial_prod_log,
         construction_licences_area_log, finished_constructions_log,google_trends_log,
         unemployment, interest_rate)

# Subset only percentual changes
data_percentual_variation <- data_percentual_variation %>%
  select(pct_var_y, pct_var_industrial_inputs, pct_var_metals, pct_var_energy,
         pct_var_shipping, pct_var_fx,pct_var_industrial_prod,
         pct_var_construction_licences_area, pct_var_finished_constructions,
         pct_var_google_trends,
         unemployment, interest_rate)

# Ploting ----------------
# reset margins
par(mfrow = c(1, 1))

# dependent variable
clean_data$category <- as.factor(clean_data$date)
# Set up the plot without drawing the points
plot(1:length(clean_data$category), clean_data$y, type = "n", xaxt = "n",
     xlab = "date", ylab = "Home Price Index", main = "Evolution of Home Prices over time")

# Add the categorical labels to the x-axis
axis(1, at = 1:length(clean_data$category), labels = clean_data$category)

# Add the lines and points to the plot
lines(1:length(clean_data$category), clean_data$y, type = "o", col = "blue", pch = 16)

# Histograms/Boxplots of the percentual variations

# Y
boxplot(data_percentual_variation$pct_var_y , horizontal=TRUE  ,frame=F)
hist(data_percentual_variation$pct_var_y , breaks=40)


# Histograms-----------------
par(mar = c(2, 2, 2, 2))  # Adjust margins to make them smaller

# Adjust layout to fit all histograms
par(mfrow = c(3, 5))  # Example layout; adjust if necessary

# Histograms for each variable
for (i in 1:ncol(data_percentual_variation)) {
  hist(data_percentual_variation[[i]], main = names(data_percentual_variation)[i], xlab = names(data_percentual_variation)[i], col = "blue", border = "black")
}


# Reset layout
par(mfrow = c(1, 1))

pairs(data_percentual_variation)

# Boxplot for all at once
#install.packages("reshape2")
library(reshape2)
# Melt the dataframe to long format
# Melt the dataframe to long format
data_long <- melt(data_percentual_variation)

# Adjust graphical parameters
par(mar = c(7, 5, 4, 2) + 0.1)  # Increase bottom margin

# Create the boxplot without x-axis labels
boxplot(value ~ variable, data = data_long,
        main = "Boxplot of Multiple Variables",
        xlab = "", ylab = "Value",
        col = "lightblue", border = "darkblue", xaxt = 'n')

# Add custom x-axis labels at a 45-degree angle
labels <- levels(data_long$variable)
text(x = 1:length(labels), y = par("usr")[3] - 0.5, labels = labels, srt = 45, adj = 1, xpd = TRUE, cex = 0.8)

# Reset graphical parameters to default
par(mar = c(5, 4, 4, 2) + 0.1)
# This one goes in the report, not so much variability in prices even a lot in X vars


# Covariance---------------


cov_matrix <- cov(data_final)
cov_matrix

correl_matrix <- cor(data_final)
correl_matrix

# Heatmap of correl

image(1:ncol(correl_matrix), 1:ncol(correl_matrix), correl_matrix,
      main = "Correlation Matrix Heatmap",
      xlab = "Variables", ylab = "Variables", axes = FALSE,
      col = heat.colors(20))
axis(1, at = 1:ncol(correl_matrix), labels = colnames(correl_matrix), las = 2)
axis(2, at = 1:ncol(correl_matrix), labels = colnames(correl_matrix), las = 2)



# Correlation matrix
cor_matrix <- cor(df_not_log, use = "complete.obs")
print(cor_matrix)

# Visualize the correlation matrix
heatmap(cor_matrix, symm = TRUE)


# Multicolinearity Check--------------------

# here a value bigger than 10 is supposed to have high multicolinearity 
vif_values <-vif(lm(y_log ~ ., data = data_final))
vif_values
# some variables like shipping, industrial prod, metals and industrial input are high,
# Metals and industrial inputs are very similar / metals is a part of industrial inputs,
# drop metals



data_final <- data_final %>% select(-metals_log)
head(data_final)

# re run multicoliniarity

vif(lm(y_log ~ ., data = data_final))

# Industrial production and unemployment could be related but cant delete one at this point.

# consider regularization using ridge or lasso
# or see how it behaves without industrial inputs

# Assume var_to_exclude is the name of the variable you want to exclude
# var_to_exclude <- "industrial_inputs_log"
# 
# # Create a formula that excludes the specific variable
# predictors <- setdiff(names(data_final), c("y_log", var_to_exclude))
# formula <- as.formula(paste("y_log ~", paste(predictors, collapse = " + ")))
# 
# # Fit the model and calculate VIF
# vif_values <- vif(lm(formula, data = data_final))
# print(vif_values)


# OLS 1 Log-Log-----------------
# Perform linear regression
model <- lm(y_log ~ ., data = data_final)  # Replace 'Y' with the dependent variable name
summary(model)

# DO AIC or BIC to select variables also

# Model on percentual vars
#model <- lm(pct_var_y ~ ., data = data_percentual_variation)
#summary(model)

# Post estimation Model 1----------------
# See standard R plots
plot(model)

# Predict and evaluate the model on the test set
predictions <- predict(model, newdata = data_final)
actuals <- data_final$y_log  # Replace 'Y' with the dependent variable name

# Calculate evaluation metrics
mse <- mean((predictions - actuals)^2)
print(mse)

# checking properties of the residuals

residuals     <- residuals(model)
y.hat <- fitted.values(model)
sum(residuals)

# Histogram of residuals
hist(residuals(model), breaks = 30, main = "Residuals Histogram")

# Plot QQ plot and histogram for residuals
qqnorm(residuals, main = "QQ Plot of Residuals", col ='blue')
qqline(residuals, col = "red")
# They look approximately normal

# Do Test of normality to residuals

# For all these tests, the null hypothesis H0  is that the residuals are normally distributed.
# pvalue > 0.05 -> Fail to reject the null hypothesis, suggesting that the residuals are normally distributed.
# Perform Shapiro-Wilk test
shapiro_test <- shapiro.test(residuals)
print(shapiro_test)

# Perform Kolmogorov-Smirnov test
ks_test <- ks.test(residuals, "pnorm", mean = mean(residuals), sd = sd(residuals))
print(ks_test)

# Both p-values are > 0.05, so the residuals should be normal


# Multicolinearity

vif(model)
# Shipping and Industrial production could be linearly correlated


# Model fit
summary(model)$r.squared


# Influential plots
# Cook's Distance Plot
plot(cooks.distance(model), main="Cook's Distance")
abline(h = 4/(nrow(data_final)-length(coef(model))), col="red")  # Common threshold line

# Leverage Plot
plot(hatvalues(model), main="Leverage Values")
abline(h = 2*mean(hatvalues(model)), col="red")  # Common threshold line


# Variable Selection-----------
# F-test for the comparison of nested models
full.mod <- lm(y_log ~ ., data = data_final)
summary(full.mod)
# Remove energy
red.mod <- update(full.mod, . ~ . -energy_log)


anova(red.mod, full.mod)

# This value indicates the significance level of the F-statistic.
# A small p-value (typically < 0.05) suggests that the additional predictors
# in the full model significantly improve the model fit compared 
# to the reduced model.

# Energy does not improve model
summary(red.mod)
# Remove industrial_inputs_log (only price variable so far)
red.mod2 <- update(red.mod, . ~ . -industrial_inputs_log)
anova(red.mod, red.mod2)
# Can safely remove industrial inputs (but makes no sense)
summary(red.mod2)
# All predictors are statistically significant

# Remove finished_constructions_log (only price variable so far)
red.mod3 <- update(red.mod2, . ~ . -finished_constructions_log)
anova(red.mod2, red.mod3)
# Can safely remove finished constructions (but makes no sense)
summary(red.mod3)
# All predictors are statistically significant

# Post estimation 2--------------

residuals_red     <- residuals(red.mod3)

# Histogram of residuals
hist(residuals(red.mod3), breaks = 30, main = "Residuals Histogram Reduced Model")

# Plot QQ plot and histogram for residuals
qqnorm(residuals_red, main = "QQ Plot of Residuals", col ='blue')
qqline(residuals_red, col = "red")
# They look approximately normal

# Do Test of normality to residuals

# For all these tests, the null hypothesis H0  is that the residuals are normally distributed.
# pvalue > 0.05 -> Fail to reject the null hypothesis, suggesting that the residuals are normally distributed.
# Perform Shapiro-Wilk test
shapiro_test <- shapiro.test(residuals_red)
print(shapiro_test)

# Perform Kolmogorov-Smirnov test
ks_test <- ks.test(residuals_red, "pnorm", mean = mean(residuals_red), sd = sd(residuals_red))
print(ks_test)

# Both p-values are > 0.05, so the residuals should be normal

# Check for Homoskedasticity
plot(fitted(red.mod3), residuals(red.mod3), main="Residuals vs Fitted")
abline(h=0, col="red")

# Breusch-Pagan Test

bptest(red.mod3)
# P value < 0.05 -> There is Heteroskedasticiy

# Multicolinearity

vif(red.mod3)
# Shipping and Industrial production could be linearly correlated


# Model fit
summary(red.mod3)$r.squared


# Influential plots
# Cook's Distance Plot
plot(cooks.distance(red.mod3), main="Cook's Distance")
abline(h = 4/(nrow(data_final)-length(coef(red.mod3))), col="red")  # Common threshold line

# Leverage Plot
plot(hatvalues(red.mod3), main="Leverage Values")
abline(h = 2*mean(hatvalues(red.mod3)), col="red")  # Common threshold line

# Homoskedasticity


# Check for Homoskedasticity
plot(fitted(red.mod3), residuals(red.mod3), main="Residuals vs Fitted")
abline(h=0, col="red")

# Breusch-Pagan Test

bptest(red.mod3)
# P value < 0.05 -> There is Heteroskedasticiy
# There seems to be homoskedasticity bc included y_1


# Autocorrelation plot
# Durbin Watson
dwtest(red.mod3)
# H0 (null hypothesis): There is no correlation among the residuals.
# p-value < 0.05 => reject H0, so there looks like there is serial correlation in residuals

# For positive serial correlation, consider adding lags of the dependent and/or independent variable to the model.
# For negative serial correlation, check to make sure that none of your variables are overdifferenced.
# For seasonal correlation, consider adding seasonal dummy variables to the model.

# Check type of autocorrelation

# Plot ACF of residuals
acf(residuals_red, main = "ACF of Regression Residuals")

# Plot PACF of residuals
pacf(residuals_red, main = "PACF of Regression Residuals")

# According to these plots, we should include the lagged y in our model
# to account for the ARIMA nature of the timeseries


# Outliers
std_res <- rstandard(red.mod3)
plot(std_res, main="Standardized Residuals")
abline(h=c(-2, 2), col="red")

# There are are 3 values of outliers

# Model with lagged y-------------

# Add lag of y to account for autocorrelation and heteroskedasticity
# Subset the dataframe by selecting specific columns
data_final <- data_reduced %>%
  select(y_log, y_1_log, shipping_log, fx_log,
         construction_licences_area_log,google_trends_log,
         unemployment, interest_rate)


model_lagged <- lm(y_log ~ ., data = data_final)  # Replace 'Y' with the dependent variable name
summary(model_lagged)

residuals_lagged    <- residuals(model_lagged)
# Create a histogram with more customization
hist(residuals_lagged, 
     main = "Histogram of Residuals", 
     xlab = "Residuals", 
     ylab = "Frequency", 
     col = "skyblue", 
     border = "white",
     breaks = 30)  # You can change the number of bins


# Plot QQ plot and histogram for residuals
qqnorm(residuals_lagged, main = "QQ Plot of Residuals", col ='blue')
qqline(residuals_lagged, col = "red")
# They look approximately normal

# Do Test of normality to residuals

# For all these tests, the null hypothesis H0  is that the residuals are normally distributed.
# pvalue > 0.05 -> Fail to reject the null hypothesis, suggesting that the residuals are normally distributed.
# Perform Shapiro-Wilk test
shapiro_test <- shapiro.test(residuals_lagged)
print(shapiro_test)

# Perform Kolmogorov-Smirnov test
ks_test <- ks.test(residuals_lagged, "pnorm", mean = mean(residuals_lagged), sd = sd(residuals_lagged))
print(ks_test)

# Both p-values are > 0.05, so the residuals should be normal



# Multicolinearity

vif(model_lagged)
# Shipping and Industrial production could be linearly correlated


# Model fit
summary(model_lagged)$r.squared


# Influential plots
# Cook's Distance Plot
plot(cooks.distance(model_lagged), main="Cook's Distance")
abline(h = 4/(nrow(data_final)-length(coef(model_lagged))), col="red")  # Common threshold line

# Leverage Plot
plot(hatvalues(model_lagged), main="Leverage Values")
abline(h = 2*mean(hatvalues(model_lagged)), col="red")  # Common threshold line

# Homoskedasticity


# Check for Homoskedasticity
plot(fitted(model_lagged), residuals(model_lagged), main="Residuals vs Fitted")
abline(h=0, col="red")

# Breusch-Pagan Test

bptest(model_lagged)
# P value < 0.05 -> There is Heteroskedasticiy



# Autocorrelation plot
# Durbin Watson
dwtest(model_lagged)
# H0 (null hypothesis): There is no correlation among the residuals.
# p-value < 0.05 => reject H0, so there looks like there is serial correlation in residuals

# For positive serial correlation, consider adding lags of the dependent and/or independent variable to the model.
# For negative serial correlation, check to make sure that none of your variables are overdifferenced.
# For seasonal correlation, consider adding seasonal dummy variables to the model.

# Check type of autocorrelation

# Plot ACF of residuals
acf(residuals_lagged, main = "ACF of Regression Residuals")

# Plot PACF of residuals
pacf(residuals_lagged, main = "PACF of Regression Residuals")

# According to these plots, we should include the lagged y in our model
# to account for the ARIMA nature of the timeseries


# Outliers
std_res <- rstandard(model_lagged)
plot(std_res, main="Standardized Residuals")
abline(h=c(-2, 2), col="red")

# OLS Robust STD Errors--------------

#install.packages("sandwich")
library(sandwich)

data_final <- data_reduced %>%
  select(y_log, shipping_log, fx_log, construction_licences_area_log,
         industrial_prod_log,google_trends_log,
         unemployment, interest_rate)

model <- lm(y_log ~ ., data = data_final)
summary (model)
# Calculate robust standard errors
robust_se <- coeftest(model, vcov = vcovHC(model, type = "HC1"))

# Display coefficients with robust standard errors
print(robust_se)


# Interpretation:
# B0: when all the covariates are 0, then y_log is 0.55,
# which is 1.74
# B1, A 1% increase in the shipping costs,
# means a 0.09% increase in house prices (MoM)

# Coefficients dont change that much

# Postestimation is the same as red.mod3
# Everything is the same, except the coefficients of the LM, which account for SE

# Forward AIC Stepwise selection-------------

# Fit the null model (intercept only)
null_model <- lm(y_log ~ 1, data = data_reduced)

# Specify the full model with all potential predictors
full_model <- lm(y_log ~ industrial_inputs_log + shipping_log + fx_log + industrial_prod_log + 
                   construction_licences_area_log + google_trends_log + 
                   unemployment + interest_rate, data = data_reduced)

# Perform forward selection based on AIC
forward_model <- step(null_model, 
                      scope = list(lower = null_model, upper = full_model), 
                      direction = "forward")

# Display the summary of the selected model
summary(forward_model)

# Backward AIC Stepwise selection-------------

full_model <- lm(y_log ~ industrial_inputs_log+ shipping_log + fx_log + industrial_prod_log + 
                   construction_licences_area_log + google_trends_log + 
                   unemployment + interest_rate, data = data_reduced)

# Perform backward selection based on AIC
backward_model <- step(full_model, 
                       direction = "backward")

# Display the summary of the selected model
summary(backward_model)

# Backward gives construction licences, which is good

# Overall the models converge to this last model (removing by p-value), stepwise...
# So we can do the post estimation of the red.mod3, because is the same,
# but use the coefficients of the model with robust errors for inference


# Regularization------------
# Done over full model
#install.packages("glmnet")
#install.packages("caret")
#install.packages("Metrics")



# Load the packages
library(glmnet)
library(caret)
library(Metrics)


# Subset the dataframe by selecting specific columns
data_final <- data_reduced %>%
  select(y_log, industrial_inputs_log, metals_log, energy_log,
         shipping_log, fx_log,industrial_prod_log,
         construction_licences_area_log, finished_constructions_log,google_trends_log,
         unemployment, interest_rate)

# Sample data preparation
# Assuming your data frame is `data_final` and your response variable is `y_log`
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(data_final$y_log, p = .8, 
                                  list = FALSE, 
                                  times = 1)
data_train <- data_final[ trainIndex,]
data_test  <- data_final[-trainIndex,]

# Extract predictors and response
X_train <- model.matrix(y_log ~ ., data_train)[, -1]  # Remove the intercept column
y_train <- data_train$y_log

X_test <- model.matrix(y_log ~ ., data_test)[, -1]  # Remove the intercept column
y_test <- data_test$y_log

# Fit linear regression model
lm_model <- lm(y_log ~ ., data = data_train)

# Predict and evaluate linear regression model
lm_predictions <- predict(lm_model, newdata = data_test)
lm_mse <- mse(y_test, lm_predictions)
lm_r2 <- R2(y_test, lm_predictions)
cat("Linear Regression MSE:", lm_mse, "\n")
cat("Linear Regression R2:", lm_r2, "\n")

# Fit LASSO regression model
lasso_model <- cv.glmnet(X_train, y_train, alpha = 1)  # alpha = 1 for LASSO
lasso_best_lambda <- lasso_model$lambda.min
lasso_predictions <- predict(lasso_model, s = lasso_best_lambda, newx = X_test)
lasso_mse <- mse(y_test, lasso_predictions)
lasso_r2 <- R2(y_test, lasso_predictions)
cat("LASSO Regression MSE:", lasso_mse, "\n")
cat("LASSO Regression R2:", lasso_r2, "\n")

# Coefficients at best lambda
lasso_coefficients <- coef(lasso_model, s = lasso_best_lambda)
print(lasso_coefficients)


# Fit Ridge regression model
ridge_model <- cv.glmnet(X_train, y_train, alpha = 0)  # alpha = 0 for Ridge
ridge_best_lambda <- ridge_model$lambda.min
ridge_predictions <- predict(ridge_model, s = ridge_best_lambda, newx = X_test)
ridge_mse <- mse(y_test, ridge_predictions)
ridge_r2 <- R2(y_test, ridge_predictions)
cat("Ridge Regression MSE:", ridge_mse, "\n")
cat("Ridge Regression R2:", ridge_r2, "\n")

# Coefficients at best lambda
ridge_coefficients <- coef(ridge_model, s = ridge_best_lambda)
print(ridge_coefficients)

# Compare the results
results <- data.frame(
  Model = c("Linear Regression", "LASSO Regression", "Ridge Regression"),
  MSE = c(lm_mse, lasso_mse, ridge_mse),
  R2 = c(lm_r2, lasso_r2, ridge_r2)
)

print(results)




# Meter el cemento (esperando refinitiv)

# Meter AIC y BIC backward and forward selection
# Meter un modelo generalizado de errores no normales
# Montar un Notebook

