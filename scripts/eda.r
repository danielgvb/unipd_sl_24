# EDA for statistical learning project------------
# Import data------------------
library(readxl)


data <- read_excel("../data/data.xlsx",sheet = "dataframe_col")
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

# Create a pairs plot excluding columns C and D
pairs(data_reduced)


# Basic boxplot for multiple columns
# Setting up the plotting area
par(mfrow = c(1, ncol(data_reduced)))  # Arrange plots in 1 row and as many columns as variables

# Loop over each column to create a boxplot
for (i in 1:ncol(data_reduced)) {
  boxplot(data_reduced[, i], main = names(data_reduced)[i], col = rainbow(ncol(data_reduced))[i])
}

# do a series plot to see if there is trend and seasonality----------------------


# Histograms-----------------
par(mar = c(2, 2, 2, 2))  # Adjust margins to make them smaller

# Adjust layout to fit all histograms
par(mfrow = c(3, 5))  # Example layout; adjust if necessary

# Histograms for each variable
for (i in 1:ncol(data_reduced)) {
  hist(data_reduced[[i]], main = names(data_reduced)[i], xlab = names(data_reduced)[i], col = "blue", border = "black")
}

# Reset layout
par(mfrow = c(1, 1))



# Covariance---------------

cov_matrix <- cov(data_reduced)
cov_matrix

correl_matrix <- cor(data_reduced)
correl_matrix



# Heatmap using base R
image(1:ncol(correl_matrix), 1:ncol(correl_matrix), correl_matrix,
      main = "Correlation Matrix Heatmap",
      xlab = "Variables", ylab = "Variables", axes = FALSE,
      col = heat.colors(20))
axis(1, at = 1:ncol(correl_matrix), labels = colnames(correl_matrix), las = 2)
axis(2, at = 1:ncol(correl_matrix), labels = colnames(correl_matrix), las = 2)



# Correlation matrix
cor_matrix <- cor(data_reduced, use = "complete.obs")
print(cor_matrix)

# Visualize the correlation matrix
heatmap(cor_matrix, symm = TRUE)

# Variable Transformations----------------
library(dplyr) # Check if can use

# Create new log-transformed variables
data_reduced <- data_reduced %>%
  mutate(across(c(y, industrial_inputs, metals, energy, shipping, fx,
                  industrial_prod, construction_licences_area, google_trends),
                list(log = ~log(.)), .names = "{.col}_log"))

# Subset the dataframe by selecting specific columns
data_final <- data_reduced %>%
  select(y_log, industrial_inputs_log, metals_log, energy_log,
         shipping_log, fx_log,industrial_prod_log,
         construction_licences_area_log, google_trends_log,
         unemployment, interest_rate)
# Multicolinearity Check--------------------
install.packages("car")
library(car)

# here a value bigger than 10 is supposed to have high multicolinearity 
vif(lm(y_log ~ ., data = data_final))

# some variables like shipping, industrial prod, metals and industrial input are high,
# consider regularization using ridge or lasso
# or see how it behaves without industrial inputs

# Assume var_to_exclude is the name of the variable you want to exclude
var_to_exclude <- "industrial_inputs_log"

# Create a formula that excludes the specific variable
predictors <- setdiff(names(data_final), c("y_log", var_to_exclude))
formula <- as.formula(paste("y_log ~", paste(predictors, collapse = " + ")))

# Fit the model and calculate VIF
vif_values <- vif(lm(formula, data = data_final))
print(vif_values)


# Linear Regression-----------------
# Perform linear regression
model <- lm(y_log ~ ., data = data_final)  # Replace 'Y' with the dependent variable name
summary(model)

# Predict and evaluate the model on the test set
predictions <- predict(model, newdata = data_final)
actuals <- data_final$y_log  # Replace 'Y' with the dependent variable name

# Calculate evaluation metrics
mse <- mean((predictions - actuals)^2)
print(mse)

# Post estimation--------------

# checking properties of the residuals

residuals     <- residuals(model)
y.hat <- fitted.values(model)
sum(residuals)

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


# Check for Homoskedasticity
plot(fitted(model), residuals(model), main="Residuals vs Fitted")
abline(h=0, col="red")

# Breusch-Pagan Test
install.packages("lmtest")
library(lmtest)
bptest(model)
# P value < 0.05 -> There is Heteroskedasticiy

# Cook's Distance Plot
plot(cooks.distance(model), main="Cook's Distance")
abline(h = 4/(nrow(data_final)-length(coef(model))), col="red")  # Common threshold line

# Leverage Plot
plot(hatvalues(model), main="Leverage Values")
abline(h = 2*mean(hatvalues(model)), col="red")  # Common threshold line


# Tests suggest normality in the residuals, but barely in the Shapiro Wilk 
# F-test for the comparison of nested models


full.mod <- lm(y_log ~ ., data = data_final)
red.mod <-  lm(sales~TV)

anova(red.mod, full.mod)

