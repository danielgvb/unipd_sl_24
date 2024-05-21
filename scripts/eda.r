# EDA for statistical learning project------------

# remove everything from memory

rm(list = ls())

# Import data------------------


library(readxl)
library(dplyr)
library(car)
library(lmtest)

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
#preocupation of winter in Europe, makes sense to leave it

# Create a pairs plot excluding columns C and D
# pairs(data_reduced) # does not say much

# Variable Transformations----------------


# Create Differences (Inflation)
# Assuming your dataframe is named 'data'
colnames(data)

# Select subset of variables you want to difference
variables_to_calculate <- c("y", "industrial_inputs", "metals",
                            "energy", "shipping", "fx", "industrial_prod",
                            "construction_licences_area", "google_trends")


# Create a function to calculate the 12-month percentual variation
percentual_variation_12_months <- function(x) {
  return((x / dplyr::lag(x, n = 12) - 1))
}

# Apply the function to the subset of variables
data_percentual_variation <- data %>%
  mutate(across(all_of(variables_to_calculate), percentual_variation_12_months, .names = "pct_var_{col}"))

View(data_percentual_variation)

# Create new log-transformed variables
data <- data %>%
  mutate(across(c(y, industrial_inputs, metals, energy, shipping, fx,
                  industrial_prod, construction_licences_area, google_trends),
                list(log = ~log(.)), .names = "{.col}_log"))



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

# Subset only percentual changes
data_percentual_variation <- data_percentual_variation %>%
  select(pct_var_y, pct_var_industrial_inputs, pct_var_metals, pct_var_energy,
         pct_var_shipping, pct_var_fx,pct_var_industrial_prod,
         pct_var_construction_licences_area, pct_var_google_trends,
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
install.packages("reshape2")
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

cov_matrix <- cov(data_reduced)
cov_matrix

correl_matrix <- cor(data_reduced)
correl_matrix

# Heatmap of correl

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




# Multicolinearity Check--------------------

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

# Model on percentual vars
#model <- lm(pct_var_y ~ ., data = data_percentual_variation)
#summary(model)

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

