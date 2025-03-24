# Big Hawaii Dataset

library(readr)
library(dplyr)
library(ggplot2)
library(corrr)

# Load the dataset
df <- read_csv("ARTwarp_final_big_hawaii_2025_all(Sheet1).csv")

# View the first few rows
head(df)

# Check column names
colnames(df)

# Get a summary of each column
summary(df)

# Check for missing values
colSums(is.na(df))

# View structure of dataset
str(df)

## Check unique values and distributions
# Count unique values in categorical columns
df %>% summarise(across(where(is.character), n_distinct))

# Histogram of a numerical column
ggplot(df, aes(x = category)) + 
  geom_histogram(bins = 30, fill = "blue", alpha = 0.5) + 
  theme_minimal()  # Replace with actual numeric column name

# Check for multicollinearity 
df_numeric <- df %>% select(where(is.numeric))
correlation_matrix <- correlate(df_numeric)
correlation_matrix

# Address multicollinearity between length and ctrlength
# Fit a linear model regressing one variable on the other
r_squared <- summary(lm(ctrlength ~ length, data = df))$r.squared

# Compute VIF
vif_value <- 1 / (1 - r_squared)
vif_value

# PCA on ctrlength and length
install.packages("FactoMineR")  # If not installed
install.packages("factoextra")  # For visualization

library(FactoMineR)
library(factoextra)

# Select only the two highly correlated variables
pca_data <- df %>% select(ctrlength, length)

# Run PCA
pca_result <- PCA(pca_data, scale.unit = TRUE, graph = FALSE)

# View explained variance
pca_result$eig

# Get PC1 values
df$PC1 <- pca_result$ind$coord[, 1]

# Check distribution
summary(df$PC1)

## Fit multinomial model 
install.packages("nnet")  # If not installed
library(nnet)

# Ensure the response variable is a factor 
df$match <- as.factor(df$category)

# Fit the multinomial model
multi_model <- multinom(category ~ PC1 + tempres + match, data = df)

# Model summary
summary(multi_model)

rare_categories <- names(which(table(df$category) < 15))  # Change threshold to 15
df$category <- as.character(df$category)
df$category[df$category %in% rare_categories] <- "Other"
df$category <- as.factor(df$category)

# Check the new category distribution
table(df$category)  # Verify category counts after merging
# We still get the same issue of having too many unique categories to fit a multinomial model

# Regularised multinomial regression 
install.packages("glmnet")
library(glmnet)

# Convert response to numeric factor
y <- as.numeric(as.factor(df$category)) - 1  # Convert category to numeric starting from 0
X <- model.matrix(~ PC1 + tempres + match - 1, data = df)  # Convert predictors to matrix

# Fit ridge-regularised multinomial model
multi_model <- cv.glmnet(X, y, family = "multinomial", alpha = 1)  # Lasso regression

# Extract model coefficients 
coef(final_model, s = best_lambda)  # Get coefficients for the chosen lambda

# Make predictions
pred_probs <- predict(final_model, newx = X, s = best_lambda, type = "response")  # Probabilities
pred_labels <- predict(final_model, newx = X, s = best_lambda, type = "class")  # Class predictions

# Evaluate model performance 
conf_matrix <- table(pred_labels, y)  # Confusion matrix
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)

# More detailed metrics 
library(caret)
confusionMatrix(as.factor(pred_labels), as.factor(y))


