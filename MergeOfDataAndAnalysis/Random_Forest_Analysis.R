# Fitting Random Forest to merged data 

# You may need to install these if not already installed
install.packages("randomForest")
library(randomForest)

# Create a CSV file
write.csv(ARTwarp_ROCCA, "ARTwarp_ROCCA.csv", row.names = FALSE)

# Subset only the selected variables
selected_vars <- c(
  "DURATION",
  "FREQABSSLOPEMEAN",
  "FREQBEG",
  "FREQMAX",
  "FREQQUARTER1",
  "FREQPOSSLOPEMEAN"
)

df_selected <- ARTwarp_ROCCA[, c("category", selected_vars)]

# Convert category to factor (required for classification)
df_selected$category <- as.factor(df_selected$category)

# Fit the Random Forest model
set.seed(123)  # For reproducibility
model <- randomForest(category ~ ., data = df_selected, ntree = 500, importance = TRUE)

# Summarize the model
print(model)

# Variable importance
importance(model)
varImpPlot(model)
