# Fitting multinomial glm to merged data 

# You may need to install these if not already installed
install.packages("nnet")
library(nnet)

# Create a csv file 
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

# Fit the multinomial logistic regression model
model <- multinom(category ~ ., data = df_selected)

# Summarise the model
summary(model)
