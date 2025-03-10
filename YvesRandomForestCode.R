library(dplyr)

relevant_data <- filtered_data %>%
  select("SELECTIONNUMBER", "FREQMAX", "FREQRANGE", "DCSTDDEV",
         "DCQUARTER1MEAN", "DCQUARTER3MEAN", "DCQUARTER4MEAN", "FREQSTEPUP",
         "FREQSTEPDOWN", "FREQSLOPEMEAN", "FREQNEGSLOPEMEAN", "FREQBEGSWEEP",
         "FREQENDSWEEP", "NUMSWEEPSUPDWN", "NUMSWEEPSUPFLAT", 
         "FREQSWEEPFLATPERCENT", "INFLMINDELTA", "INFLMAXMINDELTA") %>%
  as.data.frame()

View(relevant_data)
#install.packages("randomForest")

library(randomForest)

relevant_data <- relevant_data %>% na.omit()

#cannot have more than 53 categories
relevant_data$SELECTIONNUMBER <- as.factor(relevant_data$SELECTIONNUMBER)

set.seed(123)

train_indices <- sample(1:nrow(relevant_data), 0.7 * nrow(relevant_data))
train_data <- relevant_data[train_indices, ]
test_data <- relevant_data[-train_indices, ]

train_levels <- levels(train_data$SELECTIONNUMBER)
#filters the data set so it only consist of categories in the training data
test_data <- test_data[test_data$SELECTIONNUMBER %in% train_levels, ]
test_data$SELECTIONNUMBER <- factor(test_data$SELECTIONNUMBER, levels = train_levels)

train_data$SELECTIONNUMBER <- droplevels(train_data$SELECTIONNUMBER)

rf_model <- randomForest(
  SELECTIONNUMBER ~ .,
  data = train_data,
  ntree = 1000,
  mtry = 3,
  importance = TRUE
)

predictions <- predict(rf_model, test_data)

predictions <- factor(predictions, levels = levels(test_data$SELECTIONNUMBER))

accuracy <- sum(predictions == test_data$SELECTIONNUMBER) / nrow(test_data)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

conf_matrix <- table(Predicted = predictions, Actual = test_data$SELECTIONNUMBER)
print(conf_matrix)

varImpPlot(rf_model)

