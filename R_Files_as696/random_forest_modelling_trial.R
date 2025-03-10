#READ AND SELECT ALL THE REQUIRED DATA FOR THE MODEL
contourDataFile <- read.csv("2017_data/ContourStats-AD76_S30_Sb.csv")
selected_data <- contourDataFile[, c("SELECTIONNUMBER", "FREQMAX", "FREQRANGE","DCSTDDEV","DCQUARTER1MEAN","DCQUARTER3MEAN","DCQUARTER4MEAN","FREQSTEPUP","FREQSTEPDOWN","FREQSLOPEMEAN","FREQNEGSLOPEMEAN","FREQBEGSWEEP","FREQENDSWEEP","NUMSWEEPSUPDWN","NUMSWEEPSUPFLAT","FREQSWEEPFLATPERCENT","INFLMINDELTA","INFLMAXMINDELTA")]

#Load required Libraries
install.packages("randomForest")
library(randomForest)

#Check and prepare Data
str(selected_data) #Checking data structures
selected_data <- na.omit(selected_data)  # Remove missing values

#Split Data into Training and Testing sets
#Divide data into training (80%) and testing (20%)
set.seed(123)  # For reproducibility
splitIndex <- sample(1:nrow(selected_data), 0.8 * nrow(selected_data))

train_data <- selected_data[splitIndex, ]
test_data <- selected_data[-splitIndex, ]

table(train_data$SELECTIONNUMBER)
table(test_data$SELECTIONNUMBER)

#Train a Random Forest Model (Assuming SELECTIONNUMBER is a factor)
train_data$SELECTIONNUMBER <- as.factor(train_data$SELECTIONNUMBER)
rf_model <- randomForest(SELECTIONNUMBER ~ ., data = train_data, ntree = 500, mtry = 3, importance = TRUE)

#Evaluate Model Performance
print(rf_model)
importance(rf_model) # Identify which features are most important
varImpPlot(rf_model)

predictions <- predict(rf_model, test_data)

#Determine Model Accuracy
confusionMatrix <- table(test_data$SELECTIONNUMBER, predictions)
print(confusionMatrix)
accuracy <- sum(diag(confusionMatrix)) / sum(confusionMatrix)
print(paste("Accuracy:", round(accuracy * 100, 2), "%"))

