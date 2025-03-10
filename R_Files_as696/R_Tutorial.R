exampleFile <- readRDS("test_data")
str(exampleFile)
class(exampleFile)
num_data <- exampleFile[sapply(exampleFile, is.numeric)] #Filter In Numeric Values
dim(num_data)  # Check rows and columns

str(num_data)  # Check structure
names(num_data)  # List column names
head(num_data)  # View first rows


class(num_data)
names(num_data)

class(num_data$category)

#Find any columns with zero variance, remove them from the dataset:
zero_var_cols <- sapply(num_data, function(x) sd(x) == 0)
names(num_data)[zero_var_cols]
num_data <- num_data[, !zero_var_cols]


library(car)

# Identify highly correlated variables (|r| > 0.9) and remove one of each pair
alias(lm(category ~ ., data = num_data))
cor_matrix <- cor(num_data, use = "pairwise.complete.obs")
high_corr <- which(abs(cor_matrix) > 0.9 & lower.tri(cor_matrix), arr.ind = TRUE)
print(high_corr)

# Identify the highly correlated columns to remove
high_corr_vars <- c("FREQMIN", "FREQBEG", "FREQEND", "FREQMEAN", "FREQMEDIAN", 
                    "FREQCENTER", "FREQQUARTER1", "FREQQUARTER2", "FREQQUARTER3",
                    "FREQSTDDEV", "FREQRELBW", "FREQMAXMINRATIO", "DCQUARTER2MEAN",
                    "FREQSPREAD", "FREQSLOPERATIO", "NUMSWEEPSDWNUP", "NUMINFLECTIONS",
                    "NUMSWEEPSFLATUP", "NUMSWEEPSFLATDWN", "INFLMEANDELTA", "INFLSTDDEVDELTA", 
                    "INFLMEDIANDELTA")

# Remove these columns from the dataset
num_data_clean <- num_data[, !colnames(num_data) %in% high_corr_vars]

# Check if the dataset is now free of high correlations
cor_matrix_clean <- cor(num_data_clean, use = "pairwise.complete.obs")

                  

vif_values <- vif(lm(category ~ ., data = num_data_clean))
print(vif_values[vif_values > 5])  # Show variables with high collinearity

num_data_clean <- num_data_clean[, !colnames(num_data_clean) %in% c("FREQNUMSTEPS", "Species8", "FREQBEGDWN", "FREQENDDWN")]
num_data_clean <- num_data_clean[, !colnames(num_data_clean) %in% c("FREQSWEEPUPPERCENT", "FREQSWEEPDWNPERCENT")]

# Identify highly correlated variables (|r| > 0.9) and remove one of each pair
alias(lm(category ~ ., data = num_data_clean))
num_data_clean <- num_data_clean[, !colnames(num_data_clean) %in% c("FREQBEGENDRATIO")]
vif_values <- vif(lm(category ~ ., data = num_data_clean))
print(vif_values[vif_values > 5])  # Show variables with high collinearity

colnames(num_data_clean)

CORRECT_NON_COLLINEAR_DATA <- num_data_clean
colnames(CORRECT_NON_COLLINEAR_DATA)

plot(num_data$duration, num_data$FREQENDUP)
