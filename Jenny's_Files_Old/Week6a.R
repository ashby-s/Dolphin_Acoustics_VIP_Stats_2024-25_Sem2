# Fitting GLM

# Load necessary library
library(dplyr)

# Load dataset
data <- read.csv("ARTwarp_ROCCA.csv")

# Select only numeric columns
numeric_data <- data %>% select(where(is.numeric))

# Remove 'category' and 'SamplingRate'
numeric_data <- numeric_data %>% select(-category, -SamplingRate)

# Compute correlation matrix
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

# Display highly correlated pairs (absolute correlation > 0.9)
high_cor <- which(abs(cor_matrix) > 0.9 & lower.tri(cor_matrix), arr.ind = TRUE)

# Print highly correlated variables
print("Highly correlated variable pairs (correlation > 0.9):")
print(high_cor)

# Remove highly correlated variables
ARTwarp_ROCCA <- ARTwarp_ROCCA %>%
  select(-FREQQUARTER3, -FREQSTDDEV, -FREQRELBW, -FREQMAXMINRATIO, 
         -DCQUARTER3MEAN, -FREQMEDIAN, -FREQQUARTER1, -FREQQUARTER2, 
         -FREQSLOPERATIO, -NUMSWEEPSFLATUP, -INFLMEANDELTA)

# Check updated correlation matrix
cor_matrix <- cor(ARTwarp_ROCCA %>% select(where(is.numeric)), use = "pairwise.complete.obs")

# Understand varaible output 
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

high_cor <- which(abs(cor_matrix) > 0.9 & lower.tri(cor_matrix), arr.ind = TRUE)
print(high_cor)



