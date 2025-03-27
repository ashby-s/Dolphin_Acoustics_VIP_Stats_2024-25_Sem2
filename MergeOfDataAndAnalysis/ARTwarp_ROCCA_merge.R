# PART 1: ARTwarp
# run this code once to get all ARTwarp contours

library(dplyr)
library(readxl)

# Read in ARTwarp data
mat <- read_excel("ARTwarp96FINAL_steno_big_hawaii_results_25022025_cat2_only.xlsx")

# find out how many whistles are in that ARTwarp run
n.whistles <- dim(mat$DATA)[2] # Corresponds to Steno Hawaii Data

# turn into tidy data
ARTwarp <- tibble('ShortSourceA' = mat$name, 'category' = mat$refcontourcat)

# PART 2: ROCCA

# read in ROCCA encounter files (these need to be checked individually to get inputs for function below)
ROCCA1 <- read.csv('ContourStats-AD95_S44_Sb.csv')
ROCCA2 <- read.csv('ContourStats-AD189_S71_Sb.csv')
ROCCA3 <- read.csv('ContourStats-s125.csv')
ROCCA4 <- read.csv('ContourStats-s167.csv')
ROCCA5 <- read.csv('ContourStats-s194.csv')
ROCCA6 <- read.csv('ContourStats-s234.csv')
ROCCA7 <- read.csv('ContourStats-s245.csv')
ROCCA8 <- read.csv('ContourStats-AD59_S25_Sb.csv')
ROCCA9 <- read.csv('ContourStats-AD76_S30_Sb.csv')
ROCCA10 <- read.csv('ContourStats-AD76_S30_Sb.csv')
ROCCA11 <- read.csv('ContourStats-AD250_S111_Sb.csv')
ROCCA12 <- read.csv('ContourStats-AD259_S123_Sb.csv')

# function to strip source names down to what we need to match with ARTwarp
sourcename <- function(ROCCA) {
  
  n.ROCCA <- dim(ROCCA)[1]  # Get the number of rows
  
  ShortSource <- rep(NA, n.ROCCA)  # Initialize storage vector
  
  for (i in 1:n.ROCCA)  {
    
    # Extract selection number
    selection_number <- as.character(ROCCA$SELECTIONNUMBER[i])  
    
    # Extract recording and remove dashes, colons, and spaces
    recording_temp <- gsub("[-: ]", "", as.character(ROCCA$Recording[i])) 
    recording <- paste0(substr(recording_temp, 1, 8), "T", substr(recording_temp, 9, 14))
    
    # Extract the Encounter name
    encounter <- as.character(ROCCA$Encounter[i])
    
    # Construct the formatted name
    temp.string <- paste0("CTR-", selection_number, "-", recording, "-", encounter, ".ctr'")
    
    ShortSource[i] <- temp.string  # Store result
  }
  
  ROCCA$ShortSourceR <- ShortSource  # Add new column
  
  return(tibble::tibble(ROCCA))  # Return updated tibble
}

library(dplyr)

# Apply sourcename function to each ROCCA dataset
runs <- list(ROCCA1, ROCCA2, ROCCA3, ROCCA4, ROCCA5, ROCCA6, 
             ROCCA7, ROCCA8, ROCCA9, ROCCA10, ROCCA11, ROCCA12) 

# Process each dataset with sourcename function
processed_runs <- lapply(runs, sourcename)

# Combine all processed ROCCA datasets into one
ROCCA <- bind_rows(processed_runs)

# PART 3: Merge with the ARTwarp info
ARTwarp_ROCCA <- left_join(ARTwarp, ROCCA, by = c('ShortSourceA' = 'ShortSourceR')) 
# Omit NA values
ARTwarp_ROCCA <- na.omit(ARTwarp_ROCCA)

