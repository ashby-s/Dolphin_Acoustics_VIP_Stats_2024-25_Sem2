# PART 2
# run this code to get the ROCCA matches

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
    recording <- gsub("[-: ]", "", as.character(ROCCA$Recording[i])) 
    
    # Extract the Encounter name
    encounter_raw <- as.character(ROCCA$Encounter[i])
    
    if (grepl("_", encounter_raw)) {  
      # If Encounter contains underscores, split and extract the second part
      encounter_parts <- unlist(strsplit(encounter_raw, "_"))
      encounter <- tolower(encounter_parts[2])  # Convert "S123" -> "s123"
    } else {
      # If already in "s234" format, use as is
      encounter <- tolower(encounter_raw)
    }
    
    # Construct the formatted name
    temp.string <- paste0("CTR-", selection_number, "-", recording, "-", encounter, ".ctr")
    
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

# merge with the ARTwarp info
ARTwarp_ROCCA <- left_join(ARTwarp, ROCCA, by = c('ShortSourceA' = 'ShortSourceR')) 

# Check values 
# Check first few values from both columns
head(ARTwarp$ShortSourceA)
head(ROCCA$ShortSourceR)

# Find unmatched values
setdiff(ARTwarp$ShortSourceA, ROCCA$ShortSourceR) # Values in ARTwarp but not in

# Identify unmatched values
unmatched_in_ARTwarp <- setdiff(ARTwarp$ShortSourceA, ROCCA$ShortSourceR)
unmatched_in_ROCCA <- setdiff(ROCCA$ShortSourceR, ARTwarp$ShortSourceA)

# Print results
length(unmatched_in_ARTwarp) # Number of values in ARTwarp that don't match ROCCA
length(unmatched_in_ROCCA)   # Number of values in ROCCA that don't match ARTwarp

# Look at some examples
head(unmatched_in_ARTwarp)
head(unmatched_in_ROCCA)

sum(ARTwarp$ShortSourceA %in% ROCCA$ShortSourceR)  # Count matching keys
sum(ROCCA$ShortSourceR %in% ARTwarp$ShortSourceA)  # Count matching keys

# Compare the first few entries in each dataframe
sample_comparison <- data.frame(
  ARTwarp = ARTwarp$ShortSourceA[134],
  ROCCA = ROCCA$ShortSourceR[134]
)
print(sample_comparison)


