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
sourcename <- function(ROCCA) {  # no need for first.char parameter
  
  n.ROCCA <- dim(ROCCA)[1]  # Get number of rows
  
  EncounterID <- rep(NA, n.ROCCA)  # Initialize storage vector
  
  for (i in 1:n.ROCCA)  {
    
    temp.name <- as.character(ROCCA$Encounter[i])  # Use Encounter instead of Source
    
    # Extract encounter ID (assuming format "AD95_S44_Sb", taking last part after "_")
    encounter_parts <- unlist(strsplit(temp.name, "_"))
    encounter_id <- tail(encounter_parts, 1)  # Last part (e.g., "Sb")
    
    EncounterID[i] <- encounter_id  # Store encounter ID
  }
  
  ROCCA$EncounterID <- EncounterID  # Add new column
  
  return(tibble(ROCCA))  # Return updated tibble
}


# Standardize the sourcenames to match with ARTwarp
run1 <- sourcename(ROCCA1) # shifts character index calculations 28 characters back from the end of each source string
run2 <- sourcename(ROCCA2)
run3 <- sourcename(ROCCA3)
run4 <- sourcename(ROCCA4)
run5 <- sourcename(ROCCA5)
run6 <- sourcename(ROCCA6)
run7 <- sourcename(ROCCA7)
run8 <- sourcename(ROCCA8)
run9 <- sourcename(ROCCA9)
run10 <- sourcename(ROCCA10)
run11 <- sourcename(ROCCA11)  # need to check what is happening with row 10 here
run12 <- sourcename(ROCCA12)  # need to check here what is happening, end selXXX part isn't correct

# merge the ROCCA files
ROCCA <- bind_rows(run1, run2, run3, run4, run5, run6, run7, run8, run9, run10, run11, run12) # check what the columns on the end are

# I would just strip out the wanted columns from the ROCCA at this point

# merge with the ARTwarp info
ARTwarp_ROCCA <- inner_join(ARTwarp, ROCCA, by = "EncounterID", relationship = "many-to-many")

# Make this a csv for analysis 
write.csv(ARTwarp_ROCCA, "ARTwarp_ROCCA.csv", row.names = FALSE)

