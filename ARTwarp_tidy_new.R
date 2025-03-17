# New ARTwarp tidy data

library(dplyr)

# package to work with matlab files
library(R.matlab)

# read in the .mat output file from ARTwarp
mat <- readMat('ARTwarp96FINAL_steno_bighawaii_25022025.mat')

# find out how many whistles are in that ARTwarp run
n.whistles <- dim(mat$DATA)[2] # Corresponds to Steno Hawaii Data

# information to pull out: filename and the ARTwarp category
# placeholders
names <- rep(NA, n.whistles)
category <- rep(0, n.whistles)

# loop over whistles
for (i in 1:dim(mat$DATA)[2])  {
  
  # Extract encounter ID from ARTwarp name
  temp.name <- mat$DATA[,i,]$name 
  encounter_parts <- unlist(strsplit(temp.name, "-"))  # Split by "-"
  encounter_id <- tail(encounter_parts, 1)  # Get last part (e.g., "s125.ctr")
  
  # Remove the ".ctr" suffix if present
  encounter_id <- sub("\\.ctr$", "", encounter_id)
  
  names[i] <- encounter_id  # Store encounter ID
  
  # likely removes unwanted prefixes from the name
  
  # pull out category
  category[i] <- mat$DATA[,i,]$category # extract the category from the i-th whistle
}

# turn into tidy data
ARTwarp <- tibble('EncounterID' = names, 'category' = category)


