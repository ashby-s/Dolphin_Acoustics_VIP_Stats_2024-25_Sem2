# PART 1:
# run this code once to get all ARTwarp contours

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


## Start at 5, remove .ctr extension
# loop over whistles
n.whistles <- dim(mat$DATA)[2] # Corresponds to Steno Hawaii Data

# placeholders
names <- rep(NA, n.whistles)
category <- rep(0, n.whistles)

# loop over whistles
for (i in 1:n.whistles)  {
  
  # extract the part of the file name that can be matched to ROCCA
  temp.name <- mat$DATA[,i,]$name # access the i-th column and extract the name
  clean.name <- gsub("T", "", substr(temp.name, 4, 25))  # remove the 'T' from the timestamp
  names[i] <- paste0("CTR", substr(temp.name, 4, 25), ".ctr") # prepend "CTR", append ".ctr" and extract the substring
  
  # pull out category
  category[i] <- mat$DATA[,i,]$category # extract the category from the i-th whistle
}

# turn into tidy data
ARTwarp <- tibble('ShortSourceA' = names, 'category' = category)

