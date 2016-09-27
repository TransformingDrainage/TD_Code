# DATA TYDING
# ---------------------------------------------------------------------------------------------------
# This code was developed for cleaning raw flow data from Transforming Drainage research site VANWERT



library(tidyr)
library(lubridate)


#open VANWERT CSV file
v <- read.csv("C:/Users/gio/Documents/TD/Data/RAW/RAW Fulton Flow Data Table 2000-2011.csv",
              header = TRUE, stringsAsFactors = FALSE)

# stores as a new object, while removing redundant columns