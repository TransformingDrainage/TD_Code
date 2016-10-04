# DATA TYDING
# VAN WERT -------------------------------------------------------------------------------------------
# This code was developed for cleaning raw flow data from Transforming Drainage research site VANWERT.
# Original flow data was stored in two separate tables, first one storing data from 2001-2008, and second
# from 2009. Structure of those files are different, with unlike naming for columns. Both data is read
# and combined into one table, and stored as a CSV file.


library(tidyr)
library(lubridate)


# open VANWERT CSV files
v1 <- read.csv("C:/Users/gio/Documents/TD/Data/RAW/RAW Van Wert Flow Data Table 2001-2008.csv",
              header = TRUE, stringsAsFactors = FALSE)
v2 <- read.csv("C:/Users/gio/Documents/TD/Data/RAW/RAW Van Wert Wetland Nutrient Removal Test 2009.csv",
               header = TRUE, stringsAsFactors = FALSE)

# extract information about board height from v1 and v2
setwd("C:/Users/gio/Documents/GitHub/TransformingDrainage/TD_Code/RawDataTidying/VANWERT")
source("VANWERT_outlet_height_mngt_data_extraction.R")


# V1 -------------------------------------------------------------------------------------------------
# stores "v1" as a new object 
van <- v1

# removs redundant columns
col <- which(colnames(van) %in% c("Unique_record", 
                                "Site", 
                                "Sample_method",
                                "Lab_id",
                                "Pick_up_date",
                                "Pick_up_time",
                                "Bottle",
                                "Bottle_level",
                                "Top_Board",
                                "Head_on_weir",
                                "Board_height"))
van <- as.data.frame(van[ , -col])

# convert location from character to factor 
van$Location <- as.factor(van$Location)

#combine date and time
van$Sampler_date <- as.POSIXct(van$Sampler_date)
van$Sampler_time <- as.POSIXct(van$Sampler_time)
van$timestamp <- parse_date_time(paste(van$Sampler_date, format(van$Sampler_time, "%H:%M")), 
                                 "Ymd_HMS", truncated = 1)
van$timestamp[is.na(van$timestamp)] <- parse_date_time(paste(van$Sampler_date[is.na(van$timestamp)], "00:00:00"), 
                                                       "Ymd_HMS", truncated = 1)
# correct UTC time for Ohio
van$timestamp <- van$timestamp + hours(5)

# add year
van$year <- year(van$timestamp)

# get rid of the old date and time vectors
van$Sampler_date <- NULL
van$Sampler_time <- NULL


# replace with NA where stage has error reading (255255)
van$Stage[van$Stage > 222222] <- NA

# remove flow data when velocity has error reading (255255 and 253253) or negative value, 
# because those errors are propagated in calculated flow rate
van$Flow_link_flow_rate[van$Velocity > 222222] <- NA
van$Flow_link_flow_rate[van$Velocity < 0] <- NA
van$Velocity <- NULL


# combine two flow measurements
van$flow_measurement <- NA_character_
van$flow_measurement[!is.na(van$Calculated_flow_rate)] <- "calculated"
van$flow_measurement[!is.na(van$Flow_link_flow_rate)] <- "flowlink"
van$flow_measurement <- as.factor(van$flow_measurement)
van$flow <- NA_real_
van$flow[!is.na(van$Calculated_flow_rate)] <- van$Calculated_flow_rate[!is.na(van$Calculated_flow_rate)]
van$flow[!is.na(van$Flow_link_flow_rate)] <- van$Flow_link_flow_rate[!is.na(van$Flow_link_flow_rate)]
van$flow[van$flow > 222222] <- NA
van$Calculated_flow_rate <- NULL
van$Flow_link_flow_rate <- NULL


# add site ID
van$site_ID <- rep("VANWERT", dim(van)[1])

# reshuffle the columns and rename 
VANWERT <- van[ , c("site_ID", 
                    "year",
                    "Location",
                    "timestamp",
                    "flow_measurement",
                    "flow",
                    "Stage",
                    "Adjusted_stage",
                    "Flow_data_comments")]
names(VANWERT) <- c("site_ID", "year", "location", "timestamp", 
                    "flow_meas_type", "flow", "stage", "adj_stage", "comments")



# V2 -------------------------------------------------------------------------------------------------
# stores "v2" as a new object 
van <- v2

# removs redundant columns
col <- which(colnames(van) %in% c("ID", 
                                  "Site", 
                                  "Sample_method",
                                  "Date.and.Time",
                                  "Lab_id",
                                  "Pick_up_date",
                                  "Pick_up_time",
                                  "Bottle",
                                  "Bottle_level",
                                  "Top_Board",
                                  "Weephole.Bd_ht",
                                  "Head_on_weir",
                                  "Head.on.top.of.orifice..m.",
                                  "Head.on.bottom.of.orifice..ft.",
                                  "d.D",
                                  "A.D.2",
                                  "A_ft.2",
                                  "R.D",
                                  "R_ft",
                                  "Weephole_Q_ft.3.s",
                                  "Volume..L.",
                                  "Sum.of.volume..L..between.bottles"))
van <- as.data.frame(van[ , -col])

# convert location from character to factor 
van$Location <- as.factor(van$Location)

#combine date and time
van$Sampler_date <- as.POSIXct(van$Sampler_date)
van$Sampler_time <- as.POSIXct(van$Sampler_time)
van$timestamp <- parse_date_time(paste(van$Sampler_date, format(van$Sampler_time, "%H:%M")), 
                                 "Ymd_HMS", truncated = 1)
# correct UTC time for Ohio
van$timestamp <- van$timestamp + hours(5)

# add year
van$year <- year(van$timestamp)

# get rid of the old date and time vectors
van$Sampler_date <- NULL
van$Sampler_time <- NULL

# add new columns for flow measurement
van$flow_measurement <- "calculated"
van$flow_measurement <- as.factor(van$flow_measurement)
van$flow <- van$Calculated_flow_rate..m3.hr.
van$Calculated_flow_rate..m3.hr. <- NULL


# add site ID
van$site_ID <- rep("VANWERT", dim(van)[1])

# reshuffle the columns and rename 
van <- van[ , c("site_ID", 
                "year",
                "Location",
                "timestamp",
                "flow_measurement",
                "flow",
                "Stage",
                "Adjusted_stage",
                "Flow_data_comments")]
names(van) <- c("site_ID", "year", "location", "timestamp", 
                "flow_meas_type", "flow", "stage", "adj_stage", "comments")


# SAVING -----------------------------------------------------------------------------------------------
# add 2009 data from van to 2001-2008 data from VANWERT
VANWERT <- rbind(VANWERT, van)

# make flow_meas_type NA when there is no flow data
VANWERT$flow_meas_type[is.na(VANWERT$flow)] <- NA

# make Site ID a factor
VANWERT$site_ID <- as.factor(VANWERT$site_ID)

# save data in local drive
setwd("C:/Users/gio/Documents/TD/Data/TIDY")
write.csv(VANWERT, file = "TIDY_VANWERT_flow_data_2001-2009.csv", row.names = FALSE)
save(VANWERT, file = "TIDY_VANWERT_flow_data_2001-2009.Rda")

# get rid of unwanted objects
rm(van, col, v1, v2)
