# DATA TYDING
# ---------------------------------------------------------------------------------------------------
# This code was developed for cleaning raw flow data from Transforming Drainage research site FULTON.


library(tidyr)
library(lubridate)


#open FULTON CSV file
f <- read.csv("C:/Users/gio/Documents/TD/Data/RAW/RAW Fulton Flow Data Table 2000-2011.csv",
              header = TRUE, stringsAsFactors = FALSE)

# stores as a new object, while removing redundant columns
col <- which(colnames(f) %in% c("Unique_record", 
                                "Site", 
                                "Sample_method",
                                "Lab_id",
                                "Pick_up_date",
                                "Pick_up_time",
                                "Bottle",
                                "Bottle_level..m.",
                                "Wetland.ecotone.level..cm.",
                                "Dam.ecotone.level..cm.",
                                "Board.height..m.",
                                "Head_on_weir..m."))
ful <- as.data.frame(f[ , -col])

# convert location from character to factor 
ful$Location <- as.factor(ful$Location)

# combine date and time
ful$timestamp <- parse_date_time(paste(ful$Sampler_date, format(as.POSIXct(ful$Sampler_time), "%H:%M")), 
                                    "Ymd_HMS", truncated = 1)
ful$timestamp[is.na(ful$timestamp)] <- parse_date_time(paste(ful$Sampler_date[is.na(ful$timestamp)], "00:00:00"), 
                                                             "Ymd_HMS", truncated = 1)
#correct UTC time for Ohio
ful$timestamp <- ful$timestamp + hours(5) 

# add year
ful$year <- year(ful$Sampler_date)

# get rid of the old date and time vectors
ful$Sampler_date <- NULL
ful$Sampler_time <- NULL


# combine two flow measurements
ful$flow_measurement <- NA_character_
ful$flow_measurement[!is.na(ful$Calculated_flow_rate..m3.h.)] <- "calculated"
ful$flow_measurement[!is.na(ful$Flow_link_flow_rate..m3.h.)] <- "flowlink"
ful$flow_measurement <- as.factor(ful$flow_measurement)
ful$flow <- NA_real_
ful$flow[!is.na(ful$Calculated_flow_rate..m3.h.)] <- ful$Calculated_flow_rate..m3.h.[!is.na(ful$Calculated_flow_rate..m3.h.)]
ful$flow[!is.na(ful$Flow_link_flow_rate..m3.h.)] <- ful$Flow_link_flow_rate..m3.h.[!is.na(ful$Flow_link_flow_rate..m3.h.)]
ful$Calculated_flow_rate..m3.h. <- NULL
ful$Flow_link_flow_rate..m3.h. <- NULL

# ----------------------------------------------------------------------------------------------------
# EXTRACTING DATA to FIX FLOW OFFSET @ FULTON LATER
# allign "Velocity" values to corresponding "Flow link flow rate" values based on 253253 readings
# extract the portion of data that needs this correction to be done 
tempf <- ful[ful$Location != "C" , c(1,2,4,6:9)] #selects all columns except "Adjusted_Steage" and "Flow_data_comments"

# Flow rates at 253253 are usually below 0.067, but there are few cases when such a low
# flow rates were actually recorded/measured due to very low velocity and stage values.
# This allows to find values that does not correspond to the abovementioned pattern 
r1 <- which(tempf$Velocity < 1000 & tempf$Velocity > 0 & tempf$flow < 0.07 & tempf$Stage..m. != 0)[1]
r2 <- which(is.na(tempf$Velocity))[which(is.na(tempf$Velocity)) > r1][1] - 1
tempf$flow[r1:(r2-1)] <- tempf$flow[(r1+1):r2]
r1 <- which(tempf$Velocity > 1000 & tempf$flow > 0.07 & tempf$Stage..m. != 0)[1]
r2 <- which(is.na(tempf$Velocity))[which(is.na(tempf$Velocity)) > r1][1] - 1
tempf$flow[r1:r2] <- tempf$flow[(r1:r2)-3]
rm(r1,r2)

# return back that section to main df (fulton)
ful[ful$Location != "C" , "flow"] <- tempf$flow
rm(tempf)
# ----------------------------------------------------------------------------------------------------


# substitudes 255255 and 253253 with NA for "Stage..m." 
ful$Stage..m.[ful$Stage..m.>1000] <- NA

# add site ID
ful$site_ID <- rep("FULTON", dim(ful)[1])


# reshuffle the columns and rename 
FULTON <- ful[ , c("site_ID", 
                   "year",
                   "Location",
                   "timestamp",
                   "flow_measurement",
                   "flow",
                   "Stage..m.",
                   "Adjusted_Stage..m.",
                   "Flow_data_comments")]
names(FULTON) <- c("site_ID", "year", "location", "timestamp", 
                   "flow_meas_type", "flow", "stage", "adj_stage", "comments")


# save data in Google Drive
setwd("C:/Users/gio/Google Drive/TIDY")
write.csv(FULTON, file = "TIDY_FULTON_flow_data_2000-2011.csv", row.names = FALSE)
save(FULTON, file = "TIDY_FULTON_flow_data_2000-2011.Rda")

# save data in local drive
setwd("C:/Users/gio/Documents/TD/Data/TIDY")
write.csv(FULTON, file = "TIDY_FULTON_flow_data_2000-2011.csv", row.names = FALSE)
save(FULTON, file = "TIDY_FULTON_flow_data_2000-2011.Rda")
