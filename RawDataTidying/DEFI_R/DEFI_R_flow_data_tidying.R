# DATA TYDING
# ---------------------------------------------------------------------------------------------------
# This code was developed for cleaning raw flow data from Transforming Drainage research site DEFI_R.


library(tidyr)
library(lubridate)


#open DEFIANCE CSV file
d <- read.csv("C:/Users/gio/Documents/TD/Data/RAW/RAW Defiance Flow Data 1999-2008.csv",
              header = TRUE)

# stores as a new object, while removing redundant columns
col <- which(colnames(d) %in% c("Unique_record", 
                                "Site", 
                                "Sample_method",
                                "Sampler_date_time",
                                "Lab_id",
                                "Pick_up_date",
                                "Pick_up_time",
                                "Bottle",
                                "Bottle_level",
                                "A.D.2",
                                "A_ft.2",
                                "R.D",
                                "R_ft",
                                "Q_ft.3.s",
                                "Weephole_height_in",
                                "Downstream_stage_m",
                                "Head_on_bottom_of_orifice_ft",
                                "Head_on_top_of_orifice_ft",
                                "d.D",
                                "Cw",
                                "Weephole_Q_ft.3.s",
                                "Orifice_Q_ft.3.s",
                                "Weir_Q_ft.3.s"))
def <- as.data.frame(d[ , -col])

# extract sampler date and time, and combine 
def$time <- do.call(rbind,(strsplit(as.character.Date(def$Sampler_time), " ")))[,2]
def$date <- do.call(rbind,(strsplit(as.character.Date(def$Sampler_date), " ")))[,1]
def$timestamp <- parse_date_time(paste(def$date, def$time), "mdY HMS") + hours(5)
# raws with data and no time data fails to be parsed
# adds timestamp for failed raws using date data
def[which(is.na(def$timestamp)), "timestamp"] <- 
  ymd(strptime(def[which(is.na(def$timestamp)), "Sampler_date"], format = "%m/%d/%Y %H:%M:%S"))
def$year <- year(def$timestamp)

# substitudes 255255 and 253253 with NA for "Velocity"
# adds "V_ft.s" for K location to "Velocity" after convertion from ft/s to m/s
# removes "V_ft.s" from df
def$Velocity[def$Velocity > 100000] <- NA
def$Velocity[def$Location == "K"] <- def$V_ft.s[def$Location == "K"] * 0.3048
def$V_ft.s <- NULL

# substitudes "Head_on_weir" for O-UP location with "Head_on_weir_ft" after convertion to m
# removes "Head_on_weir_ft" from df
def$Head_on_weir[def$Location == "O-UP"] <- def$Head_on_weir_ft[def$Location == "O-UP"] * 0.3048
def$Head_on_weir_ft <- NULL

# convert comments from factors to characters 
def$Flow_data_comments <- as.character(def$Flow_data_comments)

# fixes erroneous entry of 0 with NA
def[which(def$Location == "D" & def$Adjusted_stage == 0), "Adjusted_stage"] <- NA
def[which(def$Location == "D" & def$Calculated_flow_rate == 0), "Calculated_flow_rate"] <- NA
def[which(def$Location == "K" & def$Head_on_weir == 0), "Head_on_weir"] <- NA

# substitudes 255255 and 253253 with NA for "Stage" and "Adjusted_stage"
def[which(def$Stage > 100000), "Stage"] <- NA
def[which(def$Adjusted_stage > 100000), "Adjusted_stage"] <- NA

# moves 2003 "Calculated_flow_rate" to "Flow_link_flow" for location C
# removes copied data from orginal location
def[!is.na(def$Calculated_flow_rate) & def$Location == "C", "Flow_link_flow_rate"] <- 
  def[!is.na(def$Calculated_flow_rate) & def$Location == "C", "Calculated_flow_rate"]
def[def$Location == "C", "Calculated_flow_rate"] <- NA

# substitudes wetland locations codes with a new one "O"
levels(def$Location)[levels(def$Location) %in% c("K", "O-UP")] <- "O"

# remove one erroneous entry of "Flow_link_flow_rate" at location "I" in 2006
# moves all flow data to "flow" coulmn
# adds column to destinguish calculated from flowlink data
# removes old vectors of "Calculated_flow_rate" and "Flow_link_flow_rate"
def[which(def$Location == "I" & def$year == 2006 & !is.na(def$Flow_link_flow_rate)), "Flow_link_flow_rate"] <-NA
def$flow_measurement <- NA_character_
def$flow_measurement[!is.na(def$Calculated_flow_rate)] <- "calculated"
def$flow_measurement[!is.na(def$Flow_link_flow_rate)] <- "flowlink"
def$flow_measurement <- as.factor(def$flow_measurement)
def$flow <- NA_real_
def$flow[!is.na(def$Calculated_flow_rate)] <- def$Calculated_flow_rate[!is.na(def$Calculated_flow_rate)] 
def$flow[!is.na(def$Flow_link_flow_rate)] <- def$Flow_link_flow_rate[!is.na(def$Flow_link_flow_rate)]
def$Calculated_flow_rate <- NULL
def$Flow_link_flow_rate <- NULL
def$Velocity <- NULL

# removes data with no time info 
# in Stage (3 cases of 0.01), Adjusted_stage (692 cases of 0 at location "O" in 1999-2002), 
# and flow (5 cases of 0)
def[which(def$Sampler_time == ""), c("Adjusted_stage", "Stage", "flow", "flow_measurement")] <- NA

# gets rid of extra columns for dates
# delete other vectors (columns) that has no use
col <- which(colnames(def) %in% c("Sampler_time", "Sampler_date", "date", "time"))
def <- def[ , -col]
def$Head_on_weir <- NULL
def$Top_Board <- NULL
def$Stage_ft <- NULL
rm(col)

# add site ID
def$site_ID <- rep("DEFI_R", dim(def)[1])


# reshuffle the columns and rename 
DEFI_R <- def[ , c("site_ID", 
                   "year",
                   "Location",
                   "timestamp",
                   "flow_measurement",
                   "flow",
                   "Stage",
                   "Adjusted_stage",
                   "Flow_data_comments")]
names(DEFI_R) <- c("site_ID", "year", "location", "timestamp", 
                   "flow_meas_type", "flow", "stage", "adj_stage", "comments")


# save data in Google Drive
setwd("C:/Users/gio/Google Drive/TIDY")
write.csv(DEFI_R, file = "TIDY_DEFI_R_flow_data_1999-2008.csv", row.names = FALSE)
save(DEFI_R, file = "TIDY_DEFI_R_flow_data_1999-2008.Rda")

# save data in local drive
setwd("C:/Users/gio/Documents/TD/Data/TIDY")
write.csv(DEFI_R, file = "TIDY_DEFI_R_flow_data_1999-2008.csv", row.names = FALSE)
save(DEFI_R, file = "TIDY_DEFI_R_flow_data_1999-2008.Rda")

