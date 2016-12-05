library(lubridate)

# SERF_IA ----

setwd("C:/Users/Gio/Google Drive/MODELLING/RAW_MODEL_DATA")
dir()

# read weather data downloaded from https://sites.google.com/site/transformingdrainage/database/weather-data-download
# data spans from Jan 1, 2007 to Dec 31, 2015
# daily measurements of:
# - computed  GDD (base=50, max=86) [F]
# - computer  GDD (base=40, max=86) [F]
# - variable  High Air Temp [C]
# - variable  Low Air Temp [C]
# - variable  Precipitation (rain+melted snow) [mm]
# - varible   SR from MERRA (shortwave down at ground) [MJ m-2 dy-1]
# data is tab deliminated

# read daily weather file
weath <- read.table("SERF_IA_weather_daily.txt", header = TRUE, sep = "\t")
weath$day <- as.Date(weath$day)

# read daily weather file from Crawforsville weather station
# 2007-2013 data obtained from http://mesonet.agron.iastate.edu/agclimate/hist/dailyRequest.php
# 2013-2015 data obtained from http://mesonet.agron.iastate.edu/agclimate/hist/daily.php
weath_d1 <- read.table("SERF_IA_weather_daily_2007-2013.txt", header = TRUE, sep = "\t")
weath_d2 <- read.table("SERF_IA_weather_daily_2013-2015.txt", header = TRUE, sep = "\t")

weath_d1$valid <- as_datetime(weath_d1$valid)
weath_d2$valid <- as_datetime(weath_d2$valid)

join_date <- weath_d2$valid[1]

weath_d <- weath_d1[weath_d1$valid < join_date, c("valid", "Potential.ET")]
names(weath_d2)[matches("et", vars = names(weath_d2))] <- "Potential.ET"
weath_d <- rbind.data.frame(weath_d, weath_d2[ , c("valid", "Potential.ET")])

weath_d$year <- year(weath_d$valid)
weath_d$doy <- yday(weath_d$valid)

weath_d$Potential.ET <- weath_d$Potential.ET * 25.4


# read hourly weather file
weath_hr1 <- read.table("SERF_IA_weather_hourly_2007-2013.txt", header = TRUE, sep = "\t")
weath_hr2 <- read.table("SERF_IA_weather_hourly_2013-2015.txt", header = TRUE, sep = "\t")

weath_hr1$valid <- as_datetime(weath_hr1$valid, tz = "America/Chicago")
weath_hr2$valid <- as_datetime(weath_hr2$valid, tz = "America/Chicago")

join_date <- weath_hr2$valid[1]

weath_hr <- weath_hr1[weath_hr1$valid < join_date, 
                      c("Site.Name", 
                        "valid", 
                        "Air.Temp", 
                        "RH.", 
                        "X",
                        "Hourly.Precip",
                        "Wind.Speed",
                        "Wind.Direction",
                        "X4.inch.Soil.Temp")]
colnames(weath_hr) <- c("station", 
                        "date", 
                        "air.temp", 
                        "RH", 
                        "SR",
                        "precip",
                        "wind.speed",
                        "wind.direction",
                        "soil.temp.4in")
colnames(weath_hr2) <- c("station", 
                         "date", 
                         "air.temp", 
                         "RH", 
                         "SR",
                         "precip",
                         "wind.speed",
                         "wind.direction",
                         "soil.temp.4in")

weath_hr <- rbind.data.frame(weath_hr, weath_hr2)

# fixing time
attr(weath_hr$date, "tzone")
# create a FAKE UTC time - actually it is local IA time
weath_hr$date <- with_tz(weath_hr$date, tzone = "UTC") - hours(6)
# first iteration 
dupl <- diff(weath_hr$date)
weath_hr$date[which(dupl == 0) + 1] <- weath_hr$date[which(dupl == 0) + 1] + hours(1)
# second iteration
dupl <- diff(weath_hr$date)
dupl <- which(dupl != 1)
weath_hr[(0:1)+ rep(dupl, each=2), c(2:3, 6)]

# add missing dates ----
# calculate total number of missing hours
i <- as.numeric(sum(weath_hr$date[dupl+1] - weath_hr$date[dupl] - 1))
# creat an empty df with exact number of columns and missing rows (hours)
missing_dates <- data.frame(matrix(ncol = ncol(weath_hr), nrow = i))
names(missing_dates) <- names(weath_hr)

dummy <- numeric()
class(dummy) <- "POSIXct"
for (i in 1:3) {
  tempo <- seq(from = as.POSIXct(weath_hr$date[dupl[i]]),
               to = as.POSIXct(weath_hr$date[dupl[i]+1]),
               by = "hour")
  dummy <- c(dummy, tempo[2:(length(tempo)-1)])
}
missing_dates$date <- with_tz(dummy, tzone = "UTC")

# mannualy enter daily precipitation data (in inches) for missing dates
# precipitation data is obtained from IEM Rainfall page at isu mesonet 
# using following coordinates Lat:41.19 and Long: -91.48
# https://mesonet.agron.iastate.edu/rainfall/
missing_precip <- c(.20, 0, 0)
missing_hrs <- as.integer(weath_hr$date[dupl+1] - weath_hr$date[dupl] - 1)
dummy <- numeric()
for (i in 1:3) {
  tempo <- rep(missing_precip[i]/missing_hrs[i], times = missing_hrs[i])
  dummy <- c(dummy, tempo)
}
missing_dates$precip <- dummy

weath_hr <- rbind(weath_hr, missing_dates)
weath_hr <- weath_hr[order(weath_hr$date), ]
weath_hr$station <- weath_hr$station[1]
row.names(weath_hr) <- 1:dim(weath_hr)[1]

# add DOY ----
weath_hr$year <- year(weath_hr$date)
weath_hr$doy <- yday(weath_hr$date) + hour(weath_hr$date)/24
weath_hr$doy <- round(weath_hr$doy, 3)
weath_hr[(0:1)+ rep(dupl, each=2), c(2:3, 6, 10:11)]

# convert variable units ----
weath_hr$precip <- weath_hr$precip * 25.4
weath_hr$air.temp <- (weath_hr$air.temp - 32) * 5/9
weath_hr$soil.temp.4in <- (weath_hr$soil.temp.4in - 32) * 5/9

rm(weath_hr1, weath_hr2, join_date, dupl, dummy, i, tempo, 
   missing_dates, missing_precip, missing_hrs)


# extract temperature data
TEMP_oC_SERF_IA <- weath[ , c("day", "doy", "highc", "lowc")]
TEMP_oC_SERF_IA$day <- year(TEMP_oC_SERF_IA$day)
names(TEMP_oC_SERF_IA)[1] <- "year"

# extract hourly precipitation data
#PREC_mm_hourly_SERF_IA <- weath_hr[ , c("year", "doy", "precip", "date")]
PREC_mm_hourly_SERF_IA <- weath_hr[ , c("year", "doy", "precip")]
PREC_mm_hourly_SERF_IA <- PREC_mm_hourly_SERF_IA[PREC_mm_hourly_SERF_IA$precip > 0, ]

# extract daily precipitation data
PREC_mm_daily_SERF_IA <- weath[ , c("day", "doy", "precipmm")]
PREC_mm_daily_SERF_IA$day <- year(PREC_mm_daily_SERF_IA$day)
names(PREC_mm_daily_SERF_IA)[1] <- "year"

# extract solar radiation data
SRAD_mjm2day_daily_SERF_IA <- weath[ , c("day", "doy", "merra_srad")]
SRAD_mjm2day_daily_SERF_IA$day <- year(SRAD_mjm2day_daily_SERF_IA$day)
names(SRAD_mjm2day_daily_SERF_IA)[1] <- "year"

# extract potential ET data
ET_mm_daily_SERF_IA <- weath_d[ , c("year", "doy", "Potential.ET")]



# save as txt file
write.table(TEMP_oC_SERF_IA, 
            file = "C:/Users/Gio/Google Drive/MODELLING/SERF_IA/Temp_oC_daily_SERF_IA.prn", 
            sep = "\t", row.names = FALSE, col.names = FALSE)

write.table(SRAD_mjm2day_daily_SERF_IA,
            file = "C:/Users/Gio/Google Drive/MODELLING/SERF_IA/Solar_mjm2_daily_SERF_IA.prn",
            sep = "\t", row.names = FALSE, col.names = FALSE)

write.table(PREC_mm_daily_SERF_IA,
            file = "C:/Users/Gio/Google Drive/MODELLING/SERF_IA/Rain_mm_daily_SERF_IA.prn",
            sep = "\t", row.names = FALSE, col.names = FALSE)

write.table(PREC_mm_hourly_SERF_IA,
            file = "C:/Users/Gio/Google Drive/MODELLING/SERF_IA/Rain_mm_hourly_SERF_IA.prn",
            sep = "\t", row.names = FALSE, col.names = FALSE)

write.table(ET_mm_daily_SERF_IA,
            file = "C:/Users/Gio/Google Drive/MODELLING/SERF_IA/ET_mm_daily_SERF_IA.prn",
            sep = "\t", row.names = FALSE, col.names = FALSE)


