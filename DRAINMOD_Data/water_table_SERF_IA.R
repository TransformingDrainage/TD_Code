library(googlesheets)
library(xlsx)
library(lubridate)
library(stringr)
suppressPackageStartupMessages(library(dplyr))

# SERF_IA ----

setwd("C:/Users/Go/Google Drive/MODELLING/RAW_MODEL_DATA")
dir()

# read google sheets ----
# Returns a data frame of the sheets you would see in your Google Sheets home screen.  
# This should include sheets that you own and owned by other but that you are premitted to access.
# Expect a prompt to authenticate yourself in the browser
#my_sheets <- gs_ls()
# narrow the listing down by spacifying sheet name
my_sheets <- gs_ls("SERF_IA")
my_sheets

# Register a sheet
gap <- gs_title(as.character(my_sheets[matches("Table", vars = my_sheets$sheet_title), 1]))
#gap <- gs_title("SERF_IA Water Table Depth")

# open the sheet in browser 
gs_browse(gap)

# read the names of the sheets
gs_ws_ls(gap) %>% as.numeric() -> years
gs_ws_ls(gap)[years < 2016] -> sheets

# read headings
gap %>% 
  gs_read(ws = 1, range = cell_rows(1:3)) %>% 
  names() %>% 
  word() -> heading

# read data 
# PART1
for (i in 1:5) { 
  obj_name <- paste0("data", sheets[i])
  gap %>% 
    gs_read_csv(ws = sheets[i], col_names = FALSE, skip = 2, na = "NAN") %>% 
    assign(obj_name, . , envir = .GlobalEnv )
  Sys.sleep(2)
}

# read data 
# PART2
for (i in 6:9) { 
  obj_name <- paste0("data", sheets[i])
  gap %>% 
    gs_read_csv(ws = sheets[i], col_names = FALSE, skip = 2, na = "NAN") %>% 
    assign(obj_name, . , envir = .GlobalEnv )
  Sys.sleep(2)
}

gwt <- do.call(rbind, lapply(ls(pattern = "data20"), get))
names(gwt) <- heading

# get rid of redundant objects
rm(i, heading, obj_name, years, my_sheets)

# transform df ----
gwt$S1 <- as.numeric(gwt$S1)
gwt$S2 <- as.numeric(gwt$S2)
gwt$S3 <- as.numeric(gwt$S3)
gwt$S4 <- as.numeric(gwt$S4)
gwt$S5 <- as.numeric(gwt$S5)
gwt$S6 <- as.numeric(gwt$S6)
gwt$S7 <- as.numeric(gwt$S7)
gwt$S8 <- as.numeric(gwt$S8)

gwt$Date <- parse_date_time(gwt$Date, "mdY HMS")
gwt$ymd <- as.Date(gwt$Date)
gwt$doy <- yday(gwt$Date)
gwt$time <- round.POSIXt(gwt$Date, units = "mins")
gwt$time <- format(gwt$time, "%H:%M")

# calculate daily average WT
gwt %>% 
  group_by(ymd) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) -> averageWT
averageWT[2:9] <- round(averageWT[2:9], digits = 2)
names(averageWT)[1] <- "date"

# get midnight WT
gwt %>% 
  filter(time == "00:00") -> midnightWT
midnightWT[2:9] <- round(midnightWT[2:9], digits = 2)
names(midnightWT)[1] <- "date"

# number of missing values in "midnightWT" compared to "averageWT"
sum(is.na(midnightWT[2:9]) & !is.na(averageWT[-1, 2:9])) #6,813

# substitude missing midnight values with available daily average values
a <- is.na(midnightWT[2:9]) & !is.na(averageWT[-1, 2:9])

for (i in 1:8) {
  midnightWT[a[ , i], i+1] <- averageWT[c(FALSE, a[ , i]), i+1]
}


# save whole data
write.table(gwt,
            file = "C:/Users/Gio/Google Drive/MODELLING/RAW_MODEL_DATA/Water_Table_cm_hourly_SERF_IA.txt",
            sep = "\t", row.names = FALSE)

# save daily data
write.table(averageWT[1:9],
            file = "C:/Users/Gio/Google Drive/MODELLING/SERF_IA/Water_Table_cm_daily_ave_SERF_IA.prn",
            sep = "\t", row.names = FALSE, col.names = TRUE)

write.table(midnightWT[1:9],
            file = "C:/Users/Gio/Google Drive/MODELLING/SERF_IA/Water_Table_cm_midnight_SERF_IA.prn",
            sep = "\t", na =" ", row.names = FALSE, col.names = TRUE)
