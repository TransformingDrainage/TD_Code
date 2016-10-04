# READ v1 -------------------------------------------------------------------------------------
# extract columns from v1
van <- v1[ , c("Sampler_date", 
               "Sampler_time",
               "Location",
               "Top_Board",
               "Board_height")]

# convert board hieght from inches to meters
van$Board_height <- van$Board_height*0.0254

# convert date and time to POSIXct
van$Sampler_date <- as.POSIXct(van$Sampler_date)
van$Sampler_time <- as.POSIXct(van$Sampler_time)

# add missing potion of board height data (in 2005) from "Board_height" to "Top_Board" 
van$Top_Board[year(van$Sampler_date)==2005] <- van$Board_height[year(van$Sampler_date)==2005]
# remove all data from "Board_height" 
van$Board_height <- NA

# # plot contorl structure outlet height
# qplot(data=van, x=Sampler_date, y=Top_Board, na.rm=T, color=Location)



# READ v2 -------------------------------------------------------------------------------------
# extract columns from v1
van2 <- v2[ , c("Sampler_date", 
                "Sampler_time",
                "Location",
                "Top_Board",
                "Weephole.Bd_ht")]

# convert date and time to POSIXct
van2$Sampler_date <- as.POSIXct(van2$Sampler_date)
van2$Sampler_time <- as.POSIXct(van2$Sampler_time)

# # plot contorl structure outlet height
# qplot(data=van2, x=Sampler_date, y=Top_Board, na.rm=T, color=Location)



# COMBINE van & van2 --------------------------------------------------------------------------

# renames columns of both objects to support their merging 
names(van) <- c("Date", "Time", "Location", "Board Height (m)", "Weephole Board Height (m)")
names(van2) <- c("Date", "Time", "Location", "Board Height (m)", "Weephole Board Height (m)")

# combines two data frames by rows
van <- rbind(van, van2)

# arrange dataframe in acsending order
van <- van[with(van, order(Location, Date, Time)), ]



# EXTRACT BOARD MNGT DATES --------------------------------------------------------------------

# finds date when outlet hight was changed
van$`Board Height (m)`[is.na(van$`Board Height (m)`)] <- 2 
a <- which(van$`Board Height (m)`[-length(van$`Board Height (m)`)] != van$`Board Height (m)`[-1]) + 1

# get board height management data and store as a separate object
board_mngt <- van[1, ]
board_mngt <- rbind(board_mngt, van[a, ])
board_mngt <- rbind(board_mngt, van[a-1, ])
board_mngt <- rbind(board_mngt, van[dim(van)[1], ])
board_mngt <- board_mngt[with(board_mngt, order(Location, Date, Time)), ]
board_mngt$`Board Height (m)`[board_mngt$`Board Height (m)` == 2] <- NA
board_mngt$Time <- format(board_mngt$Time, "%H:%M")
board_mngt$Adjustment <- c("START", "FINISH")

# SAVING -------------------------------------------------------------------------------------
a <- getwd()
setwd("C:/Users/gio/Documents/TD/Data/TIDY")

write.table(board_mngt, file = "VANWERT_Board_Management_2001-2009.txt", sep = "\t", 
            row.names = FALSE, quote = FALSE)

setwd(a)
rm(a, board_mngt, van, van2)
