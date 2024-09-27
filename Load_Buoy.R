#Installing the necessary packages.

library(data.table)
library(dplyr)
library(lubridate)
library(ggplot2)
library(zoo)
library(tibble)

file_root <- "https://www.ndbc.noaa.gov/view_text_file.php?filename=44013h"
tail <- ".txt.gz&dir=data/historical/stdmet/"

#Function to read the data
load_buoy_data <- function(year) {
  path <- paste0(file_root, year, tail)
  #Data before 2007 is having Tide column somewhere and in middle of data, rows starts having data for TIDE. 
  #They are missing mm column as well. Hence, Dealing with them separately!
  if (year < 2007) {        
    header <- scan(path, what = 'character', nlines = 1)
    buoy <- read.table(path, fill = TRUE, header = TRUE, sep = "")      
    buoy <- add_column(buoy, mm = NA, .after = "hh")
    buoy <- add_column(buoy, TIDE = NA, .after = "VIS")
    
  } else {
    header <- scan(path, what = 'character', nlines = 1)  
    buoy <- fread(path, header = FALSE, skip = 1, fill = TRUE)
    
    setnames(buoy, header)
  }
  
  return(buoy)
}

all_data <- lapply(1985:2023, load_buoy_data)

combined_data <- rbindlist(all_data, fill = TRUE)

print(summary(combined_data))

#Year was given in three different kind of column throughout the data. in the starting it is given using YY 
#Midway #YY is used followed by YYYY. Hence, getting them all into one.
combined_data <- combined_data %>%
  mutate(
    YY = as.character(YY),
    `#YY` = as.character(`#YY`),
    YYYY = as.character(YYYY)
  )
combined_data <- combined_data %>%
  mutate(YYYY = coalesce(YYYY, `#YY`, YY))

#PRES and BAR as well as WD and WDIR are same
combined_data <- combined_data %>%
  mutate(BAR = coalesce(as.numeric(BAR), as.numeric(PRES)), 
         WD = coalesce(as.numeric(WD), as.numeric(WDIR)))

#Removing unnecessary columns
combined_data <- combined_data %>%
  select(-TIDE, -TIDE.1, -mm.1,- WDIR, -PRES,-`#YY`,-YY)

#Creating data_time column using Lubridate
combined_data$datetime <- ymd_h(paste(combined_data$YYYY, combined_data$MM, combined_data$DD, combined_data$hh, sep = "-"))