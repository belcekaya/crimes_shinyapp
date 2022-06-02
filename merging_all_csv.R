
library(tidyverse)
library(lubridate)
library(stringr)
library(dplyr)

# Set your working directory to the folder where the CSV files are stored

paths <- Sys.glob("*/*.csv")
L <- Map(read.csv, paths)

crimes <- bind_rows(L, .id = "path")

glimpse(crimes)

# converting month to a date time
crimes$Month <- parse_date_time(crimes$Month, "ym")

# Split LSOA.name and retain just the Borough name
crimes <- separate(crimes, 'LSOA.name', into = c("Borough", "x"), sep = -5)

# Trim the trailing space on 'Borough' and convert it to a factor
crimes$Borough <- str_trim(crimes$Borough, side = "right") 

# Select and rename some of the variables
crimes <- crimes %>% select(date = Month,
                            force = Falls.within,
                            location = Location,
                            borough = Borough,
                            lsoa = LSOA.code,
                            category = Crime.type,
                            long = Longitude,
                            lat = Latitude)

# checking the NAs
summary(crimes)
# checking where are NAs
cbind(lapply(lapply(crimes, is.na), sum))

# Filtering crimes df with missing coordinates
crimes <- crimes %>% filter(!is.na(long))
# checking the data types
cbind(lapply(lapply(crimes, is.na), sum))

## changing the data type of the columns
# changing the columns that we will later put them in a slicer in the app "as factor"

col_names <- c('force' ,'borough','category')
crimes[,col_names] <- lapply(crimes[,col_names] , factor)
#str(crimes)

# checking the levels of the factor columns
levels(crimes$borough) # check the different boroughs
levels(crimes$force)
levels(crimes$category)
                            
# Export the tidy data as a CSV for later use
write_csv(crimes, "crime_data.csv")
