library(here)
library(overlap)
library(maptools)
library(lubridate)
library(tidyverse)

# Import and merge record tables ---------------------------------------------

records <- read_csv("wildcam_fulldata_2019.csv") %>% 
  rename(Camera = site)

# Format and scale times ------------------------------------------------------------

# set spatial coordinates
coords <- matrix(c(34.50, -18.82), nrow=1) %>%
  sp::SpatialPoints(proj4string=sp::CRS("+proj=longlat +datum=WGS84"))

# specify date format
records$Date <- as.POSIXct(records$date, 
                           format = "%m/%d/%y",
                           tz = "Africa/Maputo")

# convert time to radians (could be done more efficiently with pipe)
records$Time.Corrected <- hms(records$time)
records$Time.Decimal <- records$Time.Corrected$hour + records$Time.Corrected$minute/60 + records$Time.Corrected$second/3600
records$Time.Scaled <- records$Time.Decimal / 24
records$Time.Radians <- records$Time.Scaled * 2 * pi
# calculate suntime using function from overlap package, and coordinates and dates as formatted above
records$Time.Sun <- sunTime(records$Time.Radians, records$Date, coords)



# Calculate time difference -----------------------------------------------

# sort records by station, species, then time
records <- records[order(records$Camera, records$species, records$datetime),]

# format dates
records$datetime <- as.POSIXct(records$datetime)

# run loop to calculate delta time
records$delta.time.secs <- NA # create column for delta time
records$delta.time.secs[[1]] <- 0 # set first row to 0
for (i in 2:nrow(records)) {
  
  if(records$Camera[[i]] == records$Camera[[i-1]] &
     records$species[[i]] == records$species[[i-1]]) {
    
    # calculate difference 
    records$delta.time.secs[[i]] <- difftime(records$datetime[[i]], 
                                             records$datetime[[i-1]], 
                                             units = "secs")
    
  } else {
    
    records$delta.time.secs[[i]] <- 0
    
  }
  
}


# Drop records outside of operation dates ------------------------------------------------------------

metadata <- read_csv("Camera_operation_year1-4_consolidated.csv")

# get the dates into date format
metadata[, 2:ncol(metadata)] <- lapply(metadata[, 2:ncol(metadata)], as.Date, format = "%m/%d/%y")

records <- left_join(records, metadata) # join by camera

# label records to drop if outside of operation date (either before start, after end, or during problem window)
records$drop <- FALSE  # create default of false
for (i in 1:nrow(records)) {
  if (records$date[i] < records$Start[i]) {
    records$drop[i] <- TRUE}
  else if (records$date[i] > records$End[i]) {
    records$drop[i] <- TRUE}
  else if ((is.na(records$Problem1_from[i]) == FALSE) & (records$date[i] > records$Problem1_from[i]) & (records$date[i] < records$Problem1_to[i])) {
    records$drop[i] <- TRUE}
  else if ((is.na(records$Problem2_from[i]) == FALSE) & (records$date[i] > records$Problem2_from[i]) & (records$date[i] < records$Problem2_to[i])) {
    records$drop[i] <- TRUE}
  else if ((is.na(records$Problem3_from[i]) == FALSE) & (records$date[i] > records$Problem3_from[i]) & (records$date[i] < records$Problem3_to[i])) {
    records$drop[i] <- TRUE}
  else if ((is.na(records$Problem4_from[i]) == FALSE) & (records$date[i] > records$Problem4_from[i]) & (records$date[i] < records$Problem4_to[i])) {
    records$drop[i] <- TRUE}
  else {
    records$drop[i] <- FALSE}
}

summary(records$drop)

# closer look at excluded records
records_drop <- records[records$drop == TRUE,]

write.csv(records_drop, "dropped_records.csv", row.names = F)

# exclude records outside of operation dates
records <- records[records$drop == FALSE,]

# rename to match
records <- records %>% 
  rename(Species = species,
         Time = time,
         DateTimeOriginal = datetime)

# take only columns we want/need
records <- select(records, Camera, Species, DateTimeOriginal, Date, Time, delta.time.secs, Time.Sun)


# Merge with species metadata ---------------------------------------------

# bring in species traits
species <- read_csv("~/Documents/github-repos/gorongosa/gorongosa-camera-traps/data/2018spp_kingdon.csv") %>%
  rename(Species = CommName) %>%  # rename to match name of column in records
  mutate(Species = tolower(Species)) # make lowercase to match MP format
  
# join records and traits
records2 <- left_join(records, species)

# remove those with NA for common name full (this includes setup, ghost, unknown)
records2 <- drop_na(records2, CommName_Full)

# subset columns to make file smaller
records2 <- records2 %>% 
  dplyr::select(Camera, Species, delta.time.secs, Date, Time.Sun, SppCode, CommName_Full)

# Export cleaned file ---------------------------------------------

write_csv(records2, here::here('recordtable_allrecordscleaned_speciesmetadata.csv'))
