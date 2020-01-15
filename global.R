# Gorongosa Camera Trap Data Shiny App

library(tidyverse)
library(shiny)
library(shinythemes)
library(overlap)
library(plotly)
library(here)
library(shinydashboard)
library(leaflet)
library(camtrapR)
library(sp)
library(rgdal)
library(broom)
library(viridis)

setwd(here::here("shiny-rai"))

#source("modules/map_card.R")

# source custom functions
files.sources = list.files("functions", full.names = T)
sapply(files.sources, source)

# no scientific notation and round to 2 decimals
options(scipen = 999) #, digits = 2)


# Data import -------------------------------------------------------------

# import shapefile
#hexes <- sf::st_read("shapefile", "CameraGridHexes", quiet = T) %>%
#  sf::st_transform(crs = '+proj=longlat +datum=WGS84') %>%
#  rename(Camera = StudySite) 

hexes <- readOGR("shapefile", "CameraGridHexes")

hexes@data %<>%
  rename(Camera = StudySite) # records and camera_operation use "Camera" not "StudySite" so this allows them to join

# get into plottable form
hexes.df <- broom::tidy(hexes, region = "Camera")
#hexes$polyID <- sapply(slot(hexes, "polygons"), function(x) slot(x, "ID"))

# import record table
records <- read_csv("recordtable_allrecordscleaned_speciesmetadata.csv")
records$Date <- as.Date(records$Date)

# strip just month
records$Month_Year <- format(as.Date(records$Date), "%Y-%m")

# import camera operation spreadsheet
camera_operation <- read_csv("Camera_operation_years1and2.csv") %>%
  mutate_at(c("Start", "End", "Problem1_from", "Problem1_to",
              "Problem2_from", "Problem2_to", "Problem3_from", "Problem3_to"),
            ~as.Date(., format = "%m/%d/%y"))

# import camera metadata
camera_metadata <- read.csv("cam_metadata_norm_031519.csv") %>%
  rename(Camera = StudySite) # records and camera_operation use "Camera" not "StudySite" so this allows them to join


# Data manipulation -------------------------------------------------------

# join records and camera operation
records <- left_join(records, camera_operation)

# generate a camera operation matrix
camera_operation_matrix <- cameraOperation(CTtable = camera_operation,        ## data frame with metadata
                                           stationCol = "Camera",
                                           setupCol = "Start",       ## name of column in metadata with setup date
                                           retrievalCol = "End",     ## name of column in metadata with end date
                                           hasProblems = TRUE,
                                           writecsv = FALSE) %>% 
                            as_tibble(rownames = "Camera")
          
# Define timeplot function ------------------------------------------------

timeplot <-function (A, n.grid = 128, kmax = 3, linecol = "#00BFC4",  ...) 
{
  bwA <- getBandWidth(A, kmax = kmax)
  
  xsc <- 24/(2 * pi)
  xxRad <- seq(0, 2 * pi, length = n.grid)
  xx <- xxRad * xsc
  densA <- densityFit(A, xxRad, bwA)/xsc
  
  ylim <- c(0, max(densA))
  plot(0, 0, type = "n", ylim = ylim, xlim = range(xx), xlab = "Time", 
       ylab = "Activity", xaxt = "n", ...)
  axis(1, at = c(0, 6, 12, 18, 24), labels = c("Midnight", 
                                               "Sunrise", "Noon", "Sunset", "Midnight"))
  lines(xx, densA, lty = 1, col = linecol, lwd = 2)
  return(invisible(list(x = xx, densityA = densA)))
}


# Define overlap function -------------------------------------------------

overlapPlot2 <- function (A, B, xscale = 24, linetype = c(1, 1), linecol = c("#F8766D", "#00BFC4"),  linewidth = c(2, 2),
                        n.grid = 128, kmax = 3, adjust = 1, 
                        ...) 
{
  bwA <- getBandWidth(A, kmax = kmax)/adjust
  bwB <- getBandWidth(B, kmax = kmax)/adjust
  if (is.na(bwA) || is.na(bwB)) 
    stop("Bandwidth estimation failed.")
  xsc <- if (is.na(xscale))
    1
  else xscale/(2 * pi)
  xxRad <- seq(0, 2 * pi, length = n.grid)
  xx <- xxRad * xsc
  densA <- densityFit(A, xxRad, bwA)/xsc
  densB <- densityFit(B, xxRad, bwB)/xsc
  densOL <- pmin(densA, densB)
  ylim <- c(0, max(densA, densB))
  plot(0, 0, type = "n", ylim = ylim, xlim = range(xx), xlab = "Time", 
       ylab = "Density", xaxt = "n", ...)
  if (is.na(xscale)) {
    axis(1, at = c(0, pi/2, pi, 3 * pi/2, 2 * pi), labels = c("0", 
                                                              expression(pi/2), expression(pi), expression(3 * 
                                                                                                             pi/2), expression(2 * pi)))
  }
  else if (xscale == 24) {
    axis(1, at = c(0, 6, 12, 18, 24), labels = c("Midnight", 
                                                 "Sunrise", "Noon", "Sunset", "Midnight"))
  }
  else {
    axis(1)
  }
  lines(xx, densA, lty = linetype[1], col = linecol[1], lwd = linewidth[[1]])
  lines(xx, densB, lty = linetype[2], col = linecol[2], lwd = linewidth[[2]])
  return(invisible(list(x = xx, densityA = densA, densityB = densB)))
}


# Define RAI functions -----------------------------------------------------

# Define unique classification categories. We will need this later to ensure these columns are present in all tables
# There is probably a better way to do this that is not so hard-coded and uses unique(records$Species) but I can't figure it out
allclassifications <- c(Aardvark = NA_real_, Baboon = NA_real_, Buffalo = NA_real_, Bushbaby = NA_real_, 
                        Bushbuck = NA_real_, Bushpig = NA_real_, Civet = NA_real_, Duiker_common = NA_real_, 
                        Duiker_red = NA_real_, Eland = NA_real_, Elephant = NA_real_, Genet = NA_real_, 
                        Hare = NA_real_, Hartebeest = NA_real_, Hippo = NA_real_, Honey_badger = NA_real_, 
                        Impala = NA_real_, Kudu = NA_real_, Lion = NA_real_, Mongoose_banded = NA_real_, 
                        Mongoose_bushy_tailed = NA_real_, Mongoose_dwarf = NA_real_, Mongoose_large_grey = NA_real_,
                        Mongoose_marsh = NA_real_, Mongoose_slender = NA_real_, Mongoose_white_tailed = NA_real_,
                        Nyala = NA_real_, Oribi = NA_real_, Pangolin = NA_real_, Porcupine = NA_real_, 
                        Reedbuck = NA_real_, Sable_antelope = NA_real_, Samango = NA_real_, Serval = NA_real_, 
                        Vervet = NA_real_, Warthog = NA_real_, Waterbuck = NA_real_, Wildebeest = NA_real_)  

# define RAI calculation function - using record table that has ALREADY BEEN SUBSET

rai.calculate <- function(record.table.subset, camop, start.date, end.date) {
  
  # calculate how long the camera was functioning in that time period
  
    # change start and end date to character
    start.date <- as.character(start.date)
    end.date <- as.character(end.date)
    
    # sum rows within specified dates (there are 1s when camera was operating, NA when not)
    camop$Operation <- rowSums(dplyr::select(camop, start.date:end.date), na.rm=TRUE) 
    
    # get rid of the individual day columns, just select Camera, Operation
    camop <- dplyr::select(camop, Camera, Operation) 
    
  # calculate number of observations of each classification type at each camera
  record_count <- record.table.subset %>%
    dplyr::group_by(Species, Camera) %>%
    dplyr::summarise(Detections = n()) %>%     # counts number of observations of each species
    spread(key = Species, value = Detections)  # gets from long to wide format  
  
  # add columns for classes not present
  record_count <- add_column(record_count, !!!allclassifications[!names(allclassifications) %in% names(record_count)])
  
  # gather data so each class-camera is its own row again
  record_count <- record_count %>% gather(2:ncol(record_count), key = "Species", value = "Count")
  
  # replace NA with 0 
  record_count[is.na(record_count)] <- 0
  
#  # calculate for all cameras combined
#  record_count_all <- record_count %>%
#    dplyr::group_by(Species) %>%
#    dplyr::summarise(Count = sum(Count)) 
#  
#  # add "camera" column
#  record_count_all$Camera <- "All"
#  
#  # join the total to the camera
#  record_count <- dplyr::bind_rows(record_count, record_count_all)
  
  # join camera operation dates and observations
  RAI.table <- plyr::join(record_count, camop)
  
  # calculate RAI
  RAI.table$RAI <- RAI.table$Count / RAI.table$Operation
  
  # replace infinity with NA
  RAI.table %<>% mutate_if(is.numeric, list(~na_if(., Inf)))
  
  return(RAI.table)
  
}

# count records by month, for individual cameras and for all combined
# does NOT control for differences in operation date

record.count.monthly <- function(record.table.subset) {
  
  # calculate number of observations of each classification type at each camera
  record_count <- record.table.subset %>%
    dplyr::group_by(Species, Camera, Month_Year) %>%
    dplyr::summarise(Detections = n()) %>%     # counts number of observations of each species
    spread(key = Species, value = Detections)  # gets from long to wide format  
  
  # add columns for classes not present
  record_count <- add_column(record_count, !!!allclassifications[!names(allclassifications) %in% names(record_count)])
  
  # gather data so each class-camera-month is its own row again
  record_count <- record_count %>% gather(3:ncol(record_count), key = "Species", value = "Count")
  
  # replace NA with 0 
  record_count[is.na(record_count)] <- 0
  
  # calculate for all cameras combined for each month-year
  record_count_all <- record_count %>%
    dplyr::group_by(Species, Month_Year) %>%
    dplyr::summarise(Count = sum(Count)) 
  
  # add "camera" column
  record_count_all$Camera <- "All"
  
  # join the total to the camera
  record_count <- dplyr::bind_rows(record_count, record_count_all)
  
  return(record_count)
  
}

rai.monthly <- function(record.table.subset, camop, start.date, end.date) {
  
  # calculate how long the camera was functioning in that time period
  
    # change start and end date to character
    start.date <- as.character(start.date)
    end.date <- as.character(end.date)
    
    # selects columns within specified dates
    camop.subset <- dplyr::select(camop, Camera, start.date:end.date)
    
    # transpose data frame
    camop.subset.monthly <- as_tibble(cbind(names(camop.subset), t(camop.subset)))
    colnames(camop.subset.monthly) <- as.character(unlist(camop.subset.monthly[1,]))
    camop.subset.monthly = camop.subset.monthly[-1, ]
    
    # fix to make numeric
    camop.subset.monthly[, 2:ncol(camop.subset.monthly)] %<>% mutate_if(is.character, as.numeric)
    
    # sum operation for all cameras
    camop.subset.monthly$All <- camop.subset.monthly %>%
      select(-Camera) %>%
      rowSums(na.rm = TRUE)
    
    # add column for just month
    camop.subset.monthly$Month_Year <- format(as.Date(camop.subset.monthly$Camera), "%Y-%m")
    
    # calculate number of operation days for each camera in each month-year
    camop.subset.monthly.summary <- camop.subset.monthly %>%
      dplyr::select(-Camera) %>% # drop date (confusingly called camera due to transposing above)
      pivot_longer(A06:All, names_to = "Camera", values_to = "Operating") %>% # new 'gather' function
      dplyr::group_by(Camera, Month_Year) %>%
      dplyr::summarise(Operation = sum(Operating, na.rm = TRUE))
  
  # calculate number of observations of each classification type at each camera
  record_count <- record.table.subset %>%
    dplyr::group_by(Species, Camera, Month_Year) %>%
    dplyr::summarise(Detections = n()) %>%     # counts number of observations of each species
    spread(key = Species, value = Detections)  # gets from long to wide format  
  
  # add columns for classes not present
  record_count <- add_column(record_count, !!!allclassifications[!names(allclassifications) %in% names(record_count)])
  
  # gather data so each class-camera-month is its own row again
  record_count <- record_count %>% gather(3:ncol(record_count), key = "Species", value = "Count")
  
  # replace NA with 0 
  record_count[is.na(record_count)] <- 0
  
  # calculate for all cameras combined for each month-year
  record_count_all <- record_count %>%
    dplyr::group_by(Species, Month_Year) %>%
    dplyr::summarise(Count = sum(Count)) 
  
  # add "camera" column
  record_count_all$Camera <- "All"
  
  # join the total to the camera
  record_count <- dplyr::bind_rows(record_count, record_count_all)
  
  # join camera operation dates and observations
  RAI.table <- left_join(record_count, camop.subset.monthly.summary)
  
  # calculate RAI
  RAI.table$RAI <- RAI.table$Count / RAI.table$Operation

  # replace infinity with NA
  RAI.table %<>% mutate_if(is.numeric, list(~na_if(., Inf)))
  
  return(RAI.table)
  
}