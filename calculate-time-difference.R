# assess temporal independence between records   ####

assessTemporalIndependence <- function(intable,
                                       deltaTimeComparedTo,
                                       columnOfInterest,     # species/individual column
                                       cameraCol,
                                       camerasIndependent,
                                       stationCol,
                                       minDeltaTime,
                                       eventSummaryColumn,
                                       eventSummaryFunction)
{

  # prepare to add time difference between observations columns
  intable <- data.frame(intable,
                        delta.time.secs  = NA,
                        delta.time.mins  = NA,
                        delta.time.hours = NA,
                        delta.time.days  = NA,
                        independent      = ifelse(minDeltaTime == 0, TRUE, NA),   # all independent if no temporal filtering
                        stringsAsFactors = FALSE,
                        check.names      = FALSE)        # to prevent ":" being converted to ".", e.g. in EXIF:Make
  
  # sort records by station, species, then time
  intable <- intable[order(intable[, stationCol], intable[, columnOfInterest], intable$DateTimeOriginal),]
  
  for(xy in 1:nrow(intable)){     # for every record
    
    
    which.columnOfInterest <- which(intable[, columnOfInterest]  == intable[xy, columnOfInterest])          # same species/individual
    which.stationCol       <- which(intable[, stationCol]        == intable[xy, stationCol])                # at same station
    which.independent      <- which(intable$independent          == TRUE)                                   # independent (first or only record of a species at a station)
    which.earlier          <- which(intable$DateTimeOriginal     <  intable$DateTimeOriginal[xy])          # earlier than record xy (takes long)
    #which.earlier          <- 1: (xy-1)                                                                  # earlier than record xy  (fast alternative, relies on table being sorted by date/time before anything else)
    if(camerasIndependent) {
      which.cameraCol      <- which(intable[, cameraCol]  == intable[xy, cameraCol])                        # at same camera
    }
    
    # set independent = TRUE and delta.time = 0 if it is the 1st/only  record of a species / individual
    
    if(camerasIndependent == TRUE){
      which.tmp <- Reduce(intersect, list(which.columnOfInterest, 
                                          which.stationCol, 
                                          which.cameraCol))
      if(intable$DateTimeOriginal[xy]  == min(intable$DateTimeOriginal[which.tmp])){    # cameras at same station assessed independently
        intable$independent[xy]       <- TRUE
        intable$delta.time.secs[xy]   <- 0
      }
    } else {
      which.tmp <- Reduce(intersect, list(which.columnOfInterest, 
                                          which.stationCol))
      if(intable$DateTimeOriginal[xy]  == min(intable$DateTimeOriginal[which.tmp])){
        intable$independent[xy]       <- TRUE
        intable$delta.time.secs[xy]   <- 0
      }
    }
    
    # calculate time difference to previous records of same species at this station (if not the 1st/only record)
    if(is.na(intable$delta.time.secs[xy])) {
      
      if(deltaTimeComparedTo == "lastIndependentRecord"){
        
        if(camerasIndependent == TRUE){
          which_time2 <- Reduce(intersect, list(which.columnOfInterest, 
                                                which.stationCol,
                                                which.cameraCol,
                                                which.independent,
                                                which.earlier))
        } else {
          
          which_time2 <- Reduce(intersect, list(which.columnOfInterest, 
                                                which.stationCol,
                                                which.independent,
                                                which.earlier))
        }
      }  else {    # if(deltaTimeComparedTo == "lastRecord"){'
        if(camerasIndependent  == TRUE){
          which_time2 <- Reduce(intersect, list(which.columnOfInterest, 
                                                which.stationCol,
                                                which.cameraCol,
                                                which.earlier))
        } else {
          which_time2 <- Reduce(intersect, list(which.columnOfInterest, 
                                                which.stationCol,
                                                which.earlier))
        }
      }
      
      # time difference to last (independent) record
      diff_tmp <- min(na.omit(difftime(time1 = intable$DateTimeOriginal[xy],            # delta time to last independent record
                                       time2 = intable$DateTimeOriginal[which_time2],
                                       units = "secs")))
      
      # save delta time in seconds
      intable$delta.time.secs[xy] <-  diff_tmp
      if(intable$delta.time.secs[xy] >= (minDeltaTime * 60) | intable$delta.time.secs[xy] == 0){
        intable$independent[xy] <- TRUE
      } else {
        intable$independent[xy] <- FALSE
      }
      
    }   # end   if(intable$DateTimeOriginal[xy] == min(...)} else {...}
  }     # end for(xy in 1:nrow(intable))
  
  # summarise some column by independent event
  
  n_imagesColumn <- "n_images"
  intable[, n_imagesColumn] <- NA
  
  which_independent <- which(intable$independent)
  
  if(hasArg(eventSummaryColumn)){
    if(all(eventSummaryColumn %in% colnames(intable))){
      if(length(eventSummaryColumn) != length(eventSummaryFunction)) stop('"eventSummaryColumn" and "eventSummaryFunction" must have same length', call. = FALSE)
      
    } else {
      warning(paste(unique(intable[, stationCol]), ": eventSummaryColumn(s) " ,
                    paste(eventSummaryColumn[!eventSummaryColumn %in% colnames(intable)], collapse = ", "),
                    " not found in column names of recordtable"), call. = FALSE)
    }
    
    summary_column_name <- paste(eventSummaryColumn, eventSummaryFunction, sep = "_")
    intable[, summary_column_name[eventSummaryColumn %in% colnames(intable)]] <- NA
    
    
    # summary_column_name <- paste(eventSummaryColumn, eventSummaryFunction, sep = "_")[which(eventSummaryColumn %in% colnames(intable))]
    # intable[, summary_column_name] <- NA
  }    # end    if(hasArg(eventSummaryColumn)){
  
  
  for(xy in 1:length(which_independent)){     # for every independent record (the ones that end up in record table)
    
    current_row <- which_independent[xy]
    
    if(camerasIndependent){
      which_records_to_group <- which(intable[, columnOfInterest]     == intable[current_row, columnOfInterest] &   # same species
                                        intable[, stationCol]           == intable[current_row, stationCol]  &        # same station
                                        intable[, cameraCol]            == intable[current_row, cameraCol]   &        # same camera
                                        intable$DateTimeOriginal        >= intable$DateTimeOriginal[current_row] &    # later than current record
                                        !isTRUE(intable$independent))                                        # not independent
      
    } else {
      which_records_to_group <- which(intable[, columnOfInterest]     == intable[current_row, columnOfInterest] &   # same species
                                        intable[, stationCol]           == intable[current_row, stationCol]  &        # same station
                                        intable$DateTimeOriginal        >= intable$DateTimeOriginal[current_row] &
                                        !isTRUE(intable$independent))                                        # not independent
    }
    
    # subset to records before the next independent record
    if(xy < length(which_independent)){
      which_records_to_group <- which_records_to_group[which_records_to_group < which_independent[xy + 1]] #which_records_to_group[which_records_to_group %in% seq(current_row, (which_independent[xy + 1] - 1))]
    } else {
      which_records_to_group <-  which_records_to_group[which_records_to_group <= nrow(intable)]   # which_records_to_group[which_records_to_group %in% seq(current_row, nrow(intable))]
    }
    
    if(hasArg(eventSummaryColumn)){
      
      for(eventSummaryIndex in 1:length(eventSummaryColumn)) {
        
        if(eventSummaryColumn[eventSummaryIndex] %in% colnames(intable)){
          
          summary_value <- do.call(what = eventSummaryFunction[eventSummaryIndex], 
                                   args = list(intable[which_records_to_group, eventSummaryColumn[eventSummaryIndex]], 
                                               na.rm = TRUE))
          if(!is.infinite(summary_value)){
            intable[current_row, summary_column_name[eventSummaryIndex]] <- ifelse(length(summary_value) > 1, 
                                                                                   paste(summary_value, collapse = ", "),
                                                                                   summary_value)
          }
          rm(summary_value)
        }
        
        
      }    # end for(eventSummaryIndex in 1:length(eventSummaryColumn)
    }      # end if(hasArg(eventSummaryColumn))
    
    intable[ current_row, n_imagesColumn] <- length(which_records_to_group)
    rm(which_records_to_group, current_row)
    
  }        # end for(xy in 1:length(which_independent))
  
  
  # keep only independent records
  outtable <- intable[intable$independent,]
  
  
  # compute delta time in hours and days
  outtable$delta.time.secs  <- round(outtable$delta.time.secs, digits = 0)
  outtable$delta.time.mins  <- round(outtable$delta.time.secs  / 60, digits = 1)
  outtable$delta.time.hours <- round(outtable$delta.time.mins  / 60, digits = 1)
  outtable$delta.time.days  <- round(outtable$delta.time.hours / 24, digits = 1)
  
  # remove "independent" column
  
  outtable <- outtable[, !colnames(outtable) %in% "independent"]
  
  return(outtable)
}
