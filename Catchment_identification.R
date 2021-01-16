# Identifying Multisource Catchments
# Ben Smith
# Newcastle University
# 16/01/2021


# Preamble ----------------------------------------------------------------

# # Load all required packages:
# install.packages("zoo")
library("xts")
# install.packages("lfstat")
# install.packages("foreign")
# library(zoo)
# library(xts)

# install.packages("lfstat")
library(lfstat)
# library(foreign)

# Import a function for calculating base level:
source('https://raw.github.com/TonyLadson/BaseflowSeparation_LyneHollick/master/BFI.R')

# ---- Functions ----

# Set up a function to find peak fiver levels:
find_peaks <- function (y, m = 672){
  
  # A 'peak' is defined as a local maxima with m points either side of it being smaller than it. Hence, the bigger the parameter m, the more stringent is the peak funding procedure. 'm' is the minimum number of time units either side of a peak where smaller river levels are requires. i.e. when m = 672, there are no larger peaks with in (15 mins*672) 7 days either side of that peak. The function can also be used to find local minima of any sequential vector y via find_peaks(-y). [http://stats.stackexchange.com/questions/22974/how-to-find-local-peaks-valleys-in-a-series-of-data]
  
  date <- y$Date
  val  <- y$Flow
  lgth <- length(val)
  
  shape <- diff(sign(diff(val, na.pad = FALSE)))
  
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < lgth, w, lgth)
    if(all(val[c(z : i, (i + 2) : w)] <= val[i + 1], na.rm = T)) # I have added the ", na.rm = T"
      return(i + 1) else return(numeric(0)) 
  })
  
  pks <- unlist(pks)
  
  # Only use those peaks that are above threshold:
  pks <- pks[which(val[pks] >= quantile(val, na.rm = T, probs = (0.98)))]
  
  # Above function find peaks. m is equal to 1 week, so any duplicates should be due to equal values.
  # To remove these duplicates:
  remove <- which(diff(pks) <= m)
  if (length(remove)>0){pks = pks[-remove]}
  
  # Now we need to select the top 10 events: 
  df <- data.frame(index = c(1:length(pks)), value = val[pks])
  df <- df[order(-df$value),]
  df <- df[c(1:10),]
  pks <- pks[df$index]
  
  return(pks)
}

rise_fun <- function (data){
  
  rise <- rep(NA, length(data))
  
  difference <- diff(data)
  
  for (j in 4:(length(difference))){
    
    a = difference[j-3]
    b = difference[j-2]
    c = difference[j-1]
    d = difference[j]
    
    rise[j+1] <- suppressWarnings(max(a,b,c,d,
                                      a+b,
                                      a+b+c,
                                      a+b+c+d,
                                      b+c+d,
                                      c+d,
                                      b+c,
                                      na.rm = TRUE))
    # Supressing warinings stops the code aborting if all values are NAs
  }
  
  rise[is.infinite(rise)] = NA
  
  return(rise)
}

# ---- Set up dataframe for storing data ----

hardrive_path = "D:/RiverFlowData/dump/"

# List the flow records and their NRFA names:
flow_recs = data.frame(csvs = list.files(hardrive_path, full.names = FALSE, pattern = ".csv"))
flow_recs$nrfa = substr(x = flow_recs$csvs, start = 1, stop = (nchar(flow_recs$csvs))-4)

# Set the number of records to run through:
num_runs <- nrow(flow_recs)


# Set up meta data table:
meta_data_patched <- data.frame("Gauge Name"= character(num_runs),
                                "Number of NAs"= integer(num_runs),
                                "Patched_NAs"= integer(num_runs),
                                stringsAsFactors= FALSE)

# Set up the table for storing base level indicies:
base_level_indexes <- data.frame("gauge"  = character(num_runs),
                                 "bli_wmo" = numeric(num_runs),
                                 "bli_lh" = numeric(num_runs),
                                 stringsAsFactors=F)

# -----------------

# Create a folder for saving interpolated data:
if (!dir.exists("Interpolated Data")){dir.create("Interpolated Data")}

# Create a driectory for the output data:
if (!dir.exists("Low Flow Objects")){dir.create("Low Flow Objects")}

# -----------------

prop_rise = data.frame("gauge" = character(num_runs),
                       "Qt.ft" = integer(num_runs),
                       "Qt.st"   = integer(num_runs),
                       "Qt.svt"   = integer(num_runs),
                       stringsAsFactors = FALSE)

# Setup data.frame to recieve averaged source data
source_dat <- data.frame("gauge"   = character(length = num_runs),
                         "high_ql" = rep(NA, num_runs),
                         "high_bl" = rep(NA, num_runs),
                         stringsAsFactors = FALSE)

# Setup data.frame to recieve event base level data
event_record_baselevels <- data.frame("gauge"   = character(length = num_runs),
                                      "event_1" = rep(NA, num_runs),
                                      "event_2" = rep(NA, num_runs),
                                      "event_3" = rep(NA, num_runs),
                                      "event_4" = rep(NA, num_runs),
                                      "event_5" = rep(NA, num_runs),
                                      "event_6" = rep(NA, num_runs),
                                      "event_7" = rep(NA, num_runs),
                                      "event_8" = rep(NA, num_runs),
                                      "event_9" = rep(NA, num_runs),
                                      stringsAsFactors = FALSE)

# Setup data.frame to recieve event quick level data
event_record_quicklevels <- event_record_baselevels

# Setup data.frame to recieve event duration data
event_record_durations <- event_record_baselevels

# Record where there are less than 10 peak events:
no.pks <- data.frame("gauge" = character(), "peaks" = numeric())

# Prepare a PDF file for plotting events:
pdf(file=file.path("Peak hydrographs.pdf"), width = 11.69, height = 8.27)
layout(matrix(c(1:9), nrow = 3, byrow =TRUE))

# Setup the progress bar for main function
pb <- txtProgressBar(min = 1, max = num_runs, style = 3)

# Setup some time periods for use later:
four_days = 4*24*4 # 15 minutes > hours > days > 4 days
ten_days = 4*24*10 # 15 minutes > hours > days > 10 days
twelve_hours = 4*12


# The Main Function -------------------------------------------------------

# Initiate for-loop that will run through each record:
for(x in (1:2)){
  
  # ----------------------------------
  # >> Section 1 - Prepare the data <<
  # ----------------------------------
  
  g_name = flow_recs$nrfa[x]
    
  g_data = read.csv(paste0(hardrive_path, flow_recs$csvs[x]),
                    colClasses = c("character", "NULL", "numeric", "NULL"),
                    skip = 8, na.strings = "-9999.0000")
  
  # Convert characters dates into POSTIXct:
  g_data$Date <- as.POSIXct(g_data$Date, format="%Y-%m-%d %H:%M", tz = "UTC")
  
  # Save the  record to the workspace:
  # assign(x = paste0("tbl_", g_name), value = g_data)
  
  # --------------------------------------------------------------------------
  # >> Section 2 - Base Levels <<
  #
  # Calculate base level indexes for each gauge. Display these in a table.
  # This simply produces the lowflow objects (calculates base level).
  # --------------------------------------------------------------------------
  
  # --- Calculate Base levels using WMO Method ---
  
  # --- Create low flow objects
  
  # Aggregate the data to daily:
  g_data <- xts(x = g_data$Flow, order.by = g_data$Date, tzone = "GMT")
  g_data <- apply.daily(x = g_data, FUN = min) # use the minimum value as this is probably more appropriate for baseflow calculations.
  
  # Restructure data:
  g_data <- data.frame("date"  = index(g_data), "value" = coredata(g_data))
  g_data <- data.frame("flow"  = g_data$value,
                       "day"   = as.numeric(substr(g_data$date,9,10)),
                       "month" = as.numeric (substr(g_data$date,6,7)),
                       "year"  = as.numeric(substr(g_data$date,1,4)),
                       stringsAsFactors = F) 
  
  # Patch any single missing days:
  g_data$flow <- na.approx(g_data$flow, maxgap = 1, na.rm = F)
  
  # Remove any long gaps:
  # ..... # Maybe add this in, but it does not really affect results.
  
  # Create low flow object for analysis
  g_lfo <- suppressWarnings(createlfobj(x = g_data,
                                        baseflow = TRUE,
                                        hyearstart = 1)) # can change start of hydrolocical year (1-12)
  
  # Add the date back in:
  date <- paste(g_lfo$year, g_lfo$month, g_lfo$day, sep = "-")
  date <- as.POSIXct(date, format="%Y-%m-%d", tz = "UTC")
  
  g_lfo <- data.frame("date" = date,
                      "flow" = round(g_lfo$flow, digits=3),
                      "baseflow" = round(g_lfo$baseflow, digits = 3))

  # --- Format the Low Flow Object
  
  # Remove any bsaeflows when there are not river level values:
  g_lfo$baseflow[is.na(g_lfo$flow)] <- NA
  
  # Trim the NA's from the ends of the daily dataset:
  NA_baseflows <- which(!is.na(g_lfo$flow))
  if(any(is.na(NA_baseflows))){g_lfo <- g_lfo[c(min(NA_baseflows):max(NA_baseflows)),]}
  
  # Patch any single missing days of data:
  g_lfo$flow <- na.approx(g_lfo$flow, maxgap = 1, na.rm = F)
  g_lfo$baseflow <- na.approx(g_lfo$baseflow, maxgap = 1, na.rm = F)
  
  # Save data with baseflows:  
  write.csv(x = g_lfo, file = paste("Low Flow Objects/", g_name,'.Rdata',sep = ""))
  
  # --------------------------------
  # >> Section 3 - Part 1 <<
  # --------------------------------
  
  # Calculate the greatest river rise i.e. from lowest point to highest:
  # Note - Make this section an apply function - it is very slow!
  
  # Calculate the rises:
  g_data$rise = rise_fun(g_data$flow)
  
  # assign(x = g_name, value = g_data)
  
  # # -------------------------------------------------
  # # Calculate percentages of rises:
  # # (above 4/8, 5/8 & 6/8 of river level quantiles)
  # # -------------------------------------------------
  # 
  # tot = length(g_data$Flow) - sum(is.na(g_data$Flow))
  # 
  # Qt.ft <- (quantile(g_data$Flow, probs = 0.50, na.rm = T) - 
  #             quantile(g_data$Flow, probs = 0.05, na.rm = T))/4
  # Qt.st <- (quantile(g_data$Flow, probs = 0.62, na.rm = T) - 
  #             quantile(g_data$Flow, probs = 0.05, na.rm = T))/4
  # Qt.svt <- (quantile(g_data$Flow, probs = 0.75, na.rm = T) - 
  #              quantile(g_data$Flow, probs = 0.05, na.rm = T))/4
  # 
  # Qt.ft = length(which(g_data$rise > Qt.ft)) * 100/tot
  # Qt.st = length(which(g_data$rise > Qt.st)) * 100/tot
  # Qt.svt = length(which(g_data$rise > Qt.svt)) * 100/tot
  # 
  # prop_rise[x,] <- c(substr(g_name, 4, nchar(g_name)),
  #                    round(Qt.ft, digits = 2),
  #                    round(Qt.st, digits = 2),
  #                    round(Qt.svt, digits = 2))
  # 
  
  
  #-----------------------------------------------------
  # >> Section 3 - PART 2 <<
  # -----------------------------------------------------
  # Remove base level from the total River Level to leave
  # behind only surface water inputs (in theory).
  # -----------------------------------------------------
  
  
  # lf_data  <- setNames(lapply(ls(pattern="lfo"), function(x) get(x)),
  #                      ls(pattern="lfo"))
  # lf_name <- ls(lf_data)
  # lf_data <- lf_data[[1]]
  
  #---------------------------------------
  
  days <- substr(g_data$Date, 1, 10)
  
  #---------------------------------------
  
  # Group the 15 minute record into days:
  runs <- rle(days)
  
  #---------------------------------------
  
  # Note the end point of each day's record:
  consec_runs <- which(runs$lengths >= 1) 
  consec_ends <- cumsum(runs$lengths) 
  ends        <- consec_ends[consec_runs]
  
  #---------------------------------------
  
  # Determine the starting point of each day's record:
  newindex <- ifelse(consec_runs>1, consec_runs-1, 0)
  starts   <- consec_ends[newindex] + 1
  if (0 %in% newindex) starts <- c(1,starts)
  
  #---------------------------------------
  
  # Set up a blank vector to recieve 'BL normalised' values:
  normalised <- rep(NA, length(g_data$Date))
  
  
  for (j in 1:length(starts)){
    
    starts_j <- starts[j]
    ends_j   <- ends[j]
    
    # Determine whether the days in the daily record have a calcuated BL:
    z <- which(lf_data$date == as.POSIXct(days[starts_j], tz = "UTC"))
    if (length(z)>=1){
      
      # Normalise the river level is so:
      RL <- g_data$Flow[c(starts_j:ends_j)]
      BL <- lf_data$bl_wmo[z]
      normalised[starts_j:ends_j] <- RL-BL
      
      # If there is not a BL value, fill the space with NA's
    } else{
      normalised[starts_j:ends_j] <- NA
    }
  }
  
  # Make any over normalised (negative values) equal to 0:
  normalised[which(normalised<=0)] <- 0
  
  #---------------------------------------
  
  # Keep the normalised data:
  g_data$"q_level" <- normalised
  assign(x = g_name, value = g_data)
  
  #---------------------------------------
  
  # Save the data:
  filepath <- paste("Interpolated Data/", g_name,'.Rdata',sep = "")
  save(list = g_name, file = filepath)
  
  
  # ----------------------------------------------------------------------
  # >> Section 4 <<
  # Aim: Find multisource events using peak data, work out ratios for each
  # event and average them for that gauge.
  # ----------------------------------------------------------------------
  
  # Find the Peak Events ----------------------------------------------------
  
  # Find the top 10 peak events:
  index <- find_peaks(y = g_data)
  
  # Calculate threshold to define starts/close of peak events:
  rl_threshold  <- quantile(g_data$Flow, na.rm = T, probs = (0.98))
  
  # Set up dataframe to store values ready for averaging:
  ratios = data.frame("ql" = rep(NA, 9),
                      "bl" = rep(NA, 9),
                      "duration" = rep(NA, 9))
  
  # Calculate threshold for defining the occurance of a rapid rise
  # ql_threshold <- max(g_data$Flow, na.rm = T)*0.02 # 2% of Qmax
  
  # Start the loop for looking at each event:
  no.events = min(length(which(!is.na(index)==T)), 9)
  for (j in 1:no.events){
    
    # Determine start of peak event:
    
    # If there is 4 days of previous data:
    if (index[j] > four_days &
        
        # And, if there are genuine measurements for the whole of that period:
        all(!is.na(g_data$Flow[max(c(index[j] - four_days), 0) : index[j]]) == TRUE)){
      # max(c(..., 0)) is in there so that no negative index  
      # is calculated if there are less than 4 days of data.
      
      # Set the start as 4 days before the peak:
      start = index[j] - four_days
      
      # If there is not 4 days of complete data:
    } else {
      
      # See how much data there is in the run up to the event:
      # (i.e. between this point and the peak there will be no missing data)
      run = rle(diff(which(!is.na(rev(g_data$Flow[1:index[j]])))))
      
      # Set the start date as that point:
      start = index[j]-run$lengths[1]
      
      # If there is no measured data before the peak, set the peak as the start:
      if(is.na(start)){start = index[j]}
    }
    
    # Determine end of peak event:
    
    # If there is 10 days of subsequent data:
    if (index[j] < (length(g_data$Date) - ten_days) & 
        
        # And, if there are genuine measurements for the whole of that period:
        all(!is.na(g_data$Flow[index[j] : min(c(index[j] + ten_days, length(g_data$Date)))]) == TRUE)){
      # min(c(..., lngth)) is in there so that  
      # no index outside the bounds is calculated.
      
      # Set the start as 10 days after the peak:
      end = index[j] + ten_days
      
      # If there is not 10 days of complete data:
    } else {
      
      # See how much data there is in the run up to the event:
      # (i.e. between this point and the peak there will be no missing data)
      run = rle(diff(which(!is.na(g_data$Flow[index[j]: (index[j]+ten_days)]))))
      
      # Set the end date as that point:
      end = index[j] + run$lengths[1]
      
      # If there is no measured data after the peak, set the peak as the end:
      if(is.na(end)){end = index[j]}
    }
    
    # Set up vector of the indexes of a peak event:
    pk <- c(start:end)
    
    # Calculate Stats for Each Peak Event -------------------------------------
    
    # Define high river levels there are:
    high_rl <- length(which(g_data$Flow[pk] >= rl_threshold))
    
    # -----
    
    # Calculate the area under the rising limb:
    
    # First find the duration to look at:
    range = c((index[j]-twelve_hours) : index[j])
    
    # In some instances, when peaks are at the very start of a dataset, range can call negative numbers. This must be stopped:
    range = range[range>0] 
    
    minimum = which(g_data$Flow[range] == min(g_data$Flow[range],
                                              na.rm = T))
    
    # In case there are multiples, chose the latest minimum:
    minimum = minimum[length(minimum)]
    
    # Caclulate the volume of surface water you are interested in:
    runoff = g_data$q_level[range[minimum] : index[j]] - g_data$q_level[range[minimum]]
    
    high_ql = max(runoff)/sum(runoff, na.rm = TRUE) 
    # This is not actually calculating the area as you claim, that would be:
    # max(runoff)/mean(runoff, na.rm = TRUE)*length(runoff)*15*60
    # length(runoff)*15*60 is the length of the event in seconds.
    
    # -----    
    
    # Identify the highest base level during peak event j:
    high_bl = max((g_data$Flow[pk] - g_data$q_level[pk]), na.rm = TRUE)
    
    # Calculate highest base level as a proportion of the peak river level:
    high_bl = high_bl/ max(g_data$Flow[index[j]], na.rm = TRUE)
    
    # Record the Event Stats --------------------------------------------------
    ratios[j,] = c(high_ql, # Instances of Rapid Rise
                   high_bl, # The highest baselevel / highest river level
                   length(pk)/96) # length of the event in days 
    
    # ratios = round(x = ratios, digits = 2)
    
    # Plot the Event ----------------------------------------------------------
    
    if(plot_event == TRUE){
      # Plot the hydrograph:
      plot(x = g_data$Date[pk], g_data$Flow[pk],
           ylim = c(0, (max(g_data$Flow, na.rm = TRUE)+0.2)),
           type = "l", lwd = 1,
           ylab = "Flow (cumecs)",
           xlab = paste("Event Beginning: ", g_data$Date[pk[1]], sep = ""),
           main = paste("Gauge ", substr(g_name, 5, nchar(g_name)), ": Event ", j, sep =""))
      
      # Add lines to define the event:
      abline(h = rl_threshold, lty = 1, col = "lightgrey")
      
      # Add the above threshold rises (surface water):
      lines(x = g_data$Date[pk[high_ql]], 
            y = g_data$Flow[pk[high_ql]],
            col = 2, type = "p", pch = 16, cex = 0.5)
      
      # Add the base level:
      lines(x= g_data$Date,
            y = (g_data$Flow - g_data$q_level),
            type = "l", lwd = 1, col = 4)
      
      # Add info to graphs:
      legend("topleft", inset=.05,
             c(paste("gQ ", high_bl, sep = ""), 
               paste("sQ ", round(length(high_ql), digits = 0), sep = ""),
               paste("Days ", round(length(pk)/96, digits = 1))),
             fill = c(4,2,0))
      
    }
    # Fill any missing plots with blank frames
    
  }
  
  if(plot_event == TRUE){
    # Determine whether there are blank plots:
    if (9-no.events>=1){
      # Fill them if so:
      for(k in 1:(9-no.events)){ frame() }
    }
  }
  
  # Add Average Event Stats to a Dataframe for Export -----------------------
  
  # *** Needs Fixing ***
  
  event_record_baselevels[x,]   = c("gauge" = g_name, ratios$bl)
  event_record_quicklevels[x,]  = c("gauge" = g_name, ratios$ql)
  event_record_durations[x,]    = c("gauge" = g_name, ratios$duration)
  
  source_dat[x,] = c(g_name,
                     round(mean(ratios[ratios[,1]>0,1], na.rm = T),digits = 2), # the >0 is to cull -Inf values
                     round(mean(ratios[ratios[,2]>0,2], na.rm = T),digits = 2)) # the >0 is to cull -Inf values
  
  
  # And Finally -------------------------------------------------------------
  
  # Display progress bar
  setTxtProgressBar(pb, x)
  
  # Clean the Workspace
  rm(pk, temp_data)
  rm(list = ls(pattern = "lfo"))
  rm(list = ls(pattern = "tbl"))
  
} 

# This will need to skip 20 as it has no data!

# Clean the Workspace
rm(bli_wmo, flow_recs, runs, BL, consec_ends, consec_runs, date, days, end, ends, ends_j, filepath, files_in_directory,four_days, g_data, g_name, high_bl, high_ql, high_rl, index, k, lf_data, lf_name, lyne_hollick, missing, NA_Bls, newindex, no.events, no.pks, normalised, num_runs, pb, prop_rise, ql_threshold, Qt.ft, Qt.st, Qt.svt, ratios, RL, run, rl_threshold, start, starts, starts_j, ten_days, tot, z)


# Save source_dat
save(list = "source_dat", file = "Source Data - Flows - BL as proportion of peak RL.Rdata")
write.csv(source_dat, "Identified Sources - BLpks WL5cm.csv", na = "NA")

# Close the PDF
dev.off()

# Close progress bar
close(pb)

# Save event data
write.csv(event_record_baselevels, "Event_Base_Levels.csv", na = "NA")
write.csv(event_record_quicklevels, "Event_Quick_Levels.csv", na = "NA")
write.csv(event_record_durations, "Event_Durations.csv", na = "NA")



