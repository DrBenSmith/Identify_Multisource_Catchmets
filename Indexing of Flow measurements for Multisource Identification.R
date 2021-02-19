# Create Multisource Indexes from Flow records
# Entire script with set up.
# 18/12/2017

# This uses the latest method from Greg


# This notebook will fun through the code used in the final script for identifying multi sourse rivers. This will draw on scripts in **G:\Stage_Data\Raw_Data_series** but combine them to run in a single script. The difference between these scripts will be in terms of the origional data structure and that this script will use flows instead of levels.

# This script will run through all of the records, individually analysing, rather than completing analysis for all records on a section by section basis.

# The main method will be the same.

# Premable

# >>> The user may need to change some of the bits in this step

# Set the directory to the above file:
# setwd(dir="../Data/River Flow Data/")
# file_path = "G:/Greg/"

# -----------------

# Choose whether to plot the events
plot_event = TRUE

# -----------------

# Create a list of the records to analyse:
  # DODO: Does this need to be QCed? Is the QC data on your laptop?
# data_to_use <- read.csv("stations_qc_dump.csv", 
#                         stringsAsFactors = F,
#                         sep = ",", skip = 4)

data_to_use <- data.frame("files" = list.files("../Data/River Flow Data/"))
data_to_use$nrfa_id = substr(x = data_to_use$files, start = 1, stop = nchar(data_to_use$files)-4)

# Load all required packages:
# install.packages("zoo")
# install.packages("xts")
# install.packages("lfstat")
# install.packages("foreign")
library(zoo)
library(xts)
library(lfstat)
library(foreign)

# -----------------

# Set the number of records to run through:
num_runs <- length(data_to_use$nrfa_id)

# -----------------

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
if (!dir.exists("../Data/Interpolated Data")){dir.create("../Data/Interpolated Data")}

# Create a driectory for the output data:
if (!dir.exists("../Data/Low Flow Objects")){dir.create("../Data/Low Flow Objects")}

# -----------------

# Import the funciton for calculating base level:
source('https://raw.github.com/TonyLadson/BaseflowSeparation_LyneHollick/master/BFI.R')

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
                                      "event_10" = rep(NA, num_runs),
                                      stringsAsFactors = FALSE)
                                      
# Setup data.frame to receive event quick level data
event_record_quicklevels <- event_record_baselevels
                                      
# Setup data.frame to receive event duration data
event_record_durations <- event_record_baselevels

# Record where there are less than 10 peak events:
no.pks <- data.frame("gauge" = character(), "peaks" = numeric())

# -----------------

# Set up a function to find peak fiver levels:
find_peaks <- function (y, m = 672, n = 10){

  # A 'peak' is defined as a local maxima with m points either side of it being smaller than it. 
  # Hence, the bigger the parameter m, the more stringent is the peak funding procedure.
  # m is the minimum number of time units either side of a peak where smaller river levels are requires.
  # i.e. when m = 672, there are no larger peaks with in 7 days (15 mins*672) either side of that peak.
  # n is the number of peaks to find.
  
  peaks = rep(NA,n)
  for(i in 1:length(peaks)){
    peaks[i] = which.max(y)
    y[(peaks[i]-m) : (peaks[i]+m)] = NA
  }
  return(peaks)
}

  
#   date <- y$Date
#   val  <- y$Flow
#   lgth <- length(val)
#   
#   shape <- diff(sign(diff(val, na.pad = FALSE)))
#   
#   pks <- sapply(which(shape < 0), FUN = function(i){
#     z <- i - m + 1
#     z <- ifelse(z > 0, z, 1)
#     w <- i + m + 1
#     w <- ifelse(w < lgth, w, lgth)
#     if(all(val[c(z : i, (i + 2) : w)] <= val[i + 1], na.rm = T)) # I have added the ", na.rm = T"
#       return(i + 1) else return(numeric(0)) 
#   })
#   
#   pks <- unlist(pks)
#   
#   # Only use those peaks that are above threshold:
#   pks <- pks[which(val[pks] >= quantile(val, na.rm = T, probs = (0.98)))]
#   
#   # Above function find peaks. m is equal to 1 week, so any duplicates should be due to equal values.
#   # To remove these duplicates:
#   remove <- which(diff(pks) <= m)
#   if (length(remove)>0){pks = pks[-remove]}
#   
#   # Now we need to select the top 10 events: 
#   df <- data.frame(index = c(1:length(pks)), value = val[pks])
#   df <- df[order(-df$value),]
#   df <- df[c(1:10),]
#   pks <- pks[df$index]
#   
#   return(pks)
# }

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

# Prepare the PDF file for plotting events
# pdf(file=file.path("Gauge 44002 - peak hydrographs.pdf"), width = 11.69, height = 8.27)
# layout(matrix(c(1:9), nrow = 3, byrow =TRUE))

# Setup the progress bar for main function
pb <- txtProgressBar(min = 1, max = num_runs, style = 3)

# Setup some time periods for use later:
four_days = 4*24*4 # 15 minutes > hours > days > 4 days
ten_days = 4*24*10 # 15 minutes > hours > days > 10 days
twelve_hours = 4*12

# files_in_directory = list.files(path = "./dump_qc", pattern = ".csv")

# --------------------------------------------------------------------
# --------------------------------------------------------------------
# The Main Function:
# --------------------------------------------------------------------
# --------------------------------------------------------------------



# Initiate for-loop that will run through each record:
#for(x in c(1:19, 21:num_runs){
for(x in (1:num_runs)){
  
  # ----------------------------------
  # >> Section 1 - Prepare the data <<
  # ----------------------------------
  
  gauge_name = data_to_use$nrfa_id[x]
  
  # if(length(which(files_in_directory == paste0(gauge_name, ".csv")))>0){
  
  gauge_data = read.csv(paste0("../Data/River Flow Data/", data_to_use$files[x]),
                  colClasses = c("character", "NULL", "numeric", "NULL"),
                  skip = 8, na.strings = "-9999.0000")
  
  # Convert characters dates into POSTIXct:
  gauge_data$Date <- as.POSIXct(gauge_data$Date, format="%Y-%m-%d %H:%M", tz = "UTC")
  
  # Save the  record to the workspace:
  # assign(x = paste0("tbl_", gauge_name), value = gauge_data)
  
  # --------------------------------------------------------------------------
  # >> Section 2 - Base Levels <<
  #
  # Calculate base level indexes for each gauge. Display these in a table.
  # This simply produces the lowflow objects (calculates base level).
  # --------------------------------------------------------------------------

  #---------------------------------------
  # Calculate Base levels using WMO Method
  #---------------------------------------
  
  # -----------------------
  # Create daily data
  # -----------------------
  
  gauge_data_daily <- xts(x = gauge_data$Flow, order.by = gauge_data$Date, tzone = "GMT")
  gauge_data_daily <- apply.daily(x = gauge_data_daily, FUN = mean)
  
  gauge_data_daily <- data.frame("date"  = index(gauge_data_daily), "value" = coredata(gauge_data_daily))
  gauge_data_daily <- data.frame("flow"  = gauge_data_daily$value,
                                 "date" = unique(as.Date(gauge_data)),
                                 "day"   = as.numeric(substr(gauge_data_daily$date,9,10)),
                                 "month" = as.numeric(substr(gauge_data_daily$date,6,7)),
                                 "year"  = as.numeric(substr(gauge_data_daily$date,1,4)),
                                 stringsAsFactors = F) 
  
  #---------------------------------------
  
  # Patch any 'trivial' missing days:
  
  gauge_data_daily$flow <- na.approx(gauge_data_daily$flow, maxgap = 1, na.rm = F)
  
  #---------------------------------------
  
  # Remove any long gaps:
  
  # .....
  
  #---------------------------------------
  
  # Create low flow object for analysis
  # g_lfo <- suppressWarnings(createlfobj(x = gauge_data,
  #                                       baseflow = TRUE,
  #                                       hyearstart = 1)) # can change start of hydrological year (1-12)
  # 
  # g_lfo <- data.frame("date"    = as.POSIXct(paste(g_lfo$year, g_lfo$month, g_lfo$day, sep = "-"),
  #                                                format="%Y-%m-%d", tz = "UTC"),
  #                         "value"   = round(g_lfo$flow, digits=3),
  #                         "bl_wmo"  = round(g_lfo$baseflow, digits = 3))
  # 
  # rm(g_lfo)
  
  # ------------------------------------
  # Format and Cache the Low Flow Object:
  # ------------------------------------
  
  # # Trim the NA's from the ends:
  # NA_Bls <- which(!is.na(temp_data$value))
  # if (any(is.na(NA_Bls))) {temp_data <- temp_data[c(min(NA_Bls): max(NA_Bls)),]}
  # 
  # # Remove any BLs when there are not river level values: (I think)
  # temp_data$bl_wmo[is.na(temp_data$value)] <- NA
  
  # gauge_data <- temp_data
  # gauge_name <- paste0("lfo_", gauge_name)
  
  # As far as I can tell from the test data, all dates are still there
  # Some Na's are now at the end of the data
  
  # Patch any 'trivial' missing days in base level data:
  # gauge_data$value  <- na.approx(gauge_data$value, maxgap = 1, na.rm = F)
  # gauge_data$bl_wmo <- na.approx(gauge_data$bl_wmo, maxgap = 1, na.rm = F)
  
  #---------------------------------------
  
  # Remove any long gaps:
  
  # ....
  
  #---------------------------------------
  
  # Save to workspace:
  # assign(gauge_name, value = gauge_data)
  
  #---------------------------------------
  
  # Calculate WMO Base Level indexes for each record:
  # 
  # missing <- sort(unique(which(is.na(gauge_data_daily$flow))))
  # bli_wmo <- sum(gauge_data$bl_wmo[-missing]) / sum(gauge_data$value[-missing])
  # 
  # base_level_indexes[x,c(1,2)] <- c(substr(gauge_name, 5, nchar(gauge_name)), round(bli_wmo, digits = 3))
  # 
  #--------------------------------------
  # Calculate Lyne and Hollic Base Level:
  #--------------------------------------
  
  # Calculate:
  lyne_hollick = BFI(Q = gauge_data_daily$flow, alpha=0.925, ReturnQbase=TRUE, passes=3)
  
  # Add to the daily data:
  gauge_data_daily$base_flow <- round(lyne_hollick$Qbase, digits = 3)
  
  # # Add BLI to table:
  # base_level_indexes[x,3] = round(lyne_hollick$BFI, digits = 3)
  # 
  # assign(gauge_name, value = gauge_data)
  
  # --------------------------- 
  # Save Base Levels:  
  # ---------------------------
  
  # filepath <- paste("Low Flow Objects/", gauge_name,'.Rdata',sep = "") 
  # save(list = gauge_name, file = filepath)

  
  # --------------------------------
  # >> Section 3 - Part 1 <<
  # --------------------------------
  
  # Calculate the greatest river RISE i.e. from lowest point to highest:
  # Note - Make this section an apply function - it is very slow!
  
  # gauge_data <- setNames(lapply(ls(pattern="tbl"), function(x) get(x)), ls(pattern ="tbl"))
  # gauge_name <- ls(gauge_data[1])
  # gauge_data <- gauge_data[[1]]
  
  # --------------------
  # Calculate the rises:
  # --------------------
  
  gauge_data$rise = rise_fun(gauge_data$Flow)
  # 
  # assign(x = gauge_name, value = gauge_data)
  # 
  
  # -------------------------------------------------
  # Calculate percentages of rises:
  # (above 4/8, 5/8 & 6/8 of river level quantiles)
  # -------------------------------------------------
  # 
  # # Count number of non NA values:
  # tot = length(which(!is.na(gauge_data$Flow)))
  # 
  # Qt.ft <- (quantile(gauge_data$Flow, probs = 0.50, na.rm = T) - 
  #             quantile(gauge_data$Flow, probs = 0.05, na.rm = T))/4
  # Qt.st <- (quantile(gauge_data$Flow, probs = 0.62, na.rm = T) - 
  #             quantile(gauge_data$Flow, probs = 0.05, na.rm = T))/4
  # Qt.svt <- (quantile(gauge_data$Flow, probs = 0.75, na.rm = T) - 
  #              quantile(gauge_data$Flow, probs = 0.05, na.rm = T))/4
  # 
  # Qt.ft = length(which(gauge_data$rise > Qt.ft)) * 100/tot
  # Qt.st = length(which(gauge_data$rise > Qt.st)) * 100/tot
  # Qt.svt = length(which(gauge_data$rise > Qt.svt)) * 100/tot
  # 
  # prop_rise[x,] <- c(substr(gauge_name, 4, nchar(gauge_name)),
  #                    round(Qt.ft, digits = 2),
  #                    round(Qt.st, digits = 2),
  #                    round(Qt.svt, digits = 2))
  

  
  #-----------------------------------------------------
  # >> Section 3 - PART 2 <<
  # -----------------------------------------------------
  # Remove base level from the total River Level to leave
  # behind only surface water inputs.
  # -----------------------------------------------------
  
  # Add the daily base flow values to the hourly data: 
  gauge_data$base_flow <- gauge_data_daily[match(x = as.Date(gauge_data$Date), gauge_data_daily$date), "base_flow"]
  
  # Calculate the surface water component:
  gauge_data$surface_flow = gauge_data$Flow - gauge_data$base_flow
  
  # Change all negative values to 0:
  gauge_data$surface_flow[gauge_data$surface_flow<0] = 0
  
# #   # lf_data  <- setNames(lapply(ls(pattern="lfo"), function(x) get(x)),
# #   #                      ls(pattern="lfo"))
# #   # lf_name <- ls(lf_data)
# #   # lf_data <- lf_data[[1]]
# #   # 
# #   #---------------------------------------
# #   
# #   days <- as.Date(gauge_data$Date[1:5])
# #   
# #   #---------------------------------------
# #   
# #   # Group the 15 minute record into days:
# #   runs <- rle(days)
# #   
# #   #---------------------------------------
# #   
# #   # Note the end point of each day's record:
# #   consec_runs <- which(runs$lengths >= 1) 
# #   consec_ends <- cumsum(runs$lengths) 
# #   ends        <- consec_ends[consec_runs]
# #   
# #   #---------------------------------------
# #   
# #   # Determine the starting point of each day's record:
# #   newindex <- ifelse(consec_runs>1, consec_runs-1, 0)
# #   starts   <- consec_ends[newindex] + 1
# #   if (0 %in% newindex) starts <- c(1,starts)
# #   
# #   #---------------------------------------
#   
#   # Set up a blank vector to receive 'BF normalised' values:
#   normalised <- rep(NA, length(gauge_data$Date))
#   
#   
#   
#   for (j in 1:length(starts)){
#     
#     starts_j <- starts[j]
#     ends_j   <- ends[j]
#     
#     # Determine whether the days in the daily record have a calculated BL:
#     z <- which(lf_data$date == as.POSIXct(days[starts_j], tz = "UTC"))
#     if (length(z)>=1){
#       
#       # Normalise the river level is so:
#       RL <- gauge_data$Flow[c(starts_j:ends_j)]
#       BL <- lf_data$bl_wmo[z]
#       normalised[starts_j:ends_j] <- RL-BL
#       
#       # If there is not a BL value, fill the space with NA's
#     } else{
#       normalised[starts_j:ends_j] <- NA
#     }
#   }
#   
#   # Make any over normalised (negative values) equal to 0:
#   normalised[which(normalised<=0)] <- 0
#   
#   #---------------------------------------
#   
#   # Keep the normalised data:
#   gauge_data$"q_level" <- normalised
#   assign(x = gauge_name, value = gauge_data)
#   
#   #---------------------------------------
#   
#   # Save the data:
#   filepath <- paste("Interpolated Data/", gauge_name,'.Rdata',sep = "")
#   save(list = gauge_name, file = filepath)
  
  
  # ----------------------------------------------------------------------
  # >> Section 4 <<
  # Aim: Find multisource events using peak data, work out ratios for each
  # event and average them for that gauge.
  # ----------------------------------------------------------------------

  # Find the Peak Events ----------------------------------------------------

  # Find the top 10 peak events:
  index <- find_peaks(y = gauge_data)
  
  install.packages("ggpmisc")
  library(ggpmisc)
  ben = ggpmisc::find_peaks(x = gauge_data$Flow,
                      span = 1300,
                      strict = TRUE,
                      na.rm = FALSE)
  
  # ToDo - this function can be massively improved.
  
  # Calculate threshold to define starts/close of peak events:
  rl_threshold  <- quantile(gauge_data$Flow, na.rm = T, probs = (0.98))
  
  # Set up dataframe to store values ready for averaging:
  ratios = data.frame("ql" = rep(NA, 9),
                      "bl" = rep(NA, 9),
                      "duration" = rep(NA, 9))
  
  # Calculate threshold for defining the occurance of a rapid rise
  # ql_threshold <- max(gauge_data$Flow, na.rm = T)*0.02 # 2% of Qmax
  
  # Start the loop for looking at each event:
  no.events = min(length(which(!is.na(index)==T)), 9)
  for (j in 1:no.events){
    
    # Determine start of peak event:
    
    # If there is 4 days of previous data:
    if (index[j] > four_days &
        
        # And, if there are genuine measurements for the whole of that period:
        all(!is.na(gauge_data$Flow[max(c(index[j] - four_days), 0) : index[j]]) == TRUE)){
      # max(c(..., 0)) is in there so that no negative index  
      # is calculated if there are less than 4 days of data.
      
      # Set the start as 4 days before the peak:
      start = index[j] - four_days
      
      # If there is not 4 days of complete data:
    } else {
      
      # See how much data there is in the run up to the event:
      # (i.e. between this point and the peak there will be no missing data)
      run = rle(diff(which(!is.na(rev(gauge_data$Flow[1:index[j]])))))
      
      # Set the start date as that point:
      start = index[j]-run$lengths[1]
      
      # If there is no measured data before the peak, set the peak as the start:
      if(is.na(start)){start = index[j]}
    }
    
    # Determine end of peak event:
    
    # If there is 10 days of subsequent data:
    if (index[j] < (length(gauge_data$Date) - ten_days) & 
        
        # And, if there are genuine measurements for the whole of that period:
        all(!is.na(gauge_data$Flow[index[j] : min(c(index[j] + ten_days, length(gauge_data$Date)))]) == TRUE)){
      # min(c(..., lngth)) is in there so that  
      # no index outside the bounds is calculated.
      
      # Set the start as 10 days after the peak:
      end = index[j] + ten_days
      
      # If there is not 10 days of complete data:
    } else {
      
      # See how much data there is in the run up to the event:
      # (i.e. between this point and the peak there will be no missing data)
      run = rle(diff(which(!is.na(gauge_data$Flow[index[j]: (index[j]+ten_days)]))))
      
      # Set the end date as that point:
      end = index[j] + run$lengths[1]
      
      # If there is no measured data after the peak, set the peak as the end:
      if(is.na(end)){end = index[j]}
    }
    
    # Set up vector of the indexes of a peak event:
    pk <- c(start:end)
    
    # Calculate Stats for Each Peak Event -------------------------------------
    
    # Define high river levels there are:
    high_rl <- length(which(gauge_data$Flow[pk] >= rl_threshold))
    
    # -----
    
    # Calculate the area under the rising limb:
    
    # First find the duration to look at:
    range = c((index[j]-twelve_hours) : index[j])
    
    # In some instances, when peaks are at the very start of a dataset, range can call negative numbers. This must be stopped:
    range = range[range>0] 
    
    minimum = which(gauge_data$Flow[range] == min(gauge_data$Flow[range],
                                              na.rm = T))
    
    # In case there are multiples, chose the latest minimum:
    minimum = minimum[length(minimum)]
    
    # Caclulate the volume of surface water you are interested in:
    runoff = gauge_data$q_level[range[minimum] : index[j]] - gauge_data$q_level[range[minimum]]
    
    high_ql = max(runoff)/sum(runoff, na.rm = TRUE) 
        # This is not actually calculating the area as you claim, that would be:
        # max(runoff)/mean(runoff, na.rm = TRUE)*length(runoff)*15*60
        # length(runoff)*15*60 is the length of the event in seconds.
    
    # -----    
    
    # Identify the highest base level during peak event j:
    high_bl = max((gauge_data$Flow[pk] - gauge_data$q_level[pk]), na.rm = TRUE)
    
    # Calculate highest base level as a proportion of the peak river level:
    high_bl = high_bl/ max(gauge_data$Flow[index[j]], na.rm = TRUE)
    
    # Record the Event Stats --------------------------------------------------
    ratios[j,] = c(high_ql, # Instances of Rapid Rise
                   high_bl, # The highest baselevel / highest river level
                   length(pk)/96) # length of the event in days 
    
    # ratios = round(x = ratios, digits = 2)
    
    # Plot the Event ----------------------------------------------------------
    
    if(plot_event == TRUE){
      # Plot the hydrograph:
      plot(x = gauge_data$Date[pk], gauge_data$Flow[pk],
           ylim = c(0, (max(gauge_data$Flow, na.rm = TRUE)+0.2)),
           type = "l", lwd = 1,
           ylab = "Flow (cumecs)",
           xlab = paste("Event Beginning: ", gauge_data$Date[pk[1]], sep = ""),
           main = paste("Gauge ", substr(gauge_name, 5, nchar(gauge_name)), ": Event ", j, sep =""))
      
      # Add lines to define the event:
      abline(h = rl_threshold, lty = 1, col = "lightgrey")
      
      # Add the above threshold rises (surface water):
      lines(x = gauge_data$Date[pk[high_ql]], 
            y = gauge_data$Flow[pk[high_ql]],
            col = 2, type = "p", pch = 16, cex = 0.5)
      
      # Add the base level:
      lines(x= gauge_data$Date,
            y = (gauge_data$Flow - gauge_data$q_level),
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

  event_record_baselevels[x,]   = c("gauge" = gauge_name, ratios$bl)
  event_record_quicklevels[x,]  = c("gauge" = gauge_name, ratios$ql)
  event_record_durations[x,]    = c("gauge" = gauge_name, ratios$duration)
    
  source_dat[x,] = c(gauge_name,
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
  
  } 

# This will need to skip 20 as it has no data!

# Clean the Workspace
rm(bli_wmo, data_to_use, runs, BL, consec_ends, consec_runs, date, days, end, ends, ends_j, filepath, files_in_directory,four_days, gauge_data, gauge_name, high_bl, high_ql, high_rl, index, k, lf_data, lf_name, lyne_hollick, missing, NA_Bls, newindex, no.events, no.pks, normalised, num_runs, pb, prop_rise, ql_threshold, Qt.ft, Qt.st, Qt.svt, ratios, RL, run, rl_threshold, start, starts, starts_j, ten_days, tot, z)


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


