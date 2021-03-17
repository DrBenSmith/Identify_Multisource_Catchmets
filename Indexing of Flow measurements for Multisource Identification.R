# Create Multisource Indexes from Flow Records
# 20/02/2021

# This script will run through all of the records, individually analysing all records.

# -----------------

# Choose whether to plot the events
plot_event = FALSE

# -----------------

# Create a list of the records to analyse:
  # DODO: Does this need to be QCed? Is the QC data on your laptop?
# data_to_use <- read.csv("stations_qc_dump.csv", 
#                         stringsAsFactors = F,
#                         sep = ",", skip = 4)

data_to_use <- data.frame("files" = c(list.files("../Data/NRFA River Flow Data/",full.names = TRUE),
                                      list.files("../Data/EA_Data_Request/",full.names = TRUE)),
                          "gauge_id" = c(list.files("../Data/NRFA River Flow Data/",full.names = FALSE),
                                         list.files("../Data/EA_Data_Request/",full.names = FALSE)))

data_to_use$gauge_id = substr(x = data_to_use$gauge_id, start = 1, stop = nchar(data_to_use$gauge_id)-4)

# Load all required packages:
# install.packages("zoo")
# install.packages("xts")
# install.packages("lfstat")
# install.packages("foreign")
# install.packages("plotly")
# install.packages(openxlsx)

library(zoo)
library(xts)
library(lfstat)
library(foreign)
library(plotly)
library(openxlsx)

# -----------------

# Set the number of records to run through:
num_runs <- length(data_to_use$gauge_id)

# -----------------

# Import the funciton for calculating base level:
source('https://raw.github.com/TonyLadson/BaseflowSeparation_LyneHollick/master/BFI.R')

# Setup data.frame to receive averaged source data
source_dat <- data.frame("gauge"   = character(length = num_runs),
                         "SWI" = rep(NA, num_runs),
                         "GWI" = rep(NA, num_runs),
                         stringsAsFactors = FALSE)

# Setup data.frame to receive event base level data
event_gwi <- as.data.frame(matrix(data = NA, nrow = num_runs, ncol = 10))
rownames(event_gwi) = data_to_use$gauge_id
colnames(event_gwi) <- paste0("event_", 1:10)
                                      
# Setup data.frame to receive event quick level data
event_swi <- event_gwi
                                      
# Setup data.frame to receive event duration data and record lengths
event_durations <- event_gwi
event_ARI <- event_gwi

# Setup data.frame to receive event duration data
event_no <- event_gwi

# Record where there are less than 10 peak events:
no.pks <- data.frame("gauge" = character(), "peaks" = numeric())

# -----------------

# Set up a function to find peak fiver levels:
find_peaks <- function (y, run_in = 672, run_out = 672, n = 100){

  # A 'peak' is defined as a local maxima with m points either side of it being smaller than it. 
  # The run in and out periods stop the function from finding overlapping events (if used correctly).
  # So, in the code below, a run in period of the pre_event and the post event is combined.
  # n is the number of peaks to find - perhaps worth having more than you need.
  # peaks are returned in descending order.
  # ERROR:
    # Error in peaks[i] <- which.max(y) : replacement has length zero
    # Probably means that all values have been set to NAs
  
  peaks = rep(NA, n)
  
  for(i in 1:length(peaks)){
    if(length(which(!is.na(y)))<(run_in+run_out)){break}
    peaks[i] = which.max(y)
    y[max(c(peaks[i]-run_in,0)) : min(c(length(y),peaks[i]+run_out))] = NA
  }
  
  return(peaks)
}

# Prepare the PDF file for plotting events
# pdf(file=file.path("Gauge 44002 - peak hydrographs.pdf"), width = 11.69, height = 8.27)
# layout(matrix(c(1:9), nrow = 3, byrow =TRUE))

# Setup the progress bar for main function
pb <- txtProgressBar(min = 1, max = num_runs, style = 3)

# Setup some time periods for use later:
pre_event = 4*24*4 # 15 minutes > hours > days > 4 days
post_event = 4*24*10 # 15 minutes > hours > days > 10 days
short_pre_event = 4*12 # 12 hours

# --------------------------------------------------------------------
# --------------------------------------------------------------------
# The Main Function:
# --------------------------------------------------------------------
# --------------------------------------------------------------------

# difficult records for testing: 351/368/584 

# Initiate for-loop that will run through each record:
for(x in (368:num_runs)){ # num_runs
  
  # ----------------------------------
  # >> Section 1 - Prepare the data <<
  # ----------------------------------
  
  gauge_name = data_to_use$gauge_id[x]
  
  print(paste0("Loop ", x, " : ", gauge_name))
  
  # There are two data sources (NRFA and EA). The EA data has more metadata rows.
  # Test which data source it is and load accordingly:
  
  if(grepl(x = data_to_use$files[x], pattern = "NRFA")){
    
    gauge_data = read.csv(data_to_use$files[x],
                          colClasses = c("character", "NULL", "numeric", "NULL"),
                          skip = 8, na.strings = "-9999.0000")
    
    if(all(is.na(gauge_data$Flow))){next}
    
    # Convert characters dates into POSTIXct:
    gauge_data$Date <- as.POSIXct(gauge_data$Date, format="%Y-%m-%d %H:%M", tz = "UTC")
    
  } else {
    
    gauge_data = read.csv(data_to_use$files[x],
                          colClasses = c("character", "numeric", "NULL", "NULL", "NULL", "NULL"),
                          skip = 20, header = TRUE, na.strings = "---")
    
    colnames(gauge_data) = c("Date", "Flow")
    
    if(all(is.na(gauge_data$Flow))){next}
    
    # Convert characters dates into POSTIXct:
    gauge_data$Date <- as.POSIXct(gauge_data$Date, format="%d/%m/%Y %H:%M:%S", tz = "UTC")
  }
  
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
                                 "date" = unique(as.Date(gauge_data$Date)),
                                 "day"   = as.numeric(substr(gauge_data_daily$date,9,10)),
                                 "month" = as.numeric(substr(gauge_data_daily$date,6,7)),
                                 "year"  = as.numeric(substr(gauge_data_daily$date,1,4)),
                                 stringsAsFactors = F) 
  
  #---------------------------------------
  
  # Patch any single missing days:
  
  gauge_data_daily$flow <- na.approx(gauge_data_daily$flow, maxgap = 1, na.rm = F)
  
  #--------------------------------------
  # Calculate Lyne and Hollic Base Level:
  #--------------------------------------
  
  # Calculate:
  lyne_hollick = BFI(Q = gauge_data_daily$flow, alpha=0.925, ReturnQbase=TRUE, passes=3)
  
  # Add to the daily data:
  gauge_data_daily$base_flow <- round(lyne_hollick$Qbase, digits = 3)
  
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

  
  # ----------------------------------------------------------------------
  # >> Section 4 <<
  # Aim: Find multisource events using peak data, work out ratios for each
  # event and average them for that gauge.
  # ----------------------------------------------------------------------

  # Find the Peak Events ----------------------------------------------------

  # Find the top 10 peak events:
  index <- find_peaks(y = gauge_data$Flow, run_in = pre_event+post_event, run_out = post_event)
  if(length(index)<10){print(paste0("WARNING - only ", length(index), " events!!!!"))}
  
  # Set up dataframe to store values ready for averaging:
  ratios = data.frame("swf" = rep(NA, 10),
                      "bf" = rep(NA, 10),
                      "duration" = rep(NA, 10),
                      "event_no" = rep(NA, 10))
  
  
  # Fill 10 events of data:
  counter = 1
  while(is.na(ratios$swf[10])){
  
    # Determine start of peak event:
    start = index[counter] - pre_event
    if(start<1){start = 1}
    
    # Determine end of peak event:
    end = index[counter] + post_event
    if(end>nrow(gauge_data)){end = nrow(gauge_data)}
    
    # Set up vector of the indexes of a peak event:
    pk <- c(start:end)
    
    # Calculate Statistics for Each Peak Event -------------------------------------
    
    # Find the index of the rising limb of interest:
    rising_limb = c((index[counter]-short_pre_event) : index[counter])
    
    # In some instances, when peaks are at the very start of a dataset, range can call negative numbers. This must be stopped:
    rising_limb = rising_limb[rising_limb>0] 
    
    # Find the index of the minimum flow in the event:
    minimum = which.min(x = gauge_data$Flow[rising_limb])
    
    # In case there are multiples, chose the latest minimum:
    minimum = max(minimum)
    
    # Calculate the volume of surface water in rising limb:
    runoff = gauge_data$surface_flow[rising_limb[minimum] : index[counter]]
    # Subtract the starting flow: 
    runoff = runoff - gauge_data$surface_flow[rising_limb[minimum]]
    
    # Skip the event if there are only NA values for base flows:
      # Base flows are required for SWI, so are a good proxy for missing data.
      # High base flows prior to the rising limb are unlikely, and so checking only this period gives the least conservative skip logic.
    if(all(is.na(runoff)) | all(is.na(gauge_data$base_flow[rising_limb[minimum] : tail(pk,1)]))){
      counter = counter+1
      next
      }
  
    # Calculate the surface water index:
    SWI = max(runoff,na.rm = TRUE) / mean(runoff, na.rm = TRUE)
    # max(runoff) / (mean(runoff, na.rm = TRUE) * length(runoff)*15*60)
    # max(runoff) / sum(runoff, na.rm = TRUE)
    # All three of these calculate similar / proportional ratios. Top is the nicest number. 
    
    # -----    
    
    # Identify the highest base flow during peak event counter:
    GWI = max(gauge_data$base_flow[pk], na.rm = TRUE)
    
    # Calculate GWI: highest base level as a proportion of the peak river level:
    GWI = GWI / max(gauge_data$Flow[index[counter]], na.rm = TRUE)
    
    # Record the Event Statistics --------------------------------------------
    ratios[min(which(is.na(ratios$swf))),] = c(SWI, # Instances of Rapid Rise
                                               GWI, # The highest base flow / highest river flow
                                               length(pk)/24, # length of the event in hrs
                                               counter)
    
    counter = counter+1
  
    # Plot the Event ----------------------------------------------------------
    
    if(plot_event == TRUE){
      # Plot the hydrograph:
      plot_ly(mode = "lines", type = "scatter", x = gauge_data$Date[pk], y = gauge_data$Flow[pk], name = "Flow") %>%
        add_trace(x = gauge_data$Date[pk], y = gauge_data$base_flow[pk], name = "Base Flow") %>%
        add_trace(x = gauge_data$Date[pk], y = gauge_data$surface_flow[pk], name = "Surface Flow") %>%
        add_lines(x = gauge_data$Date[rising_limb[1]], y = 1:max(gauge_data$Flow[pk], na.rm = TRUE),
                  name = "Surface event start")
  
    }
  }
  
  # Add Average Event Statistics to a Dataframe for Export -----------------------
  
  r = which(rownames(event_gwi)==gauge_name)
  event_gwi[r,] = ratios$bf
  event_swi[r,] = ratios$swf
  event_durations[r,] = ratios$duration
  event_no[r,] = ratios$event_no
  event_ARI[r,] = round(length(which(!is.na(gauge_data$Flow)))/(4*24*365) / ratios$event_no, 2)
  

# And Finally -------------------------------------------------------------

  # Display progress bar
  setTxtProgressBar(pb, x,)
  
}


source_dat = data.frame("GWI" = rowMeans(event_gwi, na.rm = TRUE),
                        "SWI" = rowMeans(event_swi, na.rm = TRUE))

# Clean the Workspace
# rm(bli_wmo, data_to_use, runs, BL, consec_ends, consec_runs, date, days, end, ends, ends_j, filepath, files_in_directory,pre_event, gauge_data, gauge_name, GWI, SWI, high_rl, index, k, lf_data, lf_name, lyne_hollick, missing, NA_Bls, newindex, no.events, no.pks, normalised, num_runs, pb, prop_rise, swf_threshold, Qt.ft, Qt.st, Qt.svt, ratios, RL, run, flow_threshold, start, starts, starts_j, post_event, tot, z)


# Save source_dat
# save(list = "source_dat", file = "Source Data - Flows - BL as proportion of peak RL.Rdata")
write.xlsx(x = list("Indexes" = source_dat,
                    "GWI" = event_gwi,
                    "SWI" = event_swi,
                    "Durations" = event_durations,
                    "Event ranks" = event_no),
           file = "Indexes.xlsx", rowNames = TRUE)

# Close the PDF
# dev.off()

# Close progress bar
close(pb)

# TODO
# A sensible check is to ensure that the event numbers used are sensible. You should also check the event ARIs. Check these and remove all those that are 'too low'.
# You should then also remove the low ARI events. But! You need to check whether there is correlation between event number and stats so that this does not bias the records.