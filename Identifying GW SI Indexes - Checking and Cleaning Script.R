# Identifying GW/SI Indexes: Checking and Cleaning Script 
# 20/03/2021

library(openxlsx)

results_indexes <- read.xlsx("Indexes_20March.xlsx", sheet = "Indexes", rowNames = TRUE)
results_GWI <- read.xlsx("Indexes_20March.xlsx", sheet = "GWI", rowNames = TRUE)
results_SWI <- read.xlsx("Indexes_20March.xlsx", sheet = "SWI", rowNames = TRUE)
results_Durations <- read.xlsx("Indexes_20March.xlsx", sheet = "Durations", rowNames = TRUE)
results_ranks <- read.xlsx("Indexes_20March.xlsx", sheet = "Event ranks", rowNames = TRUE)
results_ARIs <- read.xlsx("Indexes_20March.xlsx", sheet = "Event ARIs", rowNames = TRUE)



# Check ARIs --------------------------------------------------------------
# I do not want to use any events that are under the Qmed (ARI of 2 yrs)

check = results_ARIs
fig <- plot_ly(y = check[,2], type = "box", name = "Event 1")
fig <- fig %>% add_trace(y = check[,3], name = "Event 2")
fig <- fig %>% add_trace(y = check[,4], name = "Event 3")
fig <- fig %>% add_trace(y = check[,5], name = "Event 4")
fig <- fig %>% add_trace(y = check[,6], name = "Event 5")
fig <- fig %>% add_trace(y = check[,7], name = "Event 6")
fig <- fig %>% add_trace(y = check[,8], name = "Event 7")
fig <- fig %>% add_trace(y = check[,9], name = "Event 8")
fig <- fig %>% add_trace(y = check[,10], name = "Event 9")
fig <- fig %>% add_trace(y = check[,11], name = "Event 10")

# See what kinds of ARIs we have:
plot_ly(type = "histogram", x = as.vector(as.matrix(results_ARIs)))


# Check whether there is a correlation between the ARI and the indexes --------

# Set up plot for comparing the ARIs and indexes:
p_GWI = plot_ly(mode = "lines", type = "scatter")
p_SWI = plot_ly(mode = "lines", type = "scatter")

# Set up a dataframe for holding correlations:
correlations = data.frame(GWI_ARI = rep(NA, nrow(results_GWI)),
                          SWI_ARI = rep(NA, nrow(results_SWI)))

# Create a plot of ARI vs GWI and calculate correlations:
for(i in 1:nrow(results_GWI)){
  p_GWI = add_trace(p_GWI, mode = "lines", type = "scatter", 
                    x = as.vector(as.matrix(results_ARIs[i,])), 
                    y = as.vector(as.matrix(results_GWI[i,])),
                    name = results_indexes$GWI[i]) 
}

for(i in 1:50){#nrow(results_GWI)){
  p_SWI = add_trace(p_SWI, mode = "lines", type = "scatter", 
                    x = as.vector(as.matrix(results_ARIs[i,])), 
                    y = as.vector(as.matrix(results_SWI[i,])))
}

for(i in 1:50){#nrow(results_GWI)){
  # There must be a better way of getting the values form dataframes...
  correlations$GWI_ARI[i] = cor(as.vector(as.matrix(results_ARIs[i,])),
                                as.vector(as.matrix(results_GWI[i,])))
  correlations$SWI_ARI[i] = cor(as.vector(as.matrix(results_ARIs[i,])),
                                as.vector(as.matrix(results_SWI[i,])))
}

# Plot the ARIsvs GWIs
p_GWI # >>> This shows that some of the low ARIs (<) have very high GWI. This is expected. These events are not of interest as we are only looking at peak flows.
results_GWI[results_GWI>0.4] & results_ARIs[results_ARIs<10]


p_SWI

# Plot the correlations between ARIs and GWIs and SWIs
plot_ly(type = "scatter", mode  = "markers",
        x = results_indexes$GWI, y = correlations$GWI_ARI) %>%
  # >>> This implies that the lower GWIs are the ones where the drop of GWI with ARI is strongest. 
  add_trace(type = "scatter", mode  = "markers", data = correlations,
            x = results_indexes$SWI, y = correlations$SWI_ARI)
  

# Remove all indexes that have ARIs less than 5 yrs (i.e. Qmed: 
results_GWI_clean = results_GWI
results_SWI_clean = results_SWI

results_GWI_clean[results_ARIs <5] = NA
results_SWI_clean[results_ARIs <5] = NA

results_indexes_cleaned = data.frame(GWI = rowMeans(results_GWI_clean, na.rm = TRUE),
                                     SWI = rowMeans(results_SWI_clean, na.rm = TRUE))


# See what difference this has made to the GWIs:
plot_ly(x = results_indexes$GWI, type = "box", name = "GWI", boxpoints = FALSE) %>%
  add_trace(x = results_indexes_cleaned$GWI, name = "GWI Cleaned")

summary(results_indexes$GWI - results_indexes_cleaned$GWI)

# See what difference this has made to the SWIs:
plot_ly(x = results_indexes$SWI, type = "box", name = "SWI", boxpoints = FALSE) %>%
  add_trace(x = results_indexes_cleaned$SWI, name = "SWI Cleaned")

plot_ly(mode = "markers", type = "scatter", y = (results_indexes$SWI - results_indexes_cleaned$SWI))


# Calculate the reocrd lengths for checking that the ARIs are sensible...
# They will be... but ... even so, perhaps an interesting stat?

# Write the cleaned data --------------------------------------------------

workbook <- loadWorkbook(file = "Indexes_20March.xlsx")

addWorksheet(wb = workbook, sheetName = "results_indexes_cleaned")
writeData(workbook, results_indexes_cleaned, sheet = "results_indexes_cleaned", rowNames = TRUE)

addWorksheet(wb = workbook, sheetName = "results_GWI_cleaned")
writeData(workbook, results_GWI_clean, sheet = "results_GWI_cleaned", rowNames = TRUE)

addWorksheet(wb = workbook, sheetName = "results_SWI_cleaned")
writeData(workbook, results_SWI_clean, sheet = "results_SWI_cleaned", rowNames = TRUE)

# addWorksheet(wb = workbook, sheetName = "record lengths")
# writeData(workbook, results_SWI_clean, sheet = "results_SWI_cleaned", rowNames = TRUE)

saveWorkbook(workbook, file = "Indexes_20March.xlsx", overwrite = TRUE)



# TODO:
# See what lengths the records are and whether the dismissed events are sensible.
# Perhaps add the early and late records together for the EA data