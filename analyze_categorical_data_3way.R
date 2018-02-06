# Title: Analyze Categorical Data, Three Way
# Desription: Analyze a categorical dataset using frequency tables and mosaic
#             plots with chi squared, p-values and Cramer's v to test for
#             statistical significance
# Author: James Harper, PE, ENV SP
# Date Created: February 6, 2018
# -----------------------------------------------------------------------------


###############################################################################
###############################################################################
########## CODE NOT WORKING (YET) #############################################
###############################################################################
###############################################################################

library(rio)

data = import("example_dataset_categorical.xlsx")

print("Running three-way tests...")

# Loop through selected stratifiers
for (stratifier in stratifiers) {
  
  print(paste("Running two-way tests stratified by ", names(data[stratifier]), sep = ""))
  
  # Create combinations of metrics to test for association
  metrics_2way = permutations(n = length(data) - 8, r = 2, v = 9:length(data), repeats.allowed = FALSE)
  if (var_interest[[1]] != 0) {
    temp = list()
    for (i in 1:length(var_interest)) {
      if (length(temp) == 0) {
        temp = metrics_2way[metrics_2way[,1] == var_interest[[i]],]
      } else {
        temp = rbind(temp, metrics_2way[metrics_2way[,1] == var_interest[[i]],])
      }
    }
    metrics_2way = temp
    # print(metrics_2way)
  }
  # metrics_2way = data.frame(metrics_2way)
  # metrics_2way = subset(metrics_2way, X1 != stratifier)
  # metrics_2way = subset(metrics_2way, X2 != stratifier)
  
  # Run 2-way categorical analyses stratified by this stratifier
  for (num in 1:length(metrics_2way[,1])) {
    # for (num in 1:1) {
    print(paste(metrics_2way[num,1], "_", metrics_2way[num,2], "_", stratifier))
    categorical_analysis_3way(data, metrics_2way[num,1], metrics_2way[num,2], stratifier)
  }
}



# Create temporary vectors from data
A = data[metric1][[1]]
B = data[metric2][[1]]
C = data[metric3][[1]]

# Start sending text output to dump file
file1 = file(paste(getwd(),"/Output/dump.txt", sep = ""))
sink(file1, append = TRUE)
sink(file1, append = TRUE, type = "message")

# Perform categorical analyses
freqs = table(A, B, C)                            # Create frequency table for stats
freqs_prop = prop.table(freqs, 3)
# freqs = freqs[order(-freqs[,1]),]              # Sort table by frequency in first column
# print(fisher.test(freqs))                      # Fisher Exact test
chisq_cramv = assocstats(freqs)                # Calculate chi squared and Cramer's V

# Stop sending text output to dump file
sink()
sink(type = "message")

# Create file name and plot name variables that includes first 2 p-values
p_value1 = round(chisq_cramv[[1]]$chisq_tests[2,3], digits = 3)
p_value2 = round(chisq_cramv[[2]]$chisq_tests[2,3], digits = 3)
name = paste("freqs_3way_", p_value1, "_", p_value2, "_", names(data)[[metric3]], "_", names(data)[[metric1]], 
             "_", names(data)[[metric2]], sep = "")
plot_name = paste(names(data)[[metric1]], "_", names(data)[[metric2]], "_", names(data)[[metric3]], sep = "")

# Start sending text output to text file in a given folder based on p_values
if (is.nan(p_value1) || is.nan(p_value2)) {
  
  # Create output folder
  folder = create_folder(subfolder = "p is NaN")
  
  # Start sending text output to text file in folder
  file1 = file(paste(folder, "/", name, ".txt", sep = ""))
  sink(file1, append = TRUE)
  sink(file1, append = TRUE, type = "message")
  
} else if (p_value1 > 0.05 || p_value2 > 0.05) {
  
  # Create output folder
  folder = create_folder(subfolder = "p above 0.05")
  
  # Start sending text output to text file in folder
  file1 = file(paste(folder, "/", name, ".txt", sep = ""))
  sink(file1, append = TRUE)
  sink(file1, append = TRUE, type = "message")
  
} else {
  
  # Create output folder
  folder = create_folder(subfolder = "")
  
  # Start sending text output to text file in folder
  file1 = file(paste(folder, "/", name, ".txt", sep = ""))
  sink(file1, append = TRUE)
  sink(file1, append = TRUE, type = "message")
  
}

# Add title to text file
print(paste("A = ", names(data)[[metric1]], sep = ""))
print(paste("B = ", names(data)[[metric2]], sep = ""))
print(paste("C = ", names(data)[[metric3]], sep = ""))

# Print results of analyses to text file
print(ftable(freqs_prop))
print(ftable(freqs))
print(summary(freqs))
print(chisq_cramv)

# Stop saving text output to file
sink()
sink(type = "message")

# Start saving plot to PDF in a given folder based on p_values
if (is.nan(p_value1) || is.nan(p_value2)) {
  folder = create_folder(subfolder = "p is NaN")       # Create output folder
  pdf(paste(folder, "/", name, ".pdf", sep = ""))      # Start saving plot to PDF in folder
} else if (p_value1 > 0.05 || p_value2 > 0.05) {
  folder = create_folder(subfolder = "p above 0.05")   # Create output folder
  pdf(paste(folder, "/", name, ".pdf", sep = ""))      # Start saving plot to PDF in folder
} else {
  folder = create_folder(subfolder = "")               # Create output folder
  pdf(paste(folder, "/", name, ".pdf", sep = ""))      # Start saving plot to PDF in folder
}

# Create dataframe from data
data_frame = data.frame(A, B, C)
names(data_frame) = c(names(data)[[metric1]], names(data)[[metric2]], names(data)[[metric3]])

# Generate categorical analysis plots
scpcp(data_frame, sel = "data[,3]")
mosaic(freqs, shade = TRUE, legend = TRUE, main = plot_name)          # Mosaic plot
rmb(freqs)                                                            # RMB plot
rmbmat(freqs, tv = 1)                                                 # RMB plot matrix
fluctile(freqs)                                                       # Fluctuation diagram
barplot(table(B,A), main = name, legend = colnames(freqs))            # Stacked bar plot with legend

# Stop saving plot to PDF
dev.off()
closeAllConnections()


print("One-way tests completed.")
