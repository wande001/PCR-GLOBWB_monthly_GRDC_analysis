############################################################################################
#										                                                   #  
# R-script to summarize the model performance in results in a histograms                   #
# -  created by E.H. Sutanudjaja 10 June 2014				                               #
#										                                                   #
############################################################################################

# packages needed and clear all available existing objects:
require('ggplot2');require('RColorBrewer');require(scales)
rm(list=ls()); # ls()

# functions:

classify_values <- function(input_values, breaks) {

# make sure that "input_values" have valid values
input_values[which(is.infinite(input_values))] = NA
input_values[which(     is.nan(input_values))] = NA
input_values[which(      is.na(input_values))] = NA

# handling "breaks"
#
# - "breaks" may contain one NA value and the position of NA in breaks decide the following:
# -- if the first element of "breaks" is NA, set the values below minimum (i.e. < breaks[2]) to NA
if (is.na(breaks[1])) {input_values[which(input_values < breaks[2])] = NA}
# -- if the last element of "breaks" is NA, set the values above maximum (i.e. > breaks[length(breaks)-1]) to NA
if (is.na(breaks[length(breaks)])) {input_values[which(input_values > breaks[length(breaks)-1])] = NA}

# clustering for the non NA values
breaks_without_NA = breaks[which(!is.na(breaks))]
cluster = mat.or.vec(length(input_values),1); cluster[] = NA 
min_value = 1
max_value = length(breaks_without_NA)
for (i_cluster in min_value:max_value) { 

if (i_cluster < length(breaks_without_NA)) {

cluster[which((input_values >= breaks_without_NA[i_cluster]) & 
              (input_values <  breaks_without_NA[i_cluster+1]))] = i_cluster
} else {
cluster[which( input_values >= breaks_without_NA[i_cluster]) ]   = i_cluster
}
} # end of for

# give a separate cluster for NA values
if (is.na(breaks[1])) {cluster = cluster + 1; cluster[which(is.na(input_values))] = 1; max_value = max_value + 1} 
if (is.na(breaks[length(breaks)])) {max_value = max_value +1; cluster[which(is.na(input_values))] = max_value} 

# the output will be in factor
output_in_clusters = factor(cluster, levels = seq(min_value,max_value,1))

return(output_in_clusters) 
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# folder where the analysis summaries are saved:
# - this folder contains a number of sub-folders (18 per 10 June 2014)
folder_of_analysis_summaries = "/scratch/edwin/05_arcmin_runs/2014_11_28/multi_cores_non_natural_1960_to_2010/analysis/monthly_discharge/"

# folder and file for these output histograms
histogram_output_location    = "/scratch/edwin/05_arcmin_runs/2014_11_28/multi_cores_non_natural_1960_to_2010/analysis/monthly_discharge/non_natural_run28nov2014_"

# read all summary tables:
performance_table = read.table(paste(folder_of_analysis_summaries,"01/summary.txt",sep=""),header=T,sep=";")
for (i in 2:18) {
if (i < 10) {table_file_name = paste(folder_of_analysis_summaries,"0",as.character(i),"/summary.txt",sep="")} else {
             table_file_name = paste(folder_of_analysis_summaries,    as.character(i),"/summary.txt",sep="")} 
performance_table = rbind(performance_table,read.table(table_file_name,header=T,sep=";"))
}

# use only the data with number of pairs >= 2 years (24 months)
performance_table = performance_table[which(performance_table$num_of_month_pairs >= 24),]

# calculating simulated and observed runoff in mm per year (assume the number of day per year is 365.25)
simulated_runoff  = performance_table$average_model * 24 * 60 * 60 * 365.25 * 1000 / (performance_table$model_catchment_area_in_km2 * 10^6)
observed_runoff   = performance_table$average_observation  * 24 * 60 * 60 * 365.25 * 1000 / (performance_table$grdc_catchment_area_in_km2 * 10^6)
performance_table = cbind(performance_table, observed_runoff, simulated_runoff) 

# calculating actual deviation and absoulte deviation (see Van Beek et al., 2011)
actual_deviation   = (simulated_runoff - observed_runoff) / observed_runoff 
absolute_deviation = abs(actual_deviation)
performance_table  = cbind(performance_table, absolute_deviation, actual_deviation) 

# make sure that all values in the table have valid values:
performance_table[is.na(performance_table)] = NA

# CLASSIFICATION based on "simulated_runoff" (unit: mm/year)
#############################################################################################################################################################
performance_table$simulated_runoff[which(performance_table$simulated_runoff < 0.0)] = NA
cluster_simulated_runoff = classify_values(input_values = performance_table$simulated_runoff, breaks = c(NA, 0, 50, 100, 200, 300, 500, 750, 1000))
if (length(performance_table$cluster_simulated_runoff) > 0) {
           performance_table$cluster_simulated_runoff = cluster_simulated_runoff} else {
           performance_table = cbind(performance_table, cluster_simulated_runoff)}


# CLASSIFICATION based on "grdc_catchment_area_in_km2"
#############################################################################################################################################################
performance_table$grdc_catchment_area_in_km2[which(performance_table$grdc_catchment_area_in_km2 <= 0.0)] = NA
cluster_upstream_areas = classify_values(input_values = performance_table$grdc_catchment_area_in_km2, breaks = c(NA, 0, 5000, 10000, 25000, 50000, 75000, 100000, 250000))
if (length(performance_table$cluster_upstream_areas) > 0) {
           performance_table$cluster_upstream_areas =  cluster_upstream_areas} else {
           performance_table = cbind(performance_table,cluster_upstream_areas)}


# CLASSIFICATION based on "absolute_deviation" (unit: dimensionless)
#############################################################################################################################################################
performance_table$absolute_deviation[which(performance_table$absolute_deviation < 0.0)] = NA
cluster_abs_deviation = classify_values(input_values = performance_table$absolute_deviation, breaks = c(NA, seq(0,1,0.1)))
if (length(performance_table$cluster_abs_deviation) > 0) {
           performance_table$cluster_abs_deviation = cluster_abs_deviation} else {
           performance_table = cbind(performance_table,cluster_abs_deviation)}


# CLASSIFICATION based on "ns_efficiency" (unit: dimensionless)
#############################################################################################################################################################
performance_table$ns_efficiency[which(performance_table$ns_efficiency < -1.0)] = -1.0
cluster_ns_efficiency = classify_values(input_values = performance_table$ns_efficiency, breaks = c(seq(-1.0,1.0,0.2),NA))
if (length(performance_table$cluster_ns_efficiency) > 0) {
           performance_table$cluster_ns_efficiency = cluster_ns_efficiency} else {
           performance_table = cbind(performance_table,cluster_ns_efficiency)}


# CLASSIFICATION based on "correlation" (unit: dimensionless)
#############################################################################################################################################################
performance_table$correlation[which(performance_table$correlation < -1.0)] = -1.0
cluster_correlation = classify_values(input_values = performance_table$correlation, breaks = c(seq(-1.0,1,0.1),NA))
if (length(performance_table$cluster_correlation) > 0) {
           performance_table$cluster_correlation = cluster_correlation} else {
           performance_table = cbind(performance_table,cluster_correlation)}


histogram_performance <- ggplot(performance_table, aes( x = cluster_abs_deviation, fill = cluster_upstream_areas)) + 
                         geom_histogram(aes( y = (..count..)/sum(..count..)), binwidth = 1) + 
                         scale_fill_manual(values = (rev(brewer.pal(9,"RdYlBu"))), labels = c("NA", 0, 5000, 10000, 25000, 50000, 75000, 100000, 250000)) + 
                         scale_x_discrete(limits = seq(1,12,1), labels = c("NA", seq(0.0,1,0.1))) +
                         scale_y_continuous(limits = c(0,0.25))
output_file = paste(histogram_output_location,"histogram_abs_deviation.pdf",sep="")
ggsave(output_file, plot = histogram_performance, width = 22.5, height = 8.25,units='cm')
histogram_performance_1 <- histogram_performance

histogram_performance <- ggplot(performance_table, aes( x = cluster_ns_efficiency, fill = cluster_upstream_areas)) + 
                         geom_histogram(aes( y = (..count..)/sum(..count..)), binwidth = 1) + 
                         scale_fill_manual(values = (rev(brewer.pal(9,"RdYlBu"))), labels = c("NA", 0, 5000, 10000, 25000, 50000, 75000, 100000, 250000)) + 
                         scale_x_discrete(limits = seq(1,12,1), labels = c(seq(-1.0,1.0,0.2),"NA")) +
                         scale_y_continuous(limits = c(0,0.5))
output_file = paste(histogram_output_location,"histogram_ns_efficiency.pdf",sep="")
ggsave(output_file, plot = histogram_performance, width = 22.5, height = 8.25,units='cm')
histogram_performance_2 <- histogram_performance

histogram_performance <- ggplot(performance_table, aes( x = correlation, fill = cluster_upstream_areas)) + 
                         geom_histogram(aes( y = (..count..)/sum(..count..)), binwidth = 0.2) + 
                         scale_fill_manual(values = (rev(brewer.pal(9,"RdYlBu"))), labels = c("NA", 0, 5000, 10000, 25000, 50000, 75000, 100000, 250000)) + 
                         scale_x_continuous(limits = c(-1,1)) + 
                         scale_y_continuous(limits = c(0,0.50))
output_file = paste(histogram_output_location,"correlation.pdf",sep="")
ggsave(output_file, plot = histogram_performance, width = 22.5, height = 8.25,units='cm')
histogram_performance_3 <- histogram_performance

output_file = paste(histogram_output_location,"multi_histograms.pdf",sep="")
pdf(output_file, width = 22.5/2.54, height = 25/2.54)
multiplot(histogram_performance_1, histogram_performance_2, histogram_performance_3, cols=1)
dev.off()
