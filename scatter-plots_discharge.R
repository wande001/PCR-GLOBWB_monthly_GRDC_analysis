############################################################################################
#										                                                   #  
# R-script to summarize the model performance in results in a histograms                   #
# -  created by E.H. Sutanudjaja 10 June 2014				                               #
#										                                                   #
############################################################################################

rm(list=ls()); # ls()

# folder where the analysis summaries are saved:
# - this folder contains a number of sub-folders (18 per 10 June 2014)
folder_of_analysis_summaries = "analysis_from_1960_to_2010/"

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
absolute_deviation =  abs(actual_deviation)
performance_table = cbind(performance_table, absolute_deviation, actual_deviation) 

# make sure that all values in the table have valid values:
performance_table[is.na(performance_table)] = NA

# exclude some values:
discharge = performance_table
discharge = discharge[which(discharge$model_catchment_area_in_km2 > 1),]
discharge = discharge[which(discharge$grdc_catchment_area_in_km2  > 1),]
diff_area = abs(discharge$model_catchment_area_in_km2 - discharge$grdc_catchment_area_in_km2)/discharge$grdc_catchment_area_in_km2
discharge = discharge[which(diff_area < 15.00),]
diff_area = abs(discharge$model_catchment_area_in_km2 - discharge$grdc_catchment_area_in_km2)/discharge$model_catchment_area_in_km2
discharge = discharge[which(diff_area < 15.00),]
discharge = discharge[which(discharge$average_observation > 1),]
discharge = discharge[which(discharge$average_model > 1),]
diff_flow = abs(discharge$average_model - discharge$average_observation)/discharge$average_observation
discharge = discharge[which(diff_flow < 15.00),]
diff_flow = abs(discharge$average_model - discharge$average_observation)/discharge$average_model
discharge = discharge[which(diff_flow < 15.00),]

# exclude some unrealistic stations
discharge = discharge[which(discharge$id_from_grdc != 3618001),]

plot(log(discharge$average_model,10),log(discharge$average_observation,10),xlim=c(0,5.5),ylim=c(0,5.5))
summary(lm(discharge$average_model~discharge$average_observation))

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
