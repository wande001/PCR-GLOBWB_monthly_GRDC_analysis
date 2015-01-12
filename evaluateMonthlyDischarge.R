# This scripts 

# clear the memory
rm(list=ls());ls()

# packages needed:
require('ggplot2'); require('RColorBrewer')

# set minimum number of pairs that will be analyzed:
minPairs = 60 # months

# functions:
#
nPairs_function <- function(obs, pred) length(pred[which(!is.na(obs) & !is.na(pred))])
#
NSeff_function <- function (Qobs, Qsim) {
    # throw away missing values
    Qsim <- Qsim[!is.na(Qobs)]
    Qobs <- Qobs[!is.na(Qobs)]
    Qobs <- Qobs[!is.na(Qsim)]
    Qsim <- Qsim[!is.na(Qsim)]
    if (length(Qobs) == 0 || length(Qsim) == 0) 
        return(NA)
    NS <- 1 - (sum((Qobs - Qsim)^2)/sum((Qobs - mean(Qobs))^2))
    return(NS)
}
#
NSeff_log_function <- function (Qobs, Qsim) {
    # avoid zero and negative discharge values
    Qobs[which(Qobs<=1)] = 1
    Qsim[which(Qsim<=1)] = 1
    # convert to become log values
    Qobs = log(Qobs)
    Qsim = log(Qsim)
    # throw away missing values
    Qsim <- Qsim[!is.na(Qobs)]
    Qobs <- Qobs[!is.na(Qobs)]
    Qobs <- Qobs[!is.na(Qsim)]
    Qsim <- Qsim[!is.na(Qsim)]
    if (length(Qobs) == 0 || length(Qsim) == 0) 
        return(NA)
    NS <- 1 - (sum((Qobs - Qsim)^2)/sum((Qobs - mean(Qobs))^2))
    return(NS)
}
#
avg_obs_function <- function(obs, sim) mean(obs[which(!is.na(obs) & !is.na(sim))]) # PS: While calculating average we consider only complete pairs.
avg_sim_function <- function(obs, sim) mean(sim[which(!is.na(obs) & !is.na(sim))]) # PS: While calculating average we consider only complete pairs.    
#
rmse_function    <- function(obs, pred) sqrt(mean((obs-pred)^2 ,na.rm=T))
 mae_function    <- function(obs, pred)      mean(abs(obs-pred),na.rm=T)
bias_function    <- function(obs, pred) mean(pred[which(!is.na(obs) & !is.na(pred))]) - mean(obs[which(!is.na(obs) & !is.na(pred))])  # POSITIVE indicates that the average prediction is higher than average observation. 
R2_function      <- function(obs, pred) summary(lm(obs ~ pred))$r.squared
R2ad_function    <- function(obs, pred) summary(lm(obs ~ pred))$adj.r.squared

# read the arguments
args <- commandArgs()
grdcFile   =  args[4]
modelFile  =  args[5]

# load the model result
modelTable = read.table(modelFile,header=F,sep=";")
modelTable[,1] = as.character(as.Date(modelTable[,1],origin="1901-01-01"))
#
simulation = data.frame(modelTable[,1], modelTable[,2])
names(simulation)[1] <- "date"
names(simulation)[2] <- "simulation"
simulation$date      = paste(substr(modelTable[,1], 1,8),"15",sep="")  #  simulation date -> assume day = 15
simulation$date      = as.Date(simulation$date,"%Y-%m-%d")

# load the GRDC data
grdcTableOriginal <- read.table(grdcFile,header=T,sep=";")
grdcTableOriginal <- grdcTableOriginal[which(grdcTableOriginal$Flag != 99),]    # ignore data with Flag = 99 (usage not recommended by the provider)
#
# alternative 1: using 'selective' calculated data
grdcTable <- grdcTableOriginal
grdcTable <- grdcTable[which(grdcTable$Calculated >= 0.0),]
grdcTable <- grdcTable[which(grdcTable$Flag < 325),]					# ignore data that are calculated with more than 25 missing daily values
grdcTable <- grdcTable[which(abs((grdcTable$Calculated-
                                  grdcTable$Original)/
                                  grdcTable$Calculated) < 500.00),]		# ignore data that have huge difference with original data
grdcAlternative1 <- data.frame(grdcTable[,1],grdcTable$Calculated)
names(grdcAlternative1)[1] <- "date"
names(grdcAlternative1)[2] <- "grdc_alternative1"
if ( length(grdcAlternative1$date) > 0) {
grdcAlternative1$date = paste(substr(grdcAlternative1$date, 1,8),"15",sep="")  #  simulation date -> assume day = 15
grdcAlternative1$date = as.Date(grdcAlternative1$date,"%Y-%m-%d") 
}
#
#
# alternative 2: using all calculated data
grdcTable <- grdcTableOriginal
grdcTable <- grdcTable[which(grdcTable$Calculated >= 0.0),]				
grdcAlternative2 <- data.frame(grdcTable[,1],grdcTable$Calculated)
names(grdcAlternative2)[1] <- "date"
names(grdcAlternative2)[2] <- "grdc_alternative2"
if ( length(grdcAlternative2$date) > 0) {
grdcAlternative2$date = paste(substr(grdcAlternative2$date, 1,8),"15",sep="")  #  simulation date -> assume day = 15
grdcAlternative2$date = as.Date(grdcAlternative2$date,"%Y-%m-%d")
}
#
#
# alternative 3: using original data
grdcTable <- grdcTableOriginal
grdcTable <- grdcTable[which(grdcTable$Original >= 0.0),]
grdcAlternative3 <- data.frame(grdcTable[,1],grdcTable$Original)
names(grdcAlternative3)[1] <- "date"
names(grdcAlternative3)[2] <- "grdc_alternative3"
if ( length(grdcAlternative3$date) > 0) {
grdcAlternative3$date = paste(substr(grdcAlternative3$date, 1,8),"15",sep="")  #  simulation date -> assume day = 15
grdcAlternative3$date = as.Date(grdcAlternative3$date,"%Y-%m-%d")
}
#
#
# merging model and all grdc alternatives
all_tables = merge(simulation,grdcAlternative1,by="date",all.x=TRUE)
all_tables = merge(all_tables,grdcAlternative2,by="date",all.x=TRUE)
all_tables = merge(all_tables,grdcAlternative3,by="date",all.x=TRUE)

print(all_tables)

# choose one grdc alternative
assessment = c(
              mae_function(all_tables$grdc_alternative1, all_tables$simulation)/(1 + cor(all_tables$grdc_alternative1, all_tables$simulation, use = "na.or.complete")),
              mae_function(all_tables$grdc_alternative2, all_tables$simulation)/(1 + cor(all_tables$grdc_alternative2, all_tables$simulation, use = "na.or.complete")),
              mae_function(all_tables$grdc_alternative3, all_tables$simulation)/(1 + cor(all_tables$grdc_alternative3, all_tables$simulation, use = "na.or.complete"))
              )
assessment[which(is.nan(assessment))] = NA
#
# check number of pairs
num_of_pairs = c(
               nPairs_function(all_tables$grdc_alternative1, all_tables$simulation),
               nPairs_function(all_tables$grdc_alternative2, all_tables$simulation),
               nPairs_function(all_tables$grdc_alternative3, all_tables$simulation)
               )
num_of_pairs[which(is.nan(num_of_pairs))] = 0.0
num_of_pairs[which( is.na(num_of_pairs))] = 0.0
assessment[which(num_of_pairs <= minPairs)] = NA

# default selection: using alternative1
mergedTable = data.frame(all_tables$date, all_tables$grdc_alternative1, all_tables$simulation)
              
if ( (assessment[1] == min(assessment, na.rm=TRUE)) & (!is.na(assessment[1])) ) {
mergedTable = data.frame(all_tables$date, all_tables$grdc_alternative1, all_tables$simulation)}
if ( (assessment[2] == min(assessment, na.rm=TRUE)) & (!is.na(assessment[2])) ) {
mergedTable = data.frame(all_tables$date, all_tables$grdc_alternative2, all_tables$simulation)}
if ( (assessment[3] == min(assessment, na.rm=TRUE)) & (!is.na(assessment[3])) ) {
mergedTable = data.frame(all_tables$date, all_tables$grdc_alternative3, all_tables$simulation)}

print(mergedTable)

names(mergedTable)[1] <- "date"
names(mergedTable)[2] <- "observation"
names(mergedTable)[3] <- "simulation"
mergedTable$observation[which(is.nan(mergedTable$observation))] = NA

# available observation data
length_observation = length(mergedTable$observation[which(!is.na(mergedTable$observation))])

if (length_observation < minPairs) {
print(paste("ONLY FEW OBSERVATION DATA ARE AVAILABLE: ",length_observation,sep=""))} else {

# evaluating model performance:
#
nPairs      =    nPairs_function(mergedTable$observation, mergedTable$simulation)
#
avg_obs     =   avg_obs_function(mergedTable$observation, mergedTable$simulation)
avg_sim     =   avg_sim_function(mergedTable$observation, mergedTable$simulation)
#
NSeff       =     NSeff_function(mergedTable$observation, mergedTable$simulation)
NSeff_log   = NSeff_log_function(mergedTable$observation, mergedTable$simulation)
rmse        =      rmse_function(mergedTable$observation, mergedTable$simulation)
mae         =       mae_function(mergedTable$observation, mergedTable$simulation)
bias        =      bias_function(mergedTable$observation, mergedTable$simulation)
R2          =        R2_function(mergedTable$observation, mergedTable$simulation)  
R2ad        =      R2ad_function(mergedTable$observation, mergedTable$simulation)
correlation =                cor(mergedTable$observation, mergedTable$simulation, use = "na.or.complete")
#
performance = c(nPairs,avg_obs,avg_sim,NSeff,NSeff_log,rmse,mae,bias,R2,R2ad,correlation)
performance_character = paste(nPairs,avg_obs,avg_sim,NSeff,NSeff_log,rmse,mae,bias,R2,R2ad,correlation,sep=";")

# saving model performance to outputFile (in the memory)
outputFile = paste(modelFile,".out",sep="")
cat("observation file: ",grdcFile,"\n",sep="",file=outputFile)
cat("nPairs;avg_obs;avg_sim;NSeff;NSeff_log;rmse;mae;bias;R2;R2ad;correlation","\n",sep="",file=outputFile,append=TRUE)
cat(performance_character,"\n",sep="",file=outputFile,append=TRUE)
write.table(mergedTable,file=outputFile,sep=";",quote=FALSE,append=TRUE,row.names=FALSE)

print(performance_character)

# read attribute information of station location
attributeStat = readLines(paste(modelFile,".atr",sep=""))

# Plotting the monthly chart !
####################################################################################################################################
#
# x and y- axis scales:
y_min = 0
y_max_obs = max(mergedTable$observation,na.rm=T)
y_max_sim = max( mergedTable$simulation,na.rm=T)
y_max = max(y_max_obs, y_max_sim)
if (y_max > 100) {y_max = ceiling((y_max+75)/100)*100} else {y_max = 100}
#
x_min = min(mergedTable$date,na.rm=T) - 365*5
x_max = max(mergedTable$date,na.rm=T)
#
x_info_text = x_min - 365*0.5

outplott <- ggplot()
outplott <- outplott +
 layer(data = mergedTable, mapping = aes(x = date, y = observation ), geom = "line", colour =  "red", size = 0.90) + # measurement
 layer(data = mergedTable, mapping = aes(x = date, y = simulation  ), geom = "line", colour = "blue", size = 0.35) + # model results
#
 geom_text(aes(x = x_info_text, y = 1.00*y_max, label = attributeStat[1]), size = 2.5,hjust = 0) +
 geom_text(aes(x = x_info_text, y = 0.95*y_max, label = attributeStat[2]), size = 2.5,hjust = 0) +
 geom_text(aes(x = x_info_text, y = 0.90*y_max, label = attributeStat[3]), size = 2.5,hjust = 0) +
 geom_text(aes(x = x_info_text, y = 0.85*y_max, label = attributeStat[4]), size = 2.5,hjust = 0) +
 geom_text(aes(x = x_info_text, y = 0.80*y_max, label = attributeStat[5]), size = 2.5,hjust = 0) +
 geom_text(aes(x = x_info_text, y = 0.75*y_max, label = attributeStat[6]), size = 2.5,hjust = 0) +
 geom_text(aes(x = x_info_text, y = 0.70*y_max, label = attributeStat[7]), size = 2.5,hjust = 0) +
 geom_text(aes(x = x_info_text, y = 0.65*y_max, label = attributeStat[8]), size = 2.5,hjust = 0) +
 geom_text(aes(x = x_info_text, y = 0.60*y_max, label = attributeStat[9]), size = 2.5,hjust = 0) +
#
 geom_text(aes(x = x_info_text, y = 0.50*y_max, label = paste(" nPairs = ",     round(performance[1] ,2),sep="")), size = 2.5,hjust = 0) +
 geom_text(aes(x = x_info_text, y = 0.45*y_max, label = paste(" avg_obs = ",    round(performance[2] ,2),sep="")), size = 2.5,hjust = 0) +
 geom_text(aes(x = x_info_text, y = 0.40*y_max, label = paste(" avg_sim = ",    round(performance[3] ,2),sep="")), size = 2.5,hjust = 0) +
 geom_text(aes(x = x_info_text, y = 0.35*y_max, label = paste(" NSeff = ",      round(performance[4] ,2),sep="")), size = 2.5,hjust = 0) +
 geom_text(aes(x = x_info_text, y = 0.30*y_max, label = paste(" NSeff_log = ",  round(performance[5] ,2),sep="")), size = 2.5,hjust = 0) +
 geom_text(aes(x = x_info_text, y = 0.25*y_max, label = paste(" rmse = ",       round(performance[6] ,2),sep="")), size = 2.5,hjust = 0) +
 geom_text(aes(x = x_info_text, y = 0.20*y_max, label = paste(" mae = ",        round(performance[7] ,2),sep="")), size = 2.5,hjust = 0) +
 geom_text(aes(x = x_info_text, y = 0.15*y_max, label = paste(" bias = ",       round(performance[8] ,2),sep="")), size = 2.5,hjust = 0) +
 geom_text(aes(x = x_info_text, y = 0.10*y_max, label = paste(" R2 = ",         round(performance[9] ,2),sep="")), size = 2.5,hjust = 0) +
 geom_text(aes(x = x_info_text, y = 0.05*y_max, label = paste(" R2ad = ",       round(performance[10],2),sep="")), size = 2.5,hjust = 0) +
 geom_text(aes(x = x_info_text, y = 0.00*y_max, label = paste(" correlation = ",round(performance[11],2),sep="")), size = 2.5,hjust = 0) +
#~ #
 scale_y_continuous("discharge",limits=c(y_min,y_max)) +
 scale_x_date('',limits=c(x_min,x_max)) +
 theme(legend.position = "none") 
#ggsave("screen.pdf", plot = outplott,width=30,height=8.25,units='cm')
 ggsave(paste(outputFile,".pdf",sep=""), plot = outplott,width=27,height=7,units='cm')
#
rm(outplott)
####################################################################################################################################

}
