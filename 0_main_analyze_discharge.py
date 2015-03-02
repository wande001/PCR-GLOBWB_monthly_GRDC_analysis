#!/usr/bin/env python
# -*- coding: utf-8 -*-

# This is script for evaluating monthly simulated discharge to GRDC discharge data.

import os
import sys
import glob

import dischargeGRDC

import logging
from logger import Logger
# get name for the logger
logger = logging.getLogger("main_script")

# PCR-GLOBWB results: model output directory, 
pcrglobwb_output = {}
pcrglobwb_output["folder"]               = "/home/niko/Routing/" # "/scratch/edwin/05_arcmin_runs/2014_11_28/multi_cores_non_natural_1960_to_2010/global/" # "/scratch/edwin/IWMI_run_20_nov/without_fossil_limit_with_pumping_limit_CRU/netcdf/"
pcrglobwb_output["netcdf_file_name"]     = "discharge_dailyTot_output_Flood.nc" 
pcrglobwb_output["netcdf_variable_name"] = "discharge" 

# output directory storing analysis results (results from this script)
globalAnalysisOutputDir = "/home/niko/Routing/Flood/analysis/daily_discharge/" # "/scratch/edwin/IWMI_run_20_nov/without_fossil_limit_with_pumping_limit_CRU/analysis/monthly_discharge/"
cleanOutputDir          = True # option to clean analysisOutputDir 		

# optional: PCR-GLOBWB output and analysis output folders are given in the system argument
#~ if len(sys.argv) > 1:
    #~ pcrglobwb_output["folder"] = str(sys.argv[1])
    #~ globalAnalysisOutputDir    = str(sys.argv[2])

# time range for analyses
startDate = "1980-01-01" #YYYY-MM-DD # None 
endDate   = "2009-12-31" #YYYY-MM-DD # None 

# directory for GRDC files:
#globalDirectoryGRDC = "/projects/dfguu/data/hydroworld/basedata/hydrography/GRDC/requested_by_edwin_23_april_2014/grdc_monthly_data_splitted/" # "/scratch/edwin/observation_data/grdc_monthly_data_splitted/"
globalDirectoryGRDC = "/projects/dfguu/data/hydroworld/basedata/hydrography/GRDC/requested_by_edwin_23_april_2014/grdc_daily_data_splitted/" # "/scratch/edwin/observation_data/grdc_monthly_data_splitted/"
# This directory will contain several sub-folders (18 sub-folders per 10 Jun 2014)

#~ # for 5min results of PCR-GLOBWB 2.0
#globalCloneMapFileName = "/projects/dfguu/data/hydroworld/PCRGLOBWB20/input5min/routing/lddsound_05min.map"        # "/data/hydroworld/PCRGLOBWB20/input5min/routing/lddsound_05min.map"
#lddMapFileName         = "/projects/dfguu/data/hydroworld/PCRGLOBWB20/input5min/routing/lddsound_05min.map"        # "/data/hydroworld/PCRGLOBWB20/input5min/routing/lddsound_05min.map"
#cellAreaMapFileName    = "/projects/dfguu/data/hydroworld/PCRGLOBWB20/input5min/routing/cellsize05min.correct.map" # "/data/hydroworld/PCRGLOBWB20/input5min/routing/cellsize05min.correct.map"

#~ # for 30min results (of PCR-GLOBWB 2.0)
globalCloneMapFileName = "/projects/dfguu/data/hydroworld/PCRGLOBWB20/input30min/global/Global_CloneMap_30min.map"
lddMapFileName         = "/projects/dfguu/data/hydroworld/PCRGLOBWB20/input30min/routing/lddsound_30min.map"
cellAreaMapFileName    = "/projects/dfguu/data/hydroworld/PCRGLOBWB20/input30min/routing/cellarea30min.map"

#~ # for WATCH 30min clone map
#~ globalCloneMapFileName = "/data/hydroworld/PCRGLOBWB20/input30min/global/Global_CloneMap_30min.map"
#~ lddMapFileName         = "/home/beek0120/PCR-GLOBWB/WaterTemperature/Watch/GRanD/lddsound_wfd_30min.map"
#~ cellAreaMapFileName    = "/scratch/edwin/tmp/cellarea_wfd_30min/cellarea_wfd_30min.map"

#~ # for PCR-GLOBWB 1.0, we use the maps from Rens's directory:
#~ globalCloneMapFileName = "/home/beek0120/PCR-GLOBWB/PCR-GLOBWB_V1.0/input30min/maps/Global_Clone_30min.map"
#~ lddMapFileName         = "/home/beek0120/PCR-GLOBWB/PCR-GLOBWB_V1.0/input30min/maps/Global_LDD_30min.map"
#~ cellAreaMapFileName    = "/home/beek0120/PCR-GLOBWB/PCR-GLOBWB_V1.0/input30min/maps/Global_Cellarea_30min.map"

# the following is needed for evaluating model results with 5 arcmin resolution
catchmentClassFileName = None 

# TODO: add temporary directory - It is not a good idea to store temporary files in the memory (/dev/shm)!

def main():

    # get the argument: 
    # - if it exists, this will be the sub-folder of analysisOutputDir
    #                          and the sub-folder of directoryGRDC
    argument = sys.argv
    print(argument)
    print(argument[1])
    analysisOutputDir = globalAnalysisOutputDir+"/"+argument[1]+"/"
    directoryGRDC     = globalDirectoryGRDC+"/"+argument[1]+"/"
    
    # make analysisOutputDir
    try:
        os.makedirs(analysisOutputDir) 
    except:
        if cleanOutputDir == True: os.system('rm -r '+analysisOutputDir+"/*") 

    # logger object
    logger = Logger(analysisOutputDir)
    
    # get GRDC attributes of all stations:
    dischargeEvaluation = dischargeGRDC.DischargeEvaluation(pcrglobwb_output["folder"],\
                                                            startDate,\
                                                            endDate)
    dischargeEvaluation.get_grdc_attributes(directoryGRDC)

    # evaluate model results
    dischargeEvaluation.evaluateAllModelResults(globalCloneMapFileName,\
                                                catchmentClassFileName,\
                                                lddMapFileName,\
                                                cellAreaMapFileName,\
                                                pcrglobwb_output,\
                                                analysisOutputDir)  

if __name__ == '__main__':
    sys.exit(main())
