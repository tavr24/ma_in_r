########################################################################################
#### File: Run_NetworkAnalysis.R                                                    ####
####                                                                                ####
#### This file performs Network Meta-Analysis                                       ####
####                                                                                ####
########################################################################################

## Load needed libraries
library(readxl)

## Load custom functions
source ("Custom_Functions.R")

# Read in the excel file selecting the sheet with the data
excel_file_name = "AlzheimersData.xlsx"
excel_sheet_name = "Standardized Mean Diff"
smd = read_excel(excel_file_name, sheet = excel_sheet_name)

########################################################################################
#### Network Meta-Analysis for the Higher-is-Better Model                           ####
########################################################################################
high_type = "higher-is-better"
network_ma_data = make_network_ma_data(high_type, smb)
do_network_ma(high_type, network_ma_data)

########################################################################################
#### Network Meta-Analysis for the Higher-is-Worse Model                            ####
########################################################################################
high_type = "higher-is-worse"
network_ma_data = make_network_ma_data(high_type, smb)
do_network_ma(high_type, network_ma_data)

