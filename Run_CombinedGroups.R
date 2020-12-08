########################################################################################
#### File: Run_CombinedGroups.R                                                     ####
####                                                                                ####
#### This file performs Meta-Analysis for paired outcomes                           ####
####                                                                                ####
########################################################################################

## Load needed libraries
library(readxl)

## Load custom functions
source ("Custom_Functions.R")

## Read in the excel file selecting the sheet with the data
excel_file_name = "AlzheimersData.xlsx"
excel_sheet_name = "Combined Groups Analysis"
smd = read_excel(excel_file_name, sheet = excel_sheet_name)

########################################################################################
#### Continuous Effects Meta-Analysis for the Higher-is-Better Model                ####
########################################################################################
high_type = "higher-is-better"
do_continuous_ma(high_type, smp)


########################################################################################
#### Continuous Effects Meta-Analysis for the Higher-is-Worse Model                 ####
########################################################################################
high_type = "higher-is-worse"
do_continuous_ma(high_type, smp)
