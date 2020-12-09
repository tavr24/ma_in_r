########################################################################################
#### File: Summarize_Data.R                                                         ####
####                                                                                ####
#### This file generates some basic summary graphs                                  ####
#### for studies collected for Meta-Analysis                                        ####
####                                                                                ####
########################################################################################

## Load needed libraries
library(readxl)
library(dplyr)

excel_file_name = "AlzheimersData.xlsx"

########################################################################################
####     Adverse Effects                                                            ####
########################################################################################

# Read in the excel file selecting the sheet with the data
# i.e. the data sheet that has adverse effects percentages
excel_sheet_name = "Adverse Effects"
adverse = read_excel(excel_file_name, sheet = excel_sheet_name)

# Change all treatment labels to either placebo or treatment.
adverse$TRT = ifelse((adverse$Treatment == "Placebo - carrier" | adverse$Treatment == "Placebo" | adverse$Treatment == "Control" | adverse$Treatment == "Placebo - noncarrier"), "Placebo", "Treatment")

# Some studies included percentages as "<0.01".  Remove the "<" and just leave the upper bound.
adverse = adverse %>% mutate_all(funs(str_replace(., "<", "")))

# Change columns 15 to 125 to numeric.
adverse[15:124] = sapply(adverse[15:124],as.numeric) 

# Group by study and treatment and generate the mean
adverseSum = adverse[,c(1, 15:125)] %>% group_by(ID, TRT) %>% summarise_all("mean") 

# Select only the first row.
Type = adverse[,c("ID", "TRT", "Treatment Type")] %>% group_by(ID, TRT) %>% filter(row_number()==1)

# Join the two data frames to generate the final data set.  
adverseSum1 = inner_join(adverseSum, Type, by = c("ID", "TRT"))

# Find the most commonly reported adverse effects.
na_count <-sapply(adverseSum1[,-c(1,2)], function(y) sum(length(which(!is.na(y)))))
vars = names(sort(na_count, decreasing = T)[2:11])
gadverse = adverseSum1[,c("ID", "TRT", "Treatment Type", vars)]

colnames(gadverse)[which(names(gadverse) == "At least one adverse event")]          = "AE" 
colnames(gadverse)[which(names(gadverse) == "At least one serious adverse event")]  = "sAE"
colnames(gadverse)[which(names(gadverse) == "Fall")]                                = "F"
colnames(gadverse)[which(names(gadverse) == "Headache")]                            = "H"
colnames(gadverse)[which(names(gadverse) == "Dizziness/Vertigo")]                   = "Z"
colnames(gadverse)[which(names(gadverse) == "Nausea")]                              = "N"
colnames(gadverse)[which(names(gadverse) == "Diarrhea")]                            = "D"
colnames(gadverse)[which(names(gadverse) == "Vomiting")]                            = "V"
colnames(gadverse)[which(names(gadverse) == "Urinary Tract Infection")]             = "U"
colnames(gadverse)[which(names(gadverse) == "Agitation")]                           = "A"

# Convert the data from wide to long form so that it may be plotted using ggplot.
gadverse_long = gather(gadverse, adverseEffect, percent, AE:A, factor_key=TRUE)

colnames(gadverse_long)[3] = "Type"

gadverse_long$Type[gadverse_long$Type == "natural"] = "Y" # non-natural"
gadverse_long$Type[gadverse_long$Type == "non-natural"] = "N" # non-natural"
gadverse_long$Type[gadverse_long$Type == "not-natural"] = "N" # non-natural"
gadverse_long$Type[gadverse_long$Type == "both"] = NA

# Adverse effects by non-natural and natural treatments plot file name:
adverse_effects_by_non_natural_and_natural_treatments_jpeg_file_name = 'adverse_effects_by_non_natural_and_natural_treatments_plot.jpeg'
# Adverse effects by non-natural and natural treatments plot:
jpeg(adverse_effects_by_non_natural_and_natural_treatments_jpeg_file_name, width = 18, height = 8, units = "in", res = 300)

ggplot(na.omit(gadverse_long), aes(x=Type, y=percent)) + 
  geom_boxplot() + 
  theme_bw() + 
  facet_grid(TRT ~ adverseEffect) + 
  xlab("Non-Natural (N) vs. Natural (Y)") + 
  ylab("Percent of Patients")

dev.off()

########################################################################################
####     Data Binary                                                                ####
########################################################################################

# Read in the excel file selecting the sheet with the data
# i.e. the data sheet that only has whether the treatment was effective or not for each study.
excel_sheet_name = "Data Binary"
overall = read_excel(excel_file_name, sheet = excel_sheet_name)

# Select the data of interest.
bin = overall[,c("ID", "Treatment Type", "Treatment Superior (0 = No/Undetermined, 1 = Yes)")]
colnames(bin) = c("ID", "Type", "Superior")
bin$Type[bin$Type == "not-natural"] = "non-natural" # Fix spelling error.

# Count data.
df <- bin[,2:3] %>%
  group_by(Type, Superior) %>%
  summarise(counts = n())

# Rename levels of categorical variable.
df$Superior = ifelse(df$Superior == 0, "No", "Yes")

# Convert to factor.
df$Superior = as.factor(df$Superior)

# Remove "both" (leaving "natural" and "non-natural")
df1 = df[df[, "Type"] != "both",]

# Overall effectiveness natural and non-natural treatments plot file name:
overall_effectiveness_natural_and_non_natural_treatments_jpeg_file_name = 'overall_effectiveness_natural_and_non_natural_treatments_plot.jpeg'
# Overall effectiveness natural and non-natural treatments plot:
jpeg(overall_effectiveness_natural_and_non_natural_treatments_jpeg_file_name, width = 18, height = 8, units = "in", res = 300)

ggplot(df1, aes(x = Type, y = counts, fill = Superior)) +
  geom_bar(stat = "identity") +
  theme_bw()

dev.off()

