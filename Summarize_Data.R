########################################################################################
#### File: Summarize_Data.R                                                         ####
####                                                                                ####
#### This file generates some basic summary graphs                                  ####
#### for studies collected for Meta-Analysis                                        ####
####                                                                                ####
########################################################################################

## Load needed libraries
library(readxl)
library(tidyverse)

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
adverse[15:125] = sapply(adverse[15:125],as.numeric)

# Group by study and treatment and generate the mean
adverseSum = adverse[,c(1, 15:126)] %>% group_by(ID, TRT) %>% summarise_all("mean")

# Select only the first row.
Type = adverse[,c("ID", "TRT", "Treatment Type")] %>% group_by(ID, TRT) %>% filter(row_number()==1)

# Join the two data frames to generate the final data set.  
adverseSum1 = inner_join(adverseSum, Type, by = c("ID", "TRT"))

# Find the most commonly reported adverse effects.
na_count <-sapply(adverseSum1[,-c(1,2)], function(y) sum(length(which(!is.na(y)))))
vars = names(sort(na_count, decreasing = T)[2:11])
gadverse = adverseSum1[,c("ID", "TRT", "Treatment Type", vars)]

# Convert the data from wide to long form so that it may be plotted using ggplot.
gadverse_long = gather(gadverse, adverseEffect, percent, `At least one adverse event`:Agitation, factor_key=TRUE)
colnames(gadverse_long)[3] = "Type"
gadverse_long$Type[gadverse_long$Type == "not-natural"] = "non-natural"
gadverse_long$Type[gadverse_long$Type == "both"] = NA

ggplot(na.omit(gadverse_long), aes(x=Type, y=percent)) + 
  geom_boxplot() + 
  theme_bw() + 
  facet_grid(TRT ~ adverseEffect) + 
  ggtitle("Adverse Effects by Natural and Non-Natural Treatments") + 
  xlab("Natural or Non-Natural") + 
  ylab("Percent of Patients")


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

# Plot.
ggplot(df1, aes(x = Type, y = counts, fill = Superior)) +
  geom_bar(stat = "identity") +
  theme_bw()

