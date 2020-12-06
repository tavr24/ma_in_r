### File: CombinedGroups.R
### 
###
###
### This file includes code to perform Meta-Analysis for paired outcomes.

## Set working directory
#setwd("C:/Users/Leonid/Downloads/MA12.05") # ("C:/Users/tamara.vrublevskaya/Downloads")

## Load needed libraries
library(readxl)
library(netmeta)
library(dplyr)
library(grid)
library(metasens)

## Import data file and select only the sheet "Combined Groups Analysis", which has the data needed.
smd = read_excel("AlzheimersData.xlsx", sheet = "Combined Groups Analysis")

## Create a study ID by pasting the authors and year of the study.
smd$Study = paste(smd$Authors, smd$Year, smd$`Combined Treatment`, sep = " ")

## Select only the columns necessary and remove rows with NA in those columns, as that excludes
## data that was used for the network analysis.
smd_c = na.omit(smd[,c("Study", "Treatment Type", "HigherWorse", "Nc", "Mc", "Sc", "Ne", "Me", "Se", "ES")])
colnames(smd_c)[2] = "Type"

## Separate by Higher Worse and Higher Better, then remove the third columns as it is no longer needed.
hw = smd_c[smd_c$HigherWorse == 1, -3] #Higher worse
hb = smd_c[smd_c$HigherWorse == 0, -3] #Higher Better

#####################################################################
## Continuous Effects Meta-Analysis for the Higher is Worse Model####
#####################################################################

# Fit both fixed and random effects models:
hw_model = metacont(Ne, Me, Se, Nc, Mc, Sc, data = hw, studlab = Study, sm = "SMD")

# Examine the results:
hw_model

# Print a summary:
print(summary(hw_model), digits = 3)

# Output a forest plot:
jpeg('V3_HigherWorse_PairedComparisons_ForestPlot.jpg', width = 18, height = 8, units = "in", res = 300)
forest(hw_model, xlab = "Standardized Mean Difference in ADAS-cog, CDR-SB, or NPI")
grid.text("Paired Outcomes Forest Plot for Scale Indicating Poorer Function at Higher Scores", .5, .9, gp=gpar(cex=1.5))
dev.off()

# Examine funnel plot for bias:
jpeg('V3_HigherWorse_PairedComparisons_FunnelPlot.jpg', width = 6, height = 8, units = "in", res = 300)
funnel(hw_model)
dev.off()

# Run rank correlation test for plot asymmetry (Begg and Mazumdar Test):
metabias(hw_model, method = "rank")

# Run linear regression test for plot asymmetry (Egger's Test):
metabias(hw_model, method = "linreg", plotit = TRUE)

# Run Thompson and Sharp test - a variant of Egger's test allowing for between-study heterogeneity
metabias(hw_model, method = "mm")

# Given the evidence of bias, run trim and fill and regression methods:
trimfill_hw_model = trimfill(hw_model) # Trim and Fill Method
print(trimfill_hw_model, digits = 2, comb.fixed = TRUE) # Print output of Trim and Fill Method
ll_hw_model = limitmeta(hw_model) # Regression Method

# View the Regression results:
print(summary(ll_hw_model), digits = 2)

# View the funnel plot after adjusting:
funnel(ll_hw_model)

# Conduct a sub-group analysis to determine whether there is a difference
# between natural and non-natural treatments:
hw_subgroup = metacont(Ne, Me, Se, Nc, Mc, Sc, data = hw, studlab = Study, sm = "SMD", byvar = Type)

# Examine output:
hw_subgroup

# Print a summary:
print(summary(hw_subgroup), digits = 3)

# Output a forest plot:
jpeg('V3_HigherWorse_PairedComparisons_SubgroupAnalysis_ForestPlot.jpg', width = 18, height = 10, units = "in", res = 300)
forest(hw_subgroup, xlab = "Standardized Mean Difference in ADAS-cog, CDR-SB, or NPI by Natural vs. Non-natural Treatment")
grid.text("Paired Outcomes Forest Plot for Scale Indicating Poorer Function at Higher Scores", .5, .9, gp=gpar(cex=1.5))
dev.off()

# Subgroup by ES:
metacont(Ne, Me, Se, Nc, Mc, Sc, data = hw, studlab = Study, sm = "SMD", byvar = ES)


#####################################################################
## Continuous Effects Meta-Analysis for the Higher is Better Model###
#####################################################################


#####################################################################

# Fit both fixed and random effects models:
hb_model = metacont(Ne, Me, Se, Nc, Mc, Sc, data = hb, studlab = Study, sm = "SMD")

# Examine the results:
hb_model

# Print a summary:
print(summary(hb_model), digits = 3)

# Output a forest plot:
jpeg('V3_HigherBetter_PairedComparisons_ForestPlot.jpg', width = 18, height = 8, units = "in", res = 300)
forest(hb_model, xlab = "Standardized Mean Difference in MMSE, ADCS-ADL, or SIB")
grid.text("Paired Outcomes Forest Plot for Scale Indicating Better Function at Higher Scores", .5, .9, gp=gpar(cex=1.5))
dev.off()

# Examine funnel plot for bias:
jpeg('V3_HigherBetter_PairedComparisons_FunnelPlot.jpg', width = 6, height = 8, units = "in", res = 300)
funnel(hb_model)
dev.off()

# Run rank correlation test for plot asymmetry (Begg and Mazumdar Test):
metabias(hb_model, method = "rank")

# Run linear regression test for plot asymmetry (Egger's Test):
metabias(hb_model, method = "linreg", plotit = TRUE)

# Run Thompson and Sharp test - a variant of Egger's test allowing for between-study heterogeneity
metabias(hb_model, method = "mm")

# Conduct a sub-group analysis to determine whether there is a difference
# between natural and non-natural treatments:
hb_subgroup = metacont(Ne, Me, Se, Nc, Mc, Sc, data = hb, studlab = Study, sm = "SMD", byvar = Type)

# Examine output:
hb_subgroup

# Print a summary:
print(summary(hb_subgroup), digits = 3)

# Output a forest plot:
jpeg('V3_HigherBetter_PairedComparisons_SubgroupAnalysis_ForestPlot.jpg', width = 18, height = 10, units = "in", res = 300)
forest(hb_subgroup, xlab = "Standardized Mean Difference in MMSE, ADCS-ADL, or SIB by Natural vs. Non-natural Treatment")
grid.text("Paired Outcomes Forest Plot for Scale Indicating Better Function at Higher Scores", .5, .9, gp=gpar(cex=1.5))
dev.off()

# group by ES
metacont(Ne, Me, Se, Nc, Mc, Sc, data = hb, studlab = Study, sm = "SMD", byvar = ES)

