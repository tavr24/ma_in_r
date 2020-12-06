### File: NetworkAnalysis.R
### 
###
###
### This file includes code to perform Network Meta-Analysis.

## Set working directory
#setwd("C:/Users/Leonid/Downloads/MA12.05") # ("C:/Users/tamara.vrublevskaya/Downloads")

## Load needed libraries
library(readxl)
library(netmeta)
library(dplyr)

# Read in the excel file selecting the sheet with the data.
smd = read_excel("AlzheimersData.xlsx", sheet = "Standardized Mean Diff Final")

# Subset by whether each row belongs in the higher is better (hb) or higher is worse (hw) data frame.
hw = smd[smd$HigherWorse == 1, -8]
hb = smd[smd$HigherWorse == 0, -8]

# Create a study ID such that it follows the "Authors Year" format.
hw$Study = paste(hw$Authors, hw$Year, sep = " ")
hb$Study = paste(hb$Authors, hb$Year, sep = " ")

####  Higher Better ##########################################################
##############################################################################
# Initiate a place to store data, only it is empty for now.
metaresHB = NULL

#### Data Prep - We start with n, mean, and standard deviation for each arm of the study, 
#### but we need pairwise treatment effects and standard deviations for the network 
#### meta analysis.  In this chunk of code, we cycle through each study and obtain 
#### the pairwise treatment effects and standard deviations, and add them onto
#### the data frame metaresHB.

# Get the unique study IDs so we may cycle through them.
studies = unique(hb$ID)

# Cycle through the unique study IDs, one at a time.
for(i in 1: length(studies)){
  id = studies[i] # Get a study ID
  name = hb[hb$ID == id, "Study"][1] # Store its name
  st1 = hb[hb$ID == id, c("Treatment Type", "Treatment Dose", "N", "mean", "sd")] # Extract the data for that study ID.
  
  # If there are two arms only,
  if(nrow(st1) == 2){
    
    # Calculate pairwise treatment comparisons
    comp12 = metacont(N[1], mean[1], sd[1], N[2], mean[2], sd[2], data = st1, sm = "SMD")
    
    # Get the pairwise treatment effect
    TE = c(comp12$TE)
    # Get the standard error of the pairwise treatment effect.
    seTE = c(comp12$seTE)
    
    # Label arms appropriately
    treat1 = c(st1$`Treatment Dose`[1])
    treat2 = c(st1$`Treatment Dose`[2])
    
    # Store the results to be added on to metaresHB
    dr = data.frame(TE, seTE = round(seTE, 4), treat1, treat2, studlab = name[1,], Type = st1$`Treatment Type`[1])
    
    # If there are 3 arms:
  } else if (nrow(st1) == 3){
    
    # Calculate pairwise treatment comparisons
    comp12 = metacont(N[1], mean[1], sd[1], N[2], mean[2], sd[2], data = st1, sm = "SMD")
    comp13 = metacont(N[1], mean[1], sd[1], N[3], mean[3], sd[3], data = st1, sm = "SMD")
    comp23 = metacont(N[2], mean[2], sd[2], N[3], mean[3], sd[3], data = st1, sm = "SMD")
    
    # Get the pairwise treatment effect
    TE = c(comp12$TE, comp13$TE, comp23$TE)
    # Get the standard error of the pairwise treatment effect.
    seTE = c(comp12$seTE, comp13$seTE, comp23$seTE)
    
    # Label arms appropriately
    treat1 = c(st1$`Treatment Dose`[1], st1$`Treatment Dose`[1], st1$`Treatment Dose`[2])
    treat2 = c(st1$`Treatment Dose`[2], st1$`Treatment Dose`[3], st1$`Treatment Dose`[3])
    
    # Store the results to be added on to metaresHB
    dr = data.frame(TE, seTE = round(seTE, 4), treat1, treat2, studlab = name, Type = st1$`Treatment Type`[1])
    
    # If there are 4 arms:
  } else if (nrow(st1) == 4){
    
    # Calculate pairwise treatment comparisons
    comp12 = metacont(N[1], mean[1], sd[1], N[2], mean[2], sd[2], data = st1, sm = "SMD")
    comp13 = metacont(N[1], mean[1], sd[1], N[3], mean[3], sd[3], data = st1, sm = "SMD")
    comp23 = metacont(N[2], mean[2], sd[2], N[3], mean[3], sd[3], data = st1, sm = "SMD")
    comp14 = metacont(N[1], mean[1], sd[1], N[4], mean[4], sd[4], data = st1, sm = "SMD")
    comp24 = metacont(N[2], mean[2], sd[2], N[4], mean[4], sd[4], data = st1, sm = "SMD")
    comp34 = metacont(N[3], mean[3], sd[3], N[4], mean[4], sd[4], data = st1, sm = "SMD")
    
    # Get the pairwise treatment effect
    TE = c(comp12$TE, comp13$TE, comp23$TE, comp14$TE, comp24$TE, comp34$TE)
    # Get the standard error of the pairwise treatment effect.
    seTE = c(comp12$seTE, comp13$seTE, comp23$seTE, comp14$seTE, comp24$seTE, comp34$seTE)
    
    # Label arms appropriately
    treat1 = c(st1$`Treatment Dose`[1], st1$`Treatment Dose`[1], st1$`Treatment Dose`[2], st1$`Treatment Dose`[1], st1$`Treatment Dose`[2], st1$`Treatment Dose`[3])
    treat2 = c(st1$`Treatment Dose`[2], st1$`Treatment Dose`[3], st1$`Treatment Dose`[3], st1$`Treatment Dose`[4], st1$`Treatment Dose`[4], st1$`Treatment Dose`[4])
    
    # Store the results to be added on to metaresHB
    dr = data.frame(TE, seTE = round(seTE, 4), treat1, treat2, studlab = name[1,], Type = st1$`Treatment Type`[1])
    
    # If there are 5 arms:
  } else if (nrow(st1) == 5) {
    
    # Calculate pairwise treatment comparisons
    comp12 = metacont(N[1], mean[1], sd[1], N[2], mean[2], sd[2], data = st1, sm = "SMD")
    comp13 = metacont(N[1], mean[1], sd[1], N[3], mean[3], sd[3], data = st1, sm = "SMD")
    comp14 = metacont(N[1], mean[1], sd[1], N[4], mean[4], sd[4], data = st1, sm = "SMD")
    comp15 = metacont(N[1], mean[1], sd[1], N[5], mean[5], sd[5], data = st1, sm = "SMD")
    
    comp23 = metacont(N[2], mean[2], sd[2], N[3], mean[3], sd[3], data = st1, sm = "SMD")
    comp24 = metacont(N[2], mean[2], sd[2], N[4], mean[4], sd[4], data = st1, sm = "SMD")
    comp25 = metacont(N[2], mean[2], sd[2], N[5], mean[5], sd[5], data = st1, sm = "SMD")
    
    comp34 = metacont(N[3], mean[3], sd[3], N[4], mean[4], sd[4], data = st1, sm = "SMD")
    comp35 = metacont(N[3], mean[3], sd[3], N[5], mean[5], sd[5], data = st1, sm = "SMD")
    comp45 = metacont(N[4], mean[4], sd[4], N[5], mean[5], sd[5], data = st1, sm = "SMD")
    
    # Get the pairwise treatment effect
    TE = c(comp12$TE, comp13$TE, comp14$TE, comp15$TE, comp23$TE, comp24$TE, comp25$TE, comp34$TE, comp35$TE, comp45$TE)
    # Get the standard error of the pairwise treatment effect.
    seTE = c(comp12$seTE, comp13$seTE, comp14$seTE, comp15$seTE, comp23$seTE, comp24$seTE, comp25$seTE, comp34$seTE, comp35$seTE, comp45$seTE)
    
    # Label arms appropriately
    treat1 = c(rep(st1$`Treatment Dose`[1], 4), rep(st1$`Treatment Dose`[2], 3), st1$`Treatment Dose`[3], st1$`Treatment Dose`[3], st1$`Treatment Dose`[4])
    treat2 = c(st1$`Treatment Dose`[2], st1$`Treatment Dose`[3], st1$`Treatment Dose`[4], st1$`Treatment Dose`[5], st1$`Treatment Dose`[3], st1$`Treatment Dose`[4], 
               st1$`Treatment Dose`[5], st1$`Treatment Dose`[4], st1$`Treatment Dose`[5], st1$`Treatment Dose`[5])
    
    # Store the results to be added on to metaresHB
    dr = data.frame(TE, seTE = round(seTE, 4), treat1, treat2, studlab = name[1,], Type = st1$`Treatment Type`[1])
  }
  
  # Attach results to the data frame that stores them
  metaresHB = rbind(metaresHB, dr)
  
  
}

# Rename a column so that the netmeta function can identify it.
colnames(metaresHB)[5] = "studlab"

# Run the network meta-analysis.  Note the tolerance is increased!
hb1 = netmeta(TE, seTE, treat1, treat2, studlab, data = metaresHB, sm = "SMD", tol.multiarm = 0.1, tol.multiarm.se = 0.1)

# Fix labels for proper plotting:
treatlabsHB = hb[hb$`Treatment Dose`!= "placebo"& hb$`Treatment Dose`!="10 mg/day donepezil",c("Treatment Type", "Treatment Dose")]
treatlabsHB = rbind(treatlabsHB, c("NA", "placebo"))
treatlabsHB = rbind(treatlabsHB, c("NA", "10 mg/day donepezil"))
treatlabsHB = as.data.frame(treatlabsHB)
treatlabsHB = distinct(treatlabsHB)  ### check DONEPEZIL
rownames(treatlabsHB) = treatlabsHB$`Treatment Dose`
colnames(treatlabsHB) = c("Type", "Treatment")
hb1.trts = data.frame(Treatment = hb1$trts)
labsHB = merge(hb1.trts, treatlabsHB, by = "Treatment")
rownames(labsHB) = labsHB$Treatment

# Summarize the results of the network meta-analysis:
summary(hb1, ref = "placebo")

# Random effects forest plot:
jpeg('V3_HigherBetter_NetworkMetaAnalysis_ForestPlot_RandomEffects.jpg', width = 18, height = 8, units = "in", res = 300)
f1 = forest(hb1, ref = "placebo", pooled = "random", sortvar = PScore, add.data = labsHB, leftcols = c("Type", "studlab"), col.square = "blue", rightcols = c("effect", "ci", "PScore"))
dev.off()

# Fixed effects forest plot:
jpeg('V3_HigherBetter_NetworkMetaAnalysis_ForestPlot_FixedEffects.jpg', width = 18, height = 8, units = "in", res = 300)
forest(hb1, ref = "placebo", pooled = "fixed", sortvar = PScore, add.data = labsHB, leftcols = c("Type", "studlab"), col.square = "blue", rightcols = c("effect", "ci", "PScore"))
dev.off()

# Funnel plot:
jpeg('V3_HigherBetter_NetworkMetaAnalysis_Funnel.jpg', width = 26, height = 14, units = "in", res = 300)
fl.hb = funnel.netmeta(hb1, order = f1$labels, pch = c(rep(c(15:18, 1), 8), 15, 16), col = 1:3)
dev.off()

# Tests for Bias:
metabias(metagen(TE.adj, seTE, data = fl.hb))
metabias(metagen(TE.adj, seTE, data = fl.hb), method = "mm")

####  Higher Worse ###########################################################
##############################################################################
# Initiate a place to store data, only it is empty for now.
metaresHW = NULL

#### Data Prep - We start with n, mean, and standard deviation for each arm of the study, 
#### but we need pairwise treatment effects and standard deviations for the network 
#### meta analysis.  In this chunk of code, we cycle through each study and obtain 
#### the pairwise treatment effects and standard deviations, and add them onto
#### the data frame metaresHW.  The process is the same as above, except that
#### we are working with outcomes using scales such that the higher scores
#### indicate poor cognitive function.

# Get the unique study IDs so we may cycle through them.
studiesW = unique(hw$ID)

# Cycle through the unique study IDs, one at a time.
for(i in 1: length(studiesW)){
  id = studiesW[i] # Select a study
  name = hw[hw$ID == id, "Study"][1] # Store its name
  st1 = hw[hw$ID == id, c("Treatment Type", "Treatment Dose", "N", "mean", "sd")] # Extract data for that study
  
  # If there are two arms:
  if(nrow(st1) == 2){
    
    # Calculate pairwise treatment comparisons
    comp12 = metacont(N[1], mean[1], sd[1], N[2], mean[2], sd[2], data = st1, sm = "SMD")
    
    # Get the pairwise treatment effect
    TE = c(comp12$TE)
    # Get the standard error of the pairwise treatment effect.
    seTE = c(comp12$seTE)
    
    # Label arms appropriately
    treat1 = c(st1$`Treatment Dose`[1])
    treat2 = c(st1$`Treatment Dose`[2])
    
    # Store the results to be added on to metaresHW
    dr = data.frame(TE, seTE = round(seTE, 4), treat1, treat2, studlab = name[1,], Type = st1$`Treatment Type`[1])
    
    # If there are 3 arms:
  } else if (nrow(st1) == 3){
    
    # Calculate pairwise treatment comparisons
    comp12 = metacont(N[1], mean[1], sd[1], N[2], mean[2], sd[2], data = st1, sm = "SMD")
    comp13 = metacont(N[1], mean[1], sd[1], N[3], mean[3], sd[3], data = st1, sm = "SMD")
    comp23 = metacont(N[2], mean[2], sd[2], N[3], mean[3], sd[3], data = st1, sm = "SMD")
    
    # Get the pairwise treatment effect
    TE = c(comp12$TE, comp13$TE, comp23$TE)
    # Get the standard error of the pairwise treatment effect.
    seTE = c(comp12$seTE, comp13$seTE, comp23$seTE)
    
    # Label arms appropriately
    treat1 = c(st1$`Treatment Dose`[1], st1$`Treatment Dose`[1], st1$`Treatment Dose`[2])
    treat2 = c(st1$`Treatment Dose`[2], st1$`Treatment Dose`[3], st1$`Treatment Dose`[3])
    
    # Store the results to be added on to metaresHW
    dr = data.frame(TE, seTE = round(seTE, 4), treat1, treat2, studlab = name, Type = st1$`Treatment Type`[1])
    
    # If there are 4 arms:
  } else if (nrow(st1) == 4){
    
    # Calculate pairwise treatment comparisons
    comp12 = metacont(N[1], mean[1], sd[1], N[2], mean[2], sd[2], data = st1, sm = "SMD")
    comp13 = metacont(N[1], mean[1], sd[1], N[3], mean[3], sd[3], data = st1, sm = "SMD")
    comp23 = metacont(N[2], mean[2], sd[2], N[3], mean[3], sd[3], data = st1, sm = "SMD")
    comp14 = metacont(N[1], mean[1], sd[1], N[4], mean[4], sd[4], data = st1, sm = "SMD")
    comp24 = metacont(N[2], mean[2], sd[2], N[4], mean[4], sd[4], data = st1, sm = "SMD")
    comp34 = metacont(N[3], mean[3], sd[3], N[4], mean[4], sd[4], data = st1, sm = "SMD")
    
    # Get the pairwise treatment effect
    TE = c(comp12$TE, comp13$TE, comp23$TE, comp14$TE, comp24$TE, comp34$TE)
    # Get the standard error of the pairwise treatment effect.
    seTE = c(comp12$seTE, comp13$seTE, comp23$seTE, comp14$seTE, comp24$seTE, comp34$seTE)
    
    # Label arms appropriately
    treat1 = c(st1$`Treatment Dose`[1], st1$`Treatment Dose`[1], st1$`Treatment Dose`[2], st1$`Treatment Dose`[1], st1$`Treatment Dose`[2], st1$`Treatment Dose`[3])
    treat2 = c(st1$`Treatment Dose`[2], st1$`Treatment Dose`[3], st1$`Treatment Dose`[3], st1$`Treatment Dose`[4], st1$`Treatment Dose`[4], st1$`Treatment Dose`[4])
    
    # Store the results to be added on to metaresHW
    dr = data.frame(TE, seTE = round(seTE, 4), treat1, treat2, studlab = name[1,], Type = st1$`Treatment Type`[1])
    
    # If there are 5 arms:
  } else if (nrow(st1) == 5) {
    
    # Calculate pairwise treatment comparisons
    comp12 = metacont(N[1], mean[1], sd[1], N[2], mean[2], sd[2], data = st1, sm = "SMD")
    comp13 = metacont(N[1], mean[1], sd[1], N[3], mean[3], sd[3], data = st1, sm = "SMD")
    comp14 = metacont(N[1], mean[1], sd[1], N[4], mean[4], sd[4], data = st1, sm = "SMD")
    comp15 = metacont(N[1], mean[1], sd[1], N[5], mean[5], sd[5], data = st1, sm = "SMD")
    
    comp23 = metacont(N[2], mean[2], sd[2], N[3], mean[3], sd[3], data = st1, sm = "SMD")
    comp24 = metacont(N[2], mean[2], sd[2], N[4], mean[4], sd[4], data = st1, sm = "SMD")
    comp25 = metacont(N[2], mean[2], sd[2], N[5], mean[5], sd[5], data = st1, sm = "SMD")
    
    comp34 = metacont(N[3], mean[3], sd[3], N[4], mean[4], sd[4], data = st1, sm = "SMD")
    comp35 = metacont(N[3], mean[3], sd[3], N[5], mean[5], sd[5], data = st1, sm = "SMD")
    comp45 = metacont(N[4], mean[4], sd[4], N[5], mean[5], sd[5], data = st1, sm = "SMD")
    
    # Get the pairwise treatment effect
    TE = c(comp12$TE, comp13$TE, comp14$TE, comp15$TE, comp23$TE, comp24$TE, comp25$TE, comp34$TE, comp35$TE, comp45$TE)
    # Get the standard error of the pairwise treatment effect.
    seTE = c(comp12$seTE, comp13$seTE, comp14$seTE, comp15$seTE, comp23$seTE, comp24$seTE, comp25$seTE, comp34$seTE, comp35$seTE, comp45$seTE)
    
    # Label arms appropriately
    treat1 = c(rep(st1$`Treatment Dose`[1], 4), rep(st1$`Treatment Dose`[2], 3), st1$`Treatment Dose`[3], st1$`Treatment Dose`[3], st1$`Treatment Dose`[4])
    treat2 = c(st1$`Treatment Dose`[2], st1$`Treatment Dose`[3], st1$`Treatment Dose`[4], st1$`Treatment Dose`[5], st1$`Treatment Dose`[3], st1$`Treatment Dose`[4], 
               st1$`Treatment Dose`[5], st1$`Treatment Dose`[4], st1$`Treatment Dose`[5], st1$`Treatment Dose`[5])
    
    # Store the results to be added on to metaresHW
    dr = data.frame(TE, seTE = round(seTE, 4), treat1, treat2, studlab = name[1,], Type = st1$`Treatment Type`[1])
  }
  
  # Attach results to the data frame that stores them
  metaresHW = rbind(metaresHW, dr)
  
  
}

colnames(metaresHW)[5] = "studlab" # relabel column.

# Fit the network meta-analysis
hw1 = netmeta(TE, seTE, treat1, treat2, studlab, data = metaresHW, sm = "SMD", tol.multiarm = 0.1, tol.multiarm.se = 0.1)

# Set up appropriate labels:
treatlabsHW = hw[hw$`Treatment Dose`!= "placebo" & hw$`Treatment Dose`!="10 mg/day donepezil",c("Treatment Type", "Treatment Dose")]
treatlabsHW = rbind(treatlabsHW, c("NA", "placebo"))
treatlabsHW = rbind(treatlabsHW, c("NA", "10 mg/day donepezil"))
treatlabsHW = as.data.frame(treatlabsHW)
treatlabsHW = distinct(treatlabsHW)  ### check DONEPEZIL
rownames(treatlabsHW) = treatlabsHW$`Treatment Dose`
colnames(treatlabsHW) = c("Type", "Treatment")
hw1.trts = data.frame(Treatment = hw1$trts)
labsHW = merge(hw1.trts, treatlabsHW, by = "Treatment")
rownames(labsHW) = labsHW$Treatment

# Print a summary of the model:
summary(hw1, ref = "placebo")

# Output a forest plot of the random effects model:
jpeg('V3_HigherWorse_NetworkMetaAnalysis_ForestPlot_RandomEffects.jpg', width = 18, height = 8, units = "in", res = 300)
f2 = forest(hw1, ref = "placebo", pooled = "random", sortvar = PScore, add.data = labsHW, leftcols = c("Type", "studlab"), col.square = "blue", rightcols = c("effect", "ci", "PScore"))
dev.off()

# Output a forest plot of the fixed effects model:
jpeg('V3_HigherWorse_NetworkMetaAnalysis_ForestPlot_FixedEffects.jpg', width = 18, height = 8, units = "in", res = 300)
forest(hw1, ref = "placebo", pooled = "fixed", sortvar = PScore, add.data = labsHW, leftcols = c("Type", "studlab"), col.square = "blue", rightcols = c("effect", "ci", "PScore"))
dev.off()

# Funnel plot:
jpeg('V3_HigherWorse_NetworkMetaAnalysis_Funnel.jpg', width = 26, height = 14, units = "in", res = 300)
fl.hw = funnel.netmeta(hw1, order = f2$labels, pch = c(rep(c(15:18, 1), 9), 15, 16, 17, 18), col = 1:49)
dev.off()

# Run tests of bias:
metabias(metagen(TE.adj, seTE, data = fl.hw))
metabias(metagen(TE.adj, seTE, data = fl.hw), method = "mm")
