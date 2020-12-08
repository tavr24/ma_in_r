########################################################################################
#### File: Custom_Functions.R                                                       ####
####                                                                                ####
#### This file includes functions to perform Meta-Analysis                          ####
####                                                                                ####
########################################################################################

## Load needed libraries
library(netmeta)
library(dplyr)
library(grid)
library(metasens)

########################################################################################
####                                                                                ####
####  Function make_network_ma_data builds either higher-is-worse or                ####
####  higher-is-better data frame from the given excel data sheet                   ####
####  to run the Network Meta-Analysis on                                           ####
####                                                                                ####
####  Parameters:                                                                   ####
####      high_type - "higher-is-worse" or "higher-is-better"                       ####
####      smp        - excel data sheet                                             ####
####                                                                                ####
####  Return:                                                                       ####
####      list of data items that consists of                                       ####
####         hwb 		  high_type specific input data subset                          ####
####         TE 		  pairwise treatment effect                                     ####
####         seTE 		standard error of the pairwise treatment effect               ####
####         treat1 	arm 1                                                         ####
####         treat2 	arm 2                                                         ####
####         metaresHWB	data frame 	                                                ####
####                                                                                ####
########################################################################################

make_network_ma_data <- function(high_type, smb) {
	if ((high_type != "higher-is-worse") && (high_type != "higher-is-better")) {
		stop(cat("Invalid high type: ", high_type))
	}

	# Subset by whether each row belongs in the higher is better or higher is worse (hw) data frame.
	if (high_type == "higher-is-worse") {
		hwb = smd[smd$HigherWorse == 1, -8]
	} else { # (high_type == "higher-is-better") 
		hwb = smd[smd$HigherWorse == 0, -8]
	}

	# Create a study ID such that it follows the "Authors Year" format.
	hwb$Study = paste(hwb$Authors, hwb$Year, sep = " ")

	# Initiate a place to store data, only it is empty for now.
	metaresHWB = NULL

	#### Data Prep - We start with n, mean, and standard deviation for each arm of the study, 
	#### but we need pairwise treatment effects and standard deviations for the network 
	#### meta analysis.  In this chunk of code, we cycle through each study and obtain 
	#### the pairwise treatment effects and standard deviations, and add them onto
	#### the data frame metaresHWB.

	# Get the unique study IDs so we may cycle through them.
	studies = unique(hwb$ID)

	# Cycle through the unique study IDs, one at a time.
	for (i in 1: length(studies)) {
	  id = studies[i] # Get a study ID
	  name = hwb[hwb$ID == id, "Study"][1] # Store its name
	  st1 = hwb[hwb$ID == id, c("Treatment Type", "Treatment Dose", "N", "mean", "sd")] # Extract the data for that study ID.
	  
	  # If there are two arms only,
	  if (nrow(st1) == 2) {
  		
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
	  } else if (nrow(st1) == 3) {
		
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
  		
  		# Store the results to be added on to metaresHWB
  		dr = data.frame(TE, seTE = round(seTE, 4), treat1, treat2, studlab = name, Type = st1$`Treatment Type`[1])
  		
		# If there are 4 arms:
	  } else if (nrow(st1) == 4) {
		
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
  		
  		# Store the results to be added on to metaresHWB
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
  		
  		# Store the results to be added on to metaresHWB
  		dr = data.frame(TE, seTE = round(seTE, 4), treat1, treat2, studlab = name[1,], Type = st1$`Treatment Type`[1])
	  }
	  
    # Attach results to the data frame that stores them
    metaresHWB = rbind(metaresHWB, dr)
	}	

	return_list = list("hwb" = hwb, "TE" = TE, "seTE" = seTE, "treat1" = treat1, "treat2" = treat2, "metaresHWB" = metaresHWB)

	return (return_list)
}


########################################################################################
####                                                                                ####
####  Function do_network_ma performs Network Meta-Analysis on the given            ####
####  either higher-is-worse or higher-is-better data frame                         ####
####                                                                                ####
####  Parameters:                                                                   ####
####      high_type       - "higher-is-worse" or "higher-is-better"                 ####
####      network_ma_data - return from the make_network_ma_data function           ####
####                                                                                ####
########################################################################################

do_network_ma <- function(high_type, high_data) {
	if ((high_type != "higher-is-worse") && (high_type != "higher-is-better")) {
		stop(cat("Invalid high type: ", high_type))
	}

  hwb     = high_data$hwb
	TE 			= high_data$TE
	seTE 		= high_data$seTE
	treat1 		= high_data$treat1
	treat2 		= high_data$treat2
	metaresHWB 	= high_data$metaresHWB
	
	# Rename a column so that the netmeta function can identify it.
	colnames(metaresHWB)[5] = "studlab"

	# Run the network meta-analysis.  Note the tolerance is increased!
	hwb1 = netmeta(TE, seTE, treat1, treat2, studlab, data = metaresHWB, sm = "SMD", tol.multiarm = 0.1, tol.multiarm.se = 0.1)

	# Fix labels for proper plotting:
	treatlabsHWB = hwb[hwb$`Treatment Dose`!= "placebo"& hwb$`Treatment Dose`!="10 mg/day donepezil",c("Treatment Type", "Treatment Dose")]
	treatlabsHWB = rbind(treatlabsHWB, c("NA", "placebo"))
	treatlabsHWB = rbind(treatlabsHWB, c("NA", "10 mg/day donepezil"))
	treatlabsHWB = as.data.frame(treatlabsHWB)
	treatlabsHWB = distinct(treatlabsHWB)  ### check DONEPEZIL
	rownames(treatlabsHWB) = treatlabsHWB$`Treatment Dose`
	colnames(treatlabsHWB) = c("Type", "Treatment")
	hwb1.trts = data.frame(Treatment = hwb1$trts)
	labsHWB = merge(hwb1.trts, treatlabsHWB, by = "Treatment")
	rownames(labsHWB) = labsHWB$Treatment

	# Summarize the results of the network meta-analysis:
	summary(hwb1, ref = "placebo")

	# Random effects forest plot file name:
	random_forest_jpeg_file_name = sprintf("%s%s", high_type, '_random_effects_forest_network_ma_plot.jpeg')
	# Random effects forest plot:
	jpeg(random_forest_jpeg_file_name, width = 18, height = 8, units = "in", res = 300)
	f1 = forest(hwb1, ref = "placebo", pooled = "random", sortvar = PScore, add.data = labsHWB, leftcols = c("Type", "studlab"), col.square = "blue", rightcols = c("effect", "ci", "PScore"))
	dev.off()
	
	# Fixed effects forest plot file name:
	fixed_forest_jpeg_file_name = sprintf("%s%s", high_type, '_fixed_effects_forest_network_ma_plot.jpeg', sep = '')
	# Fixed effects forest plot:
	jpeg(fixed_forest_jpeg_file_name, width = 18, height = 8, units = "in", res = 300)
	forest(hwb1, ref = "placebo", pooled = "fixed", sortvar = PScore, add.data = labsHWB, leftcols = c("Type", "studlab"), col.square = "blue", rightcols = c("effect", "ci", "PScore"))
	dev.off()
	
	# Funnel plot file name:
	funnel_jpeg_file_name = sprintf("%s%s", high_type, '_funnel_network_ma_plot.jpeg', sep = '')
	# Funnel plot:
	jpeg(funnel_jpeg_file_name, width = 26, height = 14, units = "in", res = 300)
	if (high_type == "higher-is-worse") {
		fl.hwb = funnel.netmeta(hwb1, order = f1$labels, pch = c(rep(c(15:18, 1), 9), 15, 16, 17, 18), col = 1:49)
	} else { # (high_type == "higher-is-better")
		fl.hwb = funnel.netmeta(hwb1, order = f1$labels, pch = c(rep(c(15:18, 1), 8), 15, 16), col = 1:3)
	}
	dev.off()

	# Tests for Bias:
	metabias(metagen(TE.adj, seTE, data = fl.hwb))
	metabias(metagen(TE.adj, seTE, data = fl.hwb), method = "mm")
}



########################################################################################
####                                                                                ####
####  Function do_continuous_ma performs Continuous Outcome Meta-Analysis           ####
####  on the given either higher-is-worse or higher-is-better data frame            ####
####                                                                                ####
####  Parameters:                                                                   ####
####      high_type - "higher-is-worse" or "higher-is-better"                       ####
####      smp        - excel data sheet data                                        ####
####                                                                                ####
########################################################################################

do_continuous_ma <- function(high_type, smp) {
	if ((high_type != "higher-is-worse") && (high_type != "higher-is-better")) {
		stop(cat("Invalid high type: ", high_type))
	}

	## Create a study ID by pasting the authors and year of the study.
	smd$Study = paste(smd$Authors, smd$Year, smd$`Combined Treatment`, sep = " ")

	## Select only the columns necessary and remove rows with NA in those columns, as that excludes
	## data that was used for the network analysis.
	smd_c = na.omit(smd[,c("Study", "Treatment Type", "HigherWorse", "Nc", "Mc", "Sc", "Ne", "Me", "Se", "ES")])
	colnames(smd_c)[2] = "Type"

	## Separate by Higher Worse and Higher Better, then remove the third columns as it is no longer needed.
	if (high_type == "higher-is-worse") {
		hwb = smd_c[smd_c$HigherWorse == 1, -3] #Higher worse
	} else { # (high_type == "higher-is-better") 
		hwb = smd_c[smd_c$HigherWorse == 0, -3] #Higher Better
	}

	# Fit both fixed and random effects models:
	hwb_model = metacont(Ne, Me, Se, Nc, Mc, Sc, data = hwb, studlab = Study, sm = "SMD")

	# Examine the results:
	hwb_model

	# Print a summary:
	print(summary(hwb_model), digits = 3)

	# Paired comparisons forest plot file name:
	paired_comparisons_forest_jpeg_file_name = sprintf("%s%s", high_type, '_paired_comparisons_forest_continuous_ma_plot.jpeg')
	# Paired comparisons forest plot:
	jpeg(paired_comparisons_forest_jpeg_file_name, width = 18, height = 8, units = "in", res = 300)
	if (high_type == "higher-is-worse") {
		forest(hwb_model, xlab = "Standardized Mean Difference in ADAS-cog, CDR-SB, or NPI")
#		grid.text("Paired Outcomes Forest Plot for Scale Indicating Poorer Function at Higher Scores", .5, .9, gp=gpar(cex=1.5))
	} else { # (high_type == "higher-is-better")
		forest(hwb_model, xlab = "Standardized Mean Difference in MMSE, ADCS-ADL, or SIB")
#		grid.text("Paired Outcomes Forest Plot for Scale Indicating Better Function at Higher Scores", .5, .9, gp=gpar(cex=1.5))
	}
	dev.off()

	# Paired comparisons funnel plot file name:
	paired_comparisons_funnel_jpeg_file_name = sprintf("%s%s", high_type, '_paired_comparisons_funnel_continuous_ma_plot.jpeg')
	# Examine funnel plot for bias:
	jpeg(paired_comparisons_funnel_jpeg_file_name, width = 6, height = 8, units = "in", res = 300)
	funnel(hwb_model)
	dev.off()

	# Run rank correlation test for plot asymmetry (Begg and Mazumdar Test):
	metabias(hwb_model, method = "rank")

	# Run linear regression test for plot asymmetry (Egger's Test):
	metabias(hwb_model, method = "linreg", plotit = TRUE)

	# Run Thompson and Sharp test - a variant of Egger's test allowing for between-study heterogeneity
	metabias(hwb_model, method = "mm")

	if (high_type == "higher-is-worse") {
		# Given the evidence of bias, run trim and fill and regression methods:
		trimfill_hw_model = trimfill(hwb_model) # Trim and Fill Method
		print(trimfill_hw_model, digits = 2, comb.fixed = TRUE) # Print output of Trim and Fill Method
		ll_hw_model = limitmeta(hwb_model) # Regression Method

		# View the Regression results:
		print(summary(ll_hw_model), digits = 2)
	}

	# Conduct a sub-group analysis to determine whether there is a difference
	# between natural and non-natural treatments:
	hwb_subgroup = metacont(Ne, Me, Se, Nc, Mc, Sc, data = hwb, studlab = Study, sm = "SMD", byvar = Type)

	# Examine output:
	hwb_subgroup

	# Print a summary:
	print(summary(hwb_subgroup), digits = 3)

	# Paired comparisons subgroup forest plot file name:
	paired_comparisons_subgroup_forest_jpeg_file_name = sprintf("%s%s", high_type, '_paired_comparisons_subgroup_forest_continuous_ma_plot.jpeg')
	# Paired comparisons subgroup forest plot:
	jpeg(paired_comparisons_subgroup_forest_jpeg_file_name, width = 18, height = 8, units = "in", res = 300)
	if (high_type == "higher-is-worse") {
		forest(hwb_subgroup, xlab = "Standardized Mean Difference in ADAS-cog, CDR-SB, or NPI by Natural vs. Non-natural Treatment")
#		grid.text("Paired Outcomes Forest Plot for Scale Indicating Poorer Function at Higher Scores", .5, .9, gp=gpar(cex=1.5))
	} else { # (high_type == "higher-is-better") 
		forest(hwb_subgroup, xlab = "Standardized Mean Difference in MMSE, ADCS-ADL, or SIB by Natural vs. Non-natural Treatment")
#		grid.text("Paired Outcomes Forest Plot for Scale Indicating Better Function at Higher Scores", .5, .9, gp=gpar(cex=1.5))
	}
	dev.off()
	
	# Subgroup by ES:
	metacont(Ne, Me, Se, Nc, Mc, Sc, data = hwb, studlab = Study, sm = "SMD", byvar = ES)
}



