### File: GroupCombinationFormulas.R
### 
###
### This script provides R function for the group combination formulas found in 
### https://handbook-5-1.cochrane.org/chapter_7/table_7_7_a_formulae_for_combining_groups.htm

# Output overall N from two sample sizes.
getN = function(N1, N2){ N1 + N2}

# Output overall mean from two means and two sample sizes.
getMean = function(M1, M2, N1, N2){
  (N1*M1 + N2*M2)/(N1 + N2)
}

# Output overall sd from two means, sample sizes, and standard deviations.
getSD = function(N1, N2, M1, M2, SD1, SD2){
  sqrt((((N1 - 1)*SD1^2) + ((N2 - 1)*SD2^2) + (N1*N2/(N1 + N2))*(M1^2 + M2^2 - 2*M1*M2))/(N1 + N2 - 1))
}



## Overall function - place inputs and obtain combined N, mean, and SD.
combineGroups = function(N1, N2, M1, M2, SD1, SD2){
  n = getN(N1, N2)
  
  m = getMean(M1, M2, N1, N2)
  
  sd = getSD(N1, N2, M1, M2, SD1, SD2)
  
  print(list(n = n, Mean = m, SD = sd))
}
