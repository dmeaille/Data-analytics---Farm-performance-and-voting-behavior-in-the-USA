# @author : Davi Méaille
# Created : 01/08/2022
# Last modification : 25/08/2022
# Description : 
# 
# This file uses the data sets for agricultural performance and electoral output created 
# in '1 - Agricultural Data - Preparation.R' and '3 - Electoral Data - Preparation.R'.
# It proceeds to merging them over the FIPS identifier and the Years. 
# Then, it sorts the few identifiers that happen to be repeated.
# Finally, it uses the panel linear model library plm to estimate the equation specified
# in the essay for our four vote variables.
# It stores the final data set in Final_data.csv. 
#

rm(list = ls())

setwd("C:/Users/davim/OneDrive/Desktop/Cours/LMU/Data analytics/Data/Created")

library(tidyverse) # for data handling 
library(plm) # for panel regression
library(stargazer) # To get html/latex/ASCII tables of summary statistics


##################################################################################
#
#                  MERGING AGRICULTURAL AND ELECTORAL DATASET                    #
#
##################################################################################

# We load the two data sets that we have to merge 
elec <- read.csv2("Election_Data.csv") # we read with csv2 because we wrote the file with csv2
agr <- read.csv("Agricultural_Data.csv") # same for csv rather than csv2


elec <- elec[,-1] # the first column is only made of index, so we don't keep it 


length(unique(elec$`FIPS`))
# 3185
length(unique(agr$`FIPS`))
# 3131
sum(unique(elec$`FIPS`) %in% unique(agr$`FIPS`)) 
# 3073 is thus the number of common FIPS that we have in both documents 

# We proceed to the merging over both FIPS code and year
merged <- inner_join(agr, elec, by = c("FIPS" = "FIPS", "YEAR"= "YEAR"))

length(unique(merged$`FIPS`))
# 3073 so we have the correct number 
length(unique(merged$YEAR))



##################################################################################
#
#               WE HAVE TO CHECK WHETHER THE DATA SET IS USABLE                  #
#
##################################################################################

# While converting our data into a panel data frame (which is also done in the regression if we do not do it here)
# we can see a warning, that some of our identifiers (Year and Fips code) are not unique.
# If the warning does not appear here, trying to run the regression will make it appear 

prepare_pdataframe <- function(merged) {
  data <- pdata.frame(merged, index=c("FIPS","YEAR"), drop.index=FALSE, row.names=TRUE, drop.unused.levels = TRUE)

  # As recommended by the error message, we can verify that with : 
  table(index(data), useNA = "ifany")

  # We will thus remove them 
  # We remove the few identifiers that happen to be repeated because they prevent the regression from functioning 
  # This will probably not have any meaningful consequence, given the large size of the data set


  indexx <- table(index(data))
  indexx <- as.data.frame(indexx) # this allows us to have the frequency as a variable that we can use 

  a <- indexx[indexx$Freq > 1,]$FIPS # we store the repeated Fips, that is the FIPS for the observations 
# that have been observed more than once. We call an observation a couple FIPS-YEAR. 
  a <- as.character(a) # to use them, we have to convert them into character type. 

  merged <- merged[!(merged$FIPS %in% a),] # we remove from our merged data set the repeated identifiers based on their Fips code

  return(merged)
}

merged <- prepare_pdataframe(merged)

###################### 
# CODE FOR THE ESSAY

stargazer(merged, type = "text") # to print the output in the console 
stargazer(merged, type = "latex") # # to paste the output in the essay



# Now, we can either perform the panel regression on the merged dataset, or on a pdata.frame object
# We choose the second option for conveniences 

data <- pdata.frame(merged, index=c("FIPS","YEAR"), drop.index=FALSE, row.names=TRUE, drop.unused.levels = TRUE)






##################################################################################
#
#                      SUMMARY STATISTICS FOR THE ESSAY - CODE                   #
#
##################################################################################

stargazer(merged, type = "text") # for the READ_ME

stargazer(data, type = "text", summary.stat = c("n", "mean", "sd", "min", "max")) # for the data finally used, where we have filtered meaningless vote data 
stargazer(data, type = "latex", summary.stat = c("n", "mean", "sd", "min", "max")) # for the essay





##################################################################################
#
#                 ALL REGRESSIONS - INDIVIDUAL FIXED EFFECT - CODE               #
#
##################################################################################

#### For all our other regressions, we use each time 
# - our control variables
# - meaningful results 


# With democrate congressional vote 
reg_dem_cong <- plm(formula = AVERAGE.VALUE.PER.ACRES ~ CONG.DEM + AVERAGE.AREA + TOTPOP, data = filter(data, CONG.DEM < 100), effect = "individual", method = "within")

# With republican congressional vote
reg_rep_cong <- plm(formula = AVERAGE.VALUE.PER.ACRES ~ CONG.REP + AVERAGE.AREA + TOTPOP, data = filter(data, CONG.REP < 100), effect = "individual", method = "within")

# With democrat presidential vote
reg_dem_pres <- plm(formula = AVERAGE.VALUE.PER.ACRES ~ PRES.DEM + AVERAGE.AREA + TOTPOP, data = filter(data, PRES.DEM < 100), effect = "individual", method = "within")

# With republican presidential vote
reg_rep_pres <- plm(formula = AVERAGE.VALUE.PER.ACRES ~ PRES.REP + AVERAGE.AREA + TOTPOP, data = filter(data, PRES.REP < 100), effect = "individual", method = "within")
  

#### Summary table for the essay
stargazer(reg_dem_cong, reg_rep_cong, reg_dem_pres, reg_rep_pres, type = "text") # to print the output in the console
stargazer(reg_dem_cong, reg_rep_cong, reg_dem_pres, reg_rep_pres, type = "latex") # to print the latex code for the essay

# Hurray, we have some significant results !!! 




# Finally, we save our final dataset 
write.csv(merged, "Final_data.csv")




##################################################################################
#
#                         REGRESSIONS WITH LAGS - CODE                           #
#
##################################################################################




##################### Creating the data set with lags for political variables 


lag_vote_variable <- function(elec,j) {
  
  pelec <- pdata.frame(elec, index=c("FIPS","YEAR")) # we convert the electoral data into pdataframe class
  
  
  vote <- pelec[, c(4, 6:8)] # we select only the variable that we want to lag (the ones of vote)
  
  for (i in 1:4) {
    vote[,i] <- lag(vote[,i], k = j) # we apply the lag 
  }
  
  # and we reconstitute the original dataset with the lagged variable 
  
  pelec[, c(4,6:8)] <- vote 
  
  elec <- as.data.frame(pelec)
  elec$FIPS <- as.numeric(as.character(elec$FIPS))
  elec$YEAR <- as.numeric(as.character(elec$YEAR))
  
  return(elec)

}

# we create our lagged data 
elec_lag <- lag_vote_variable(elec = elec, j = 4) # we apply the lag of four years 
merged_lag <- inner_join(agr, elec_lag, by = c("FIPS" = "FIPS", "YEAR"= "YEAR")) # we join with agricultural data
merged_lag <- prepare_pdataframe(merged_lag) # we prepare our data frame like above 
data_lag  <- pdata.frame(merged_lag, index=c("FIPS","YEAR"), drop.index=FALSE, row.names=TRUE, drop.unused.levels = TRUE) # we convert it into pdataframe type for convenience


##################### We run the regressions again 

# We run the four regressions with lag data
reg_dem_cong_lag <- plm(formula = AVERAGE.VALUE.PER.ACRES ~ CONG.DEM + AVERAGE.AREA + TOTPOP, data = filter(data_lag, CONG.DEM < 100), effect = "individual", method = "within")
reg_rep_cong_lag <- plm(formula = AVERAGE.VALUE.PER.ACRES ~ CONG.REP + AVERAGE.AREA + TOTPOP, data = filter(data_lag, CONG.REP < 100), effect = "individual", method = "within")
reg_dem_pres_lag <- plm(formula = AVERAGE.VALUE.PER.ACRES ~ PRES.DEM + AVERAGE.AREA + TOTPOP, data = filter(data_lag, PRES.DEM < 100), effect = "individual", method = "within")
reg_rep_pres_lag <- plm(formula = AVERAGE.VALUE.PER.ACRES ~ PRES.REP + AVERAGE.AREA + TOTPOP, data = filter(data_lag, PRES.REP < 100), effect = "individual", method = "within")


#### Summary table for the essay
stargazer(reg_dem_cong_lag, reg_rep_cong_lag, reg_dem_pres_lag, reg_rep_pres_lag, type = "text") # to print the output in the console
stargazer(reg_dem_cong_lag, reg_rep_cong_lag, reg_dem_pres_lag, reg_rep_pres_lag, type = "latex") # to print the latex code for the essay



rm(elec, agr) # we clean the environment because they are now useless 



