# @author : Davi Méaille
# Created : 01/08/2022
# Last modification : 25/08/2022
# Description : 
# 
# This file creates the data set for  agricultural performance and control variables related to counties and farms.
# It uses ICPSR-35206 data as input, and the file'table for data.txt' to select the variables from each file. This
# file had to be created by hand, by looking at all Codebook descriptions to decide which variables from which file
# to include and which not to. We use metaprogramming, that is writing a code that produces the code to run because 
# of how R deals with references for variable names from dataframe. See more at:
# https://adv-r.hadley.nz/metaprogramming.html. 
# The code then produces Data_complete.csv as output.
#

rm(list = ls())

dir = 'C:/Users/davim/OneDrive/Desktop/Cours/LMU/Data analytics/Data/Raw/ICPSR_35206'
setwd(dir)

library(foreign) # for .dta file
library(tidyverse) # for data handling
library(rlang) # for metaprogramming when choosing our variables 
library(naniar) # for handling "" and NA


##################################################################################
#
#                            PREPARING THE ENVIRONMENT                          #
#
##################################################################################

# We load the table with the variabels that we want in each files
dt <- read.table("table for data.txt", sep = ";", header = TRUE)
dt = na_if(dt, "") # to convert the "" into NA

# We specify which files we will use, that is what are their number after "DS00"
v = c(6:10,12:13,15:16,20:21, 25, 27, 30, 33, 36, 39:40)

# We use the first file for Average value per acre until 1959 
# We will get the following years from the corresponding .dta files
setwd(paste0(dir, "/DS0001")) # we go to the relevant folder
d = list.files(pattern = "*.dta") # we get all the files with .dta
value_acre <- as.data.frame(read.dta(d[1])) # we read the first (and only one) of them

colnames(value_acre) <- gsub("FAVAL", "1", colnames(value_acre)) # we simplify the variable names, to facilitate the future matching
setwd("..") # we return to the parent folder


# We need a table to create the equivalence between FIPS and ICPSR codes for files without FIPS 
# We use to this purpose one of the documents that have both codes
# We choose for instance file number 12
setwd(paste0(dir, "/DS0012"))
d = list.files(pattern = "*.dta")
icpsr_to_string  <- as.data.frame(read.dta(d[1]))
icpsr_to_string <- icpsr_to_string %>% select(STATE, COUNTY, FIPS) # We select only the variables of interest, that is ICPSR and FIPS
setwd("..")

# We initiate the data set that will contain the agricultural data
df = data.frame(matrix(vector(), 0, 6, dimnames=list(c())))


##################################################################################
#
#                  CREATING THE DATASET OF AGRICULTURAL DATA                     #
#
##################################################################################



for (i in v){     # We iterate over the files that we have chosen 
  
  # We start by going to the folder containing the file.
  # We distinguish the cases where we need an additional 0 in the folder name
  
  if (i <10){
    setwd(paste0(dir, "/DS000", i))
  }
  
  else {
    setwd(paste0(dir, "/DS00", i))
  }
  
  d = list.files(pattern = "*.dta")  # We get the files with .dta in the folder
  dd <- as.data.frame(read.dta(d[1]))  # We load the one (and only) that we want 
  
  
  if (i %in% c(25, 27)) { # We handle the cases of files with no FIPS code 
    dd <- inner_join(dd, icpsr_to_string, by = c("STATE" = "STATE", "COUNTY" = "COUNTY")) # we merge with the table giving the FIPS code
    dd <- do.call(select, list(dd, eval(dt$FIPS[i]),  eval(dt$NAME[i]), eval(dt$POP[i]), eval(dt$AREA.NUMBER.OF.FARMS[i]))) # We use metaprogramming to make shorter the selection of the relevant variables in all folders
    acre <- value_acre[, c(which(dt$YEAR[i] == colnames(value_acre)), 20)] # We get the corresponding year of average value per acre in file 1 by matching the year with the name of the variable 
    dd <- inner_join(dd, acre, by = c("FIPS" = "FIPS")) # We join with FIPS the farm performance variable 
    colnames(dd) <- c("FIPS", "NAME", "TOTPOP", "AVERAGE AREA", "AVERAGE VALUE PER ACRES") # We rename the columns to facilitate the vertical merging
  }

  if (i == 30) {  # We handle the files that do not have FIPS code and from which we have to recover the farm performance variable 
    dd <- dd %>% inner_join(icpsr_to_string, by = c("STATE" = "STATE", "COUNTY" = "COUNTY"))
    dd <- dd %>% inner_join(icpsr_to_string, by = c("STATE" = "STATE", "COUNTY" = "COUNTY")) # I don't know why it does not evaluate the command on its first call so I had to add a second call in the program 
    dd <- do.call(select, list(dd, eval(dt$FIPS[i]),  eval(dt$NAME[i]), eval(dt$POP[i]), eval(dt$AREA.NUMBER.OF.FARMS[i]), eval(dt$VALUE.PER.ACRES[i])))
    colnames(dd) <- c("FIPS", "NAME", "TOTPOP", "AVERAGE AREA", "AVERAGE VALUE PER ACRES")
  }
  
  else if (is.na(dt$TOTAL.LAND[i]) && dt$YEAR[i] < 1960 && !(i %in% c(25,27,30))) { # We handle files with variable for average farmsize 
    dd <- do.call(select, list(dd, eval(dt$FIPS[i]),  eval(dt$NAME[i]), eval(dt$POP[i]), eval(dt$AREA.NUMBER.OF.FARMS[i])))
    acre <- value_acre[, c(which(dt$YEAR[i] == colnames(value_acre)), 20)]
    dd <- inner_join(dd, acre, by = c("FIPS" = "FIPS"))  # We join with FIPS the farm performance variable 
    colnames(dd) <- c("FIPS", "NAME", "TOTPOP", "AVERAGE AREA", "AVERAGE VALUE PER ACRES") # We rename the columns to facilitate the vertical merging
  }
  
  else if(dt$YEAR[i] >= 1960 && (i != 30)) { # We years whose farm performance is not included in the first file
    dd <- do.call(select, list(dd, eval(dt$FIPS[i]),  eval(dt$NAME[i]), eval(dt$POP[i]), eval(dt$AREA.NUMBER.OF.FARMS[i]), eval(dt$VALUE.PER.ACRES[i])))
    colnames(dd) <- c("FIPS", "NAME", "TOTPOP", "AVERAGE AREA", "AVERAGE VALUE PER ACRES") # We rename the columns to facilitate the vertical merging
  }
 
  else if((!is.na(dt$TOTAL.LAND[i])) && i < 27){ # We handle files without variable for average farmsize, so that we have to construct it from number of farms and total land used for agriculture 
    dd <- do.call(select, list(dd, eval(dt$FIPS[i]),  eval(dt$NAME[i]), eval(dt$POP[i]), eval(dt$AREA.NUMBER.OF.FARMS[i]), eval(dt$TOTAL.LAND[i])))
    dd <- dd %>%
      dplyr::mutate(FARMSIZE = eval(parse_expr(paste0(eval(dt$TOTAL.LAND[i]), "/", eval(dt$AREA.NUMBER.OF.FARMS[i])))), .keep = "unused" ) # We create the average area variable again with metaprogramming 
    acre <- value_acre[, c(which(dt$YEAR[i] == colnames(value_acre)), 20)]
    dd <- inner_join(dd, acre, by = c("FIPS" = "FIPS"))  # We join with FIPS the farm performance variable 
    colnames(dd) <- c("FIPS", "NAME", "TOTPOP", "AVERAGE AREA", "AVERAGE VALUE PER ACRES") # We rename the columns to facilitate the vertical merging
  }
  
  YEAR <- matrix(data = rep(dt$YEAR[i], nrow(dd)), ncol = 1, nrow =  nrow(dd)) # We construct the column for the year 
  dd <- cbind(dd, YEAR) # we first bind it to the intermediary dataset
  df <- rbind(df, dd) # we then combine our intermediary dataset to our final dataset 
  setwd("..") # We come back to the parent folder  
}

# The error are normal. The code is made to include years after 1960 but the last data files 
# do not contain the control variables that we want to add. As such, we have just prepared the 
# code for future research where we will be able to include these variables, but for now, the 
# error message will just say that the for loop has stopped for years after 1960.

rm(acre, agr, dd, dt, icpsr_to_string, value_acre)
rm(YEAR, d, dir, i, v, x)



# To save the output in a csv file in the folder for created data set.
write_csv(df, 'C:/Users/davim/OneDrive/Desktop/Cours/LMU/Data analytics/Data/Created/Agricultural_Data.csv')

##################################################################################
#
#                      SUMMARY STATISTICS FOR THE ESSAY - CODE                   #
#
##################################################################################


# For more detailed summary statistics
library(stargazer) # To get html/latex/ASCII tables of summary statistics
stargazer(df, type = "text", summary.stat = c("n", "mean", "sd", "min", "max")) # to see the output in the console
stargazer(df, type = "latex", summary.stat = c("n", "mean", "sd", "min", "max")) # to paste the output in the essay 



#################################
#   OPTIONNAL - NOT NECESSARY
#################################

# the last part is not necessary to have a result in the regression and takes too much time while creating 
# a data set that is too heavy for nothing 
# so not necessary to run this last part 

##################################################################################
#
#                 COMPLETING THE MISSING YEARS WITH EXISTING ONES                 #
#
##################################################################################


# missing FIPS in frequencies are missing because there were not created at the beginning of the period of interest 
# maybe we can just ignore the years with missing population for now 

# we want to have values for all years (might be a bad idea)

agr <- df

x <- levels(factor(df$YEAR))
x[15] <- 1974
x <- as.numeric(x)
i = 1880

for (j in 1:length(x)) {
  while (i < x[j+1]) {
    data <- df %>% filter(YEAR == i)
    data$YEAR = data$YEAR + 1
    df <- rbind(df, data)
    i = i + 1
  }
  
  i = x[j]
}


###### Unloading the packages to avoid some common references and functions between them 

detach("package:tidyverse", unload=TRUE)
detach("package:naniar", unload=TRUE)



