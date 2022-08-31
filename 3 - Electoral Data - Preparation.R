# @author : Davi Méaille
# Created : 01/08/2022
# Last modification : 25/08/2022
# Description : 
# 
# This file creates the data set for electoral turnout.
# It uses ICPSR-08611 data as input, and the files Correspondence.csv and StateFIPSicsprAB.xls 
# to create an unique FIPS identifier that matches each State and Counties. 
# It produces Election_Data_Democrats.csv as output.
#

rm(list = ls()) # To clean the Global environment 
##################################################################################
#
#                             PREPARING THE ENVIRONMENT                          #
#
##################################################################################

setwd("C:/Users/davim/OneDrive/Desktop/Cours/LMU/Data analytics/Data/Raw/ICPSR_08611")

library(foreign) # for .dta data file
library(stringr) # to deal with strings
library(tidyverse) # for general data handling 
library(readxl) # for reading xls file
library(stargazer) # for nice exportable table and to get html/latex/ASCII tables of summary statistics
library(plyr) # to merge several data set at once

# we load the initial data set
df <- read.dta("08611-0001-Data.dta")

# We first need to give meaningful names to the columns to facilitate futur handling of data.
# For that, we use the labels of the variables that are already present in the file and we 
# paste them to the names of the variables
colnames(df) <- attr(df, "var.labels")



##################################################################################
#
#                    CREATING THE FIPS CODE IN ELECTORAL DATA                    #
#
##################################################################################

# We want to convert the ICPSR Codes to FIPS codes to match our agricultural data.
# In the Codebook of electoral data, we can observe that a general rule that seems 
# to apply states that we need to divide the ICPSR county code by then to retrieve 
# the corresponding FIPS County Code.
# However, there is a short problem : some ICPSR are not multiples of 10

df[df$`COUNTY IDENTIFICATION NO` %% 10 != 0, 1:3]

# We might have to get rid of them to continu our task.
# So let's check what type of location they pertain to.
# We can see that they are not very relevant : tulalip res washington dc is a resort hotel for instance.
# They might all be some identifiers for special places that will probably have nothing to do with agriculture.
# So we will just throw them away.

df <- df[df$`COUNTY IDENTIFICATION NO` %% 10 == 0,]

# For the remaining, we will divide them by 10, as specified in the Codebook.

df$`COUNTY IDENTIFICATION NO` <- df$`COUNTY IDENTIFICATION NO`/10


# Additionally, we also need to correct the counties in ICPSR 08611 that do not match the FIPS code only by dividing by 10 
# they are all located in 52 which is Maryland 

table(df$`ICPSR STATE CODE`) # so we look for MARYLAND (1788)


# We load what we created with our Python code, that is our table of correspondences to check 
# for the FIPS and ICPSR codes, to make them match properly 
correspondences <- read.csv("Correspondences.csv")

# we take away the column for the index 
correspondences <- correspondences[,2:6]

# We correct the names 
colnames(correspondences) <- c("County name", "ICPSR State", "ICPSR County", "FIPS State", "FIPS County")

# we instantiate the column for FIPS in df 
df$FIPS <- NA

# We Fulfill the column with the FIPS identifier
# by adding state and county number 
# and by taking the right numbers for the specified counties 
# and by taking the right state number from the matching table given by the professor 

state_match <- read_excel("StateFIPSicsprAB.xls")

# For the names to match, we have to 
# set in capital letters in state_match
state_match$NAME <- toupper(state_match$NAME)
# we remove the date from df
df$`ICPSR STATE CODE` <- gsub(" \\(.*\\)", "", df$`ICPSR STATE CODE`)

# Little verification
sum(df$`ICPSR STATE CODE` %in% state_match$NAME)
# 3185 which is the number that we seek 

# In the condition of the first if: 
# we consider only the counties in the list that we have to change 
# and we add the condition State = Maryland to avoid counties with same names but different state
# and Maryland because it is the state at stake here 
for (i in 1:nrow(df)){
  if (df$`COUNTY NAME`[i] %in% correspondences$`County name` & df$`ICPSR STATE CODE` == "MARYLAND") {
    df$FIPS[i] = correspondences[which(correspondences$`County name` %in% df$`COUNTY NAME`[i]), "FIPS State"] * 1000 +  correspondences[which(correspondences$`County name` %in% df$`COUNTY NAME`[i]), "FIPS County"]
  }
  else {
    df$FIPS[i] = state_match[which(state_match$NAME %in% df$`ICPSR STATE CODE`[i]), "FIPS"] * 1000 + df$`COUNTY IDENTIFICATION NO`[i]
  }
}

# Because it is in form of list, we convert it to handle it more easily 
df$FIPS <- as.numeric(df$FIPS)

length(unique(df$FIPS)) - length(df$FIPS)
# We have created the unique identifier !! We will then use it to merge the data



# We clean again our global environment
rm(correspondences, state_match)



##################################################################################
#
#                       SELECTING OUR VARIABLES OF INTEREST                      #
#
##################################################################################


############## We create a function that will collect what we want for all type of queries 

collect <- function(char) {
  
  if(!is.character(char)){ # a little security, to make sure that the input is of type character
    char <- as.character(char)
  }
  
  # We do not want to include the equivalent of our variables (DEM-EQ, REP-EQ mainly)
  # We thus look for the character as input without "-EQ" among the variables names to get the indexes
  # or the variable that interest us.
  x <- setdiff(grep(char,colnames(df)),grep(paste0(char, "-EQ"), colnames(df)))
  
  
  # Now, we will create effectively the data set 
  
  dt <- df[,c(1:2,x, 760)] # Let's create a data set with only the variable that we are interested in while keeping FIPS (column 760)
  
  data <-  data.frame(matrix(vector(), 0, 5)) # We instantiate the data set that will store our final data 
  
  for (i in 3:(length(dt)-1)) { # we loop only over the variable for congressional democrat votes 
    dd <- dt[,c(1:2, length(dt),i)]  # We store an intermediate data set with the a variable for democrat votes for one year 
    dd[,5] <- matrix(str_trunc(colnames(df)[x[i - 2]], 4, ellipsis = ''), nrow(dd), ncol = 1) # We create a fifth column for the year
    # the previous line cuts the year from the names of the variables, by looking at their position in the character strings 
    colnames(dd) <- c("ICPSR STATE CODE", "COUNTY NAME", "FIPS", char, "YEAR") # we rename the columns to facilitate the merging
    data <- rbind(data, dd) # we bind the intermediary data to our final data set 
  }
  
  data$YEAR <- as.numeric(data$YEAR) # this will be useful for the following part 
  
  return(data) # we return what we have created to save it 
}






########### In order to imitate a "continuous" vote, we create another function to fill each year in between
# with the preceding years. This will facilitate the merging with agricultural data. Also, this will allow us 
# to use the year present in our agricultural data set, that are less frequent. 

# Since we have always one year over two for congressional data, we will simply use the value for each year 
# that we have to put it in the values for the year + 1. It will be the same for presidential data, but with 
# year t+1, t+2 and t+3. 

fill_year <- function(data, i) { # i = 1 for data set with one year over two (congressional elections) and i = 1 and then i = 2 for data set with one year over four (presidential elections)
  data2 <- data  # We create a new data set
  data2$YEAR <- data2$YEAR + i  # We increase the values of all years by one 
  data <- rbind(data, data2) # Now we bind both data set 
  
  return(data)
}

library(plyr)




################ Collecting Democrates Congressional Votes Variables 

democrate_cong <- collect("CONG DEM")



###### to export the data set statistics before completing for each year

stargazer(data, type = "text", summary.stat = c("n", "mean", "sd", "min", "max"))
stargazer(data, type = "latex", summary.stat = c("n", "mean", "sd", "min", "max"))


### we fill the missing years 

democrate_cong <- fill_year(democrate_cong, 1)





##################### We repeat the process for the other variables that we want to extract


#### Republican votes for Congressional elections
republican_cong <- collect("CONG REP")
stargazer(republican_cong, type = "text")

republican_cong <- fill_year(republican_cong, 1)

#### Democrats votes for Presidential elections
pres_dem <- collect("PRES DEM")
stargazer(pres_dem, type = "text")

pres_dem <- fill_year(pres_dem, 1)
pres_dem <- fill_year(pres_dem, 2)

#### Republican votes for Presidential elections
pres_rep <- collect("PRES REP")
stargazer(pres_rep, type = "text")

pres_rep <- fill_year(pres_rep, 1)
pres_rep <- fill_year(pres_rep, 2)




##################################################################################
#
#                      SUMMARY STATISTICS FOR THE ESSAY - CODE                   #
#
##################################################################################


############################################ We merge our different data set in one

# We use left join because democrats data include more year and we don't want to loose them yet 

merged <- join_all(dfs = list(democrate_cong, republican_cong, pres_dem, pres_rep), by = c("YEAR", "FIPS", "ICPSR STATE CODE", "COUNTY NAME"), type = "left")


######### We create summary tables


stargazer(merged, type = "text", summary.stat = c("n", "mean", "sd", "min", "max")) # to print the output in the console
stargazer(merged, type = "latex", summary.stat = c("n", "mean", "sd", "min", "max")) # to paste the output in the essay 




##################################################################################
#
#                   SAVING THE FINAL OUTPUT THAT WILL BE MERGED                  #
#
##################################################################################


write.csv2(merged, "C:/Users/davim/OneDrive/Desktop/Cours/LMU/Data analytics/Data/Created/Election_Data.csv") # note that choosing cvs2 over csv has 
# few consequences : it only indicates whether we want to use "," or ";" in the data. 
# We only need to make sure that we use the correct read.csv/read.csv2 afterward. 


rm(i)
rm(collect, fill_year)
rm(democrate_cong, republican_cong, pres_rep, pres_dem, df, merged)


detach("package:plyr", unload=TRUE)
