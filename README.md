%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Readme
# Author : Davi Méaille
## Date created : 01/08/2022
## Date last modification : 25/08/2022
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



##				    PURPOSE OF THIS PROJECT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
With this project, we want to estimate the correlation between electoral vote and farm productivity 
in the USA. More precisely, we estimate the relation between a proxy for farm productivity, 
average value per acre for farmlands, and different variables of votes that are presented 
below. This aims at understanding how vote can be a determinant of farm performance. 

##		  		     INPUT DATA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
We use agricultural data in the USA  and electoral data from https://www.icpsr.umich.edu/web/pages/. 
We use data over the period 1880 to 1980, for conveniences of the data that we have. 


##					         CODES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
→ 1 - Agricultural Data - Preparation.R : 
It builds our dataset for agricultural performance. It loads all the datafile in which 
we have found variables of interest and select the variables using "table for data.txt". 
The latter file contains the name of the variables that we require for each file. 

We load the following variables : 
- FIPS code as an identifier variable 
- Total population of the county/the state as a control variable
- Average farm size in the county/state as a control variable
- Average value per acre in the county/state as our performance variable
And we create a variable YEAR for each of them.

For the variable on performance, we use the first dataset for years up until 1959
and then a variable contained within the dataset.

There are four possibilities. 
Some files do not have FIPS code and we have to use their ICPSR code to match 
and give them a FIPS code variable. 
Some files do not have average value per acres and we have to load number of farms
and total area covered in land to create it.
We have to upload farm performance for some files and not for some others.
Eventually, some files have several of these particular cases. 
This explains the several if loop within our main for loop. 

The created dataset is then stored in "Agricultural_Data.csv". 

→ 2 - Get Electoral Data Table.py : 
It gets the table for special correspondences exposed in the Codebook for electoral
data, to make it usable while creating the FIPS code for electoral data. It then creates 
the file "Correspondences.csv".

→ 3 - Electoral Data - Preparation.R : 
It loads the electoral data to select our variable of interest and to build a valid 
FIPS code for all state to facilitate the merging with agricultural data. The FIPS 
code is created using "Correspondences.csv" and "StateFIPSicsprAD.csv". 

The variable that we select are the following: 
- Congressional votes by counties for democrates as our main variable 
- Congressional votes for republican, presidential votes for democrates and republican as robustness check variables

We create a dataset containing all of our result. The created dataset is then stored in "Election_Data.csv".


→ 4 - Merge and Regress.R : 
It loads agricultural data and electoral data to merge both dataset and then proceed
to our regressions of interest.

%%%% Order :
The files have to be run in the order given by their own numbers. 



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##					SUMMARY STATISTICS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


==============================================================================================
Statistic                 		N       		Mean     		St. Dev.    	  Min  	Pctl(25)     Pctl(75)       	 Max     
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
FIPS                    			41,606 	30,483.550 	14,916.520   1,001  	19,041      45,071      	56,045    
TOTPOP                 	 		41,581 	39,244.350 	139,051.700 0.000 	9,639.000 30,286.000 	6,038,771.000
AVERAGE.AREA           		39,872  	438.889    		3,296.247  	  0.000  	75.117      209.800    	450,000.000 
AVERAGE.VALUE.PER.ACRES 	41,087   	90.749     		763.960   	  0.000  	20.000      85.000    	85,741.000  
YEAR                    			41,606 	1,929.808    	24.538    	  1880   	1910        1950        	1964    
CONG.DEM                		41,606  	105.684     		211.532   	  0.000  	38.900      90.700      	999.990   
CONG.REP                		41,606   	88.376     		215.296   	  0.000  	14.500      59.538      	999.990   
PRES.DEM                		41,606   	71.529     		140.115   	  0.000  	37.030      65.100      	999.900   
PRES.REP               	 	41,606   	65.267     		140.950   	  0.000  	32.440      59.800      	999.900   
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
where:
- FIPS is the FIPS code
- TOTPOPT is total population of the county
- AVERAGE.AREA is the average size of farms in a county
- AVERAGE.VALUE.PER.ACRES is also on a county scale
- CONG.DEM is the share of democrates at congressional votes by counties 
- REP.DEM is the share of republicans at congressional votes by counties 
- PRES.DEM is the share of democrates at presidential votes by counties
- PRES.REP is the share of republicans at presidential votes by counties  
