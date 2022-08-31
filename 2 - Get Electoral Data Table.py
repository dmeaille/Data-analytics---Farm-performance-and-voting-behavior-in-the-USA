# -*- coding: utf-8 -*-
"""
Created on Thu Aug 11 10:28:54 2022

@author: Davi Méaille 

Description : 

    This file is used to get the table of correspondences 
    between ICPSR 8611 and FIPS code, as stated in the 
    CodeBook for Electoral Data. 


"""

from tabula import read_pdf
import os 

dir = "C:/Users/davim/OneDrive/Desktop/Cours/LMU/Data analytics/Data/Raw/ICPSR_08611"

os.chdir(dir)

# We import the second page, that is the one that interests us 
df = read_pdf("08611-0001-Codebook.pdf", pages = "2")

# We get the element of the list that interests us, that is the only one table of the page
df = df[0]

# We remove empty columns (FIPS and ICPSR) as well as redondant ones (county names) 
df = df[['Unnamed: 0',  'Unnamed: 1', 'Unnamed: 2', 'Unnamed: 4', '0']]

# We write some understandable variable names
df.columns = ['County name', 'ICPSR State', 'ICPSR County', 'FIPS State', 'FIPS County']

# We erase the first line that contains variable names 
df = df.iloc[1:26]

# to print the result properly
from tabulate import tabulate 
print(tabulate(df))


# We now proceed to save the file  

df.to_csv("Correspondences.csv")


# the rest happens on R !!