# SICE-R-Code
R code created from https://doi.org/10.1186/s40537-020-00313-w

Used after running MICE on the different categories of data e.g. binary, ordinal, categorical, continuous with a sample of 30 of imputated datasets achieved from 1 iteration of the predictive model selected for MICE (restricted due to computer hardware i.e. ideal would be 30 iterations of 30 datasets and then sort through the resuling 900 datasets)

Compares each category of data with a dataset that contains variables that originally, before MICE, did not have any missing data. 

The code is split into mulitple sections including continous, binary, and categorical. 

WARNING: If the computer running MICE and SICE together is not up to Data Science industry standards, it can take up to 6 minutes to run (how long it takes with my hardware with a dataset of 400 participants and 100 variables)
