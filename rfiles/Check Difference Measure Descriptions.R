#set working directory to the Citrix folder

setwd("Z:/Folders/CPUC10 (Group D - Custom EM&V)/4 Deliverables/00 - 2017 Evaluation/Database Review/E350 Data Additions/Raw Data Files")

#read in  the file Faith created for custom claims, and reduce dataset to unique Measure descriptions
cedarsinput = read.csv("custom_claims_extract_FD.csv")
uniquecedars = as.data.frame(unique(cedarsinput$MeasDescription))


#read in this file I created based on the SQL query
exantedistinct = read.csv("PostgreSQL Exante.Exante Distinct Measure.csv")

#find the the difference between the 2 files
dim(setdiff(uniquecedars, exantedistinct))


#read in the entire "Measure" table
exantecomplete = read.csv("PostgreSQL Exante.Exante.Measure Complete Table.csv")

#read in the entire "EnergyImpact" table
energyimpact = read.csv("PostgreSQL Exante.Exante.EnergyImpact.csv")


#find the number of unique Energy impact IDs in the Measure table that are not in the Energy Impact table
dim(setdiff(unique(exantecomplete$EnergyImpactID),unique(energyimpact$EnergyImpactID)))





