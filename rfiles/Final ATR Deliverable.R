#set working directory to the Citrix folder
setwd("Z:/Folders/CPUC10 (Group D - Custom EM&V)/4 Deliverables/00 - 2017 Evaluation/Database Review/E350 Data Additions/Raw Data Files")

#read in the claims database
claims = read.csv("Custom Claims Extract.csv")

#read in the NTG File
NTGinput = read.csv("NTG Export.csv")

#read in the RUL & EUL file
EULinput = read.csv("EUL & RUL Export.csv")

#create the final table we need
finaldev = as.data.frame(matrix(nrow = dim(NTGinput)[1], ncol = 30))
colnames(finaldev) <- c("ClaimID", "EvalNTGRUseCategory", "EvalNTGRUseSubcategory", "EvalNTGRTechGroup", "EvalNTGRTechType", "EvalEULUseCategory", "EvalEULUseSubcategory", "EvalEULTechGroup", "EvalEULTechType", "EvalNTG_ID", "EvalNTG_kWH", "EvalNTG_therms", "EvalEUL_ID", "EvalEUL_Yrs", "EvalRUL_Yrs", "EvalNTGRUseCategoryFlag", "EvalNTGRUseSubcategoryFlag", "EvalNTGRTechGroupFlag", "EvalNTGRTechTypeFlag", "EvalEULUseCategoryFlag ", "EvalEULUseSubcategoryFlag", "EvalEULTechGroupFlag", "EvalEULTechTypeFlag", "EvalNTG_IDFlag", "EvalNTG_kWHFlag", "EvalNTG_thermsFlag", "EvalEUL_IDFlag", "EvalEUL_YrsFlag", "EvalRUL_YrsFlag", "Install Flag")


#set the final table's ClaimID column to be the same as the NTG claims Column
finaldev$ClaimID = NTGinput$Claims.ID


#set the final table's Use Category to be the same as the NTG UseCategory Claims Column
finaldev$EvalNTGRUseCategory = NTGinput$Use.Category

#for each of the measures, if the use categories are equal, the flag is 0, otherwise it's 1
for(i in 1:dim(NTGinput)[1])
{
  if(claims$UseCategory[i] == NTGinput$Use.Category[i])
  {
    finaldev$EvalNTGRUseCategoryFlag[i] = 0
  }
  else
  {
    finaldev$EvalNTGRUseCategoryFlag[i] = 1
  }
}


#set the final table's Use SubCategory to be the same as the NTG UseSubCategorry
finaldev$EvalNTGRUseSubcategory = NTGinput$Use.SubCategory

#for each of the measures, if the use categories are equal, the flag is 0, otherwise it's 1
for(i in 1:dim(NTGinput)[1])
{
  if(claims$UseSubCategory[i] == NTGinput$Use.SubCategory[i])
  {
    finaldev$EvalNTGRUseSubcategoryFlag[i] = 0
  }
  else
  {
    finaldev$EvalNTGRUseSubcategoryFlag[i] = 1
  }
}


#set the final table's Tech Group to that of the NTG Tech Group
finaldev$EvalNTGRTechGroup = NTGinput$Tech.Group

#for each of the measures, if the use categories are equal, the flag is 0, otherwise it's 1
for(i in 1:dim(NTGinput)[1])
{
  if(claims$TechGroup[i] == NTGinput$Tech.Group[i])
  {
    finaldev$EvalNTGRTechGroupFlag[i] = 0
  }
  else
  {
    finaldev$EvalNTGRTechGroupFlag[i] = 1
  }
}


#set the final tables Tech Type to that of the NTG Tech Group
finaldev$EvalNTGRTechType = NTGinput$Tech.Type

#for each of the measures, if the use categories are equal, the flag is 0, otherwise it's 1
for(i in 1:dim(NTGinput)[1])
{
  if(claims$TechType[i] == NTGinput$Tech.Type[i])
  {
    finaldev$EvalNTGRTechTypeFlag[i] = 0
  }
  else
  {
    finaldev$EvalNTGRTechTypeFlag[i] = 1
  }
}


#set the final tables Use Category to that of the EUL Use Category
finaldev$EvalEULUseCategory = EULinput$Use.Category

#for each of the measures, if the use categories are equal, the flag is 0, otherwise it's 1
for(i in 1:dim(EULinput)[1])
{
  if(claims$TechType[i] == NTGinput$Tech.Type[i])
  {
    finaldev$`EvalEULUseCategoryFlag `[i] = 0
  }
  else
  {
    finaldev$`EvalEULUseCategoryFlag `[i] = 1
  }
}


#set the final tables Use SubCategory to that of the EUL Use SubCategory
finaldev$EvalEULUseSubcategory = EULinput$Use.SubCategory

#for each of the measures, if the use categories are equal, the flag is 0, otherwise it's 1
for(i in 1:dim(EULinput)[1])
{
  if(claims$UseSubCategory[i] == EULinput$Use.SubCategory[i])
  {
    finaldev$EvalEULUseSubcategoryFlag[i] = 0
  }
  else
  {
    finaldev$EvalEULUseSubcategoryFlag[i] = 1
  }
}


#set the final tables Tech Group to that of the EUL Tech Group
finaldev$EvalEULTechGroup = EULinput$Tech.Group

#for each of the measures, if the use categories are equal, the flag is 0, otherwise it's 1
for(i in 1:dim(EULinput)[1])
{
  if(claims$TechGroup[i] == EULinput$Tech.Group[i])
  {
    finaldev$EvalEULTechGroupFlag[i] = 0
  }
  else
  {
    finaldev$EvalEULTechGroupFlag[i] = 1
  }
}


#set the final tables Tech Type to that of the EUL Tech Type
finaldev$EvalEULTechType = EULinput$Tech.Type

#for each of the measures, if the use categories are equal, the flag is 0, otherwise it's 1
for(i in 1:dim(EULinput)[1])
{
  if(claims$TechType[i] == EULinput$Tech.Type[i])
  {
    finaldev$EvalEULTechTypeFlag[i] = 0
  }
  else
  {
    finaldev$EvalEULTechTypeFlag[i] = 1
  }
}


#set the final tables NTG ID to that of the NTG ID
finaldev$EvalNTG_ID = NTGinput$NTG.ID

#for each of the measures, if the use categories are equal, the flag is 0, otherwise it's 1
for(i in 1:dim(EULinput)[1])
{
  if( (claims$NTG_ID[i] != NTGinput$NTG.ID[i]) | (is.na(claims$NTG_ID[i]) & !is.na(NTGinput$NTG.ID[i])) | (!is.na(claims$NTG_ID[i]) & is.na(NTGinput$NTG.ID[i])) )
  {
    finaldev$EvalNTG_IDFlag[i] = 1
  }
  else
  {
    finaldev$EvalNTG_IDFlag[i] = 0
  }
}


#set the final tables NTG kWh to that of the NTG kWh
finaldev$EvalNTG_kWH = NTGinput$X.NTG.kWh

#for each of the measures, if the use categories are equal, the flag is 0, otherwise it's 1
for(i in 1:dim(NTGinput)[1])
{
  if( (claims$NTGRkWh[i] != NTGinput$X.NTG.kWh[i]) | (is.na(claims$NTGRkWh[i]) & !is.na(NTGinput$X.NTG.kWh[i])) | (!is.na(claims$NTGRkWh[i]) & is.na(NTGinput$X.NTG.kWh[i])) )
  {
    finaldev$EvalNTG_kWHFlag[i] = 1
  }
  else
  {
    finaldev$EvalNTG_kWHFlag[i] = 0
  }
}


#set the final tables NTG Therm to that of the NTG Therm
finaldev$EvalNTG_therms = NTGinput$NTG.Therms

#for each of the measures, if the use categories are equal, the flag is 0, otherwise it's 1
for(i in 1:dim(NTGinput)[1])
{
  if( (claims$NTGRTherm[i] != NTGinput$NTG.Therms[i]) | (is.na(claims$NTGRTherm[i]) & !is.na(NTGinput$NTG.Therms[i])) | (!is.na(claims$NTGRTherm[i]) & is.na(NTGinput$NTG.Therms[i])) )
  {
    finaldev$EvalNTG_thermsFlag[i] = 1
  }
  else
  {
    finaldev$EvalNTG_thermsFlag[i] = 0
  }
}


#set the final tables EUL ID to that of the EUL ID
finaldev$EvalEUL_ID = EULinput$EUL.ID

#for each of the measures, if the use categories are equal, the flag is 0, otherwise it's 1
for(i in 1:dim(EULinput)[1])
{
  if((is.na(claims$EUL_ID[i]) & !is.na(EULinput$EUL.ID[i])))
  {
   finaldev$EvalEUL_IDFlag = 1
  }
  else
  {
    finaldev$EvalEUL_IDFlag = 0 
  }
}


#set the final tables EUL Yrs to that of the EUL Yrs & the flag to the flag in the RUL & EUL file
finaldev$EvalEUL_Yrs = EULinput$EUL.Years
finaldev$EvalEUL_YrsFlag = EULinput$EUL.Change.Flag


#set the final Table RUL to that of the RUL Yrs & the flag to the flag in the RUL & EUL file
finaldev$EvalRUL_Yrs = EULinput$RUL.Years
finaldev$EvalRUL_YrsFlag = EULinput$RUL.Change.Flag

#look for "2017" in the Installation Date Column of the claims database
yrchange = grepl("2017", claims$InstallationDate, fixed = FALSE)

#look for these projects in the Claim ID column of the claims pull. These are exceptions and will be set to 2017 installation date based on the email
chngproj = grepl("SCE-2017-Q2-0077563|SCE-2017-Q2-0077564|SCE-2017-Q2-0077601|SCE-2017-Q3-0000033", claims$ClaimID, fixed = FALSE)

#set the flag to 1 if the installation date is not 2017, and set to 0 if the installation date is 2017
for(i in 1:length(yrchange))
{
  if( (yrchange[i] == TRUE) | (chngproj[i] == TRUE))
  {
    finaldev$`Install Flag`[i] = 0
  }
  else
  {
    finaldev$`Install Flag`[i] = 1
  }
}


#set working directory to this
setwd("Z:/Folders/CPUC10 (Group D - Custom EM&V)/4 Deliverables/00 - 2017 Evaluation/Database Review/E350 Data Additions/Raw Data Files")

#write out the final final
write.csv(finaldev, "Report Data DB Review.csv")













