install.packages("lubridate")
library(lubridate)

#start timer
start.time = Sys.time()

#set working directory to the Citrix folder
setwd("Z:/Favorites/CPUC10 (Group D - Custom EM&V)/4 Deliverables/00 - 2017 Evaluation/ESPI input prep/Final ATR Code")

#read in the custom ATR filled file
ATRfilled = read.csv("ATR_2017.csv")
bind = as.data.frame(matrix(nrow = dim(ATRfilled)[1], ncol = 12), row.names = FALSE)
colnames(bind) <- c("Eval_NTG_Use.Category", "Eval_NTG_Use.SubCategory", "Eval_NTG_Tech.Type", "Eval_NTG_Tech.Group", "Eval.NTG_ID", "Eval.EUL_ID", "Eval_EUL_Use.Category", "Eval_EUL_Use.SubCategory", "Eval_EUL_Tech.Type", "Eval_EUL_Tech.Group", "RUL_Change_Flag", "EUL_Change_Flag")
ATRfilled = cbind(ATRfilled, bind)

#Let R read the dates as formatted in Excel
ATRfilled$InstallationDate = as.Date(ATRfilled$InstallationDate, format = '%m/%d/%Y')

#read in the claims database
claims = read.csv("Custom Claims Extract.csv")

#read in the NTG File
NTGinput = read.csv("NTG Export.csv")

#read in the RUL & EUL file (Change to "Original EUL & RUL Output to generate results Gina wants)
EULinput = read.csv("EUL & RUL Export.csv")

#read in the exempted projects
exempt = read.csv("PA Exempt Claims.csv")

#for all of the measures
for(i in 1:dim(ATRfilled)[1])
{
  #set the field equal to a concatenation of year of the system date - 2 and "_NoneCustom"
  ATRfilled$EvalReportName[i] = ("2017_NoneCustom")
  
  #If the ATR ID is matched in the claims database
  if(ATRfilled$ClaimId[i] %in% claims$ClaimID)
  {
    #set that Report Group to 2017 Savings Review
    ATRfilled$EvalStdReportGroup[i] = "2017_Savings_Review"
  }
  #if it is not matched in the claims database
  else
  {
    #set the Report Group to None
    ATRfilled$EvalStdReportGroup[i] = "None"
  }
  
  #if the measure's Report Group is "none"
  if(ATRfilled$EvalStdReportGroup[i] == "None")
  {
    
    #set the net and gross pass thorugh to 1
    ATRfilled$EvalNetPassThru[i] = 1
    ATRfilled$EvalGrossPassThru[i] = 1
  }
  #if the Report is "2017_Savings_Report"
  else
  {
    #set the net and gross pass thorugh to 0
    ATRfilled$EvalNetPassThru[i] = 0
    ATRfilled$EvalGrossPassThru[i] = 0
  }
  #if the Report Group is none
  if(ATRfilled$EvalStdReportGroup[i] == "None") 
  {
    #set the ER_EvalExpost value to the ER_IOU reported value
    ATRfilled$ER_EvalExPost[i] = ATRfilled$ER_IOUReported[i]
  }
  #else if the Measure App Type is ER
  else if(ATRfilled$MeasAppType[i] == "ER")
  {
    #set the value to 1
    ATRfilled$ER_EvalExPost[i] = 1
  }
  else
  {
    #set the value to 0
    ATRfilled$ER_EvalExPost[i] = 0
  }
  
  #the EvalInstallation Rate kW, kWh and Therm will all pass through
  ATRfilled$EvalInstallationRatekW[i] = ATRfilled$InstallationRatekW[i]
  ATRfilled$EvalInstallationRatekWh[i] = ATRfilled$InstallationRatekWh[i]
  ATRfilled$EvalInstallationRateTherm[i] = ATRfilled$InstallationRateTherm[i]
  
  
  #if the claim ID is in 2017 or the exemption database, then pass through the NumUnits and the date flag = 0, otherwise the EvalNumUnits is 0 and the date flag = 1 
    if(year(ATRfilled$InstallationDate[i])==2017 || ATRfilled$ClaimId[i] %in% exempt$ClaimID)
   {
      ATRfilled$EvalNumUnits[i] = ATRfilled$NumUnits[i]
      ATRfilled$Install_Date_Flag[i] = 0
    }
    else 
    {
      ATRfilled$EvalNumUnits[i] = 0
      ATRfilled$Install_Date_Flag[i] = 1
    }
  
  #set the Eval 1st and 2nd Baselines for Kw, kWh and Therm to the  Unit values for each of them
  ATRfilled$EvalUnitkW1stBaseline[i] = ATRfilled$UnitkW1stBaseline[i]
  ATRfilled$EvalUnitkWh1stBaseline[i] = ATRfilled$UnitkWh1stBaseline[i]
  ATRfilled$EvalUnitTherm1stBaseline[i] = ATRfilled$UnitTherm1stBaseline[i]
  ATRfilled$EvalUnitkW2ndBaseline[i] = ATRfilled$UnitkW2ndBaseline[i]
  ATRfilled$EvalUnitkWh2ndBaseline[i] = ATRfilled$UnitkWh2ndBaseline[i]
  ATRfilled$EvalUnitTherm2ndBaseline[i] = ATRfilled$UnitTherm2ndBaseline[i]

  #set the Eval RR lifecyle and first year to the values for each of the measures
  ATRfilled$EvalRRFirstYearkW[i] = ATRfilled$RealizationRatekW[i]
  ATRfilled$EvalRRFirstYearkWh[i] = ATRfilled$RealizationRatekWh[i]
  ATRfilled$EvalRRFirstYearTherm[i] = ATRfilled$RealizationRateTherm[i]
  ATRfilled$EvalRRLifecyclekW[i] = ATRfilled$RealizationRatekW[i]
  ATRfilled$EvalRRLifecyclekWh[i] = ATRfilled$RealizationRatekWh[i]
  ATRfilled$EvalRRLifecycleTherm[i] = ATRfilled$RealizationRateTherm[i]
  ATRfilled$EvalNTGRCost[i] = ATRfilled$NTGRCost[i]
  
  #if the claim ID is in the claims database, then set the following variables (After "$") to the evaluated variables in the respective files, otherwise pass through the NTG therm, kW, kWh, ID and EUL
  if(ATRfilled$ClaimId[i] %in% claims$ClaimID)
  {
    
    ATRfilled$EvalNTGRkWh[i] = NTGinput$X.NTG.kWh[which(as.character(NTGinput$Claims.ID) == as.character(ATRfilled$ClaimId[i]))] 
    ATRfilled$EvalNTGRkW[i] = NTGinput$X.NTG.kWh[which(as.character(NTGinput$Claims.ID) == as.character(ATRfilled$ClaimId[i]))]
    ATRfilled$EvalNTGRTherm[i] = NTGinput$NTG.Therms[which(as.character(NTGinput$Claims.ID) == as.character(ATRfilled$ClaimId[i]))]
    ATRfilled$EvalEUL_Yrs[i] = EULinput$EUL.Years[which(as.character(EULinput$Claims.ID) == as.character(ATRfilled$ClaimId[i]))]
    ATRfilled$Eval_NTG_Use.Category[i] = as.character(NTGinput$Use.Category[which(as.character(NTGinput$Claims.ID) == as.character(ATRfilled$ClaimId[i]))])
    ATRfilled$Eval_NTG_Use.SubCategory[i] = as.character(NTGinput$Use.SubCategory[which(as.character(NTGinput$Claims.ID) == as.character(ATRfilled$ClaimId[i]))])
    ATRfilled$Eval_NTG_Tech.Type[i] = as.character(NTGinput$Tech.Type[which(as.character(NTGinput$Claims.ID) == as.character(ATRfilled$ClaimId[i]))])
    ATRfilled$Eval_NTG_Tech.Group[i] = as.character(NTGinput$Tech.Group[which(as.character(NTGinput$Claims.ID) == as.character(ATRfilled$ClaimId[i]))])
    ATRfilled$Eval.NTG_ID[i] = as.character(NTGinput$NTG.ID[which(as.character(NTGinput$Claims.ID) == as.character(ATRfilled$ClaimId[i]))])
    ATRfilled$Eval.EUL_ID[i] = as.character(EULinput$EUL.ID[which(as.character(EULinput$Claims.ID) == as.character(ATRfilled$ClaimId[i]))])
    ATRfilled$Eval_EUL_Use.Category[i] = as.character(EULinput$Use.Category[which(as.character(EULinput$Claims.ID) == as.character(ATRfilled$ClaimId[i]))])
    ATRfilled$Eval_EUL_Use.SubCategory[i] = as.character(EULinput$Use.SubCategory[which(as.character(EULinput$Claims.ID) == as.character(ATRfilled$ClaimId[i]))])
    ATRfilled$Eval_EUL_Tech.Type[i] = as.character(EULinput$Tech.Type[which(as.character(EULinput$Claims.ID) == as.character(ATRfilled$ClaimId[i]))])
    ATRfilled$Eval_EUL_Tech.Group[i] = as.character(EULinput$Tech.Group[which(as.character(EULinput$Claims.ID) == as.character(ATRfilled$ClaimId[i]))])
    }
  else
  {
    ATRfilled$EvalNTGRkWh[i] = ATRfilled$NTGRkWh[i]
    ATRfilled$EvalNTGRkW[i] = ATRfilled$NTGRkW[i]
    ATRfilled$EvalNTGRTherm[i] = ATRfilled$NTGRTherm[i]
    ATRfilled$EvalEUL_Yrs[i] = ATRfilled$EUL_Yrs[i]
    ATRfilled$Eval.NTG_ID[i] = ATRfilled$NTG_ID[i]
  }
  
  #pass through the RUL for the IDs not in our group, if in our group and MEasAppType is ER, then look up and set equal to that, otherwise set to 0
  if(ATRfilled$EvalStdReportGroup[i] == "None")
  {
    ATRfilled$EvalRUL_Yrs[i] = ATRfilled$RUL_Yrs[i]
  }
  
  else if (ATRfilled$MeasAppType[i] == "ER" & ATRfilled$ClaimId[i] %in% claims$ClaimID)
  {
    ATRfilled$EvalRUL_Yrs[i] = EULinput$RUL.Years[which(as.character(EULinput$Claims.ID) == as.character(ATRfilled$ClaimId[i]))]
  }
  else
  {
    ATRfilled$EvalRUL_Yrs[i] = 0
  }
  
  #calculations for the EvalExPost Gross Variables
  ATRfilled$EvalExPostFirstYearGrosskW[i] = ATRfilled$EvalNumUnits[i]*ATRfilled$EvalUnitkW1stBaseline[i]*ATRfilled$EvalRRFirstYearkW[i]
  ATRfilled$EvalExPostFirstYearGrosskWh[i] = ATRfilled$EvalNumUnits[i]*ATRfilled$EvalUnitkWh1stBaseline[i]*ATRfilled$EvalRRFirstYearkWh[i]
  ATRfilled$EvalExPostFirstYearGrossTherm[i] = ATRfilled$EvalNumUnits[i]*ATRfilled$EvalUnitTherm1stBaseline[i]*ATRfilled$EvalRRFirstYearTherm[i]
  
  #calculations for the EvalExPost Net Variables
  ATRfilled$EvalExPostFirstYearNetkW[i] = ATRfilled$EvalExPostFirstYearGrosskW[i]*(ATRfilled$EvalNTGRkW[i] + .05)
  ATRfilled$EvalExPostFirstYearNetkWh[i] = ATRfilled$EvalExPostFirstYearGrosskWh[i]*(ATRfilled$EvalNTGRkWh[i] + .05)
  ATRfilled$EvalExPostFirstYearNetTherm[i] = ATRfilled$EvalExPostFirstYearGrossTherm[i]*(ATRfilled$EvalNTGRTherm[i] + .05)
  
 
  #Calculations for the Eval ExPost Lifecycle Gross Variables
  
  #if the Measure Application Type is "ER"
  if(ATRfilled$MeasAppType[i] == "ER")
  {
  #use the following method to calculate the Gross Lifecycle Variables
  ATRfilled$EvalExPostLifecycleGrosskW[i] = ATRfilled$EvalRRLifecyclekW[i]*ATRfilled$EvalNumUnits[i]*((ATRfilled$EvalUnitkW1stBaseline[i]*ATRfilled$EvalRUL_Yrs[i]) + (ATRfilled$EvalUnitkW2ndBaseline[i]*(ATRfilled$EvalEUL_Yrs[i]-ATRfilled$EvalRUL_Yrs[i])))
  ATRfilled$EvalExPostLifecycleGrosskWh[i] = ATRfilled$EvalRRLifecyclekWh[i]*ATRfilled$EvalNumUnits[i]*((ATRfilled$EvalUnitkWh1stBaseline[i]*ATRfilled$EvalRUL_Yrs[i]) + (ATRfilled$EvalUnitkWh2ndBaseline[i]*(ATRfilled$EvalEUL_Yrs[i]-ATRfilled$EvalRUL_Yrs[i])))
  ATRfilled$EvalExPostLifecycleGrossTherm[i] = ATRfilled$EvalRRLifecycleTherm[i]*ATRfilled$EvalNumUnits[i]*((ATRfilled$EvalUnitTherm1stBaseline[i]*ATRfilled$EvalRUL_Yrs[i]) + (ATRfilled$EvalUnitTherm2ndBaseline[i]*(ATRfilled$EvalEUL_Yrs[i]-ATRfilled$EvalRUL_Yrs[i])))
  }

  else
  {
    #otherwise, use this method to calculate the Gross Lifecycle variables
    ATRfilled$EvalExPostLifecycleGrosskW[i] = ATRfilled$EvalRRLifecyclekW[i]*ATRfilled$EvalNumUnits[i]*ATRfilled$EvalUnitkW1stBaseline[i]*ATRfilled$EvalEUL_Yrs[i]
    ATRfilled$EvalExPostLifecycleGrosskWh[i] = ATRfilled$EvalRRLifecyclekWh[i]*ATRfilled$EvalNumUnits[i]*ATRfilled$EvalUnitkWh1stBaseline[i]*ATRfilled$EvalEUL_Yrs[i]
    ATRfilled$EvalExPostLifecycleGrossTherm[i] = ATRfilled$EvalRRLifecycleTherm[i]*ATRfilled$EvalNumUnits[i]*ATRfilled$EvalUnitTherm1stBaseline[i]*ATRfilled$EvalEUL_Yrs[i]
  }
  
  #calcualtions of the lifecycle Net Variables
  ATRfilled$EvalExPostLifecycleNetkW[i] = ATRfilled$EvalExPostLifecycleGrosskW[i]*(ATRfilled$EvalNTGRkW[i] + .05)
  ATRfilled$EvalExPostLifecycleNetkWh[i] = ATRfilled$EvalExPostLifecycleGrosskWh[i]*(ATRfilled$EvalNTGRkWh[i] + .05)
  ATRfilled$EvalExPostLifecycleNetTherm[i] = ATRfilled$EvalExPostLifecycleGrossTherm[i]*(ATRfilled$EvalNTGRTherm[i] + .05)

  #check if the exantelifecylce and eval ex post lifecycle are the same for kW, kWh, and Therm
  if(ATRfilled$NTGRkW[i] == ATRfilled$EvalNTGRkW[i])
  {
    ATRfilled$EvalNTG_kW_Flag[i] = 0
  }
  else
  {
    ATRfilled$EvalNTG_kW_Flag[i] = 1
  }
  
  if(ATRfilled$NTGRkWh[i] == ATRfilled$EvalNTGRkWh[i])
  {
    ATRfilled$EvalNTG_kWh_Flag[i] = 0
  }
  else
  {
    ATRfilled$EvalNTG_kWh_Flag[i] = 1
  }
  
  if(ATRfilled$NTGRTherm[i] == ATRfilled$EvalNTGRTherm[i])
  {
    ATRfilled$EvalNTG_Therm_Flag[i] = 0
  }
  else
  {
    ATRfilled$EvalNTG_Therm_Flag[i] = 1
  }
  
  # for all the filled in columns, set the NA's equal to 0
  for(j in 60:98)
  {
    if(is.na(ATRfilled[i,j]))
    {
      ATRfilled[i,j] = 0
    }
    else
    {
      ATRfilled[i,j] = ATRfilled[i,j]
    }
  }
      if(ATRfilled$EvalRUL_Yrs[i] != ATRfilled$RUL_Yrs[i] & ATRfilled$ClaimId[i] %in% claims$ClaimID)
    {
      ATRfilled$RUL_Change_Flag[i] = 1
    }
    else
    {
      ATRfilled$RUL_Change_Flag[i] = 0
    }
  
    if(ATRfilled$EvalEUL_Yrs[i] != ATRfilled$EUL_Yrs[i] & ATRfilled$ClaimId[i] %in% claims$ClaimID)
    {
      ATRfilled$EUL_Change_Flag[i] = 1
    }
    else
    {
      ATRfilled$EUL_Change_Flag[i] = 0
    }
}


############
#run checks on output matrix prior to writing file. These should make sure all of the 








write.csv(ATRfilled, "Test Fill.csv", row.names = FALSE)

Sys.time()- start.time

