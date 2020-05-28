#set working directory to the Citrix folder
setwd("Z:/Folders/CPUC10 (Group D - Custom EM&V)/4 Deliverables/00 - 2017 Evaluation/Database Review/E350 Data Additions/Raw Data Files")

#read in the claims database
claims = read.csv("Custom Claims Extract.csv")

#read in the unique measures database for 2017 measures only
uniquemeas = read.csv("Unique ExAnte NTG Measures.csv")

#set a new data frame with measure names, Use Cat, Use SubCat, Tech Group, Tech Type, NTG Value and EUL Value
newdata = as.data.frame(matrix(nrow = dim(claims[1]), ncol = 11))
colnames(newdata) = c("Claims ID","Measure Description",  "Use Category", "Use SubCategory", "Tech Group", "Tech Type", "Delivery Type", "NTG ID", " NTG kWh", "NTG Therms", "Change Flag")

#set the claim ID to the correct claim ID
newdata$`Claims ID` = claims$ClaimID


#Lighting Section
#looks for matches to "LED", "light", "Light", "lighting" and "Lighting"
lightingmatch = (grepl("LED|CFL", claims$MeasDescription, fixed = TRUE) | grepl("light|Light|lighting|Lighting|T8|T5|CFL|fixture|fixture replacement|Fixture|Fixture Replacement|Lighting control|Lighting Control|Int-Control|Ext-Control|Occupancy|occupancy|Daylighting|daylighting|Interior|Exterior", claims$MeasDescription, fixed = FALSE))

# Outputs the row number and measure which matches the description
for(i in which(lightingmatch))
{
  #Puts the Measure Description in the right column, and then makes the Use category Lighting
  newdata$`Measure Description`[i] = as.character(claims$MeasDescription[i])
  newdata$`Use Category`[i] = "Lighting"
  newdata$`Delivery Type`[i] = as.character(claims$DeliveryType[i])
}

#Building Envelope Section

#looks for matches to Envelope, SHGC, Insulation, Weatherization
buildenvmatch = (grepl("Envelope|SHGC|Insulation|Weatherization|Shell|shell|windows|Windows|Fenestration|fenestration", claims$MeasDescription, fixed = FALSE))

for(i in which(buildenvmatch))
{
  #Puts the Measure Description in te right column, and then makes the Use category Food Service
  newdata$`Measure Description`[i] = as.character(claims$MeasDescription[i])
  newdata$`Use Category`[i] = "BldgEnv"
  newdata$`Delivery Type`[i] = as.character(claims$DeliveryType[i])
}

#Commercial Refrigeration Section

#looks for matches to Refrigeration, refrigerated, defrost or evaporator
comfrigmatch = (grepl("Gasket|gasket|Curtain|curtain", claims$MeasDescription, fixed = FALSE))

for(i in which(comfrigmatch))
{
  #Puts the Measure Description in te right column, and then makes the Use category Food Service
  newdata$`Measure Description`[i] = as.character(claims$MeasDescription[i])
  newdata$`Use Category`[i] = "ComRefrig"
  newdata$`Delivery Type`[i] = as.character(claims$DeliveryType[i])
}

#Plug & App Section

#looks for matches to plug load or Appliance load
plugmatch = (grepl("television|Television", claims$MeasDescription, fixed = FALSE))

for(i in which(plugmatch))
{
  #Puts the Measure Description in te right column, and then makes the Use category Food Service
  newdata$`Measure Description`[i] = as.character(claims$MeasDescription[i])
  newdata$`Use Category`[i] = "AppPlug"
  newdata$`Delivery Type`[i] = as.character(claims$DeliveryType[i])
}

#Process Distribution Section

#looks for matches to New pump, efficeint motor, oil well, motor, VFD, VSD
procdistmatch = (grepl("Pipe insultaion|Pipe Insulation| pipe insulation|Oil Well|oil well| Oil well", claims$MeasDescription, fixed = FALSE))

for(i in which(procdistmatch))
{
  #Puts the Measure Description in te right column, and then makes the Use category Food Service
  newdata$`Measure Description`[i] = as.character(claims$MeasDescription[i])
  newdata$`Use Category`[i] = "ProcDist"
  newdata$`Delivery Type`[i] = as.character(claims$DeliveryType[i])
}

#Process Heat Section

#looks for matches to steam, heat, boiler
procheatmatch = (grepl("Steam Trap|steam trap|Steam trap", claims$MeasDescription, fixed = FALSE))

for(i in which(procheatmatch))
{
  #Puts the Measure Description in te right column, and then makes the Use category Food Service
  newdata$`Measure Description`[i] = as.character(claims$MeasDescription[i])
  newdata$`Use Category`[i] = "ProcHeat"
  newdata$`Delivery Type`[i] = as.character(claims$DeliveryType[i])
}

#Service Section

#looks for matches to comisissioning, retreocommissioning, SHW, DHW,  HW
servmatch = (grepl("Commissioning-Monitoring|Commissioning-RCX Recode|Commissioning-RCX Repair|Commissioning-RCX Reset|Retrocomissioning|retrocomissioning", claims$MeasDescription, fixed = FALSE))

for(i in which(servmatch))
{
  #Puts the Measure Description in te right column, and then makes the Use category Food Service
  newdata$`Measure Description`[i] = as.character(claims$MeasDescription[i])
  newdata$`Use Category`[i] = "Service"
  newdata$`Delivery Type`[i] = as.character(claims$DeliveryType[i])
}

#HVAC Section

#looks for matches to AC, heat pump, package, chiller
hvacmatch = (grepl("Chiller|HP|Heat Pump|Package|package|A/C|a/c|A-C", claims$MeasDescription, fixed = FALSE))

for(i in which(hvacmatch))
{
  #Puts the Measure Description in the right column, and then makes the Use category Food Service
  newdata$`Measure Description`[i] = as.character(claims$MeasDescription[i])
  newdata$`Use Category`[i] = "HVAC"
  newdata$`Delivery Type`[i] = as.character(claims$DeliveryType[i])
}

#for each thing that hasn't yet been classified
anymatch = is.na(newdata$`Measure Description`)

for(i in which(anymatch))
{
  #keep the original measure description, use category, use subcategory, tech group, delivery type, and tech type
  newdata$`Measure Description`[i] = as.character(claims$MeasDescription[i])
  newdata$`Use Category`[i] = as.character(claims$UseCategory[i])
  newdata$`Delivery Type`[i] = as.character(claims$DeliveryType[i]) 
  newdata$`Use SubCategory`[i] = as.character(claims$UseSubCategory[i])
  newdata$`Tech Group`[i] = as.character(claims$TechGroup[i]) 
  newdata$`Tech Type`[i] = as.character(claims$TechType[i])
  newdata$`NTG ID`[i] = as.character(claims$NTG_ID[i])
  newdata$` NTG kWh`[i] = claims$NTGRkWh[i]
  newdata$`NTG Therms`[i] = claims$NTGRTherm[i]
}

#HVAC Sub Category Classification

#Chiller Classification

#looks only for matches to chiller which have been categorized as HVAC
chillers = (grepl("Chiller|chiller", newdata$`Measure Description`, fixed = FALSE) & newdata$`Use Category` == "HVAC")

#look for the Chiller measures in the HVAC Category
uniquechiller = (grepl("Chiller|chiller", uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$UseCategoryLookUp == "HVAC")

#for all of the chiller measures
for(i in which(chillers))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniquechiller)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniquechiller)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniquechiller)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniquechiller)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniquechiller)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniquechiller)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniquechiller)]
}

#Air Conditioners and Heat Pump Classification

#looks only for matches to chiller which have been categorized as HVAC
ACHP = (grepl("A-C|A/C|HP|Heat Pump|heat pump", newdata$`Measure Description`,fixed = FALSE) & newdata$`Use Category` == "HVAC")

#looks for A/C or heat pumps in the HVAC Use Category
uniqueACHP = (grepl("A-C|A/C|HP|Heat Pump|heat pump", uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$UseCategoryLookUp == "HVAC")

#for all of the AC and HP measures
for(i in which(ACHP))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniqueACHP)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniqueACHP)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniqueACHP)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniqueACHP)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniqueACHP)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniqueACHP)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniqueACHP)]
}

#Other Package HVAC Classification

#for any leftover package HVAC units(not a chiller, AC or HP, but has been classified as HVAC)
otherHVAC = (grepl("Package|package", newdata$`Measure Description`, fixed = FALSE) & newdata$`Use Category` == "HVAC" & newdata$`Delivery Type` == "PreRebUp")

#find the other HVAC system unique measure
uniqueotherHVAC = (grepl("Package|package", uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$UseCategoryLookUp == "HVAC" & uniquemeas$Measure_Delivery == "PreRebUp")

#for all other HVAC units
for(i in which(otherHVAC))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniqueotherHVAC)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniqueotherHVAC)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniqueotherHVAC)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniqueotherHVAC)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniqueotherHVAC)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniqueotherHVAC)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniqueotherHVAC)]
}

#all other HVAC Classification

#all other HVAC will have nothing in the Use Subcategory, and the Category will be HVAC
anyHVAC = is.na(newdata$`NTG ID`) & newdata$`Use Category` == "HVAC"
for(i in which(anyHVAC))
{
  #keep the original measure description, use category, use subcategory, tech group, delivery type, and tech type
  newdata$`Measure Description`[i] = as.character(claims$MeasDescription[i])
  newdata$`Use Category`[i] = as.character(claims$UseCategory[i])
  newdata$`Delivery Type`[i] = as.character(claims$DeliveryType[i]) 
  newdata$`Use SubCategory`[i] = as.character(claims$UseSubCategory[i])
  newdata$`Tech Group`[i] = as.character(claims$TechGroup[i]) 
  newdata$`Tech Type`[i] = as.character(claims$TechType[i])
  newdata$`NTG ID`[i] = as.character(claims$NTG_ID[i])
  newdata$` NTG kWh`[i] = claims$NTGRkWh[i]
  newdata$`NTG Therms`[i] = claims$NTGRTherm[i]
}


#Process Distribution SubCategory Section

#Oil Well Controls

#look for the oil well in the measure description
oilwells = (grepl("Oil Well|Oil well|oil well", newdata$`Measure Description`, fixed = FALSE) & newdata$`Use Category` == "ProcDist")

#look for oil well in the unique measure description
uniqueoilwells = (grepl("Oil Well|Oil well|oil well", uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$UseCategoryLookUp == "ProcDist")

#for all of the oil well measures
for(i in which(oilwells))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniqueoilwells)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniqueoilwells)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniqueoilwells)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniqueoilwells)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniqueoilwells)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniqueoilwells)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniqueoilwells)]
}

#Custom Pipe Insulation
custpipe = grepl("Pipe Insulation|pipe insulation|Pipe insulation",newdata$`Measure Description`, fixed = FALSE) & newdata$`Delivery Type` == "CustIncent" & newdata$`Use Category` == "ProcDist"

#unique custom pipe measure
uniquecustpipe = grepl("Pipe Insulation|pipe insulation|Pipe insulation",uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$Measure_Delivery == "CustIncent" & uniquemeas$UseCategoryLookUp == "ProcDist"

#for all of the custom pipe insulation measures
for(i in which(custpipe))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniquecustpipe)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniquecustpipe)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniquecustpipe)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniquecustpipe)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniquecustpipe)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniquecustpipe)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniquecustpipe)]
}

#Custom Pipe Insulation
deempipe = grepl("Pipe Insulation|pipe insulation|Pipe insulation", newdata$`Measure Description`, fixed = FALSE) & newdata$`Delivery Type` != "CustIncent" & newdata$`Use Category` == "ProcDist"

#unique custom pipe measure
uniquedeempipe = grepl("Pipe Insulation|pipe insulation|Pipe insulation",uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$Measure_Delivery != "CustIncent" & uniquemeas$UseCategoryLookUp == "ProcDist"

#for all of the deemed pipe insulation measures
for(i in which(deempipe))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniquedeempipe)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniquedeempipe)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniquedeempipe)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniquedeempipe)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniquedeempipe)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniquedeempipe)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniquedeempipe)]
}

#all other ProcDist will have nothing in the NTG ID, and the Category will be ProcDist
anyprocdist = is.na(newdata$`NTG ID`) & newdata$`Use Category` == "ProcDist"
for(i in which(anyprocdist))
{
  #keep the original measure description, use category, use subcategory, tech group, delivery type, and tech type
  newdata$`Measure Description`[i] = as.character(claims$MeasDescription[i])
  newdata$`Use Category`[i] = as.character(claims$UseCategory[i])
  newdata$`Delivery Type`[i] = as.character(claims$DeliveryType[i]) 
  newdata$`Use SubCategory`[i] = as.character(claims$UseSubCategory[i])
  newdata$`Tech Group`[i] = as.character(claims$TechGroup[i]) 
  newdata$`Tech Type`[i] = as.character(claims$TechType[i])
  newdata$`NTG ID`[i] = as.character(claims$NTG_ID[i])
  newdata$` NTG kWh`[i] = claims$NTGRkWh[i]
  newdata$`NTG Therms`[i] = claims$NTGRTherm[i]
}

#BUILDING ENVELOPE Use SubCategory Classification

#find heat curtain and delivery type is custom incentive
custheat = grepl("heat curtain|Heat curtain|Heat Curtain", newdata$`Measure Description`, fixed = FALSE) & newdata$`Delivery Type` == "CustIncent" & newdata$`Use Category` == "BldgEnv"

#unique heat curtain and delivery type is custom incentive
uniquecustheat = grepl("heat curtain|Heat curtain|Heat Curtain",uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$Measure_Delivery == "CustIncent" & uniquemeas$UseCategoryLookUp == "BldgEnv"

#for all of the custheat measures
for(i in which(custheat))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniquecustheat)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniquecustheat)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniquecustheat)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniquecustheat)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniquecustheat)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniquecustheat)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniquecustheat)]
}

#find heat curtain and delivery type is not custom incentive
deemheat = grepl("heat curtain|Heat curtain|Heat Curtain", newdata$`Measure Description`, fixed = FALSE) & newdata$`Delivery Type` != "CustIncent" & newdata$`Use Category` == "BldgEnv"

#unique heat curtain and delivery type is not custom incentive
uniquedeemheat = grepl("heat curtain|Heat curtain|Heat Curtain",uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$Measure_Delivery != "CustIncent" & uniquemeas$UseCategoryLookUp == "BldgEnv"

#for all of the custheat measures
for(i in which(deemheat))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniquedeemheat)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniquedeemheat)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniquedeemheat)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniquedeemheat)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniquedeemheat)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniquedeemheat)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniquedeemheat)]
}

#find infrared film and delivery type is custom incentive
custir = grepl("infrared|Infrared|IR", newdata$`Measure Description`, fixed = FALSE) & newdata$`Delivery Type` == "CustIncent" & newdata$`Use Category` == "BldgEnv"

#unique infrared and delivery type is custom incentive
uniquecustir = grepl("infrared|Infrared|IR",uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$Measure_Delivery == "CustIncent" & uniquemeas$UseCategoryLookUp == "BldgEnv"

#for all of the custir measures
for(i in which(custir))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniquecustir)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniquecustir)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniquecustir)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniquecustir)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniquecustir)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniquecustir)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniquecustir)]
}

#find infrared film and delivery type is not custom incentive
deemir = grepl("infrared|Infrared|IR", newdata$`Measure Description`, fixed = FALSE) & newdata$`Delivery Type` != "CustIncent" & newdata$`Use Category` == "BldgEnv"

#unique infrared film and delivery type is not custom incentive
uniquedeemheat = grepl("infrared|Infrared|IR",uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$Measure_Delivery != "CustIncent" & uniquemeas$UseCategoryLookUp == "BldgEnv"

#for all of the deemir measures
for(i in which(deemir))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniquedeemir)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniquedeemir)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniquedeemir)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniquedeemir)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniquedeemir)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniquedeemir)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniquedeemir)]
}

#all other BldgEnv will have nothing in the NTG ID, and the Category will be BldgEnv
anybldgenv = is.na(newdata$`NTG ID`) & newdata$`Use Category` == "BldgEnv"

for(i in which(anybldgenv))
{
  #keep the original measure description, use category, use subcategory, tech group, delivery type, and tech type
  newdata$`Measure Description`[i] = as.character(claims$MeasDescription[i])
  newdata$`Use Category`[i] = as.character(claims$UseCategory[i])
  newdata$`Delivery Type`[i] = as.character(claims$DeliveryType[i]) 
  newdata$`Use SubCategory`[i] = as.character(claims$UseSubCategory[i])
  newdata$`Tech Group`[i] = as.character(claims$TechGroup[i]) 
  newdata$`Tech Type`[i] = as.character(claims$TechType[i])
  newdata$`NTG ID`[i] = as.character(claims$NTG_ID[i])
  newdata$` NTG kWh`[i] = claims$NTGRkWh[i]
  newdata$`NTG Therms`[i] = claims$NTGRTherm[i]
}


# Proc Heat Use SubCategory Classification

#find ind steam traps measures
indheat = grepl("Industrial|industrial", newdata$`Measure Description`, fixed = FALSE) & newdata$`Use Category` == "ProcHeat"

#unique ind steam trap measure
uniqueindheat = grepl("Industrial|industrial",uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$UseCategoryLookUp == "ProcHeat"

#for all of the custir measures
for(i in which(indheat))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniqueindheat)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniqueindheat)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniqueindheat)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniqueindheat)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniqueindheat)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniqueindheat)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniqueindheat)]
}

#find ind steam traps
comheat = grepl("Commercial|commercial", newdata$`Measure Description`, fixed = FALSE) & newdata$`Use Category` == "ProcHeat"

#unique ind steam and delivery type is custom incentive
uniquecomheat = grepl("Commercial|commercial",uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$UseCategoryLookUp == "ProcHeat"

#for all of the custir measures
for(i in which(comheat))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniquecomheat)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniquecomheat)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniquecomheat)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniquecomheat)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniquecomheat)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniquecomheat)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniquecomheat)]
}

#all other ProcHeat will have nothing in the NTG ID, and the Category will be ProcHeat
anyprocheat = is.na(newdata$`NTG ID`) & newdata$`Use Category` == "ProcHeat"
for(i in which(anyprocheat))
{
  #keep the original measure description, use category, use subcategory, tech group, delivery type, and tech type
  newdata$`Measure Description`[i] = as.character(claims$MeasDescription[i])
  newdata$`Use Category`[i] = as.character(claims$UseCategory[i])
  newdata$`Delivery Type`[i] = as.character(claims$DeliveryType[i]) 
  newdata$`Use SubCategory`[i] = as.character(claims$UseSubCategory[i])
  newdata$`Tech Group`[i] = as.character(claims$TechGroup[i]) 
  newdata$`Tech Type`[i] = as.character(claims$TechType[i])
  newdata$`NTG ID`[i] = as.character(claims$NTG_ID[i])
  newdata$` NTG kWh`[i] = claims$NTGRkWh[i]
  newdata$`NTG Therms`[i] = claims$NTGRTherm[i]
}


#Service SubCategory Classification

#find RCA with non up steam delivery
servrca = grepl("adjustment|Adjustment", newdata$`Measure Description`, fixed = FALSE) & newdata$`Delivery Type` != "PreRebDown" & newdata$`Use Category` == "Service"

#unique RCA with non up steam delivery
uniqueservrca = grepl("charge adjustment|Charge Adjustment",uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$Measure_Delivery != "PreRebDown" & uniquemeas$UseCategoryLookUp == "Service"

#for all of the servrca measures
for(i in which(servrca))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniqueservrca)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniqueservrca)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniqueservrca)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniqueservrca)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniqueservrca)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniqueservrca)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniqueservrca)]
}

#all other Service will have nothing in the NTG ID, and the Category will be Service
anyserv = is.na(newdata$`NTG ID`) & newdata$`Use Category` == "Service"
for(i in which(anyserv))
{
  #keep the original measure description, use category, use subcategory, tech group, delivery type, and tech type
  newdata$`Measure Description`[i] = as.character(claims$MeasDescription[i])
  newdata$`Use Category`[i] = as.character(claims$UseCategory[i])
  newdata$`Delivery Type`[i] = as.character(claims$DeliveryType[i]) 
  newdata$`Use SubCategory`[i] = as.character(claims$UseSubCategory[i])
  newdata$`Tech Group`[i] = as.character(claims$TechGroup[i]) 
  newdata$`Tech Type`[i] = as.character(claims$TechType[i])
  newdata$`NTG ID`[i] = as.character(claims$NTG_ID[i])
  newdata$` NTG kWh`[i] = claims$NTGRkWh[i]
  newdata$`NTG Therms`[i] = claims$NTGRTherm[i]
}


#AppPlug SubCategory Classification

#find meaasures with tv
tv = grepl("television|Television|TV|tv", newdata$`Measure Description`, fixed = FALSE) & newdata$`Use Category` == "AppPlug"

#unique RCA with non up steam delivery
uniquetv = grepl("television|Television|TV|tv",uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$UseCategoryLookUp == "AppPlug"

#for all of the servrca measures
for(i in which(tv))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniquetv)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniquetv)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniquetv)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniquetv)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniquetv)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniquetv)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniquetv)]
}

#all other AppPlug will have nothing in the NTG ID, and the Category will be AppPlug
anyplug = is.na(newdata$`NTG ID`) & newdata$`Use Category` == "AppPlug"
for(i in which(anyplug))
{
  #keep the original measure description, use category, use subcategory, tech group, delivery type, and tech type
  newdata$`Measure Description`[i] = as.character(claims$MeasDescription[i])
  newdata$`Use Category`[i] = as.character(claims$UseCategory[i])
  newdata$`Delivery Type`[i] = as.character(claims$DeliveryType[i]) 
  newdata$`Use SubCategory`[i] = as.character(claims$UseSubCategory[i])
  newdata$`Tech Group`[i] = as.character(claims$TechGroup[i]) 
  newdata$`Tech Type`[i] = as.character(claims$TechType[i])
  newdata$`NTG ID`[i] = as.character(claims$NTG_ID[i])
  newdata$` NTG kWh`[i] = claims$NTGRkWh[i]
  newdata$`NTG Therms`[i] = claims$NTGRTherm[i]
}



#ComRefrig SubCategory Classification

#find meaasures with door gasket
door = grepl("door gasket|Door Gasket|Door gasket", newdata$`Measure Description`, fixed = FALSE) & newdata$`Use Category` == "ComRefrig"

#unique door gasket
uniquedoor = grepl("door gasket|Door Gasket|Door gasket",uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$UseCategoryLookUp == "ComRefrig"

#for all of the door measures
for(i in which(door))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniquedoor)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniquedoor)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniquedoor)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniquedoor)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniquedoor)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniquedoor)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniquedoor)]
}

#find measures with door curtain
curt = grepl("door curtain|Door Curtain|Door curtain", newdata$`Measure Description`, fixed = FALSE) & newdata$`Use Category` == "ComRefrig"

#unique door curtain
uniquecurt = grepl("door curtain|Door Curtain|Door curtain",uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$UseCategoryLookUp == "ComRefrig"

#for all of the door measures
for(i in which(curt))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniquecurt)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniquecurt)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniquecurt)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniquecurt)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniquecurt)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniquecurt)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniquecurt)]
}

#all other AppPlug will have nothing in the NTG ID, and the Category will be ComRefrig
anycomfrig = is.na(newdata$`NTG ID`) & newdata$`Use Category` == "ComRefrig"
for(i in which(anycomfrig))
{
  #keep the original measure description, use category, use subcategory, tech group, delivery type, and tech type
  newdata$`Measure Description`[i] = as.character(claims$MeasDescription[i])
  newdata$`Use Category`[i] = as.character(claims$UseCategory[i])
  newdata$`Delivery Type`[i] = as.character(claims$DeliveryType[i]) 
  newdata$`Use SubCategory`[i] = as.character(claims$UseSubCategory[i])
  newdata$`Tech Group`[i] = as.character(claims$TechGroup[i]) 
  newdata$`Tech Type`[i] = as.character(claims$TechType[i])
  newdata$`NTG ID`[i] = as.character(claims$NTG_ID[i])
  newdata$` NTG kWh`[i] = claims$NTGRkWh[i]
  newdata$`NTG Therms`[i] = claims$NTGRTherm[i]
}


#Lighting Subsection Classification

#look for the lighting controls
lightcont = grepl("lighting control|Lighting Control|Lighting control|Lighting control|Control|control", newdata$`Measure Description`, fixed = FALSE) & newdata$`Delivery Type` == "DirInstall"

#find the same unique measure
uniquelightcont = grepl("lighting control|Lighting Control|Lighting control|Lighting control|Control|contro", uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$Measure_Delivery == "DirInstall"

#for all of the lighting control measures
for(i in which(lightcont))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniquelightcont)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniquelightcont)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniquelightcont)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniquelightcont)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniquelightcont)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniquelightcont)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniquelightcont)]
}

#look for the other lighting controls
otherlightcont = grepl("lighting control|Lighting Control|Lighting control|Lighting control|Day lighting|day lighting|daylighting", newdata$`Measure Description`, fixed = FALSE) & newdata$`Delivery Type` != "DirInstall" & newdata$`Use Category` == "Lighting"

#find the same unique measure
uniqueotherlightcont = grepl("lighting control|Lighting Control|Lighting control|Lighting control|Daylighting|day lighting|daylighting", uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$Measure_Delivery != "DirInstall" & uniquemeas$UseCategoryLookUp == "Lighting"

#for all of the lighting control measures
for(i in which(otherlightcont))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniqueotherlightcont)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniqueotherlightcont)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniqueotherlightcont)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniqueotherlightcont)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniqueotherlightcont)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniqueotherlightcont)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniqueotherlightcont)]
}

#all occupancy sensor measures
occsens = grepl("occupancy sensor|Occupancy Sensor|occupancy Sensor|Occupancy sensor", newdata$`Measure Description`, fixed = FALSE) & newdata$`Use Category` == "Lighting"

#find the unique occupancy sensor measures
uniqueoccsens = grepl("occupancy sensor|Occupancy Sensor|occupancy Sensor|Occupancy sensor", uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$UseCategoryLookUp == "Lighting"

#for all of the occupancy sensor measures
for(i in which(occsens))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniqueoccsens)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniqueoccsens)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniqueoccsens)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniqueoccsens)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniqueoccsens)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniqueoccsens)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniqueoccsens)]
}


#all delamping  measures
delamp = grepl("delamp|Delamp", newdata$`Measure Description`, fixed = FALSE) & newdata$`Use Category` == "Lighting"

#find the unique delamping measures
uniquedelamp = grepl("delamp|Delamp",uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$UseCategoryLookUp == "Lighting"

#for all of the delamping measures
for(i in which(delamp))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniquedelamp)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniquedelamp)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniquedelamp)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniquedelamp)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniquedelamp)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniquedelamp)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniquedelamp)]
}

#all t5 measures
t5 = grepl("t5|T5|T-5|t-5", newdata$`Measure Description`, fixed = FALSE) & newdata$`Use Category` == "Lighting"

#find the unique t5 measures
uniquet5 = grepl("t5|T5|T-5|t-5", uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$UseCategoryLookUp == "Lighting"

#for all the t5 measures
for(i in which(t5))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniquet5)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniquet5)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniquet5)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniquet5)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniquet5)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniquet5)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniquet5)]
}

#find the high bay measures
highbay = grepl("High Bay|high bay|High bay|high Bay|HID|hid", newdata$`Measure Description`, fixed = FALSE) & newdata$`Use Category` == "Lighting"

#find the unique high bay measures
uniquehighbay = grepl("High Bay|high bay|High bay|high Bay|HID|hid", uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$UseCategoryLookUp == "Lighting"

#for all of the high bay measures
for(i in which(highbay))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniquehighbay)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniquehighbay)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniquehighbay)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniquehighbay)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniquehighbay)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniquehighbay)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniquehighbay)]
}

#find the other linear fluorescent measures
otherlf = grepl("Linear Fluorescent", newdata$`Measure Description`, fixed = FALSE) & newdata$`Use Category` == "Lighting"

#find the unique linear fluorecent measure when no other one applies
uniqueotherlf = grepl("Fluorescent: measures not", uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$UseCategoryLookUp == "Lighting"

#for all of the other linear fluorescent measures
for(i in which(otherlf))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniqueotherlf)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniqueotherlf)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniqueotherlf)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniqueotherlf)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniqueotherlf)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniqueotherlf)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniqueotherlf)]
}

#find CFL screw in measures
cflscrew = grepl("CFL-screw|CFL screw", newdata$`Measure Description`, fixed = FALSE) & newdata$`Use Category` == "Lighting" & claims$NTG_ID == "NonRes-sAll-mCFL"

#find the unique CFL screw in measure
uniquecflscrew = grepl("CFL-screw|CFL screw", uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$UseCategoryLookUp == "Lighting" & uniquemeas$NTG_ID == "NonRes-sAll-mCFL"

#for all of the CFL screw in measures
for(i in which(cflscrew))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniquecflscrew)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniquecflscrew)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniquecflscrew)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniquecflscrew)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniquecflscrew)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniquecflscrew)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniquecflscrew)]
}

#find CFL measures with PreRebUp Delivery
precflscrew = grepl("CFL|cfl", newdata$`Measure Description`, fixed = FALSE) & newdata$`Use Category` == "Lighting" & newdata$`Delivery Type` == "PreRebUp" &!which(cflscrew)

#find the unique CFL measure with PreRebUp Delivery
uniqueprecflscrew = grepl("CFL|cfl", uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$UseCategoryLookUp == "Lighting" & uniquemeas$Measure_Delivery == "PreRebUp"

#for all of the CFL measures with PreRebUp Delivery
for(i in which(precflscrew))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniqueprecflscrew)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniqueprecflscrew)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniqueprecflscrew)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniqueprecflscrew)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniqueprecflscrew)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniqueprecflscrew)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniqueprecflscrew)]
}

#find all other CFL measures
othercfl = grepl("CFL|cfl", newdata$`Measure Description`, fixed = FALSE) & newdata$`Use Category` == "Lighting"

#find the un
uniqueothercfl = grepl("CFLs: deemed", uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$UseCategoryLookUp == "Lighting"

#for all of the other linear fluorescent measures
for(i in which(othercfl))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniqueothercfl)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniqueothercfl)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniqueothercfl)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniqueothercfl)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniqueothercfl)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniqueothercfl)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniqueothercfl)]
}

#find all screw in & reflector measures
screw = grepl("A-lamp|a-lamp|Reflector|reflector", newdata$`Measure Description`, fixed = FALSE) & newdata$`Use Category` == "Lighting"

#find the unique screw in & reflector measures
uniquescrew = grepl("reflector", uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$UseCategoryLookUp == "Lighting"

#for all screw in & reflector measures
for(i in which(screw))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniquescrew)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniquescrew)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniquescrew)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniquescrew)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniquescrew)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniquescrew)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniquescrew)]
}

#find all LED with NonUpStrm delivery measures
nonupstrmLED = grepl("LED", newdata$`Measure Description`, fixed = FALSE) & newdata$`Use Category` == "Lighting" & newdata$`Delivery Type` == "NonUpStrm"

#find the unique LED with NonUpStrm delivery measures
uniquenonupstrmLED = grepl("LED", uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$UseCategoryLookUp == "Lighting" & uniquemeas$Measure_Delivery == "NonUpStrm"

#for all LED with NonUpStrm delivery measures
for(i in which(nonupstrmLED))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniquenonupstrmLED)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniquenonupstrmLED)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniquenonupstrmLED)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniquenonupstrmLED)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniquenonupstrmLED)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniquenonupstrmLED)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniquenonupstrmLED)]
}


#find all LED with NonUpStrm delivery measures
upstrmLED = grepl("LED", newdata$`Measure Description`, fixed = FALSE) & newdata$`Use Category` == "Lighting" & newdata$`Delivery Type` != "NonUpStrm"

#find the unique LED with NonUpStrm delivery measures
uniqueupstrmLED = grepl("specialty LED", uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$UseCategoryLookUp == "Lighting" & uniquemeas$Measure_Delivery != "NonUpStrm"

#for all LED with NonUpStrm delivery measures
for(i in which(upstrmLED))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniqueupstrmLED)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniqueupstrmLED)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniqueupstrmLED)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniqueupstrmLED)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniqueupstrmLED)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniqueupstrmLED)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniqueupstrmLED)]
}

#all other lighting will have nothing in the NTG ID, and the Category will be Lighting
anylight = is.na(newdata$`NTG ID`) & newdata$`Use Category` == "Lighting"
for(i in which(anylight))
{
  #keep the original measure description, use category, use subcategory, tech group, delivery type, and tech type
  newdata$`Measure Description`[i] = as.character(claims$MeasDescription[i])
  newdata$`Use Category`[i] = as.character(claims$UseCategory[i])
  newdata$`Delivery Type`[i] = as.character(claims$DeliveryType[i]) 
  newdata$`Use SubCategory`[i] = as.character(claims$UseSubCategory[i])
  newdata$`Tech Group`[i] = as.character(claims$TechGroup[i]) 
  newdata$`Tech Type`[i] = as.character(claims$TechType[i])
  newdata$`NTG ID`[i] = as.character(claims$NTG_ID[i])
  newdata$` NTG kWh`[i] = claims$NTGRkWh[i]
  newdata$`NTG Therms`[i] = claims$NTGRTherm[i]
}


#for all of the measures which were not speifically in one use category
for(i in which(anymatch))
{
  #keep the original measure description, use category, use subcategory, tech group, delivery type, and tech type
  newdata$`Measure Description`[i] = as.character(claims$MeasDescription[i])
  newdata$`Use Category`[i] = as.character(claims$UseCategory[i])
  newdata$`Delivery Type`[i] = as.character(claims$DeliveryType[i]) 
  newdata$`Use SubCategory`[i] = as.character(claims$UseSubCategory[i])
  newdata$`Tech Group`[i] = as.character(claims$TechGroup[i]) 
  newdata$`Tech Type`[i] = as.character(claims$TechType[i])
  newdata$`NTG ID`[i] = as.character(claims$NTG_ID[i])
  newdata$` NTG kWh`[i] = claims$NTGRkWh[i]
  newdata$`NTG Therms`[i] = claims$NTGRTherm[i]
}



#for all school measures
school = grepl("School|K-12|school|College|college|community|Community", newdata$`Measure Description`, fixed = FALSE) & newdata$`NTG ID` == "K-12School-ComCollege"

#find the unique school measure 
uniqueschool = grepl("School|K-12|school|College|college|community|Community", uniquemeas$NTG_Measure_Type, fixed = FALSE) & uniquemeas$NTG_ID == "K-12School-ComCollege"

#for all schools
for(i in which(uniqueschool))
{
  #set the Use Subcategory, Tech group, tech type, Delivery type, NTG ID, and both NTG (therm and kWh) absed on the measure
  newdata$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategoryLookUp[which(uniqueschool)])
  newdata$`Tech Group`[i] = as.character(uniquemeas$TechGroupLookUp[which(uniqueschool)])
  newdata$`Tech Type`[i] = as.character(uniquemeas$TechTypeLookUp[which(uniqueschool)])
  newdata$`Delivery Type`[i] = as.character(uniquemeas$Measure_Delivery[which(uniqueschool)])
  newdata$`NTG ID`[i] = as.character(uniquemeas$NTG_ID[which(uniqueschool)])
  newdata$`NTG Therms`[i] = uniquemeas$NTGR_therm[which(uniqueschool)]
  newdata$` NTG kWh`[i] = uniquemeas$NTGR_kWh[which(uniqueschool)]
}

for(i in 1:dim(claims)[1])
{
  if(claims$NTGRkWh[i]== newdata$` NTG kWh`[i] & claims$NTGRTherm[i] == newdata$`NTG Therms`[i])
  {
    newdata$`Change Flag`[i] = 0
  }
  else
  {
    newdata$`Change Flag`[i] = 1
  }
}


#create a list for all of the matching done earlier
doubles = list(buildenvmatch,comfrigmatch,hvacmatch,lightingmatch,plugmatch,procdistmatch,procheatmatch,servmatch, anymatch)

#names for the respective lists to be used in the final output if there is any double claimins (one measure appears in more than one list)
names = c("Building Envelope", "Commercial Refrigeration", "HVAC", "Lighting", "Plug & Appliance Load", "Process Distribution", "Process Heat", "Service", "Any")

#for each of the measure descriptions, see if it appears twice in any non-"Any" lists. If so, print out the measure description and the 2 use categories it is classified as
for(i in 1:(length(doubles)-2))
{
  for(j in i+1)
  {
    for(k in 1:length(claims$MeasDescription))
    {
      if(isTRUE(doubles[[i]][k]) & isTRUE(doubles[[j]][k]))
      {
        cat(as.character(newdata$`Measure Description`[k]) ,"is classified as both", names[i], "and", names[j],".\n")
      }
    }
  }
}


#set working directory to this
setwd("Z:/Folders/CPUC10 (Group D - Custom EM&V)/4 Deliverables/00 - 2017 Evaluation/Database Review/E350 Data Additions/Raw Data Files")

#write out the file
write.csv(newdata, file = "NTG Export.csv")


