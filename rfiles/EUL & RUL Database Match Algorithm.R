#set working directory to the Citrix folder
setwd("Z:/Folders/CPUC10 (Group D - Custom EM&V)/4 Deliverables/00 - 2017 Evaluation/Database Review/E350 Data Additions/Raw Data Files")

#read in the claims database
claims = read.csv("Custom Claims Extract.csv")

#read in the unique measures database for 2017 measures only
uniquemeas = read.csv("Unique EUL & RUL Measures.csv")

#set a new data frame with measure names, Use Cat, Use SubCat, Tech Group, Tech Type, NTG Value and EUL Value
output = as.data.frame(matrix(nrow = dim(claims[1]), ncol = 13))
colnames(output) = c("Claims ID", "Measure Description", "Bldg Type", "Delivery Type", "Use Category", "Use SubCategory", "Tech Group", "Tech Type", "EUL ID", "EUL Years", "RUL Years", "EUL Change Flag", "RUL Change Flag")

#set the claim ID, Measure Description, DElivery Type, and Bldg Type
output$`Claims ID` = claims$ClaimID
output$`Bldg Type` = claims$BldgType
output$`Measure Description` = claims$MeasDescription
output$`Delivery Type` = claims$DeliveryType


#USECATEGORY


#find all measures which contain washer, ozone, copier, power supply, or occupancy
appplug = grepl("washer|Washer|ozone|Ozone|copier|Copier|vending|Vending|Power Supply|Power supply|power supply", output$`Measure Description`, fixed = FALSE)

#for each of those measures
for(i in which(appplug))
{
  #set the Use Category to AppPlug
  output$`Use Category`[i] = "AppPlug"
}

#find all measures which contain roof, floor, window, film
bldgenv = grepl("Floor|floor|Roof|roof|Window|window|Film|film", output$`Measure Description`, fixed = FALSE)

#for all those measures
for(i in which(bldgenv))
{
  #set the use Category to Bldgenv
  output$`Use Category`[i] = "BldgEnv"
}

#find all the measures with compress
compair = grepl("Compress|compress", output$`Measure Description`, fixed = FALSE)

#for all those measures
for(i in which(compair))
{
  #set the use category to compair
  output$`Use Category`[i] = "CompAir"
}


#find all the measures with refrigerat[(or),(ion),(ed)], freezer, ice, cooler, evaporator, anti-sweat, gasket, curtain, door
comrefrig = grepl("Refrigerat|refrigerat|freezer|Freezer|Ice|ice|Cooler|cooler|evaporator|Evaporator|anti-sweat|Antisweat|gasket|Gasket|curatin|Curtain|door|Door", output$`Measure Description`, fixed = FALSE) | (grepl("boiler|Boiler", output$`Measure Description`, fixed = FALSE) & grepl("Space|space", output$`Measure Description`, fixed = FALSE))

#for all those measures
for(i in which(comrefrig))
{
  #set the use category to comRefrig
  output$`Use Category`[i] = "ComRefrig"
}

#find all the measures with oven, fryer, griddle, cooker, cabinet, pot, wrapping
foodserv = grepl("Oven|oven|Fryer|fryer|griddle|Griddle|COoker|cooker|Stock Pot|stock pot|wrapping|wrapping", output$`Measure Description`, fixed = FALSE)

#for all those measures
for(i in which(foodserv))
{
  #set the use category to FoodServ
  output$`Use Category`[i] = "FoodServ"
}

#find all the measures with any words in green
hvac = grepl("Air|air|Fan|fan|Package|package|Heat|heat|VSD|Chiller|chiller|Economizer|econmizer|CO2|Furnace|furnace|setback|Setback|water loop|Water Loop|duct|Duct|Cool Indirect|Quality Maintenance|quality Mainenance|Energy Management System|energy management system|Reducing Overventilation|reducing overventilation|Refrigerant Charge|refrigerant charge|cogged|Cogged", output$`Measure Description`, fixed = FALSE) | (grepl("boiler|Boiler", output$`Measure Description`, fixed = FALSE) & grepl("water|Water|steam|Steam", output$`Measure Description`, fixed = FALSE))

#for all those measures
for(i in which(hvac))
{
  #set the use category to HVAC
  output$`Use Category`[i] = "HVAC"
}

#find all the measures with sprinkler or well pump
irrigate = grepl("Sprinkler|sprinkler|well pump|Well Pump", output$`Measure Description`, fixed = FALSE)

#for all those measures
for(i in which(irrigate))
{
  #set the use category to HVAC
  output$`Use Category`[i] = "Irrigation"
}

#find all the measures with any words in green
lighting = grepl("CFL|Fluorescent|LED|Int|Ext|Lighting|lighting|A Lamp|HID|occupancy|Occupancy|Timeclock|timeclock|incandescent|Incandescent|Cold Cathode|cold cathode", output$`Measure Description`, fixed = FALSE)

#for all those measures
for(i in which(lighting))
{
  #set the use category to Lighting
  output$`Use Category`[i] = "Lighting"
}

#find all the measures with milk or Ag Pump
procdist = grepl("Milk|milk|Ag Pump", output$`Measure Description`, fixed = FALSE)

#for all those measures
for(i in which(procdist))
{
  #set the use category to ProcDist
  output$`Use Category`[i] = "ProcDist"
  output$`Use SubCategory`[i] = "Pumping"
}

#find all the measures with milk or Ag Pump
process = grepl("Variable Speed Drive on Process Fan Control", output$`Measure Description`, fixed = TRUE) | (grepl("Boiler|boiler", output$`Measure Description`, fixed = FALSE) & grepl("VFD|vfd", output$`Measure Description`, fixed = FALSE))

uniqueprocess = grepl("Variable Speed Drive on Process Fan Control", uniquemeas$Description, fixed = TRUE)

#for all those measures
for(i in which(process))
{
  #set the use category to Process, and assign the rest based on the unique Process measure
  output$`Use Category`[i] = as.character(uniquemeas$UseCategory[uniqueprocess])
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueprocess])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueprocess])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueprocess])
  output$`EUL Years`[i] = claims$EUL_Yrs[i]
  output$`RUL Years`[i] = claims$RUL_Yrs[i]
}

#find all the measures with milk or Ag Pump
procheat = grepl("Tank Wrap|tank wrap|Steam Trap|Steam trap|Greenhouse|greenhouse|compressor heat|Compressor Heat", output$`Measure Description`, fixed = FALSE)

#for all those measures
for(i in which(procheat))
{
  #set the use category to ProcHeat
  output$`Use Category`[i] = "ProcHeat"
}

#find all the measures with words in green
procrefrig = grepl("Refrigeration Upgrades|Wine Tank|wine tank|refrigeration upgrades|Refrigeration Scroll|refrigeration scroll|Refrigeration Insulation|refrigeration insulation|Pre-Cooler|pre-cooler", output$`Measure Description`, fixed = FALSE)

#for all those measures
for(i in which(procrefrig))
{
  #set the use category to ProcRefrig
  output$`Use Category`[i] = "ProcRefrig"
}

#find all the measures with words in green
recreate = grepl("Pool|pool", output$`Measure Description`, fixed = FALSE)

#for all those measures
for(i in which(recreate))
{
  #set the use category to Recreate
  output$`Use Category`[i] = "Recreate"
  output$`Use SubCategory`[i] = "Pool"
}

#find all the measures with words in green
service = grepl("Retrocommissioning|retrocommissioning|Coils|coils|Tune-up|tune-up|Tune up|tune up", output$`Measure Description`, fixed = FALSE)

#for all those measures
for(i in which(service))
{
  #set the use category to Service
  output$`Use Category`[i] = "Service"
}

#find all the measures with words in green
shw = grepl("water heat|Water Heat|aerators|Aerators|Water heat|Low flow|low flow|Low Flow|timeclock retorfit|Timeclock retrofit|Timeclock Retrofit", output$`Measure Description`, fixed = FALSE)

#for all those measures
for(i in which(shw))
{
  #set the use category to Service
  output$`Use Category`[i] = "SHW"
}

for(i in which(is.na(output$`Use Category`)))
{
  output$`Use Category`[i] = as.character(claims$UseCategory[i])
  output$`Use SubCategory`[i] = as.character(claims$UseSubCategory[i])
  output$`Tech Group`[i] = as.character(claims$TechGroup[i])
  output$`Tech Type`[i] = as.character(claims$TechType[i])
}



#SUBCATEGORIES & EUL ASSIGNMENTS

#AppPlug

#find all of the dishwasher measures
dishwasher = grepl("Dishwasher", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "AppPlug"

#find the measure within the Unique Measure list which contains dishwasher
uniquedishwasher = grepl("dishwasher", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "AppPlug"

#for each of the dishwasher measures
for(i in which(dishwasher))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique dishwaher measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquedishwasher])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquedishwasher])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquedishwasher])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquedishwasher])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquedishwasher]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquedishwasher]
}

#find all of the clothes washer measures
clothes = grepl("clothes|Clothes", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "AppPlug"

#find the measure within the Unique Measure list which contains dishwasher
uniqueclothes = grepl("clothes|Clothes", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "AppPlug"

#for each of the clothes washer measures
for(i in which(clothes))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique clothes measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueclothes])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueclothes])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueclothes])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueclothes])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueclothes]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueclothes]
}

#find all of the ozone measures
ozone = grepl("ozone|Ozone", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "AppPlug"

#find the measure within the Unique Measure list which contains ozone
uniqueozone = grepl("ozone|Ozone", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "AppPlug"

#for each of the ozone measures
for(i in which(ozone))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique ozone measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueozone])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueozone])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueozone])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueozone])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueozone]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueozone]
}

#find all of the power measures
power = grepl("power|Power", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "AppPlug"

#find the measure within the Unique Measure list which contains power
uniquepower = grepl("power|Power", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "AppPlug"

#for each of the power measures
for(i in which(power))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique power measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquepower])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquepower])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquepower])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquepower])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquepower]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquepower]
}

#find all of the copier measures
copier = grepl("copier|Copier", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "AppPlug"

#find the measure within the Unique Measure list which contains copier
uniquecopier = grepl("copier|Copier", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "AppPlug"

#for each of the copier measures
for(i in which(copier))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique copier measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquecopier])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquecopier])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquecopier])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquecopier])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquecopier]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquecopier]
}

#find all of the occupancy measures in AppPlug 
plugoccupan = grepl("occupancy|Occupancy", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "AppPlug"

#find the measure within the Unique Measure list which contains occupan
uniqueplugoccupan = grepl("occupancy|Occupancy", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "AppPlug"

#for each of the plugoccupan measures
for(i in which(plugoccupan))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique plugoccupan measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueplugoccupan])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueplugoccupan])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueplugoccupan])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueplugoccupan])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueplugoccupan]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueplugoccupan]
}

#find all of the occupancy measures in AppPlug 
vending = grepl("vending|Vending", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "AppPlug"

#find the measure within the Unique Measure list which contains vending
uniquevending = grepl("vending|Vending", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "AppPlug"

#for each of the vending measures
for(i in which(vending))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique vending measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquevending])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquevending])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquevending])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquevending])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquevending]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquevending]
}

#for all of the other AppPlug measures not identified
otherplug = is.na(output$`EUL Years`) & output$`Use Category`== "AppPlug"

#for all of the other AppPlug measures, the output will be the same as the claims database since it didn't fit an unique row
for(i in which(otherplug))
{
  #the properties of the measures are the same
  output$`Use SubCategory`[i] = as.character(claims$UseSubCategory[i])
  output$`Tech Group`[i] = as.character(claims$TechGroup[i])
  output$`Tech Type`[i] = as.character(claims$TechType[i])
  output$`EUL ID`[i] = as.character(claims$EUL_ID[i])
  output$`EUL Years`[i] = claims$EUL_Yrs[i]
  output$`RUL Years`[i] = claims$RUL_Yrs[i]
}


#BLDGENV 

#find all of the infrared BldgEnv measures
ir = grepl("infrared|Infrared|IR", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "BldgEnv"

#find the unique infrared BldgEnv measure
uniqueir = grepl("infrared|Infrared|IR", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "BldgEnv"

#for each of the ir measures
for(i in which(ir))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique ir measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueir])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueir])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueir])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueir])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueir]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueir]
}

#find all of the cool BldgEnv measures
cool = grepl("cool|Cool", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "BldgEnv"

#find the unique cool BldgEnv measure
uniquecool = grepl("cool|Cool", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "BldgEnv"

#for each of the cool measures
for(i in which(cool))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique cool measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquecool])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquecool])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquecool])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquecool])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquecool]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquecool]
}

#find all of the floor BldgEnv measures
floor = grepl("floor|Floor", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "BldgEnv"

#find the unique floor BldgEnv measure
uniquefloor = grepl("floor|Floor", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "BldgEnv"

#for each of the floor measures
for(i in which(floor))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique floor measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquefloor])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquefloor])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquefloor])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquefloor])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquefloor]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquefloor]
}

#find all of the roof/ceiling BldgEnv measures
roofceil = grepl("roof|Roof|ceiling|Ceiling", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "BldgEnv"

#find the unique roofceil BldgEnv measure
uniqueroofceil = grepl("roof|Roof|ceiling|Ceiling", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "BldgEnv"

#for each of the roofceil measures
for(i in which(roofceil))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique roofceil measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueroofceil])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueroofceil])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueroofceil])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueroofceil])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueroofceil]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueroofceil]
}

#find all of the hpwind BldgEnv measures
hpwind = grepl("Performance Window|Performance window|performance window", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "BldgEnv"

#find the unique hpwind BldgEnv measure
uniquehpwind = grepl("Performance Window|Performance window|performance window", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "BldgEnv"

#for each of the hpwind measures
for(i in which(hpwind))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique hpwind measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquehpwind])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquehpwind])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquehpwind])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquehpwind])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquehpwind]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquehpwind]
}

#find all of the shgc BldgEnv measures
shgc = grepl("solar|SHGC|Solar", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "BldgEnv"

#find the unique shgc BldgEnv measure
uniqueshgc = grepl("solar|SHGC|Solar", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "BldgEnv"

#for each of the shgc measures
for(i in which(shgc))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique shgc measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueshgc])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueshgc])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueshgc])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueshgc])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueshgc]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueshgc]
}

#find all of the refwind BldgEnv measures
refwind = grepl("reflective window|Reflective Window|Reflective window", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "BldgEnv"

#find the unique refwind BldgEnv measure
uniquerefwind = grepl("reflective window|Reflective Window|Reflective window", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "BldgEnv"

#for each of the refwind measures
for(i in which(refwind))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique refwind measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquerefwind])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquerefwind])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquerefwind])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquerefwind])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquerefwind]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquerefwind]
}

#for all of the other BldgEnv measures not identified
otherbldg = is.na(output$`EUL Years`) & output$`Use Category`== "BldgEnv"

#for all of the other BldgEnv measures, the output will be the same as the claims database since it didn't fit an unique row
for(i in which(otherbldg))
{
  #the properties of the measures are the same
  output$`Use SubCategory`[i] = as.character(claims$UseSubCategory[i])
  output$`Tech Group`[i] = as.character(claims$TechGroup[i])
  output$`Tech Type`[i] = as.character(claims$TechType[i])
  output$`EUL ID`[i] = as.character(claims$EUL_ID[i])
  output$`EUL Years`[i] = claims$EUL_Yrs[i]
  output$`RUL Years`[i] = claims$RUL_Yrs[i]
}


#COMPAIR

#find all of the dryer CompAir measures
dryer = grepl("air dryer|Air Dryer|Air dryer", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "CompAir"

#find the unique dryer CompAir measure
uniquedryer = grepl("air dryer|Air Dryer|Air dryer", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "CompAir"

#for each of the dryer measures
for(i in which(dryer))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique dryer measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquedryer])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquedryer])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquedryer])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquedryer])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquedryer]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquedryer]
}

#find all of the drive CompAir measures
drive = grepl("drive|Drive|VSD", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "CompAir"

#find the unique drive CompAir measure
uniquedrive = grepl("drive|Drive|VSD", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "CompAir"

#for each of the drive measures
for(i in which(drive))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique drive measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquedrive])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquedrive])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquedrive])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquedrive])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquedrive]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquedrive]
}

#find all of the drain CompAir measures
drain = grepl("drain|Drain", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "CompAir"

#find the unique drain CompAir measure
uniquedrain = grepl("drain|Drain", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "CompAir"

#for each of the drain measures
for(i in which(drain))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique drain measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquedrain])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquedrain])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquedrain])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquedrain])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquedrain]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquedrain]
}

#for all of the other compairEnv measures not identified
othercompair = is.na(output$`EUL Years`) & output$`Use Category`== "CompAir"

#for all of the other compair measures, the output will be the same as the claims database since it didn't fit an unique row
for(i in which(othercompair))
{
  #the properties of the measures are the same
  output$`Use SubCategory`[i] = as.character(claims$UseSubCategory[i])
  output$`Tech Group`[i] = as.character(claims$TechGroup[i])
  output$`Tech Type`[i] = as.character(claims$TechType[i])
  output$`EUL ID`[i] = as.character(claims$EUL_ID[i])
  output$`EUL Years`[i] = claims$EUL_Yrs[i]
  output$`RUL Years`[i] = claims$RUL_Yrs[i]
}


#ComRefrig

#find all of the sweat ComRefrig measures
sweat = grepl("sweat|Sweat|ASH", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ComRefrig"

#find the unique sweat ComRefrig measure
uniquesweat = grepl("sweat|Sweat|ASH", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ComRefrig"

#for each of the sweat measures
for(i in which(sweat))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique sweat measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquesweat])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquesweat])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquesweat])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquesweat])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquesweat]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquesweat]
}

#find all of the walkin ComRefrig measures
walkin = grepl("Auto-Closer|auto-closer", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ComRefrig"

#find the unique walkin ComRefrig measure
uniquewalkin = grepl("Auto-Closer|auto-closer", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ComRefrig"

#for each of the walkin measures
for(i in which(walkin))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique walkin measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquewalkin])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquewalkin])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquewalkin])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquewalkin])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquewalkin]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquewalkin]
}

#find all of the fancont ComRefrig measures
fancont = grepl("fan cont|Fan cont|Fan Cont", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ComRefrig"

#find the unique fancont ComRefrig measure
uniquefancont = grepl("fan cont|Fan cont|Fan Cont", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ComRefrig"

#for each of the fancont measures
for(i in which(fancont))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique fancont measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquefancont])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquefancont])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquefancont])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquefancont])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquefancont]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquefancont]
}

#find all of the heatrec ComRefrig measures
heatrec = grepl("heat rec|Heat Rec|Heat rec", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ComRefrig"

#find the unique heatrec ComRefrig measure
uniqueheatrec = grepl("heat rec|Heat Rec|Heat rec", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ComRefrig"

#for each of the heatrec measures
for(i in which(heatrec))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique heatrec measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueheatrec])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueheatrec])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueheatrec])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueheatrec])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueheatrec]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueheatrec]
}

#find all of the ice ComRefrig measures
ice = grepl("ice machine|Ice Machine|Ice machine", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ComRefrig"

#find the unique ice ComRefrig measure
uniqueice = grepl("ice machine|Ice Machine|Ice machine", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ComRefrig"

#for each of the ice measures
for(i in which(ice))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique ice measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueice])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueice])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueice])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueice])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueice]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueice]
}

#find all of the newcase ComRefrig measures
newcase = grepl("New case|New Case|new case", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ComRefrig"

#find the unique newcase ComRefrig measure
uniquenewcase = grepl("New case|New Case|new case", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ComRefrig"

#for each of the newcase measures
for(i in which(newcase))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique newcase measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquenewcase])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquenewcase])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquenewcase])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquenewcase])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquenewcase]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquenewcase]
}

#find all of the Night cover ComRefrig measures
cover = grepl("cover|Cover", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ComRefrig"

#find the unique night cover ComRefrig measure
uniquecover = grepl("cover|Cover", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ComRefrig"

#for each of the night cover measures
for(i in which(cover))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique cover measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquecover])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquecover])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquecover])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquecover])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquecover]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquecover]
}

#find all of the subcooling ComRefrig measures
subcooling = grepl("subcooling|Subcooling", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ComRefrig"

#find the unique subcooling ComRefrig measure
uniquesubcooling = grepl("subcooling|Subcooling", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ComRefrig"

#for each of the subcooling measures
for(i in which(subcooling))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique subcooling measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquesubcooling])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquesubcooling])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquesubcooling])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquesubcooling])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquesubcooling]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquesubcooling]
}

#find all of the condenser ComRefrig measures
condenser = grepl("condenser|Condenser", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ComRefrig"

#find the unique condenser ComRefrig measure
uniquecondenser = grepl("condenser|Condenser", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ComRefrig"

#for each of the condenser measures
for(i in which(condenser))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique condenser measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquecondenser])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquecondenser])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquecondenser])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquecondenser])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquecondenser]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquecondenser]
}

#find all of the head ComRefrig measures
head = grepl("head|Head", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ComRefrig"

#find the unique head ComRefrig measure
uniquehead = grepl("head|Head", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ComRefrig"

#for each of the head measures
for(i in which(head))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique head measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquehead])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquehead])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquehead])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquehead])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquehead]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquehead]
}

#find all of the suction ComRefrig measures
suction = grepl("suction|Suction", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ComRefrig"

#find the unique suction ComRefrig measure
uniquesuction = grepl("suction|Suction", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ComRefrig"

#for each of the suction measures
for(i in which(suction))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique suction measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquesuction])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquesuction])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquesuction])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquesuction])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquesuction]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquesuction]
}

#find all of the strip ComRefrig measures
strip = grepl("strip|Strip", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ComRefrig"

#find the unique strip ComRefrig measure
uniquestrip = grepl("strip|Strip", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ComRefrig"

#for each of the strip measures
for(i in which(strip))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique strip measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquestrip])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquestrip])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquestrip])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquestrip])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquestrip]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquestrip]
}

#find all of the zeroheat ComRefrig measures
zeroheat = grepl("zero heat|Zero heat|Zero Heat", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ComRefrig"

#find the unique zeroheat ComRefrig measure
uniquezeroheat = grepl("zero heat|Zero heat|Zero Heat", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ComRefrig"

#for each of the zeroheat measures
for(i in which(zeroheat))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique zeroheat measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquezeroheat])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquezeroheat])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquezeroheat])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquezeroheat])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquezeroheat]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquezeroheat]
}

#find all of the reachin ComRefrig measures
reachin = grepl("reach-in|Reach-in|Reach-In", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ComRefrig" & grepl("Storage|storage", output$`Measure Description`, fixed = FALSE)

#find the unique reachin ComRefrig measure
uniquereachin = grepl("reach-in|Reach-in|Reach-In", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ComRefrig" & uniquemeas$UseSubCategory == "Storage"

#for each of the reachin measures
for(i in which(reachin))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique reachin measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquereachin])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquereachin])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquereachin])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquereachin])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquereachin]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquereachin]
}

#find all of the storgas ComRefrig measures
storgas = grepl("Gasket|gasket", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ComRefrig" & grepl("Storage|storage", output$`Measure Description`, fixed = FALSE)

#find the unique storgas ComRefrig measure
uniquestorgas = grepl("Gasket|gasket", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ComRefrig" & uniquemeas$UseSubCategory == "Storage"

#for each of the storgas measures
for(i in which(storgas))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique storgas measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquestorgas])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquestorgas])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquestorgas])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquestorgas])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquestorgas]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquestorgas]
}

#find all of the dispgas ComRefrig measures
dispgas = grepl("Gasket|gasket", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ComRefrig" & grepl("Display|display", output$`Measure Description`, fixed = FALSE)

#find the unique dispgas ComRefrig measure
uniquedispgas = grepl("Gasket|gasket", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ComRefrig" & uniquemeas$UseSubCategory == "Display"

#for each of the dispgas measures
for(i in which(dispgas))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique dispgas measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquedispgas])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquedispgas])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquedispgas])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquedispgas])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquedispgas]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquedispgas]
}

#find all of the equipevap ComRefrig measures
equipevap = grepl("high|High", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ComRefrig" & grepl("equip|Equip", output$`Measure Description`, fixed = FALSE)

#find the unique equipevap ComRefrig measure
uniqueequipevap = grepl("high|High", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ComRefrig" & uniquemeas$UseSubCategory == "Equipment"

#for each of the equipevap measures
for(i in which(equipevap))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique equipevap measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueequipevap])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueequipevap])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueequipevap])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueequipevap])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueequipevap]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueequipevap]
}

#find all of the storevap ComRefrig measures
storevap = grepl("high|High", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ComRefrig" & grepl("Storage|storage", output$`Measure Description`, fixed = FALSE)

#find the unique storevap ComRefrig measure
uniquestorevap = grepl("high|High", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ComRefrig" & uniquemeas$UseSubCategory == "Storage"

#for each of the storevap measures
for(i in which(storevap))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique storevap measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquestorevap])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquestorevap])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquestorevap])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquestorevap])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquestorevap]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquestorevap]
}

#for all of the other comrefrig measures not identified
othercomrefrig = is.na(output$`EUL Years`) & output$`Use Category`== "ComRefrig"

#for all of the other comrefrig measures, the output will be the same as the claims database since it didn't fit an unique row
for(i in which(othercomrefrig))
{
  #the properties of the measures are the same
  output$`Use SubCategory`[i] = as.character(claims$UseSubCategory[i])
  output$`Tech Group`[i] = as.character(claims$TechGroup[i])
  output$`Tech Type`[i] = as.character(claims$TechType[i])
  output$`EUL ID`[i] = as.character(claims$EUL_ID[i])
  output$`EUL Years`[i] = claims$EUL_Yrs[i]
  output$`RUL Years`[i] = claims$RUL_Yrs[i]
}



#FOODSERV

#find all of the combelec FoodServ measures
combelec = grepl("Comb|comb", output$`Measure Description`, fixed = FALSE) & grepl("electric|Electric", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "FoodServ" 

#find the unique combelec FoodServ measure
uniquecombelec = grepl("Combination Oven - Elec", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "FoodServ" 

#for each of the combelec measures
for(i in which(combelec))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique combelec measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquecombelec])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquecombelec])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquecombelec])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquecombelec])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquecombelec]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquecombelec]
}

#find all of the combgas FoodServ measures
combgas = grepl("Comb|comb", output$`Measure Description`, fixed = FALSE) & grepl("Gas|gas", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "FoodServ" 

#find the unique combgas FoodServ measure
uniquecombgas = grepl("Combination Oven - Ga", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "FoodServ" 

#for each of the combgas measures
for(i in which(combgas))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique combgas measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquecombgas])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquecombgas])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquecombgas])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquecombgas])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquecombgas]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquecombgas]
}

#find all of the gasrack FoodServ measures
gasrack = grepl("Gas rack|Gas Rack|gas rack", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "FoodServ" 

#find the unique gasrack FoodServ measure
uniquegasrack = grepl("Gas Rack", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "FoodServ" 

#for each of the gasrack measures
for(i in which(gasrack))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique gasrack measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquegasrack])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquegasrack])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquegasrack])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquegasrack])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquegasrack]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquegasrack]
}

#find all of the holdcab FoodServ measures
holdcab = grepl("hold|Hold", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "FoodServ" 

#find the unique holdcab FoodServ measure
uniqueholdcab = grepl("Holding", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "FoodServ" 

#for each of the holdcab measures
for(i in which(holdcab))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique holdcab measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueholdcab])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueholdcab])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueholdcab])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueholdcab])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueholdcab]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueholdcab]
}

#find all of the convelec FoodServ measures
convelec = grepl("Conv|conv", output$`Measure Description`, fixed = FALSE) & grepl("electric|Electric", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "FoodServ" 

#find the unique convelec FoodServ measure
uniqueconvelec = grepl("Convection Oven - Elect", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "FoodServ" 

#for each of the convelec measures
for(i in which(convelec))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique convelec measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueconvelec])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueconvelec])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueconvelec])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueconvelec])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueconvelec]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueconvelec]
}

#find all of the convgas FoodServ measures
convgas = grepl("Conv|conv", output$`Measure Description`, fixed = FALSE) & grepl("Gas|gas", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "FoodServ" 

#find the unique convgas FoodServ measure
uniqueconvgas = grepl("Convection Oven - Ga", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "FoodServ" 

#for each of the convgas measures
for(i in which(convgas))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique convgas measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueconvgas])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueconvgas])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueconvgas])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueconvgas])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueconvgas]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueconvgas]
}

#find all of the fryelec FoodServ measures
fryelec = grepl("fry|Fry", output$`Measure Description`, fixed = FALSE) & grepl("electric|Electric", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "FoodServ" 

#find the unique fryelec FoodServ measure
uniquefryelec = grepl("Electric Fry", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "FoodServ" 

#for each of the fryelec measures
for(i in which(fryelec))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique fryelec measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquefryelec])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquefryelec])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquefryelec])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquefryelec])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquefryelec]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquefryelec]
}

#find all of the frygas FoodServ measures
frygas = grepl("Fry|fry", output$`Measure Description`, fixed = FALSE) & grepl("Gas|gas", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "FoodServ" 

#find the unique frygas FoodServ measure
uniquefrygas = grepl("Gas Fry", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "FoodServ" 

#for each of the frygas measures
for(i in which(frygas))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique frygas measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquefrygas])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquefrygas])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquefrygas])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquefrygas])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquefrygas]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquefrygas]
}

#find all of the finbot FoodServ measures
finbot = grepl("fin|Fin", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "FoodServ" 

#find the unique finbot FoodServ measure
uniquefinbot = grepl("fin|Fin", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "FoodServ" 

#for each of the finbot measures
for(i in which(finbot))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique finbot measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquefinbot])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquefinbot])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquefinbot])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquefinbot])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquefinbot]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquefinbot]
}

#find all of the elecgrid FoodServ measures
elecgrid = grepl("grid|Grid", output$`Measure Description`, fixed = FALSE) & grepl("electric|Electric", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "FoodServ" 

#find the unique elecgrid FoodServ measure
uniqueelecgrid = grepl("Griddle - Elect", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "FoodServ" 

#for each of the elecgrid measures
for(i in which(elecgrid))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique elecgrid measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueelecgrid])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueelecgrid])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueelecgrid])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueelecgrid])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueelecgrid]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueelecgrid]
}

#find all of the gasgrid FoodServ measures
gasgrid = grepl("grid|Grid", output$`Measure Description`, fixed = FALSE) & grepl("Gas|gas", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "FoodServ" 

#find the unique gasgrid FoodServ measure
uniquegasgrid = grepl("Griddle - Ga", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "FoodServ" 

#for each of the gasgrid measures
for(i in which(gasgrid))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique gasgrid measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquegasgrid])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquegasgrid])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquegasgrid])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquegasgrid])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquegasgrid]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquegasgrid]
}

#find all of the handwrap FoodServ measures
handwrap = grepl("hand|Hand", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "FoodServ" 

#find the unique handwrap FoodServ measure
uniquehandwrap = grepl("Hand", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "FoodServ" 

#for each of the handwrap measures
for(i in which(handwrap))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique handwrap measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquehandwrap])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquehandwrap])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquehandwrap])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquehandwrap])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquehandwrap]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquehandwrap]
}

#find all of the elecstm FoodServ measures
elecstm = grepl("steam|Steam", output$`Measure Description`, fixed = FALSE) & grepl("electric|Electric", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "FoodServ" 

#find the unique elecstm FoodServ measure
uniqueelecstm = grepl("Steam Cooker (electric)", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "FoodServ" 

#for each of the elecstm measures
for(i in which(elecstm))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique elecstm measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueelecstm])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueelecstm])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueelecstm])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueelecstm])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueelecstm]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueelecstm]
}

#find all of the gasstm FoodServ measures
gasstm = grepl("steam|Steam", output$`Measure Description`, fixed = FALSE) & grepl("Gas|gas", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "FoodServ" 

#find the unique gasstm FoodServ measure
uniquegasstm = grepl("Steam Cooker (gas)", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "FoodServ" 

#for each of the gasstm measures
for(i in which(gasstm))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique gasstm measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquegasstm])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquegasstm])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquegasstm])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquegasstm])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquegasstm]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquegasstm]
}

#find all of the vatfry FoodServ measures
vatfry = grepl("vat|Vat", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "FoodServ" 

#find the unique vatfry FoodServ measure
uniquevatfry = grepl("Vat", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "FoodServ" 

#for each of the vatfry measures
for(i in which(vatfry))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique vatfry measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquevatfry])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquevatfry])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquevatfry])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquevatfry])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquevatfry]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquevatfry]
}

#for all of the other foodserv measures not identified
otherfoodserv = is.na(output$`EUL Years`) & output$`Use Category`== "FoodServ"

#for all of the other foodserv measures, the output will be the same as the claims database since it didn't fit an unique row
for(i in which(otherfoodserv))
{
  #the properties of the measures are the same
  output$`Use SubCategory`[i] = as.character(claims$UseSubCategory[i])
  output$`Tech Group`[i] = as.character(claims$TechGroup[i])
  output$`Tech Type`[i] = as.character(claims$TechType[i])
  output$`EUL ID`[i] = as.character(claims$EUL_ID[i])
  output$`EUL Years`[i] = claims$EUL_Yrs[i]
  output$`RUL Years`[i] = claims$RUL_Yrs[i]
}



#HVAC

#find all of the airac HVAC measures
airac = grepl("A/C|A-C|air condition|Air condition|Air Condition", output$`Measure Description`, fixed = FALSE) & grepl("air-cool|Air-cool|air cool|Air cool|Air-Cool|Air Cool", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique airac HVAC measure
uniqueairac = grepl("Air Conditioners air-cooled, split and unitary", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the airac measures
for(i in which(airac))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique airac measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueairac])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueairac])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueairac])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueairac])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueairac]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueairac]
}

#find all of the evapac HVAC measures
evapac = grepl("A/C|A-C|air condition|Air condition|Air Condition", output$`Measure Description`, fixed = FALSE) & grepl("evap|Evap", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique evapac HVAC measure
uniqueevapac = grepl("Air Conditioners evaporatively-cooled, split and unitary", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the evapac measures
for(i in which(evapac))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique evapac measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueevapac])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueevapac])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueevapac])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueevapac])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueevapac]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueevapac]
}

#find all of the packac HVAC measures
packac = grepl("A/C|A-C|air condition|Air condition|Air Condition", output$`Measure Description`, fixed = FALSE) & grepl("pack|Pack", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique packac HVAC measure
uniquepackac = grepl("Air Conditioners packaged terminal AC", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the packac measures
for(i in which(packac))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique packac measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquepackac])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquepackac])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquepackac])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquepackac])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquepackac]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquepackac]
}

#find all of the waterac HVAC measures
waterac = grepl("A/C|A-C|air condition|Air condition|Air Condition", output$`Measure Description`, fixed = FALSE) & grepl("pack|Pack", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique waterac HVAC measure
uniquewaterac = grepl("Air Conditioners water-cooled, split and unitary", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the waterac measures
for(i in which(waterac))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique waterac measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquewaterac])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquewaterac])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquewaterac])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquewaterac])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquewaterac]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquewaterac]
}

#find all of the a2aheat HVAC measures
a2aheat = grepl("Air to Air|air to air", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique a2aheat HVAC measure
uniquea2aheat = grepl("Air to Air", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the a2aheat measures
for(i in which(a2aheat))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique a2aheat measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquea2aheat])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquea2aheat])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquea2aheat])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquea2aheat])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquea2aheat]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquea2aheat]
}

#find all of the cogbelt HVAC measures
cogbelt = grepl("Cogged", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#for each of the cogbelt measures
for(i in which(cogbelt))
{
  #find the unique cogbelt HVAC measure
  uniquecogbelt = grepl("Cogged", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" & uniquemeas$BldgType == as.character(output$`Bldg Type`[i])
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique cogbelt measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquecogbelt])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquecogbelt])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquecogbelt])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquecogbelt])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquecogbelt]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquecogbelt]
}

#find all of the cooltow HVAC measures
cooltow = grepl("Tower|tower", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique cooltow HVAC measure
uniquecooltow = grepl("Tower", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the cooltow measures
for(i in which(cooltow))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique cooltow measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquecooltow])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquecooltow])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquecooltow])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquecooltow])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquecooltow]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquecooltow]
}

#find all of the ductinsul HVAC measures
ductinsul = grepl("Insulation|insulation", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique ductinsul HVAC measure
uniqueductinsul = grepl("Insulation", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the ductinsul measures
for(i in which(ductinsul))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique ductinsul measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueductinsul])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueductinsul])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueductinsul])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueductinsul])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueductinsul]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueductinsul]
}

#find all of the ductseal HVAC measures
ductseal = grepl("Seal|seal", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique ductseal HVAC measure
uniqueductseal = grepl("Sealing", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the ductseal measures
for(i in which(ductseal))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique ductseal measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueductseal])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueductseal])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueductseal])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueductseal])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueductseal]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueductseal]
}

#find all of the emshvac HVAC measures
emshvac = grepl("Energy Management System|EMS|energy maanagement system", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique emshvac HVAC measure
uniqueemshvac = grepl("Energy Management System", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the emshvac measures
for(i in which(emshvac))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique emshvac measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueemshvac])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueemshvac])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueemshvac])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueemshvac])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueemshvac]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueemshvac]
}

#find all of the indcool HVAC measures
indcool = grepl("Cool Indirect|cool indirect|Cool indirect", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique indcool HVAC measure
uniqueindcool = grepl("Cool Indirect", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the indcool measures
for(i in which(indcool))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique indcool measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueindcool])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueindcool])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueindcool])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueindcool])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueindcool]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueindcool]
}

#find all of the mixbox HVAC measures
mixbox = grepl("mix|Mix", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique mixbox HVAC measure
uniquemixbox = grepl("Mix", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the mixbox measures
for(i in which(mixbox))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique mixbox measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquemixbox])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquemixbox])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquemixbox])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquemixbox])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquemixbox]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquemixbox]
}

#find all of the airhp HVAC measures
airhp = grepl("HP|Heat Pump|Heat pump|heat pump", output$`Measure Description`, fixed = FALSE) & grepl("Air-Cool|air-cool|Air-cool", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique airhp HVAC measure
uniqueairhp = grepl("Heat Pumps air-cooled, split and unitary", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the airhp measures
for(i in which(airhp))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique airhp measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueairhp])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueairhp])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueairhp])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueairhp])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueairhp]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueairhp]
}

#find all of the packhp HVAC measures
packhp = grepl("HP|Heat Pump|Heat pump|heat pump", output$`Measure Description`, fixed = FALSE) & grepl("pack|Pack", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique packhp HVAC measure
uniquepackhp = grepl("Heat Pumps packaged terminal", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the packhp measures
for(i in which(packhp))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique packhp measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquepackhp])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquepackhp])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquepackhp])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquepackhp])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquepackhp]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquepackhp]
}

#find all of the hvacboiler HVAC measures
hvacboiler = grepl("boiler|Boiler", output$`Measure Description`, fixed = FALSE) & grepl("Efficient Unit", output$`Measure Description`, fixed = FALSE) & output$`Use Category` =="HVAC" 

#find the unique hvacboiler HVAC measure
uniquehvacboiler = grepl("Boiler", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the hvacboiler measures
for(i in which(hvacboiler))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique hvacboiler measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquehvacboiler])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquehvacboiler])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquehvacboiler])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquehvacboiler])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquehvacboiler]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquehvacboiler]
}

#find all of the hvacchiller HVAC measures
hvacchiller = grepl("chiller|Chiller", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique hvacchiller HVAC measure
uniquehvacchiller = grepl("Chiller", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the hvacchiller measures
for(i in which(hvacchiller))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique hvacchiller measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquehvacchiller])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquehvacchiller])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquehvacchiller])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquehvacchiller])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquehvacchiller]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquehvacchiller]
}

#find all of the hvacfurnace HVAC measures
hvacfurnace = grepl("furnace|Furnace", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique hvacfurnace HVAC measure
uniquehvacfurnace = grepl("Furnace", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the hvacfurnace measures
for(i in which(hvacfurnace))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique hvacfurnace measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquehvacfurnace])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquehvacfurnace])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquehvacfurnace])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquehvacfurnace])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquehvacfurnace]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquehvacfurnace]
}

#find all of the waterhp HVAC measures
waterhp = grepl("HP|Heat Pump|Heat pump|heat pump", output$`Measure Description`, fixed = FALSE) & grepl("water|Water", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique waterhp HVAC measure
uniquewaterhp = grepl("High Efficiency Water Source Heat Pump", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the waterhp measures
for(i in which(waterhp))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique waterhp measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquewaterhp])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquewaterhp])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquewaterhp])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquewaterhp])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquewaterhp]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquewaterhp]
}

#find all of the hvacfanmot HVAC measures
hvacfanmot = grepl("Fan motor|Fan Motor|fan motor", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique hvacfanmot HVAC measure
uniquehvacfanmot = grepl("Fan Motor", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the hvacfanmot measures
for(i in which(hvacfanmot))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique hvacfanmot measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquehvacfanmot])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquehvacfanmot])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquehvacfanmot])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquehvacfanmot])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquehvacfanmot]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquehvacfanmot]
}

#find all of the hydrohp HVAC measures
hydrohp = grepl("VAV|Valve", output$`Measure Description`, fixed = FALSE) & grepl("HP|Heat Pump|Heat Pump|heat pump", output$`Measure Description`, fixed = FALSE) & grepl("Liquid|liquid", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique hydrohp HVAC measure
uniquehydrohp = grepl("Hydronic Heat Pump Var Flow Valve", uniquemeas$Description, fixed = FALSE) & uniquemeas$TechGroup == "LiquidCirc" 

#for each of the hydrohp measures
for(i in which(hydrohp))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique hydrohp measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquehydrohp])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquehydrohp])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquehydrohp])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquehydrohp])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquehydrohp]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquehydrohp]
}

#find all of the equiphp HVAC measures
equiphp = grepl("VAV|Valve", output$`Measure Description`, fixed = FALSE) & grepl("HP|Heat Pump|Heat Pump|heat pump", output$`Measure Description`, fixed = FALSE) & grepl("Liquid|liquid", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique equiphp HVAC measure
uniqueequiphp = grepl("Hydronic Heat Pump Var Flow Valve", uniquemeas$Description, fixed = FALSE) & uniquemeas$TechGroup == "dxHP_equip" 

#for each of the equiphp measures
for(i in which(equiphp))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique equiphp measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueequiphp])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueequiphp])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueequiphp])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueequiphp])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueequiphp]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueequiphp]
}

#find all of the termac HVAC measures
termac = grepl("Package|package", output$`Measure Description`, fixed = FALSE) &  grepl("control|Control", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#for each of the termac measures
for(i in which(termac))
{
  #find the unique termac HVAC measure
  uniquetermac = grepl("Package", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" & uniquemeas$BldgType == as.character(output$`Bldg Type`[i])
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique termac measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquetermac])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquetermac])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquetermac])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquetermac])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquetermac]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquetermac]
}

#find all of the qualmain HVAC measures
qualmain = grepl("Quality|quality", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique qualmain HVAC measure
uniquequalmain = grepl("Quality", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the qualmain measures
for(i in which(qualmain))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique qualmain measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquequalmain])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquequalmain])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquequalmain])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquequalmain])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquequalmain]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquequalmain]
}

#find all of the overvent HVAC measures
overvent = grepl("overvent|Overvent", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique overvent HVAC measure
uniqueovervent = grepl("Overvent", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the overvent measures
for(i in which(overvent))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique overvent measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueovervent])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueovervent])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueovervent])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueovervent])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueovervent]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueovervent]
}

#find all of the refrigcharge HVAC measures
refrigcharge = grepl("refrigerant|Refrigerant", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique refrigcharge HVAC measure
uniquerefrigcharge = grepl("Refrigerant", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the refrigcharge measures
for(i in which(refrigcharge))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique refrigcharge measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquerefrigcharge])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquerefrigcharge])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquerefrigcharge])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquerefrigcharge])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquerefrigcharge]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquerefrigcharge]
}

#find all of the repaireco HVAC measures
repaireco = grepl("Repair|repair", output$`Measure Description`, fixed = FALSE) & grepl("economizer|Economizer", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique repaireco HVAC measure
uniquerepaireco = grepl("Repair", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the repaireco measures
for(i in which(repaireco))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique repaireco measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquerepaireco])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquerepaireco])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquerepaireco])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquerepaireco])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquerepaireco]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquerepaireco]
}

#find all of the rotheat HVAC measures
rotheat = grepl("rotary heat|Rotary Heat|Rotary heat", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique rotheat HVAC measure
uniquerotheat = grepl("Rotary", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the rotheat measures
for(i in which(rotheat))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique rotheat measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquerotheat])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquerotheat])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquerotheat])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquerotheat])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquerotheat]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquerotheat]
}

#find all of the setprog HVAC measures
setprog = grepl("Setback Prog|setback prog|Setback prog", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique setprog HVAC measure
uniquesetprog = grepl("Setback", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the setprog measures
for(i in which(setprog))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique setprog measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquesetprog])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquesetprog])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquesetprog])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquesetprog])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquesetprog]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquesetprog]
}

#find all of the hvacstmtrap HVAC measures
hvacstmtrap = grepl("System-Steam", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC"

#find the unique hvacstmtrap HVAC measure
uniquehvacstmtrap = grepl("Steam", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the hvacstmtrap measures
for(i in which(hvacstmtrap))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique hvacstmtrap measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquehvacstmtrap])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquehvacstmtrap])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquehvacstmtrap])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquehvacstmtrap])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquehvacstmtrap]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquehvacstmtrap]
}

#find all of the time HVAC measures
time = grepl("time|Time", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique time HVAC measure
uniquetime = grepl("Time", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the time measures
for(i in which(time))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique time measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquetime])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquetime])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquetime])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquetime])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquetime]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquetime]
}

#find all of the twospd HVAC measures
twospd = grepl("two speed|Two speed|2 speed", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique twospd HVAC measure
uniquetwospd = grepl("Two-Speed", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the twospd measures
for(i in which(twospd))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique twospd measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquetwospd])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquetwospd])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquetwospd])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquetwospd])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquetwospd]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquetwospd]
}

#find all of the flwwtrloop HVAC measures
flwwtrloop = grepl("Flow Water Loop| Flow water loop| flow Water Loop", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique flwwtrloop HVAC measure
uniqueflwwtrloop = grepl("Flow Water Loop", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the flwwtrloop measures
for(i in which(flwwtrloop))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique flwwtrloop measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueflwwtrloop])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueflwwtrloop])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueflwwtrloop])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueflwwtrloop])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueflwwtrloop]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueflwwtrloop]
}

#find all of the crbndxde HVAC measures
crbndxde = grepl("Carbon|carbon|CO2|co2", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique crbndxde HVAC measure
uniquecrbndxde = grepl("CO2", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the crbndxde measures
for(i in which(crbndxde))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique crbndxde measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquecrbndxde])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquecrbndxde])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquecrbndxde])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquecrbndxde])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquecrbndxde]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquecrbndxde]
}

#find all of the supplyfan HVAC measures
supplyfan = grepl("Supply Fan|supply fan|Supply fan", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique supplyfan HVAC measure
uniquesupplyfan = grepl("Supply Fan", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the supplyfan measures
for(i in which(supplyfan))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique supplyfan measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquesupplyfan])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquesupplyfan])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquesupplyfan])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquesupplyfan])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquesupplyfan]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquesupplyfan]
}

#find all of the wtrlooprst HVAC measures
wtrlooprst = grepl("Loop Reset|loop reset|Loop reset", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique wtrlooprst HVAC measure
uniquewtrlooprst = grepl("Loop Reset", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the wtrlooprst measures
for(i in which(wtrlooprst))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique wtrlooprst measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquewtrlooprst])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquewtrlooprst])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquewtrlooprst])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquewtrlooprst])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquewtrlooprst]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquewtrlooprst]
}

#find all of the watereco HVAC measures
watereco = grepl("Water Side|water side|Water side", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "HVAC" 

#find the unique watereco HVAC measure
uniquewatereco = grepl("Side Economizer", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "HVAC" 

#for each of the watereco measures
for(i in which(watereco))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique watereco measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquewatereco])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquewatereco])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquewatereco])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquewatereco])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquewatereco]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquewatereco]
}


#for all of the other HVAC measures not identified
otherhvac = is.na(output$`EUL Years`) & output$`Use Category`== "HVAC"

#for all of the other hvac measures, the output will be the same as the claims database since it didn't fit an unique row
for(i in which(otherhvac))
{
  #the properties of the measures are the same
  output$`Use SubCategory`[i] = as.character(claims$UseSubCategory[i])
  output$`Tech Group`[i] = as.character(claims$TechGroup[i])
  output$`Tech Type`[i] = as.character(claims$TechType[i])
  output$`EUL ID`[i] = as.character(claims$EUL_ID[i])
  output$`EUL Years`[i] = claims$EUL_Yrs[i]
  output$`RUL Years`[i] = claims$RUL_Yrs[i]
}



#IRRIGATION

#find all of the permnozz Irrigate measures
permnozz = grepl("permanent|Permanent", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Irrigate" 

#find the unique permnozz Irrigate measure
uniquepermnozz = grepl("permanent", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Irrigate" 

#for each of the permnozz measures
for(i in which(permnozz))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique permnozz measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquepermnozz])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquepermnozz])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquepermnozz])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquepermnozz])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquepermnozz]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquepermnozz]
}

#find all of the portnozz Irrigate measures
portnozz = grepl("portable|Portable", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Irrigate" 

#find the unique portnozz Irrigate measure
uniqueportnozz = grepl("portable", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Irrigate" 

#for each of the portnozz measures
for(i in which(portnozz))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique portnozz measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueportnozz])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueportnozz])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueportnozz])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueportnozz])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueportnozz]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueportnozz]
}

#find all of the dripconv Irrigate measures
dripconv = grepl("drip|Drip", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Irrigate" 

#find the unique dripconv Irrigate measure
uniquedripconv = grepl("Drip", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Irrigate" 

#for each of the dripconv measures
for(i in which(dripconv))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique dripconv measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquedripconv])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquedripconv])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquedripconv])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquedripconv])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquedripconv]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquedripconv]
}

#find all of the wellpump Irrigate measures
wellpump = grepl("well pump|Well pump|Well Pump", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Irrigate" 

#find the unique wellpump Irrigate measure
uniquewellpump = grepl("Well Pump", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Irrigate" 

#for each of the wellpump measures
for(i in which(wellpump))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique wellpump measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquewellpump])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquewellpump])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquewellpump])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquewellpump])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquewellpump]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquewellpump]
}

#for all of the other Irrigate measures not identified
otherirrigate = is.na(output$`EUL Years`) & output$`Use Category`== "Irrigate"

#for all of the other irrigate measures, the output will be the same as the claims database since it didn't fit an unique row
for(i in which(otherirrigate))
{
  #the properties of the measures are the same
  output$`Use SubCategory`[i] = as.character(claims$UseSubCategory[i])
  output$`Tech Group`[i] = as.character(claims$TechGroup[i])
  output$`Tech Type`[i] = as.character(claims$TechType[i])
  output$`EUL ID`[i] = as.character(claims$EUL_ID[i])
  output$`EUL Years`[i] = claims$EUL_Yrs[i]
  output$`RUL Years`[i] = claims$RUL_Yrs[i]
}




#LIGHTING

#find all of the occsens Lighting measures
occsens = grepl("Occ|occ", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" 

#find the unique occsens Lighting measure
uniqueoccsens = grepl("Occupancy", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" 

#for each of the occsens measures
for(i in which(occsens))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique occsens measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueoccsens])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueoccsens])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueoccsens])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueoccsens])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueoccsens]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueoccsens]
}

#find all of the highpress Lighting measures
highpress = grepl("HID", output$`Measure Description`, fixed = FALSE) & grepl("High Pressure|high pressure|High pressure", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" 

#for each of the highpress measures
for(i in which(highpress))
{
  #find the unique highpress Lighting measure
  uniquehighpress = grepl("High Pressure", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" & uniquemeas$BldgType == as.character(output$`Bldg Type`[i])
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique highpress measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquehighpress])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquehighpress])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquehighpress])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquehighpress])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquehighpress]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquehighpress]
}


#find all of the induct Lighting measures
induct = grepl("HID", output$`Measure Description`, fixed = FALSE) & grepl("induct|Induct", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" 

#for each of the induct measures
for(i in which(induct))
{
  #find the unique induct Lighting measure
  uniqueinduct = grepl("Induction", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" & uniquemeas$BldgType == as.character(output$`Bldg Type`[i])
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique induct measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueinduct])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueinduct])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueinduct])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueinduct])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueinduct]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueinduct]
}


#find all of the mtlhld Lighting measures
mtlhld = grepl("HID", output$`Measure Description`, fixed = FALSE) & grepl("metal|Metal|halide|Halide", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" 

#for each of the mtlhld measures
for(i in which(mtlhld))
{
  #find the unique mtlhld Lighting measure
  uniquemtlhld = grepl("Metal Halide", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" & uniquemeas$BldgType == as.character(output$`Bldg Type`[i])
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique mtlhld measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquemtlhld])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquemtlhld])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquemtlhld])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquemtlhld])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquemtlhld]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquemtlhld]
}


#find all of the hidt5 Lighting measures
hidt5 = grepl("HID", output$`Measure Description`, fixed = FALSE) & grepl("T-5|T5|t5|t-5", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" 

#for each of the hidt5 measures
for(i in which(hidt5))
{
  #find the unique hidt5 Lighting measure
  uniquehidt5 = grepl("T-5", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" & uniquemeas$BldgType == as.character(output$`Bldg Type`[i])
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique hidt5 measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquehidt5])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquehidt5])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquehidt5])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquehidt5])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquehidt5]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquehidt5]
}


#find all of the outhidt5 Lighting measures
outhidt5 = grepl("HID", output$`Measure Description`, fixed = FALSE) & grepl("T-5|T5|t5|t-5", output$`Measure Description`, fixed = FALSE) & grepl("Out|out|Ext|ext", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" 

#find the unique outhidt5 Lighting measure
uniqueouthidt5 = grepl("OUtdoor HID", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting"

#for each of the outhidt5 measures
for(i in which(outhidt5))
{
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique outhidt5 measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueouthidt5])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueouthidt5])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueouthidt5])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueouthidt5])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueouthidt5]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueouthidt5]
}


#find all of the otherhid Lighting measures
otherhid = grepl("HID", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" & is.na(output$`Use SubCategory`)

#for each of the otherhid measures
for(i in which(otherhid))
{
  
  #find the unique otherhid Lighting measure
  uniqueotherhid = grepl("Any HID Fixture", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" & uniquemeas$BldgType == as.character(output$`Bldg Type`[i])
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique otherhid measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueotherhid])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueotherhid])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueotherhid])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueotherhid])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueotherhid]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueotherhid]
}


#find all of the daylight Lighting measures
daylight = grepl("day|Day", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" 

#find the unique daylight Lighting measure
uniquedaylight = grepl("Day", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" 

#for each of the daylight measures
for(i in which(daylight))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique daylight measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquedaylight])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquedaylight])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquedaylight])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquedaylight])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquedaylight]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquedaylight]
}


#find all of the exitlight Lighting measures
exitlight = grepl("exit|Exit", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" 

#find the unique exitlight Lighting measure
uniqueexitlight = grepl("Exit", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" 

#for each of the exitlight measures
for(i in which(exitlight))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique exitlight measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueexitlight])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueexitlight])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueexitlight])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueexitlight])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueexitlight]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueexitlight]
}


#find all of the dispcont Lighting measures
dispcont = grepl("disp|Disp", output$`Measure Description`, fixed = FALSE) & grepl("cont|Cont", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" 

#find the unique dispcont Lighting measure
uniquedispcont = grepl("Case Lighting Control", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" 

#for each of the dispcont measures
for(i in which(dispcont))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique dispcont measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquedispcont])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquedispcont])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquedispcont])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquedispcont])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquedispcont]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquedispcont]
}


#find all of the displed Lighting measures
displed = grepl("disp|Disp", output$`Measure Description`, fixed = FALSE) & grepl("LED", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" 

#find the unique displed Lighting measure
uniquedispled = grepl("Case Lighting LED", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" 

#for each of the displed measures
for(i in which(displed))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique displed measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquedispled])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquedispled])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquedispled])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquedispled])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquedispled]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquedispled]
}


#find all of the incandes Lighting measures
incandes = grepl("incand|Incand", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" 

#find the unique incandes Lighting measure
uniqueincandes = grepl("Incand", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" 

#for each of the incandes measures
for(i in which(incandes))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique incandes measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueincandes])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueincandes])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueincandes])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueincandes])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueincandes]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueincandes]
}


#find all of the cathode Lighting measures
cathode = grepl("Cthd|cthd|Cathode|cathode", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" 

#for each of the cathode measures
for(i in which(cathode))
{
  
  #find the unique cathode Lighting measure
  uniquecathode = grepl("CldCthd", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" & uniquemeas$BldgType == as.character(output$`Bldg Type`[i])
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique cathode measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquecathode])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquecathode])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquecathode])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquecathode])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquecathode]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquecathode]
}


#find all of the intalamp Lighting measures
intalamp = grepl("A-Lamp|a-lamp|A Lamp|a lamp|A lamp", output$`Measure Description`, fixed = FALSE) & grepl("int|Int|indoor|Indoor", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" 

#for each of the intalamp measures
for(i in which(intalamp))
{
  
  #find the unique intalamp Lighting measure
  uniqueintalamp = grepl("LED A-Lamp - Indoor", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" & uniquemeas$BldgType == as.character(output$`Bldg Type`[i])
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique intalamp measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueintalamp])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueintalamp])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueintalamp])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueintalamp])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueintalamp]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueintalamp]
}


#find all of the extalamp Lighting measures
extalamp = grepl("A-Lamp|a-lamp|A Lamp|a lamp|A lamp", output$`Measure Description`, fixed = FALSE) & grepl("ext|Ext|Outdoor|outdoor", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" 

#for each of the extalamp measures
for(i in which(extalamp))
{
  
  #find the unique extalamp Lighting measure
  uniqueextalamp = grepl("LED A-Lamp - Outdoor", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" & uniquemeas$BldgType == as.character(output$`Bldg Type`[i])
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique extalamp measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueextalamp])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueextalamp])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueextalamp])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueextalamp])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueextalamp]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueextalamp]
}


#find all of the timephoto Lighting measures
timephoto = grepl("time|Time", output$`Measure Description`, fixed = FALSE) & grepl("photo|Photo", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" 

#find the unique timephoto Lighting measure
uniquetimephoto = grepl("Timeclock with", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" & uniquemeas$EUL_ID == "OLtg-All-TmClkPhoto"

#for each of the timephoto measures
for(i in which(timephoto))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique timephoto measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquetimephoto])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquetimephoto])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquetimephoto])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquetimephoto])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquetimephoto]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquetimephoto]
}


#find all of the lighttime Lighting measures
lighttime = grepl("time|Time", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" 

#find the unique lighttime Lighting measure
uniquelighttime = grepl("Timeclock with", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" & uniquemeas$EUL_ID == "OLtg-All-TmClk"

#for each of the lighttime measures
for(i in which(lighttime))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique lighttime measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquelighttime])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquelighttime])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquelighttime])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquelighttime])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquelighttime]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquelighttime]
}


#find all of the fluorfix Lighting measures
fluorfix = grepl("linear|Linear", output$`Measure Description`, fixed = FALSE) & grepl("fixture|Fixture", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" 

#find the unique fluorfix Lighting measure
uniquefluorfix = grepl("Fluorescent - Fixtures", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" & uniquemeas$EUL_ID == "OLtg-All-TmClkPhoto"

#for each of the fluorfix measures
for(i in which(fluorfix))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique fluorfix measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquefluorfix])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquefluorfix])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquefluorfix])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquefluorfix])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquefluorfix]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquefluorfix]
}

#find all of the intfluorelec Lighting measures
intfluorelec = grepl("linear|Linear", output$`Measure Description`, fixed = FALSE) & grepl("elect|Elect", output$`Measure Description`, fixed = FALSE)  & grepl("int|Int|indoor|Indoor", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" 

#for each of the intfluorelec measures
for(i in which(intfluorelec))
{
  
  #find the unique intfluorelec Lighting measure
  uniqueintfluorelec = grepl("Fluorescent with Electronic", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" & uniquemeas$BldgType == as.character(output$`Bldg Type`[i])
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique intfluorelec measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueintfluorelec])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueintfluorelec])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueintfluorelec])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueintfluorelec])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueintfluorelec]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueintfluorelec]
}


#find all of the intfluormag Lighting measures
intfluormag = grepl("linear|Linear", output$`Measure Description`, fixed = FALSE) & grepl("magnet|Magent", output$`Measure Description`, fixed = FALSE)  & grepl("int|Int|indoor|Indoor", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" 

#for each of the intfluormag measures
for(i in which(intfluormag))
{
  
  #find the unique intfluormag Lighting measure
  uniqueintfluormag = grepl("Fluorescent with Magnetic", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" & uniquemeas$BldgType == as.character(output$`Bldg Type`[i])
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique intfluormag measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueintfluormag])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueintfluormag])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueintfluormag])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueintfluormag])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueintfluormag]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueintfluormag]
}


#find all of the extfluorelec Lighting measures
extfluorelec = grepl("linear|Linear", output$`Measure Description`, fixed = FALSE) & grepl("elect|Elect", output$`Measure Description`, fixed = FALSE)  & grepl("ext|Ext|outdoor|Outdoor", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" 

#for each of the extfluorelec measures
for(i in which(extfluorelec))
{
  
  #find the unique extfluorelec Lighting measure
  uniqueextfluorelec = grepl("Fluorescent with Electronic", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" & uniquemeas$BldgType == as.character(output$`Bldg Type`[i])
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique extfluorelec measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueextfluorelec])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueextfluorelec])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueextfluorelec])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueextfluorelec])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueextfluorelec]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueextfluorelec]
}


#find all of the extfluormag Lighting measures
extfluormag = grepl("linear|Linear", output$`Measure Description`, fixed = FALSE) & grepl("magnet|Magent", output$`Measure Description`, fixed = FALSE)  & grepl("ext|Ext|outdoor|Outdoor", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" 

#for each of the extfluormag measures
for(i in which(extfluormag))
{
  
  #find the unique extfluormag Lighting measure
  uniqueextfluormag = grepl("Fluorescent with Magnetic", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" & uniquemeas$BldgType == as.character(output$`Bldg Type`[i])
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique extfluormag measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueextfluormag])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueextfluormag])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueextfluormag])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueextfluormag])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueextfluormag]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueextfluormag]
}


#find all of the intmagt12 Lighting measures
intmagt12 = grepl("linear|Linear", output$`Measure Description`, fixed = FALSE) & grepl("T-12|t-12|T12|t12", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" 

#for each of the intmagt12 measures
for(i in which(intmagt12))
{
  
  #find the unique intmagt12 Lighting measure
  uniqueintmagt12 = grepl("T12", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" & uniquemeas$BldgType == as.character(output$`Bldg Type`[i])
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique intmagt12 measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueintmagt12])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueintmagt12])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueintmagt12])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueintmagt12])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueintmagt12]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueintmagt12]
}


#find all of the fluordusk Lighting measures
fluordusk = grepl("linear|Linear", output$`Measure Description`, fixed = FALSE) & grepl("dusk|Dusk", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" 

#find the unique fluordusk Lighting measure
uniquefluordusk = grepl("Dusk to Dawn", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" & uniquemeas$EUL_ID == "OLtg-All-TmClkPhoto"

#for each of the fluordusk measures
for(i in which(fluordusk))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique fluordusk measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquefluordusk])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquefluordusk])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquefluordusk])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquefluordusk])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquefluordusk]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquefluordusk]
}


#find all of the int10k Lighting measures
int10k = grepl("10,000|10000", output$`Measure Description`, fixed = FALSE) & grepl("int|Int|indoor|Indoor", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" 

#for each of the int10k measures
for(i in which(int10k))
{
  
  #find the unique int10k Lighting measure
  uniqueint10k = grepl("Indoor- Commercial - 10,000 Rated Hours", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" & uniquemeas$BldgType == as.character(output$`Bldg Type`[i])
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique int10k measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueint10k])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueint10k])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueint10k])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueint10k])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueint10k]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueint10k]
}


#find all of the ext10k Lighting measures
ext10k = grepl("10,000|10000", output$`Measure Description`, fixed = FALSE) & grepl("int|Int|indoor|Indoor", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" 

#find the unique ext10k Lighting measure
uniqueext10k = grepl("Outdoor CFL Lamps", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting"

#for each of the ext10k measures
for(i in which(ext10k))
{
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique ext10k measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueext10k])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueext10k])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueext10k])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueext10k])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueext10k]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueext10k]
}


#find all of the int8k Lighting measures
int8k = grepl("8,000|8000", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" 

#for each of the int8k measures
for(i in which(int8k))
{
  
  #find the unique int8k Lighting measure
  uniqueint8k = grepl("Indoor- Commercial - 8,000 Rated Hours", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" & uniquemeas$BldgType == as.character(output$`Bldg Type`[i])
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique int8k measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueint8k])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueint8k])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueint8k])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueint8k])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueint8k]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueint8k]
}


#find all of the int6k Lighting measures
int6k = grepl("6,000|6000", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" 

#for each of the int6k measures
for(i in which(int6k))
{
  
  #find the unique int6k Lighting measure
  uniqueint6k = grepl("Indoor- Commercial - 6,000 Rated Hours", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" & uniquemeas$BldgType == as.character(output$`Bldg Type`[i])
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique int6k measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueint6k])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueint6k])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueint6k])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueint6k])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueint6k]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueint6k]
}


#find all of the int12k Lighting measures
int12k = grepl("12,000|12000", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" 

#for each of the int12k measures
for(i in which(int12k))
{
  
  #find the unique int12k Lighting measure
  uniqueint12k = grepl("Indoor- Commercial - 12,000 Rated Hours", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" & uniquemeas$BldgType == as.character(output$`Bldg Type`[i])
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique int12k measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueint12k])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueint12k])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueint12k])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueint12k])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueint12k]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueint12k]
}


#find all of the intothercfl Lighting measures
intothercfl = grepl("CFL", output$`Measure Description`, fixed = FALSE) & grepl("int|Int|indoor|Indoor", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" & is.na(output$`Use SubCategory` )

#find the unique intothercfl Lighting measure
uniqueintothercfl = grepl("Fixtures - Indoor- Commercial", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting"

#for each of the intothercfl measures
for(i in which(intothercfl))
{
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique intothercfl measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueintothercfl])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueintothercfl])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueintothercfl])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueintothercfl])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueintothercfl]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueintothercfl]
}


#find all of the extcflfix Lighting measures
extcflfix = grepl("CFL", output$`Measure Description`, fixed = FALSE) & grepl("ext|Ext|outdoor|Outdoor", output$`Measure Description`, fixed = FALSE) & grepl("fix|Fix", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" & is.na(output$`Use SubCategory` )

#find the unique extcflfix Lighting measure
uniqueextcflfix = grepl(" Fixtures - Outdoor - Dusk", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting"

#for each of the extcflfix measures
for(i in which(extcflfix))
{
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique extcflfix measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueextcflfix])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueextcflfix])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueextcflfix])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueextcflfix])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueextcflfix]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueextcflfix]
}


#find all of the extcfllamp Lighting measures
extcfllamp = grepl("CFL", output$`Measure Description`, fixed = FALSE) & grepl("ext|Ext|outdoor|Outdoor", output$`Measure Description`, fixed = FALSE) & grepl("lamp|Lamp", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting" & is.na(output$`Use SubCategory` )

#find the unique extcfllamp Lighting measure
uniqueextcfllamp = grepl("Fixtures - Outdoor - Dusk", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting"

#for each of the extcfllamp measures
for(i in which(extcfllamp))
{
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique extcfllamp measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueextcfllamp])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueextcfllamp])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueextcfllamp])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueextcfllamp])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueextcfllamp]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueextcfllamp]
}


#find all of the poolclose Lighting measures
poolclose = grepl("pool|Pool", output$`Measure Description`, fixed = FALSE) & grepl("close|Close", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting"

#find the unique poolclose Lighting measure
uniquepoolclose = grepl("Close", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting"

#for each of the poolclose measures
for(i in which(poolclose))
{
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique poolclose measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquepoolclose])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquepoolclose])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquepoolclose])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquepoolclose])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquepoolclose]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquepoolclose]
}


#find all of the pooldawn Lighting measures
pooldawn = grepl("pool|Pool", output$`Measure Description`, fixed = FALSE) & grepl("dawn|dawn", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting"

#find the unique pooldawn Lighting measure
uniquepooldawn = grepl("Dawn", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting"

#for each of the pooldawn measures
for(i in which(pooldawn))
{
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique pooldawn measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquepooldawn])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquepooldawn])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquepooldawn])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquepooldawn])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquepooldawn]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquepooldawn]
}


#find all of the extledfix Lighting measures
extledfix = grepl("LED", output$`Measure Description`, fixed = FALSE) & grepl("outdoor|Outdoor|ext|Ext", output$`Measure Description`, fixed = FALSE) & grepl("fix|Fix", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting"

#find the unique extledfix Lighting measure
uniqueextledfix = grepl("LED Fixture - Outdoor- Commercial", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting"

#for each of the extledfix measures
for(i in which(extledfix))
{
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique extledfix measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueextledfix])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueextledfix])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueextledfix])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueextledfix])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueextledfix]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueextledfix]
}


#find all of the extledlamp Lighting measures
extledlamp = grepl("LED", output$`Measure Description`, fixed = FALSE) & grepl("outdoor|Outdoor|ext|Ext", output$`Measure Description`, fixed = FALSE) & grepl("lamp|Lamp", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting"

#find the unique extledlamp Lighting measure
uniqueextledlamp = grepl("LED lamp - Outdoor- Commercial", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" & uniquemeas$EUL_ID == "OLtg-Com-LED-20000hr"

#for each of the extledlamp measures
for(i in which(extledlamp))
{
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique extledlamp measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueextledlamp])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueextledlamp])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueextledlamp])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueextledlamp])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueextledlamp]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueextledlamp]
}



#find all of the extledglobe Lighting measures
extledglobe = grepl("LED", output$`Measure Description`, fixed = FALSE) & grepl("outdoor|Outdoor|ext|Ext", output$`Measure Description`, fixed = FALSE) & grepl("lamp|Lamp", output$`Measure Description`, fixed = FALSE) & grepl("watt|Watt", output$`Measure Description`, fixed = FALSE)  & output$`Use Category` == "Lighting"

#find the unique extledglobe Lighting measure
uniqueextledglobe = grepl("Small wattage Globe", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting"

#for each of the extledglobe measures
for(i in which(extledglobe))
{
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique extledglobe measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueextledglobe])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueextledglobe])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueextledglobe])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueextledglobe])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueextledglobe]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueextledglobe]
}


#find all of the intledfix Lighting measures
intledfix = grepl("LED", output$`Measure Description`, fixed = FALSE) & grepl("int|Int|Indoor|indoor", output$`Measure Description`, fixed = FALSE) & grepl("fix|Fix", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting"

#for each of the intledfix measures
for(i in which(intledfix))
{
  
  #find the unique intledfix Lighting measure
  uniqueintledfix = grepl("LED Fixture - Indoor", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" & uniquemeas$BldgType == as.character(output$`Bldg Type`[i])
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique intledfix measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueintledfix])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueintledfix])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueintledfix])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueintledfix])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueintledfix]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueintledfix]
}


#find all of the intledlamp Lighting measures
intledlamp = grepl("LED", output$`Measure Description`, fixed = FALSE) & grepl("int|Int|Indoor|indoor", output$`Measure Description`, fixed = FALSE) & grepl("lamp|Lamp", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting"

#for each of the intledlamp measures
for(i in which(intledlamp))
{
  
  #find the unique intledlamp Lighting measure
  uniqueintledlamp = grepl("LED Lamp - Indoor", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" & uniquemeas$BldgType == as.character(output$`Bldg Type`[i])
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique intledlamp measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueintledlamp])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueintledlamp])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueintledlamp])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueintledlamp])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueintledlamp]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueintledlamp]
}


#find all of the extledlamp Lighting measures
extledlamp = grepl("LED", output$`Measure Description`, fixed = FALSE) & grepl("ext|Ext|Outdoor|outdoor", output$`Measure Description`, fixed = FALSE) & grepl("lamp|Lamp", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting"

#find the unique extledlamp Lighting measure
uniqueextledlamp = grepl("LED lamp - Outdoor", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting" & uniquemeas$BldgType == as.character(output$`Bldg Type`[i])

#for each of the extledlamp measures
for(i in which(extledlamp))
{

  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique extledlamp measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueextledlamp])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueextledlamp])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueextledlamp])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueextledlamp])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueextledlamp]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueextledlamp]
}

#find all of the intledglobe Lighting measures
intledglobe = grepl("LED", output$`Measure Description`, fixed = FALSE) & grepl("int|Int|indoor|Indoor", output$`Measure Description`, fixed = FALSE) & grepl("lamp|Lamp", output$`Measure Description`, fixed = FALSE) & grepl("watt|Watt", output$`Measure Description`, fixed = FALSE)  & output$`Use Category` == "Lighting"

#find the unique intledglobe Lighting measure
uniqueintledglobe = grepl("Small wattage Globe", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting"

#for each of the intledglobe measures
for(i in which(intledglobe))
{
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique intledglobe measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueintledglobe])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueintledglobe])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueintledglobe])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueintledglobe])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueintledglobe]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueintledglobe]
}


#find all of the coolled Lighting measures
coolled = grepl("LED", output$`Measure Description`, fixed = FALSE) & grepl("cool|Cool|Freeze|freeze", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting"

#find the unique coolled Lighting measure
uniquecoolled = grepl("Coolers and Freezers", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting"

#for each of the coolled measures
for(i in which(coolled))
{
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique coolled measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquecoolled])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquecoolled])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquecoolled])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquecoolled])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquecoolled]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquecoolled]
}


#find all of the openled Lighting measures
openled = grepl("LED", output$`Measure Description`, fixed = FALSE) & grepl("open|Open", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting"

#find the unique openled Lighting measure
uniqueopenled = grepl("Open", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Lighting"

#for each of the openled measures
for(i in which(openled))
{
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique openled measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueopenled])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueopenled])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueopenled])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueopenled])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueopenled]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueopenled]
}


#find all of the signled Lighting measures
signled = grepl("LED", output$`Measure Description`, fixed = FALSE) & grepl("sign|Sign|advert|Advert", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Lighting"

#find the unique signled Lighting measure
uniquesignled = grepl("LED Lighting", uniquemeas$Description, fixed = TRUE) & uniquemeas$UseCategory == "Lighting" & uniquemeas$EUL_ID == "Oltg-LED"

#for each of the signled measures
for(i in which(signled))
{
  
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique signled measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquesignled])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquesignled])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquesignled])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquesignled])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquesignled]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquesignled]
}




#for all of the other lighting measures not identified
otherlighting = is.na(output$`EUL Years`) & output$`Use Category`== "Lighting"

#for all of the other lighting measures, the output will be the same as the claims database since it didn't fit an unique row
for(i in which(otherlighting))
{
  #the properties of the measures are the same
  output$`Use SubCategory`[i] = as.character(claims$UseSubCategory[i])
  output$`Tech Group`[i] = as.character(claims$TechGroup[i])
  output$`Tech Type`[i] = as.character(claims$TechType[i])
  output$`EUL ID`[i] = as.character(claims$EUL_ID[i])
  output$`EUL Years`[i] = claims$EUL_Yrs[i]
  output$`RUL Years`[i] = claims$RUL_Yrs[i]
}



#PROCDIST


#find all of the centrifugal ProcDist measures
centrifugal = grepl("centrifugal|Centrifugal", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ProcDist" 

#find the unique centrifugal ProcDist measure
uniquecentrifugal = grepl("Centrifugal", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ProcDist" 

#for each of the centrifugal measures
for(i in which(centrifugal))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique centrifugal measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquecentrifugal])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquecentrifugal])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquecentrifugal])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquecentrifugal])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquecentrifugal]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquecentrifugal]
}

#find all of the subboost ProcDist measures
subboost = grepl("Submersible Boost|Submersible boost|submersible Boost", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ProcDist" 

#find the unique subboost ProcDist measure
uniquesubboost = grepl("Submersible Booster", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ProcDist" 

#for each of the subboost measures
for(i in which(subboost))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique subboost measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquesubboost])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquesubboost])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquesubboost])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquesubboost])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquesubboost]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquesubboost]
}

#find all of the subwell ProcDist measures
subwell = grepl("Submersible Well|Submersible well|submersible Well", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ProcDist" 

#find the unique subwell ProcDist measure
uniquesubwell = grepl("Submersible Well", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ProcDist" 

#for each of the subwell measures
for(i in which(subwell))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique subwell measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquesubwell])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquesubwell])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquesubwell])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquesubwell])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquesubwell]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquesubwell]
}

#find all of the turbboost ProcDist measures
turbboost = grepl("Turbine Boost|Turbine boost|turbine boost", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ProcDist" 

#find the unique turbboost ProcDist measure
uniqueturbboost = grepl("Turbine Boost", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ProcDist" 

#for each of the turbboost measures
for(i in which(turbboost))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique turbboost measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueturbboost])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueturbboost])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueturbboost])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueturbboost])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueturbboost]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueturbboost]
}

#find all of the turbwell ProcDist measures
turbwell = grepl("Turbine Well|Turbine well|turbine well", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ProcDist" 

#find the unique turbwell ProcDist measure
uniqueturbwell = grepl("Turbine Well", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ProcDist" 

#for each of the turbwell measures
for(i in which(turbwell))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique turbwell measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueturbwell])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueturbwell])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueturbwell])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueturbwell])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueturbwell]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueturbwell]
}

#find all of the transpump ProcDist measures
transpump = grepl("Transfer|transfer", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ProcDist" 

#find the unique transpump ProcDist measure
uniquetranspump = grepl("Transfer", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ProcDist" 

#for each of the transpump measures
for(i in which(transpump))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique transpump measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquetranspump])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquetranspump])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquetranspump])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquetranspump])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquetranspump]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquetranspump]
}

#find all of the vacpump ProcDist measures
vacpump = grepl("Vacuum|vacuum", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ProcDist" 

#find the unique vacpump ProcDist measure
uniquevacpump = grepl("Vacuum", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ProcDist" 

#for each of the vacpump measures
for(i in which(vacpump))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique vacpump measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquevacpump])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquevacpump])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquevacpump])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquevacpump])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquevacpump]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquevacpump]
}

#for all of the other ProcDist measures not identified
otherprocdist = is.na(output$`EUL Years`) & output$`Use Category`== "ProcDist"

#for all of the other procdist measures, the output will be the same as the claims database since it didn't fit an unique row
for(i in which(otherprocdist))
{
  #the properties of the measures are the same
  output$`Use SubCategory`[i] = as.character(claims$UseSubCategory[i])
  output$`Tech Group`[i] = as.character(claims$TechGroup[i])
  output$`Tech Type`[i] = as.character(claims$TechType[i])
  output$`EUL ID`[i] = as.character(claims$EUL_ID[i])
  output$`EUL Years`[i] = claims$EUL_Yrs[i]
  output$`RUL Years`[i] = claims$RUL_Yrs[i]
}



#PROCESS

#find all of the procfan Process measures
procfan = grepl("Vacuum|vacuum", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Process" 

#find the unique procfan Process measure
uniqueprocfan = grepl("Vacuum", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Process" 

#for each of the procfan measures
for(i in which(procfan))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique procfan measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueprocfan])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueprocfan])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueprocfan])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueprocfan])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueprocfan]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueprocfan]
}

#for all of the other Process measures not identified
otherprocess = is.na(output$`EUL Years`) & output$`Use Category`== "Process"

#for all of the other Process measures, the output will be the same as the claims database since it didn't fit an unique row
for(i in which(otherprocess))
{
  #the properties of the measures are the same
  output$`Use SubCategory`[i] = as.character(claims$UseSubCategory[i])
  output$`Tech Group`[i] = as.character(claims$TechGroup[i])
  output$`Tech Type`[i] = as.character(claims$TechType[i])
  output$`EUL ID`[i] = as.character(claims$EUL_ID[i])
  output$`EUL Years`[i] = claims$EUL_Yrs[i]
  output$`RUL Years`[i] = claims$RUL_Yrs[i]
}



#PROCHEAT

#find all of the liqcomp ProcHeat measures
liqcomp = grepl("Compress|compress", output$`Measure Description`, fixed = FALSE) & grepl("liquid|Liquid", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ProcHeat" 

#find the unique liqcomp ProcHeat measure
uniqueliqcomp = grepl("Compress", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ProcHeat" 

#for each of the liqcomp measures
for(i in which(liqcomp))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique liqcomp measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueliqcomp])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueliqcomp])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueliqcomp])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueliqcomp])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueliqcomp]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueliqcomp]
}

#find all of the grnhse ProcHeat measures
grnhse = grepl("greenhouse|Greenhouse", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ProcHeat" 

#find the unique grnhse ProcHeat measure
uniquegrnhse = grepl("Greenhouse", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ProcHeat" 

#for each of the grnhse measures
for(i in which(grnhse))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique grnhse measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquegrnhse])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquegrnhse])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquegrnhse])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquegrnhse])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquegrnhse]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquegrnhse]
}

#find all of the wtrboil ProcHeat measures
wtrboil = grepl("Boiler|boiler", output$`Measure Description`, fixed = FALSE) & grepl("water|Water", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ProcHeat" 

#find the unique wtrboil ProcHeat measure
uniquewtrboil = grepl("Boiler", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ProcHeat" & uniquemeas$TechGroup == "WaterHtg_eq"

#for each of the wtrboil measures
for(i in which(wtrboil))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique wtrboil measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquewtrboil])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquewtrboil])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquewtrboil])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquewtrboil])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquewtrboil]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquewtrboil]
}

#find all of the stmboil ProcHeat measures
stmboil = grepl("Boiler|boiler", output$`Measure Description`, fixed = FALSE) & grepl("Steam|steam", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ProcHeat" 

#find the unique stmboil ProcHeat measure
uniquestmboil = grepl("Boiler", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ProcHeat" & uniquemeas$TechGroup == "SteamHtg_eq"

#for each of the stmboil measures
for(i in which(stmboil))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique stmboil measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquestmboil])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquestmboil])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquestmboil])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquestmboil])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquestmboil]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquestmboil]
}

#find all of the stmtrap ProcHeat measures
stmtrap = grepl("System-Steam", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ProcHeat" 

#find the unique stmtrap ProcHeat measure
uniquestmtrap = grepl("Trap", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ProcHeat" 

#for each of the stmtrap measures
for(i in which(stmtrap))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique stmtrap measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquestmtrap])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquestmtrap])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquestmtrap])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquestmtrap])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquestmtrap]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquestmtrap]
}

#find all of the wtrtank ProcHeat measures
wtrtank = grepl("tank|Tank", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ProcHeat" 

#find the unique wtrtank ProcHeat measure
uniquewtrtank = grepl("Tank", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ProcHeat" 

#for each of the wtrtank measures
for(i in which(wtrtank))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique wtrtank measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquewtrtank])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquewtrtank])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquewtrtank])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquewtrtank])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquewtrtank]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquewtrtank]
}

#for all of the other ProcHeat measures not identified
otherprocheat = is.na(output$`EUL Years`) & output$`Use Category`== "ProcHeat"

#for all of the other ProcHeat measures, the output will be the same as the claims database since it didn't fit an unique row
for(i in which(otherprocheat))
{
  #the properties of the measures are the same
  output$`Use SubCategory`[i] = as.character(claims$UseSubCategory[i])
  output$`Tech Group`[i] = as.character(claims$TechGroup[i])
  output$`Tech Type`[i] = as.character(claims$TechType[i])
  output$`EUL ID`[i] = as.character(claims$EUL_ID[i])
  output$`EUL Years`[i] = claims$EUL_Yrs[i]
  output$`RUL Years`[i] = claims$RUL_Yrs[i]
}



#ProcRefrig

#find all of the milkcool ProcRefrig measures
milkcool = grepl("milk|Milk", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ProcRefrig" 

#find the unique milkcool ProcRefrig measure
uniquemilkcool = grepl("Milk", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ProcRefrig" 

#for each of the milkcool measures
for(i in which(milkcool))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique milkcool measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquemilkcool])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquemilkcool])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquemilkcool])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquemilkcool])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquemilkcool]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquemilkcool]
}

#find all of the insulfrig ProcRefrig measures
insulfrig = grepl("bare|Bare", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ProcRefrig" 

#find the unique insulfrig ProRefrig measure
uniqueinsulfrig = grepl("Bare", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ProcRefrig" 

#for each of the insulfrig measures
for(i in which(insulfrig))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique insulfrig measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueinsulfrig])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueinsulfrig])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueinsulfrig])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueinsulfrig])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueinsulfrig]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueinsulfrig]
}

#find all of the compscroll ProcRefrig measures
compscroll = grepl("Scroll|scroll", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ProcRefrig" 

#find the unique compscroll ProRefrig measure
uniquecompscroll = grepl("Scroll", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ProcRefrig" 

#for each of the compscroll measures
for(i in which(compscroll))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique compscroll measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquecompscroll])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquecompscroll])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquecompscroll])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquecompscroll])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquecompscroll]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquecompscroll]
}

#find all of the subcool ProcRefrig measures
procsubcool = grepl("subcooling|Subcooling", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ProcRefrig"

#find the unique procsubcool ProcRefrig measure
uniqueprocsubcool = grepl("subcooling|Subcooling", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ProcRefrig"

#for each of the procsubcool measures
for(i in which(procsubcool))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique procsubcool measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueprocsubcool])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueprocsubcool])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueprocsubcool])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueprocsubcool])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueprocsubcool]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueprocsubcool]
}

#find all of the condenser ProcRefrig measures
proccondenser = grepl("condenser|Condenser", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ProcRefrig"

#find the unique condenser ProcRefrig measure
uniqueproccondenser = grepl("condenser|Condenser", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ProcRefrig"

#for each of the proccondenser measures
for(i in which(proccondenser))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique proccondenser measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueproccondenser])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueproccondenser])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueproccondenser])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueproccondenser])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueproccondenser]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueproccondenser]
}

#find all of the head ProcRefrig measures
prochead = grepl("head|Head", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ProcRefrig"

#find the unique head ProcRefrig measure
uniqueprochead = grepl("head|Head", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ProcRefrig"

#for each of the head measures
for(i in which(prochead))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique prochead measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueprochead])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueprochead])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueprochead])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueprochead])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueprochead]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueprochead]
}

#find all of the suction ProcRefrig measures
procsuction = grepl("suction|Suction", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ProcRefrig"

#find the unique suction ProcRefrig measure
uniqueprocsuction = grepl("suction|Suction", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ProcRefrig"

#for each of the suction measures
for(i in which(procsuction))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique procsuction measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueprocsuction])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueprocsuction])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueprocsuction])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueprocsuction])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueprocsuction]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueprocsuction]
}

#find all of the procspeed ProcRefrig measures
procspeed = grepl("speed|Speed", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ProcRefrig"

#find the unique procspeed ProcRefrig measure
uniqueprocspeed = grepl("speed|Speed", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ProcRefrig"

#for each of the procspeed measures
for(i in which(procspeed))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique procspeed measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueprocspeed])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueprocspeed])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueprocspeed])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueprocspeed])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueprocspeed]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueprocspeed]
}

#find all of the winetank ProcRefrig measures
winetank = grepl("wine|Wine", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "ProcRefrig"

#find the unique winetank ProcRefrig measure
uniquewinetank = grepl("Wine", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "ProcRefrig"

#for each of the winetank measures
for(i in which(winetank))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique winetank measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquewinetank])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquewinetank])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquewinetank])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquewinetank])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquewinetank]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquewinetank]
}

#for all of the other ProcRefrig measures not identified
otherprocrefrig = is.na(output$`EUL Years`) & output$`Use Category`== "ProcRefrig"

#for all of the other Process measures, the output will be the same as the claims database since it didn't fit an unique row
for(i in which(otherprocrefrig))
{
  #the properties of the measures are the same
  output$`Use SubCategory`[i] = as.character(claims$UseSubCategory[i])
  output$`Tech Group`[i] = as.character(claims$TechGroup[i])
  output$`Tech Type`[i] = as.character(claims$TechType[i])
  output$`EUL ID`[i] = as.character(claims$EUL_ID[i])
  output$`EUL Years`[i] = claims$EUL_Yrs[i]
  output$`RUL Years`[i] = claims$RUL_Yrs[i]
}



#Recreate

#find all of the poolhtr Recreate measures
poolhtr = grepl("heater|Heater", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Recreate"

#find the unique poolhtr Recreate measure
uniquepoolhtr = grepl("Heater", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Recreate"

#for each of the poolhtr measures
for(i in which(poolhtr))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique poolhtr measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquepoolhtr])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquepoolhtr])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquepoolhtr])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquepoolhtr])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquepoolhtr]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquepoolhtr]
}

#find all of the poolcovr Recreate measures
poolcovr = grepl("cover|Cover", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Recreate"

#find the unique poolcovr Recreate measure
uniquepoolcovr = grepl("Cover", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Recreate"

#for each of the poolcovr measures
for(i in which(poolcovr))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique poolcovr measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquepoolcovr])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquepoolcovr])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquepoolcovr])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquepoolcovr])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquepoolcovr]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquepoolcovr]
}

#for all of the other Recreate measures not identified
otherrecreate = is.na(output$`EUL Years`) & output$`Use Category`== "Recreate"

#for all of the other Process measures, the output will be the same as the claims database since it didn't fit an unique row
for(i in which(otherrecreate))
{
  #the properties of the measures are the same
  output$`Use SubCategory`[i] = as.character(claims$UseSubCategory[i])
  output$`Tech Group`[i] = as.character(claims$TechGroup[i])
  output$`Tech Type`[i] = as.character(claims$TechType[i])
  output$`EUL ID`[i] = as.character(claims$EUL_ID[i])
  output$`EUL Years`[i] = claims$EUL_Yrs[i]
  output$`RUL Years`[i] = claims$RUL_Yrs[i]
}



#SERVICE

#find all of the boiltune Service measures
boiltune = grepl("tune|Tune", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Service"

#find the unique boiltune Service measure
uniqueboiltune = grepl("Tune", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Service"

#for each of the boiltune measures
for(i in which(boiltune))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique boiltune measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueboiltune])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueboiltune])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueboiltune])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueboiltune])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueboiltune]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueboiltune]
}

#find all of the condcoil Service measures
condcoil = grepl("condenser coil|Condenser Coil", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Service"

#find the unique condcoil Service measure
uniquecondcoil = grepl("Condenser", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Service"

#for each of the condcoil measures
for(i in which(condcoil))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique condcoil measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquecondcoil])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquecondcoil])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquecondcoil])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquecondcoil])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquecondcoil]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquecondcoil]
}

#find all of the evapcoil Service measures
evapcoil = grepl("evaporator coil|Evaporator coil", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Service"

#find the unique evapcoil Service measure
uniqueevapcoil = grepl("Evaporator", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Service"

#for each of the evapcoil measures
for(i in which(evapcoil))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique evapcoil measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueevapcoil])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueevapcoil])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueevapcoil])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueevapcoil])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueevapcoil]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueevapcoil]
}

#find all of the refgroc Service measures
refgroc = grepl("grocery|Grocery", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Service"

#find the unique refgroc Service measure
uniquerefgroc = grepl("Grocery", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Service"

#for each of the refgroc measures
for(i in which(refgroc))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique refgroc measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquerefgroc])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquerefgroc])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquerefgroc])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquerefgroc])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquerefgroc]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquerefgroc]
}

#find all of the refware Service measures
refware = grepl("warehouse|Warehouse", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Service"

#find the unique refware Service measure
uniquerefware = grepl("Warehouse", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Service"

#for each of the refware measures
for(i in which(refware))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique refware measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquerefware])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquerefware])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquerefware])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquerefware])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquerefware]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquerefware]
}

#find all of the therm Service measures
therm = grepl("thermostat|Thermostat", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Service"

#find the unique therm Service measure
uniquetherm = grepl("Thermostat", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Service"

#for each of the therm measures
for(i in which(therm))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique therm measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquetherm])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquetherm])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquetherm])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquetherm])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquetherm]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquetherm]
}

#find all of the rturetro Service measures
rturetro = grepl("rooftop|Rooftop", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "Service"

#find the unique rturetro Service measure
uniquerturetro = grepl("Rooftop", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Service"

#for each of the rturetro measures
for(i in which(rturetro))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique rturetro measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquerturetro])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquerturetro])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquerturetro])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquerturetro])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquerturetro]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquerturetro]
}

#for all of the other Service measures not identified
otherservice = is.na(output$`EUL Years`) & output$`Use Category`== "Service"

#for all of the other Process measures, the output will be the same as the claims database since it didn't fit an unique row
for(i in which(otherservice))
{
  #the properties of the measures are the same
  output$`Use SubCategory`[i] = as.character(claims$UseSubCategory[i])
  output$`Tech Group`[i] = as.character(claims$TechGroup[i])
  output$`Tech Type`[i] = as.character(claims$TechType[i])
  output$`EUL ID`[i] = as.character(claims$EUL_ID[i])
  output$`EUL Years`[i] = claims$EUL_Yrs[i]
  output$`RUL Years`[i] = claims$RUL_Yrs[i]
}



#SHW

#find all of the circtime SHW measures
circtime = grepl("circ|Circ", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "SHW"

#find the unique circtime SHW measure
uniquecirctime = grepl("Circ", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "SHW"

#for each of the circtime measures
for(i in which(circtime))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique circtime measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquecirctime])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquecirctime])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquecirctime])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquecirctime])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquecirctime]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquecirctime]
}

#find all of the instwtrhtr SHW measures
instwtrhtr = grepl("instant|Instant", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "SHW"

#find the unique instwtrhtr SHW measure
uniqueinstwtrhtr = grepl("Instant", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "SHW"

#for each of the instwtrhtr measures
for(i in which(instwtrhtr))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique instwtrhtr measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueinstwtrhtr])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueinstwtrhtr])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueinstwtrhtr])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueinstwtrhtr])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueinstwtrhtr]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueinstwtrhtr]
}

#find all of the comwtrhtr SHW measures
comwtrhtr = grepl("commercial water heater|Commercial Water Heater|Commmercial water heater", output$`Measure Description`, fixed = TRUE) & output$`Use Category` == "SHW"

#find the unique comwtrhtr SHW measure
uniquecomwtrhtr = grepl("Commercial Water heater", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "SHW"

#for each of the comwtrhtr measures
for(i in which(comwtrhtr))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique comwtrhtr measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquecomwtrhtr])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquecomwtrhtr])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquecomwtrhtr])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquecomwtrhtr])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquecomwtrhtr]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquecomwtrhtr]
}

#find all of the shwcomp SHW measures
shwcomp = grepl("comp|Comp", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "SHW"

#find the unique shwcomp SHW measure
uniqueshwcomp = grepl("Compressor", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "SHW"

#for each of the shwcomp measures
for(i in which(shwcomp))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique shwcomp measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueshwcomp])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueshwcomp])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueshwcomp])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueshwcomp])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueshwcomp]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueshwcomp]
}

#find all of the shwcomp SHW measures
shwcomp = grepl("comp|Comp", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "SHW"

#find the unique shwcomp SHW measure
uniqueshwcomp = grepl("Compressor", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "SHW"

#for each of the shwcomp measures
for(i in which(shwcomp))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique shwcomp measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueshwcomp])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueshwcomp])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueshwcomp])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueshwcomp])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueshwcomp]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueshwcomp]
}

#find all of the faucet SHW measures
faucet = grepl("faucet|Faucet", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "SHW"

#find the unique faucet SHW measure
uniquefaucet = grepl("Faucet", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "SHW"

#for each of the faucet measures
for(i in which(faucet))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique faucet measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquefaucet])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquefaucet])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquefaucet])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquefaucet])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquefaucet]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquefaucet]
}

#find all of the shwlowflow SHW measures
shwlowflow = grepl("low flow|Low flow|Low Flow", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "SHW"

#find the unique shwlowflow SHW measure
uniqueshwlowflow = grepl("Low flow", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "SHW"

#for each of the shwlowflow measures
for(i in which(shwlowflow))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique shwlowflow measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueshwlowflow])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueshwlowflow])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueshwlowflow])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueshwlowflow])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueshwlowflow]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueshwlowflow]
}

#find all of the elecinsul SHW measures
elecinsul = grepl("insulation|Insulation", output$`Measure Description`, fixed = FALSE) & grepl("electric|Electric", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "SHW"

#find the unique elecinsul SHW measure
uniqueelecinsul = grepl("Electric Water Heater", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "SHW"

#for each of the elecinsul measures
for(i in which(elecinsul))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique elecinsul measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueelecinsul])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueelecinsul])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueelecinsul])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueelecinsul])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueelecinsul]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueelecinsul]
}

#find all of the gasinsul SHW measures
gasinsul = grepl("insulation|Insulation", output$`Measure Description`, fixed = FALSE) & grepl("gas|Gas", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "SHW"

#find the unique gasinsul SHW measure
uniquegasinsul = grepl("Gas Water Heater", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "SHW"

#for each of the gasinsul measures
for(i in which(gasinsul))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique gasinsul measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquegasinsul])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquegasinsul])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquegasinsul])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquegasinsul])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquegasinsul]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquegasinsul]
}

#find all of the elecwrap SHW measures
elecwrap = grepl("tank|Tank", output$`Measure Description`, fixed = FALSE) & grepl("electric|Electric", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "SHW"

#find the unique elecwrap SHW measure
uniqueelecwrap = grepl("Wrap - Electric", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "SHW"

#for each of the elecwrap measures
for(i in which(elecwrap))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique elecwrap measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueelecwrap])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueelecwrap])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueelecwrap])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueelecwrap])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueelecwrap]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueelecwrap]
}

#find all of the gaswrap SHW measures
gaswrap = grepl("tank|Tank", output$`Measure Description`, fixed = FALSE) & grepl("gas|Gas", output$`Measure Description`, fixed = FALSE) & output$`Use Category` == "SHW"

#find the unique gaswrap SHW measure
uniquegaswrap = grepl("Wrap - Gas", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "SHW"

#for each of the gaswrap measures
for(i in which(gaswrap))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique gaswrap measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquegaswrap])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquegaswrap])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquegaswrap])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquegaswrap])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquegaswrap]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquegaswrap]
}

#for all of the other SHW measures not identified
othershw = is.na(output$`EUL Years`) & output$`Use Category`== "SHW"

#for all of the other Process measures, the output will be the same as the claims database since it didn't fit an unique row
for(i in which(othershw))
{
  #the properties of the measures are the same
  output$`Use SubCategory`[i] = as.character(claims$UseSubCategory[i])
  output$`Tech Group`[i] = as.character(claims$TechGroup[i])
  output$`Tech Type`[i] = as.character(claims$TechType[i])
  output$`EUL ID`[i] = as.character(claims$EUL_ID[i])
  output$`EUL Years`[i] = claims$EUL_Yrs[i]
  output$`RUL Years`[i] = claims$RUL_Yrs[i]
}


#ANY


#find all of the behave measures
behave = grepl("behavioral|Behavioral", output$`Measure Description`, fixed = FALSE)

#find the unique behave measure
uniquebehave = grepl("behavioral", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Any"

#for each of the behave measures
for(i in which(behave))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique behave measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquebehave])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquebehave])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquebehave])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquebehave])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquebehave]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquebehave]
}


#find all of the powsoft measures
powsoft = grepl("management software|Management Software|Management software", output$`Measure Description`, fixed = FALSE)

#find the unique powsoft measure
uniquepowsoft = grepl("Management Software", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Any"

#for each of the powsoft measures
for(i in which(powsoft))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique powsoft measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquepowsoft])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquepowsoft])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquepowsoft])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquepowsoft])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquepowsoft]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquepowsoft]
}


#find all of the premmot  measures
premmot = grepl("Premium", output$`Measure Description`, fixed = FALSE)

#find the unique premmot measure
uniquepremmot = grepl("Premium", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Any"

#for each of the premmot measures
for(i in which(premmot))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique premmot measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquepremmot])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquepremmot])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquepremmot])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquepremmot])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquepremmot]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquepremmot]
}


#find all of the wtrlooppump measures
wtrlooppump = grepl("Water Loop Pump", output$`Measure Description`, fixed = FALSE) & output$`Measure Description` == "Any"

#find the unique wtrlooppump measure
uniquewtrlooppump = grepl("Water Loop", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Any"

#for each of the wtrlooppump measures
for(i in which(wtrlooppump))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique wtrlooppump measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquewtrlooppump])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquewtrlooppump])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquewtrlooppump])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquewtrlooppump])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquewtrlooppump]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquewtrlooppump]
}


#find all of the enerpolicy measures
enerpolicy = grepl("energy policy Manual|Energy policy manual|Energy Policy Manual", output$`Measure Description`, fixed = FALSE) & output$`Measure Description` == "Any"

#find the unique enerpolicy measure
uniqueenerpolicy = grepl("Energy Policy Manual", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Any"

#for each of the enerpolicy measures
for(i in which(enerpolicy))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique enerpolicy measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniqueenerpolicy])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniqueenerpolicy])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniqueenerpolicy])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniqueenerpolicy])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniqueenerpolicy]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniqueenerpolicy]
}


#find all of the rcxgen measures
rcxgen = grepl("RCX|rcx|Retrocommiss|retrocommiss", output$`Measure Description`, fixed = FALSE) 

#find the unique rcxgen measure
uniquercxgen = grepl("retrocommissioning", uniquemeas$Description, fixed = FALSE) & uniquemeas$UseCategory == "Any"

#for each of the rcxgen measures
for(i in which(rcxgen))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique rcxgen measure
  output$`Use SubCategory`[i] = as.character(uniquemeas$UseSubCategory[uniquercxgen])
  output$`Tech Group`[i] = as.character(uniquemeas$TechGroup[uniquercxgen])
  output$`Tech Type`[i] = as.character(uniquemeas$TechType[uniquercxgen])
  output$`EUL ID`[i] = as.character(uniquemeas$EUL_ID[uniquercxgen])
  output$`EUL Years`[i] = uniquemeas$EUL_Yrs[uniquercxgen]
  output$`RUL Years`[i] = uniquemeas$RUL_Yrs[uniquercxgen]
}


#find the unique rcxgen measure
allother = is.na(output$`EUL Years`)

#for each of the rcxgen measures
for(i in which(allother))
{
  #set the Subcategory, Tech group, tech type, EUL ID, RUL and EUL based on the unique rcxgen measure
  output$`Use SubCategory`[i] = as.character(claims$UseSubCategory[i])
  output$`Tech Group`[i] = as.character(claims$TechGroup[i])
  output$`Tech Type`[i] = as.character(claims$TechType[i])
  output$`EUL ID`[i] = as.character(claims$EUL_ID[i])
  output$`EUL Years`[i] = claims$EUL_Yrs[i]
  output$`RUL Years`[i] = claims$RUL_Yrs[i]
}

#set change flag for RUL (1254/8491)
for(i in 1:dim(claims)[1])
{
  if( claims$RUL_Yrs[i]!= output$`RUL Years`[i] | is.na(output$`RUL Years`[i]))
  {
    output$`RUL Change Flag`[i] = 1
  }
  else
  {
    output$`RUL Change Flag`[i] = 0
  }
}

#set change flag for EUL (239/8491)
for(i in 1:dim(claims)[1])
{
  if(claims$EUL_Yrs[i]!= output$`EUL Years`[i] | is.na(output$`EUL Years`[i]))
  {
    output$`EUL Change Flag`[i] = 1
  }
  else
  {
    output$`EUL Change Flag`[i] = 0
  }
}


#set working directory to this
setwd("Z:/Folders/CPUC10 (Group D - Custom EM&V)/4 Deliverables/00 - 2017 Evaluation/Database Review/E350 Data Additions/Raw Data Files")

#write out the file
write.csv(output, file = "EUL & RUL Export.csv")


