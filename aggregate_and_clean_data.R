# ********** PROCESSING UNIQUE NUMERIC IDs FOR COMBINING DATA INTO A SINGLE DATA FRAME ************************ #
ids_income = income_data$GeoId
standard_id_length = 7
ids_income_ints = unlist(lapply(ids_income, convert_income_id_to_integer, desired_int_length = standard_id_length, num_offset = 7))

# INSERT COLUMN FOR SCHOOL DISTRICT IDs IN NUMERIC FORM FOR INCOME DATA_FRAME
income_data$district_id = ids_income_ints

# CREATE NEW DATAFRAME BY MATCHING RECORDS FROM GRAD_RATES DATA WITH THOSE OF INCOME DATA FRAME USING  district_id AS MATCHING IDENTIFIER
matching_indices = match(income_data$district_id, grad_rates$leaid12)
matching_indices = na.omit(matching_indices)
ed_data = grad_rates[matching_indices,1:6]

# INSERT COLUMN 'median_household_income' IN NUMERIC FORM FOR INCOME DATA_FRAME
rates_numeric = unlist(lapply(ed_data$ALL_RATE_1213, process_rate))
NA_indices_ed_data = which(is.na(rates_numeric))
indices_with_negativeone_gradrate = which(rates_numeric == -1)
ed_data$acgr = rates_numeric

# Remove any rows that had acgr assigned to -1 (data elements that do not belong to both sets)
rows_to_remove = which(ed_data$acgr == -1)
if (length(rows_to_remove) != 0){
  print('Removing records with acgr = -1')
  ed_data = ed_data[-rows_to_remove,]
}

# INSERT INCOME VALUES FROM INCOME_DATAFRAME INTO THE COMPOSITE DATAFRAME ed_data
matching_indices = match(ed_data$leaid12, income_data$district_id)
ed_data$income = income_data$CDP03_16[matching_indices]   #CDP03_16 = median household income in dollars

# PROCESS DEMOGRAPHIC DATA
ids_from_demo = demographics$Agency.ID...NCES.Assigned..District..Latest.available.year
demographics$district_id = unlist(lapply(ids_from_demo,convert_income_id_to_integer, desired_int_length = 7, num_offset = 1))

# Process (i.e. cleanse) and then insert columns from demographic data
matching_indices = match(ed_data$leaid12, demographics$district_id)
if (number_of_na_values(matching_indices) > 0){
  matching_indices = na.omit(matching_indices)  
}

ed_data$FirstNationsCount = unlist(lapply(demographics$American.Indian.Alaska.Native.Students..District..2012.13[matching_indices], convert_numeric_factor_to_integer))

ed_data$AsianPacificCount = unlist(lapply(demographics$Asian.or.Asian.Pacific.Islander.Students..District..2012.13[matching_indices], convert_numeric_factor_to_integer)) + unlist(lapply(demographics$Hawaiian.Nat..Pacific.Isl..Students..District..2012.13[matching_indices],convert_numeric_factor_to_integer))

ed_data$HispanicCount = unlist(lapply(demographics$Hispanic.Students..District..2012.13[matching_indices],convert_numeric_factor_to_integer))

ed_data$BlackCount = unlist(lapply(demographics$Black.Students..District..2012.13[matching_indices],convert_numeric_factor_to_integer))

ed_data$WhiteCount = unlist(lapply(demographics$White.Students..District..2012.13[matching_indices],convert_numeric_factor_to_integer))

ed_data$MultiRacialCount = unlist(lapply(demographics$Two.or.More.Races.Students..District..2012.13[matching_indices],convert_numeric_factor_to_integer))

ed_data$TotalRace = unlist(lapply(demographics$Total.Race.Ethnicity..District..2012.13[matching_indices],convert_numeric_factor_to_integer))

ed_data$StudentTeacher = unlist(lapply(demographics$Pupil.Teacher.Ratio..District..2012.13[matching_indices],convert_numeric_factor_to_integer))

ed_data$UrbanLocale = demographics$Urban.centric.Locale..District..2012.13[matching_indices]
#ed_data$MetroMicroAreaCode = demographics$Metro.Micro.Area.Code..District..2012.13

ed_data$MalesCount = unlist(lapply(demographics$Male.Students..District..2012.13[matching_indices],convert_numeric_factor_to_integer))
ed_data$FemalesCount = unlist(lapply(demographics$Female.Students..District..2012.13[matching_indices],convert_numeric_factor_to_integer))

ed_data$ELL_Count = unlist(lapply(demographics$Limited.English.Proficient..LEP....English.Language.Learners..ELL...District..2012.13[matching_indices],convert_numeric_factor_to_integer))

ed_data$Reduced_lunch = unlist(lapply(demographics$Total.Free.and.Reduced.Lunch.Students..Public.School..2012.13[matching_indices],convert_numeric_factor_to_integer))

ed_data$TotalRevenuePerPupil = unlist(lapply(demographics$Total.Revenue..TOTALREV..per.Pupil..V33...District.Finance..2012.13[matching_indices],convert_numeric_factor_to_integer))

ed_data$FederalRevenuePerPupil = unlist(lapply(demographics$Total.Revenue...Federal.Sources..TFEDREV..per.Pupil..V33...District.Finance..2012.13[matching_indices],convert_numeric_factor_to_integer))

ed_data$InstrExpend = unlist(lapply(demographics$Total.Current.Expenditures...Instruction..TCURINST..per.Pupil..V33...District.Finance..2012.13[matching_indices],convert_numeric_factor_to_integer))

ed_data$pctFedRevToTotal = 
  unlist(lapply(demographics$Total.Revenue...Federal.Sources..TFEDREV..as.Percentage.of.Total.Revenue..TOTALREV...District.Finance..2012.13[matching_indices],convert_numeric_factor_to_integer))

ed_data$pctInstrExpOfDaily = unlist(lapply(demographics$Total.Current.Expenditures...Instruction..TCURINST..as.Percentage.of.Curr.El.SEC..TCURELSC...District.Finance..2012.13[matching_indices],convert_numeric_factor_to_integer))

ed_data$SSvcOfDayCost = unlist(lapply(demographics$Total.Current.Expenditures...Support.Services..TCURSSVC..as.Percentage.of.Curr.El.Sec..TCURELSC...District.Finance..2012.13[matching_indices],convert_numeric_factor_to_integer))

ed_data$Latitude = unlist(lapply(demographics$Latitude..District..2012.13[matching_indices],convert_numeric_factor_to_numeric))
ed_data$Longitude = unlist(lapply(demographics$Longitude..District..2012.13[matching_indices],convert_numeric_factor_to_numeric))

# Transformed / Calculated variables
ed_data$Lunch = round( (ed_data$Reduced_lunch / ed_data$TotalRace)*100)
ed_data$Male = round( (ed_data$MalesCount / ed_data$TotalRace)*100)
ed_data$White = round( ( ed_data$WhiteCount / ed_data$TotalRace)*100)
ed_data$Black = round( ( ed_data$BlackCount / ed_data$TotalRace)*100)
ed_data$FirstNations = round( ( ed_data$FirstNationsCount / ed_data$TotalRace)*100)
ed_data$Hispanic = round( ( ed_data$HispanicCount / ed_data$TotalRace)*100)
ed_data$Asian = round( ( ed_data$AsianPacificCount / ed_data$TotalRace)*100)
ed_data$MultiRacial = round( ( ed_data$MultiRacialCount / ed_data$TotalRace)*100)
ed_data$nonWhite = round( ( (ed_data$TotalRace - ed_data$WhiteCount) / ed_data$TotalRace)*100)
ed_data$under_minority = round( ( (ed_data$TotalRace - ed_data$WhiteCount - ed_data$AsianPacificCount) / ed_data$TotalRace)*100) 
ed_data$pctELL = round( ed_data$ELL_Count / ed_data$TotalRace * 100)

# From ed_data, remove records that have zero or NA values in the following columns {TotalRace}
zero_students = which(ed_data$TotalRace ==0)
ed_data = ed_data[-zero_students,]