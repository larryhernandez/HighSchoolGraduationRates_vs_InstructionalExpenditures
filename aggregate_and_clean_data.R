# ********** PROCESSING UNIQUE NUMERIC IDs FOR COMBINING DATA INTO A SINGLE DATA FRAME ************************ #
ids_income = income_data$GeoId
standard_id_length = 7
ids_income_ints = sapply(ids_income, convert_income_id_to_integer, desired_int_length = standard_id_length, num_offset = 7)

# INSERT COLUMN FOR SCHOOL DISTRICT IDs IN NUMERIC FORM FOR INCOME DATA_FRAME
income_data$district_id = ids_income_ints

############# CREATE 'ed_data' BY MATCHING RECORDS FROM 'grad_rates' WITH income_data USING 'district_id' AS VARIABLE TO MATCH ON #######
########################################################################################################################################
#matching_indices = match(income_data$district_id, grad_rates$leaid12)
matching_indices = match(income_data$district_id, grad_rates$LEAID)
matching_indices = na.omit(matching_indices)
ed_data = grad_rates[matching_indices,1:6]
colnames(ed_data)[c(3)]<-c("leaid12")

# Insert graduation rates as 'rates_numeric'
rates_numeric = sapply(ed_data$ALL_RATE_1213, process_rate)
ed_data$acgr = rates_numeric

# Remove records where acgr was assigned to -1 (i.e. grad rates reported as "PS" to protect student privacy)
rows_to_remove = which(ed_data$acgr == -1)
N = length(rows_to_remove)
if (N != 0){
  print(paste('Removing ',N,'records with acgr = -1'))
  ed_data = ed_data[-rows_to_remove,]
}

# Insert income values from 'income_data' into 'ed_data'
# Note: CDP03_16 := median household income in dollars
matching_indices = match(ed_data$leaid12, income_data$district_id)
ed_data$income = income_data$CDP03_16[matching_indices]

# PROCESS DEMOGRAPHIC DATA
ids_from_demo = demographics$Agency.ID...NCES.Assigned..District..Latest.available.year
demographics$district_id = sapply(ids_from_demo,convert_income_id_to_integer, desired_int_length = 7, num_offset = 1)

# Process (i.e. cleanse) and then insert columns from demographic data
matching_indices = match(ed_data$leaid12, demographics$district_id)

# Add student demographic information to ed_data
ed_data$FirstNationsCount = sapply(demographics$American.Indian.Alaska.Native.Students..District..2012.13[matching_indices], convert_numeric_factor_to_integer)

ed_data$AsianPacificCount = sapply(demographics$Asian.or.Asian.Pacific.Islander.Students..District..2012.13[matching_indices], convert_numeric_factor_to_integer) + sapply(demographics$Hawaiian.Nat..Pacific.Isl..Students..District..2012.13[matching_indices],convert_numeric_factor_to_integer)

ed_data$HispanicCount = sapply(demographics$Hispanic.Students..District..2012.13[matching_indices],convert_numeric_factor_to_integer)

ed_data$BlackCount = sapply(demographics$Black.Students..District..2012.13[matching_indices],convert_numeric_factor_to_integer)

ed_data$WhiteCount = sapply(demographics$White.Students..District..2012.13[matching_indices],convert_numeric_factor_to_integer)

ed_data$MultiRacialCount = sapply(demographics$Two.or.More.Races.Students..District..2012.13[matching_indices],convert_numeric_factor_to_integer)

ed_data$TotalRace = sapply(demographics$Total.Race.Ethnicity..District..2012.13[matching_indices],convert_numeric_factor_to_integer)

ed_data$StudentTeacher = sapply(demographics$Pupil.Teacher.Ratio..District..2012.13[matching_indices],convert_numeric_factor_to_integer)

ed_data$StudentTeacher.2013.14 = sapply(demographics$Pupil.Teacher.Ratio..District..2013.14[matching_indices],convert_numeric_factor_to_integer)

ed_data$UrbanLocale = demographics$Urban.centric.Locale..District..2012.13[matching_indices]
#ed_data$MetroMicroAreaCode = demographics$Metro.Micro.Area.Code..District..2012.13

ed_data$ELL_Count = sapply(demographics$Limited.English.Proficient..LEP....English.Language.Learners..ELL...District..2012.13[matching_indices],convert_numeric_factor_to_integer)

ed_data$ELL_Count.2013.14 = sapply(demographics$Limited.English.Proficient..LEP....English.Language.Learners..ELL...District..2013.14[matching_indices],convert_numeric_factor_to_integer)

ed_data$ReducedLunch = sapply(demographics$Total.Free.and.Reduced.Lunch.Students..Public.School..2012.13[matching_indices],convert_numeric_factor_to_integer)

ed_data$TotalRevenuePerPupil = sapply(demographics$Total.Revenue..TOTALREV..per.Pupil..V33...District.Finance..2012.13[matching_indices],convert_numeric_factor_to_integer)

ed_data$FederalRevenuePerPupil = sapply(demographics$Total.Revenue...Federal.Sources..TFEDREV..per.Pupil..V33...District.Finance..2012.13[matching_indices],convert_numeric_factor_to_integer)

ed_data$InstrExpend = sapply(demographics$Total.Current.Expenditures...Instruction..TCURINST..per.Pupil..V33...District.Finance..2012.13[matching_indices],convert_numeric_factor_to_integer)

ed_data$pctFedRevToTotal = 
  sapply(demographics$Total.Revenue...Federal.Sources..TFEDREV..as.Percentage.of.Total.Revenue..TOTALREV...District.Finance..2012.13[matching_indices],convert_numeric_factor_to_integer)

ed_data$pctInstrExpOfDaily = sapply(demographics$Total.Current.Expenditures...Instruction..TCURINST..as.Percentage.of.Curr.El.SEC..TCURELSC...District.Finance..2012.13[matching_indices],convert_numeric_factor_to_integer)

ed_data$SSvcOfDayCost = sapply(demographics$Total.Current.Expenditures...Support.Services..TCURSSVC..as.Percentage.of.Curr.El.Sec..TCURELSC...District.Finance..2012.13[matching_indices],convert_numeric_factor_to_integer)

ed_data$Latitude = sapply(demographics$Latitude..District..2012.13[matching_indices],convert_numeric_factor_to_numeric)

ed_data$Longitude = sapply(demographics$Longitude..District..2012.13[matching_indices],convert_numeric_factor_to_numeric)

############################## Impute missing data with median values for corresponding state #################################################
ed_data = fill_missing_values_by_category(ed_data, category = "FIPST", col_to_impute = "FirstNationsCount", alt_fill = "None", method = "median", round_to_whole = TRUE)
ed_data = fill_missing_values_by_category(ed_data, category = "FIPST", col_to_impute = "AsianPacificCount", alt_fill = "None", method = "median", round_to_whole = TRUE)
ed_data = fill_missing_values_by_category(ed_data, category = "FIPST", col_to_impute = "HispanicCount", alt_fill = "None", method = "median", round_to_whole = TRUE)
ed_data = fill_missing_values_by_category(ed_data, category = "FIPST", col_to_impute = "BlackCount", alt_fill = "None", method = "median", round_to_whole = TRUE)
ed_data = fill_missing_values_by_category(ed_data, category = "FIPST", col_to_impute = "WhiteCount", alt_fill = "None", method = "median", round_to_whole = TRUE)
ed_data = fill_missing_values_by_category(ed_data, category = "FIPST", col_to_impute = "MultiRacialCount", alt_fill = "None", method = "median", round_to_whole = TRUE)
ed_data = fill_missing_values_by_category(ed_data, category = "FIPST", col_to_impute = "ReducedLunch", alt_fill = "None", method = "median", round_to_whole = TRUE)
ed_data = fill_missing_values_by_category(ed_data, category = "FIPST", col_to_impute = "TotalRevenuePerPupil", alt_fill = "None", method = "median", round_to_whole = TRUE)
ed_data = fill_missing_values_by_category(ed_data, category = "FIPST", col_to_impute = "FederalRevenuePerPupil", alt_fill = "None", method = "median", round_to_whole = TRUE)
ed_data = fill_missing_values_by_category(ed_data, category = "FIPST", col_to_impute = "InstrExpend", alt_fill = "None", method = "median", round_to_whole = TRUE)
ed_data = fill_missing_values_by_category(ed_data, category = "FIPST", col_to_impute = "ELL_Count", alt_fill = "ELL_Count.2013.14", method = "median", round_to_whole = TRUE)
ed_data = fill_missing_values_by_category(ed_data, category = "FIPST", col_to_impute = "StudentTeacher", alt_fill = "StudentTeacher.2013.14", method = "median", round_to_whole = TRUE)

############################################################################################################################
##############################     Calculate values if missing      #####################################################
############################################################################################################################
# Calculate TotalRace
zero_values = which(ed_data$TotalRace == 0)
na_values = which(is.na(ed_data$TotalRace))
indices_to_fill = c(zero_values,na_values)
races = c("FirstNationsCount","AsianPacificCount","HispanicCount","BlackCount","WhiteCount","MultiRacialCount")
ed_data[indices_to_fill,"TotalRace"] = sum(ed_data[indices_to_fill,races])

# Calculate pctFedRevToTotal
na_values = which(is.na(ed_data$pctFedRevToTotal))
ed_data[na_values,"pctFedRevToTotal"] = round(ed_data[na_values,"FederalRevenuePerPupil"] / ed_data[na_values, "TotalRevenuePerPupil"])

############################## Transformed / Calculated variables  ######################################################
project_scale = 100
ed_data$Lunch = linear_scale(ed_data$ReducedLunch / ed_data$TotalRace, project_scale)
ed_data$White = linear_scale(ed_data$WhiteCount / ed_data$TotalRace, project_scale)
ed_data$Black = linear_scale(ed_data$BlackCount / ed_data$TotalRace, project_scale)
ed_data$FirstNations = linear_scale(ed_data$FirstNationsCount / ed_data$TotalRace, project_scale)
ed_data$Hispanic = linear_scale(ed_data$HispanicCount / ed_data$TotalRace, project_scale)
ed_data$Asian = linear_scale(ed_data$AsianPacificCount / ed_data$TotalRace,project_scale)
ed_data$MultiRacial = linear_scale(ed_data$MultiRacialCount / ed_data$TotalRace, project_scale)
ed_data$nonWhite = linear_scale( (ed_data$TotalRace - ed_data$WhiteCount) / ed_data$TotalRace, project_scale)
ed_data$under_minority = linear_scale( (ed_data$TotalRace - ed_data$WhiteCount - ed_data$AsianPacificCount) / ed_data$TotalRace, project_scale)
ed_data$pctELL = linear_scale(ed_data$ELL_Count / ed_data$TotalRace, project_scale)

############################################ Remove records that do not have latitude or longitude ############
no_Lat = which(is.na(ed_data$Latitude))
ed_data = remove_records(ed_data,no_Lat)

no_Long = which(is.na(ed_data$Longitude))
ed_data = remove_records(ed_data,no_Lat)

######################### Remove any incomplete cases at this point  ###########################################
#ed_data = na.omit(ed_data)