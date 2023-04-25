library(readr)
srvyin_1999 <- paste("~/Downloads/NHANES_1999_2000_MORT_2015_PUBLIC.dat")   # full .DAT name here
srvyin_2001 <- paste("~/Downloads/NHANES_2001_2002_MORT_2015_PUBLIC.dat")   # full .DAT name here
srvyin_2003 <- paste("~/Downloads/NHANES_2003_2004_MORT_2015_PUBLIC.dat")   # full .DAT name here
srvyin_2005 <- paste("~/Downloads/NHANES_2005_2006_MORT_2015_PUBLIC.dat")   # full .DAT name here


# read in the fixed-width format ASCII file
mort_1999<- read_fwf(file=srvyin_1999,
                     col_types = "ciiiiiiiddii",
                     fwf_cols(publicid = c(1,14),
                              eligstat = c(15,15),
                              mortstat = c(16,16),
                              ucod_leading = c(17,19),
                              diabetes = c(20,20),
                              hyperten = c(21,21),
                              dodqtr = c(22,22),
                              dodyear = c(23,26),
                              wgt_new = c(27,34),
                              sa_wgt_new = c(35,42),
                              permth_int = c(43,45),
                              permth_exm = c(46,48)
                     ),
                     na = "."
)
mort_1999$SEQN <- substr(mort_1999$publicid,1,5)
# NOTE:   SEQN is the unique ID for NHANES.

#Drop NHIS variables
mort_1999 <- dplyr::select(mort_1999, -publicid)
mort_1999 <- dplyr::select(mort_1999, -dodqtr)
mort_1999 <- dplyr::select(mort_1999, -dodyear)
mort_1999 <- dplyr::select(mort_1999, -wgt_new)
mort_1999 <- dplyr::select(mort_1999, -sa_wgt_new)
mort_1999$YEAR = 1999

mort_2001<- read_fwf(file=srvyin_2001,
                     col_types = "ciiiiiiiddii",
                     fwf_cols(publicid = c(1,14),
                              eligstat = c(15,15),
                              mortstat = c(16,16),
                              ucod_leading = c(17,19),
                              diabetes = c(20,20),
                              hyperten = c(21,21),
                              dodqtr = c(22,22),
                              dodyear = c(23,26),
                              wgt_new = c(27,34),
                              sa_wgt_new = c(35,42),
                              permth_int = c(43,45),
                              permth_exm = c(46,48)
                     ),
                     na = "."
)
mort_2001$SEQN <- substr(mort_2001$publicid,1,5)
# NOTE:   SEQN is the unique ID for NHANES.

#Drop NHIS variables
mort_2001 <- dplyr::select(mort_2001, -publicid)
mort_2001 <- dplyr::select(mort_2001, -dodqtr)
mort_2001 <- dplyr::select(mort_2001, -dodyear)
mort_2001 <- dplyr::select(mort_2001, -wgt_new)
mort_2001 <- dplyr::select(mort_2001, -sa_wgt_new)
mort_2001$YEAR = 2001

mort_2003<- read_fwf(file=srvyin_2003,
                     col_types = "ciiiiiiiddii",
                     fwf_cols(publicid = c(1,14),
                              eligstat = c(15,15),
                              mortstat = c(16,16),
                              ucod_leading = c(17,19),
                              diabetes = c(20,20),
                              hyperten = c(21,21),
                              dodqtr = c(22,22),
                              dodyear = c(23,26),
                              wgt_new = c(27,34),
                              sa_wgt_new = c(35,42),
                              permth_int = c(43,45),
                              permth_exm = c(46,48)
                     ),
                     na = "."
)
mort_2003$SEQN <- substr(mort_2003$publicid,1,5)
# NOTE:   SEQN is the unique ID for NHANES.

#Drop NHIS variables
mort_2003 <- dplyr::select(mort_2003, -publicid)
mort_2003 <- dplyr::select(mort_2003, -dodqtr)
mort_2003 <- dplyr::select(mort_2003, -dodyear)
mort_2003 <- dplyr::select(mort_2003, -wgt_new)
mort_2003 <- dplyr::select(mort_2003, -sa_wgt_new)
mort_2003$YEAR = 2003

mort_2005 = read_fwf(file=srvyin_2005,
                     col_types = "ciiiiiiiddii",
                     fwf_cols(publicid = c(1,14),
                              eligstat = c(15,15),
                              mortstat = c(16,16),
                              ucod_leading = c(17,19),
                              diabetes = c(20,20),
                              hyperten = c(21,21),
                              dodqtr = c(22,22),
                              dodyear = c(23,26),
                              wgt_new = c(27,34),
                              sa_wgt_new = c(35,42),
                              permth_int = c(43,45),
                              permth_exm = c(46,48)
                     ),
                     na = "."
)
mort_2005$SEQN <- substr(mort_2005$publicid,1,5)
# NOTE:   SEQN is the unique ID for NHANES.
mort_2005 <- dplyr::select(mort_2005, -publicid)
mort_2005 <- dplyr::select(mort_2005, -dodqtr)
mort_2005 <- dplyr::select(mort_2005, -dodyear)
mort_2005 <- dplyr::select(mort_2005, -wgt_new)
mort_2005 <- dplyr::select(mort_2005, -sa_wgt_new)
mort_2005$YEAR = 2005

mort_data = rbind(mort_1999, mort_2001, mort_2003, mort_2005)
mort_data = mort_data[which(mort_data$eligstat == 1), ]



#---- Covariate data---#

DEMO_1999 = foreign::read.xport("/Users/prosenjitkundu/Desktop/NCI_multi_cancer/Reference_data/NHANES/1999_2000/DEMO.XPT")
BODY_MEASURE_1999 = foreign::read.xport("/Users/prosenjitkundu/Desktop/NCI_multi_cancer/Reference_data/NHANES/1999_2000/BMX.XPT")
ALCO_1999 = foreign::read.xport("/Users/prosenjitkundu/Desktop/NCI_multi_cancer/Reference_data/NHANES/1999_2000/ALQ.XPT")
SMOKE_1999 = foreign::read.xport("/Users/prosenjitkundu/Desktop/NCI_multi_cancer/Reference_data/NHANES/1999_2000/SMQ.XPT")


NHANES_1999 = merge(merge(merge(DEMO_1999, BODY_MEASURE_1999, by = "SEQN", all = T), ALCO_1999, by = "SEQN", all = T), SMOKE_1999, by = "SEQN", all = T)
NHANES_1999$ALQ101 = NHANES_1999$ALQ100

DEMO_2001 = foreign::read.xport("/Users/prosenjitkundu/Desktop/NCI_multi_cancer/Reference_data/NHANES/2001_2002/DEMO_B.XPT")
BODY_MEASURE_2001 = foreign::read.xport("/Users/prosenjitkundu/Desktop/NCI_multi_cancer/Reference_data/NHANES/2001_2002/BMX_B.XPT")
ALCO_2001 = foreign::read.xport("/Users/prosenjitkundu/Desktop/NCI_multi_cancer/Reference_data/NHANES/2001_2002/ALQ_B.XPT")
SMOKE_2001 = foreign::read.xport("/Users/prosenjitkundu/Desktop/NCI_multi_cancer/Reference_data/NHANES/2001_2002/SMQ_B.XPT")


NHANES_2001 = merge(merge(merge(DEMO_2001, BODY_MEASURE_2001, by = "SEQN", all = T), ALCO_2001, by = "SEQN", all = T), SMOKE_2001, by = "SEQN", all = T)
NHANES_2001$ALQ101 = NHANES_2001$ALD100

DEMO_2003 = foreign::read.xport("/Users/prosenjitkundu/Desktop/NCI_multi_cancer/Reference_data/NHANES/2003_2004/DEMO_C.XPT")
BODY_MEASURE_2003 = foreign::read.xport("/Users/prosenjitkundu/Desktop/NCI_multi_cancer/Reference_data/NHANES/2003_2004/BMX_C.XPT")
ALCO_2003 = foreign::read.xport("/Users/prosenjitkundu/Desktop/NCI_multi_cancer/Reference_data/NHANES/2003_2004/ALQ_C.XPT")
SMOKE_2003 = foreign::read.xport("/Users/prosenjitkundu/Desktop/NCI_multi_cancer/Reference_data/NHANES/2003_2004/SMQ_C.XPT")


NHANES_2003 = merge(merge(merge(DEMO_2003, BODY_MEASURE_2003, by = "SEQN", all = T), ALCO_2003, by = "SEQN", all = T), SMOKE_2003, by = "SEQN", all = T)
NHANES_2003$WTMEC4YR = NA

DEMO_2005 = foreign::read.xport("/Users/prosenjitkundu/Desktop/NCI_multi_cancer/Reference_data/NHANES/2005_2006/DEMO_D.XPT")
BODY_MEASURE_2005 = foreign::read.xport("/Users/prosenjitkundu/Desktop/NCI_multi_cancer/Reference_data/NHANES/2005_2006/BMX_D.XPT")
ALCO_2005 = foreign::read.xport("/Users/prosenjitkundu/Desktop/NCI_multi_cancer/Reference_data/NHANES/2005_2006/ALQ_D.XPT")
SMOKE_2005 = foreign::read.xport("/Users/prosenjitkundu/Desktop/NCI_multi_cancer/Reference_data/NHANES/2005_2006/SMQ_D.XPT")


NHANES_2005 = merge(merge(merge(DEMO_2005, BODY_MEASURE_2005, by = "SEQN", all = T), ALCO_2005, by = "SEQN", all = T), SMOKE_2005, by = "SEQN", all = T)
NHANES_2005$WTMEC4YR = NA



common_colns = intersect(intersect(intersect(colnames(NHANES_1999), colnames(NHANES_2001)), colnames(NHANES_2003)), colnames(NHANES_2005))
reference_data = rbind(
  subset(NHANES_1999, select = common_colns), 
  subset(NHANES_2001, select = common_colns),
  subset(NHANES_2003, select = common_colns),
  subset(NHANES_2005, select = common_colns)
)
reference_data$age = reference_data$RIDAGEYR
reference_data$race = reference_data$RIDRETH1
reference_data$sex = reference_data$RIAGENDR
reference_data$education_level = reference_data$DMDEDUC2
reference_data$bmi = reference_data$BMXBMI



#SEX
reference_data$sex = if_else(reference_data$sex == 1, "Male", "Female")

#Race
reference_data = reference_data %>% mutate(race_ethnicity = case_when(race <= 2  ~ "hispanic",
                                                                      race == 3 ~ "white",
                                                                      race == 4 ~ "black",
                                                                      race == 6 ~ "asian"))

#Education
reference_data = reference_data %>% mutate(education = case_when(education_level == 1  ~ "less_than_9th_grade",
                                                                 education_level == 2 ~ "9th_11th_grade",
                                                                 education_level == 3 ~ "high_school_grad_GED_equivalent",
                                                                 education_level == 4 ~ "some_college_AA_degree",
                                                                 education_level == 5 ~ "college_grad_or_above"))


#Marital Status
reference_data = reference_data %>% mutate(marital_status = case_when(DMDMARTL == 1 ~ "married",
                                                                      DMDMARTL == 2 | DMDMARTL == 3 | DMDMARTL == 4 ~ "divorce",
                                                                      DMDMARTL == 5 ~ "never_married"))
reference_data = reference_data %>% mutate(smoking_status = case_when(SMQ020 == 1 & SMQ040 <= 2 ~ "Current",
                                                                      SMQ020 == 1 & SMQ040 == 3 ~ "Former",
                                                                      SMQ020 == 2 ~ "Never"))
reference_data$SMD057[which(reference_data$SMD057 == 777 | reference_data$SMD057 == 999)] = NA
reference_data$SMD070[which(reference_data$SMD070 == 777 | reference_data$SMD070 == 999)] = NA
reference_data$no_of_cigarettes =NA
reference_data$no_of_cigarettes[which(reference_data$smoking_status == "Current" & is.na(reference_data$SMD057) == T & is.na(reference_data$SMD070) == F)] = reference_data$SMD070[which(reference_data$smoking_status == "Current" & is.na(reference_data$SMD057) == T & is.na(reference_data$SMD070) == F)]
reference_data$no_of_cigarettes[which(reference_data$smoking_status == "Current" & is.na(reference_data$SMD057) == F & is.na(reference_data$SMD070) == T)] = reference_data$SMD057[which(reference_data$smoking_status == "Current" & is.na(reference_data$SMD057) == F & is.na(reference_data$SMD070) == T)]
reference_data$no_of_cigarettes[which(reference_data$smoking_status == "Former"  & is.na(reference_data$SMD057) == T & is.na(reference_data$SMD070) == F)] = reference_data$SMD070[which(reference_data$smoking_status == "Former" & is.na(reference_data$SMD057) == T & is.na(reference_data$SMD070) == F)]
reference_data$no_of_cigarettes[which(reference_data$smoking_status == "Former"  & is.na(reference_data$SMD057) == F & is.na(reference_data$SMD070) == T)] = reference_data$SMD057[which(reference_data$smoking_status == "Former" & is.na(reference_data$SMD057) == F & is.na(reference_data$SMD070) == T)]

#reference_data$no_of_cigarettes[which(reference_data$smoking_status == "Current" & reference_data$smoking_status == "Former" & is.na(reference_data$SMD057) == T & is.na(reference_data$SMD070) == T)] = NA
#reference_data$no_of_cigarettes = reference_data$no_of_cigarettes-1
reference_data$no_of_cigarettes[which(reference_data$smoking_status == "Never")] = 0
reference_data$ALQ130[which(reference_data$ALQ130 >= 77)] = NA
reference_data$ALQ120Q[which(reference_data$ALQ120Q >= 777)] = NA
reference_data = reference_data %>% mutate(average_drinking = case_when(ALQ120U == 1 ~ ALQ130 * (ALQ120Q * 52.1429 * ALQ120U )/365.25,
                                                                        ALQ120U == 2 ~ ALQ130 * (ALQ120Q * 12 * ALQ120U )/365.25,
                                                                        ALQ120U == 3 ~ ALQ130 * (ALQ120Q * 1 * ALQ120U )/365.25))

#reference_data = reference_data[-which(reference_data$average_drinking>=20), ]
#reference_data = reference_data %>% mutate(alcohol_status = case_when(sex == "Male" & average_drinking > 0 & average_drinking < 0.5 ~ "light",
#                                                                     sex == "Male" & average_drinking >= 0.5 & average_drinking < 2.5 ~ "moderate",
#                                                                    sex == "Male" & average_drinking >= 2.5 ~ "heavy",
#                                                                   sex == "Female" & average_drinking > 0 & average_drinking < 0.5 ~ "light",
#                                                                  sex == "Female" & average_drinking >= 0.5 & average_drinking < 1.5 ~ "moderate",
#                                                                 sex == "Female" & average_drinking >= 1.5 ~ "heavy"))
reference_data$ALQ101[which(reference_data$ALQ101 == 9)] = NA
reference_data$ALQ110[which(reference_data$ALQ110 == 9)] = NA

reference_data = reference_data %>% mutate(alcohol_status = case_when(ALQ110 == 2~ "Never",
                                                                      ALQ101 == 2 & ALQ120Q == 0 ~ "Former",
                                                                      ALQ101 == 1 & ALQ120Q == 0 ~ "Former"))
reference_data$alcohol_status[which(reference_data$ALQ120Q >= 1 )] = "Current"
#reference_data$alcohol_status[which(reference_data$ALQ110 == 1 & reference_data$ALQ120Q >= 1 )] = "Current"
reference_data$alcohol_status_new = NA
reference_data$avg_drinks_per_week = NA
reference_data$alcohol_status_new[which(reference_data$ALQ101 == 2 | reference_data$ALQ110 == 2 | reference_data$ALQ120Q == 0)] = "Nondrinker"
reference_data$avg_drinks_per_week[which(reference_data$ALQ120U == 1)] = reference_data$ALQ120Q[which(reference_data$ALQ120U == 1)] * reference_data$ALQ130[which(reference_data$ALQ120U == 1)]
reference_data$avg_drinks_per_week[which(reference_data$ALQ120U == 2)] = reference_data$ALQ120Q[which(reference_data$ALQ120U == 2)] * reference_data$ALQ130[which(reference_data$ALQ120U == 2)]*7/30
reference_data$avg_drinks_per_week[which(reference_data$ALQ120U == 3)] = reference_data$ALQ120Q[which(reference_data$ALQ120U == 3)] * reference_data$ALQ130[which(reference_data$ALQ120U == 3)]*7/365

reference_data$alcohol_status_new[which(reference_data$avg_drinks_per_week >= 8 & reference_data$sex == "Female")] = "Heavy"
reference_data$alcohol_status_new[which(reference_data$avg_drinks_per_week < 8 & reference_data$sex == "Female")] = "Moderate"
reference_data$alcohol_status_new[which(reference_data$avg_drinks_per_week >= 15 & reference_data$sex == "Male")] = "Heavy"
reference_data$alcohol_status_new[which(reference_data$avg_drinks_per_week < 15 & reference_data$sex == "Male")] = "Moderate"


reference_data$MEC8YR = NA
reference_data$MEC8YR[which(reference_data$SDDSRVYR == 1 | reference_data$SDDSRVYR == 2)] = 2/4 * reference_data$WTMEC4YR[which(reference_data$SDDSRVYR == 1 | reference_data$SDDSRVYR == 2)]
#/* for 1999-2002 */
reference_data$MEC8YR[which(reference_data$SDDSRVYR == 3 | reference_data$SDDSRVYR == 4)] = 1/4 * reference_data$WTMEC2YR[which(reference_data$SDDSRVYR == 3 | reference_data$SDDSRVYR == 4)]
#/* for 2003-2006 */

reference_data$SEQN = str_pad(reference_data$SEQN, 5, pad = "0")
nhanes_combined = merge(reference_data, mort_data, by = "SEQN")
nhanes_combined$bmi_obese = if_else(nhanes_combined$bmi >= 30, 1, 0)


#---18+
nhanes_combined = nhanes_combined[which(nhanes_combined$RIDAGEYR >= 40 & nhanes_combined$RIDAGEYR < 70), ]

#colnames(nhanes_combined)[92] = "MORTSTAT"
#nhanes_combined = nhanes_combined  %>% mutate(mortstat_rec = case_when(MORTSTAT == 2 ~ 0,
#                                                                                YEAR == 1999 & MORTDODY <= 2010 ~ 1,
#                                                                               YEAR == 1999 & MORTDODY > 2010 ~ 0,
#                                                                              YEAR == 2001 & MORTDODY <= 2011 ~ 1,
#                                                                             YEAR == 2001 & MORTDODY > 2011 ~ 0,
#                                                                            YEAR == 2003 & MORTDODY <= 2012 ~ 1,
#                                                                           YEAR == 2003 & MORTDODY > 2012 ~ 0,
#                                                                          YEAR == 2005 & MORTDODY <= 2013 ~ 1,
#                                                                         YEAR == 2005 & MORTDODY > 2013 ~ 0))

#---White--#
nhanes_combined_white = nhanes_combined[which(nhanes_combined$race_ethnicity == "white"), ]
final_data = nhanes_combined_white


MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_fullsample.csv")
BMI_quantiles = MR_vcov_file$bmi.quant
delta_HR = exp(MR_vcov_file$est)
MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[8]-delta_HR[6])/2, delta_HR[8] + (delta_HR[10]-delta_HR[8])/2, delta_HR[10] + (delta_HR[12]-delta_HR[10])/2, delta_HR[12] + (delta_HR[13]-delta_HR[12])/2, delta_HR[13] + (delta_HR[14]-delta_HR[13])/2))

nhanes_combined_white = nhanes_combined_white %>% mutate(bmi_cat = case_when(bmi < BMI_quantiles[3] ~ "Underweight",
                                                                             bmi >= BMI_quantiles[3] & bmi < BMI_quantiles[6] ~ "Normal",
                                                                             bmi >= BMI_quantiles[6] & bmi < BMI_quantiles[8] ~ "ObeseI",
                                                                             bmi >= BMI_quantiles[8] & bmi < BMI_quantiles[10] ~ "ObeseII",
                                                                             bmi >= BMI_quantiles[10] & bmi < BMI_quantiles[12] ~ "ObeseIII",
                                                                             bmi >= BMI_quantiles[12] & bmi < BMI_quantiles[13] ~ "ObeseIV",
                                                                             bmi >= BMI_quantiles[13] ~ "ObeseV"))

#nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(bmi_cat = case_when(BMI < 18.5 ~ "Underweight",
#                                                                          BMI >= 18.5 & BMI < 25 ~ "Normal",
#                                                                         BMI >= 25 & BMI < 30 ~ "ObeseI",
#                                                                        BMI >= 30 & BMI < 35 ~ "ObeseII",
#                                                                       BMI >= 35 ~"ObeseIII"))

#---Removing underweight people--#
nhanes_combined_white = nhanes_combined_white[-which(nhanes_combined_white$bmi_cat == "Underweight"), ]



myVars = c("age", "sex", "education", "smoking_status", "alcohol_status_new", "marital_status", "no_of_cigarettes", "mortstat")
catVars = c("sex", "education", "smoking_status", "alcohol_status_new", "marital_status", "mortstat")
#data_processed$sampweight = data_processed$sampweight/(sum(data_processed$sampweight))
#options(survey.adjust.domain.lonely=TRUE)
#options(survey.lonely.psu="adjust")
#data_svy = svydesign(id = ~0, data = data_processed, weights = ~sampweight)
tab_entire <- CreateTableOne(vars = myVars, data = nhanes_combined_white, factorVars = catVars)

tab_allcause <- CreateTableOne(vars = myVars, strata = "bmi_cat" , data = nhanes_combined_white, factorVars = catVars)


#----Heart disease
MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_cardiovascular.csv")
BMI_quantiles = MR_vcov_file$bmi.quant
delta_HR = exp(MR_vcov_file$est)
MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[8]-delta_HR[6])/2, delta_HR[8] + (delta_HR[10]-delta_HR[8])/2, delta_HR[10] + (delta_HR[12]-delta_HR[10])/2, delta_HR[12] + (delta_HR[13]-delta_HR[12])/2, delta_HR[13] + (delta_HR[14]-delta_HR[13])/2))

#heart_data = nhanes_combined_white
nhanes_combined_white = final_data
nhanes_combined_white = nhanes_combined_white %>% mutate(bmi_cat = case_when(bmi < BMI_quantiles[3] ~ "Underweight",
                                                                             bmi >= BMI_quantiles[3] & bmi < BMI_quantiles[6] ~ "Normal",
                                                                             bmi >= BMI_quantiles[6] & bmi < BMI_quantiles[8] ~ "ObeseI",
                                                                             bmi >= BMI_quantiles[8] & bmi < BMI_quantiles[10] ~ "ObeseII",
                                                                             bmi >= BMI_quantiles[10] & bmi < BMI_quantiles[12] ~ "ObeseIII",
                                                                             bmi >= BMI_quantiles[12] & bmi < BMI_quantiles[13] ~ "ObeseIV",
                                                                             bmi >= BMI_quantiles[13] ~ "ObeseV"))
nhanes_combined_white = nhanes_combined_white[-which(nhanes_combined_white$bmi_cat == "Underweight"), ]


if(length(which(is.na(nhanes_combined_white$ucod_leading) == T & nhanes_combined_white$mortstat == 1)) > 0)
  nhanes_combined_white = nhanes_combined_white[-which(is.na(nhanes_combined_white$ucod_leading) == T & nhanes_combined_white$mortstat == 1),]

nhanes_combined_white = nhanes_combined_white %>% mutate(mortstat_rec = case_when(mortstat == 1 & ucod_leading == 1  ~ 1,
                                                                                  mortstat == 1 & ucod_leading !=1  ~ 0,
                                                                                  mortstat == 0 ~ 0))

myVars = c("age", "sex", "education", "smoking_status", "alcohol_status_new", "marital_status", "no_of_cigarettes", "mortstat_rec")
catVars = c("sex", "education", "smoking_status", "alcohol_status_new", "marital_status", "mortstat_rec")
#data_processed$sampweight = data_processed$sampweight/(sum(data_processed$sampweight))
#options(survey.adjust.domain.lonely=TRUE)
#options(survey.lonely.psu="adjust")
#data_svy = svydesign(id = ~0, data = data_processed, weights = ~sampweight)
tab_heart <- CreateTableOne(vars = myVars, strata = "bmi_cat" , data = nhanes_combined_white, factorVars = catVars)


#----cancer
MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_cancer.csv")
BMI_quantiles = MR_vcov_file$bmi.quant
delta_HR = exp(MR_vcov_file$est)
MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[8]-delta_HR[6])/2, delta_HR[8] + (delta_HR[10]-delta_HR[8])/2, delta_HR[10] + (delta_HR[12]-delta_HR[10])/2, delta_HR[12] + (delta_HR[13]-delta_HR[12])/2, delta_HR[13] + (delta_HR[14]-delta_HR[13])/2))

#heart_data = nhanes_combined_white
nhanes_combined_white = final_data
nhanes_combined_white = nhanes_combined_white %>% mutate(bmi_cat = case_when(bmi < BMI_quantiles[3] ~ "Underweight",
                                                                             bmi >= BMI_quantiles[3] & bmi < BMI_quantiles[6] ~ "Normal",
                                                                             bmi >= BMI_quantiles[6] & bmi < BMI_quantiles[8] ~ "ObeseI",
                                                                             bmi >= BMI_quantiles[8] & bmi < BMI_quantiles[10] ~ "ObeseII",
                                                                             bmi >= BMI_quantiles[10] & bmi < BMI_quantiles[12] ~ "ObeseIII",
                                                                             bmi >= BMI_quantiles[12] & bmi < BMI_quantiles[13] ~ "ObeseIV",
                                                                             bmi >= BMI_quantiles[13] ~ "ObeseV"))
nhanes_combined_white = nhanes_combined_white[-which(nhanes_combined_white$bmi_cat == "Underweight"), ]


if(length(which(is.na(nhanes_combined_white$ucod_leading) == T & nhanes_combined_white$mortstat == 1)) > 0)
  nhanes_combined_white = nhanes_combined_white[-which(is.na(nhanes_combined_white$ucod_leading) == T & nhanes_combined_white$mortstat == 1),]

nhanes_combined_white = nhanes_combined_white %>% mutate(mortstat_rec = case_when(mortstat == 1 & ucod_leading == 2  ~ 1,
                                                                                  mortstat == 1 & ucod_leading !=2  ~ 0,
                                                                                  mortstat == 0 ~ 0))

myVars = c("age", "sex", "education", "smoking_status", "alcohol_status_new", "marital_status", "no_of_cigarettes", "mortstat_rec")
catVars = c("sex", "education", "smoking_status", "alcohol_status_new", "marital_status", "mortstat_rec")
#data_processed$sampweight = data_processed$sampweight/(sum(data_processed$sampweight))
#options(survey.adjust.domain.lonely=TRUE)
#options(survey.lonely.psu="adjust")
#data_svy = svydesign(id = ~0, data = data_processed, weights = ~sampweight)
tab_cancer <- CreateTableOne(vars = myVars, strata = "bmi_cat" , data = nhanes_combined_white, factorVars = catVars)



