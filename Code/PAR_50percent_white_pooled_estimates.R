library(survey)
library(stringr)
library(weights)
library(dplyr)
library(readr)
#-------Derivative of expit----#
dexpit = function(x)
{
  e1 = as.vector(1/(1 + exp(-x)))
  e2 = as.vector(1/(1 + exp(x)))
  l = e1*e2
  nan_indices = which(l %in% NaN == TRUE)
  l[nan_indices] = 0
  return(l)
}

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
reference_data$no_of_cigarettes = reference_data$no_of_cigarettes-1
reference_data$no_of_cigarettes[which(reference_data$smoking_status == "Never")] = 0
#nhanes_combined_white$MEC8YR = nhanes_combined_white$MEC8YR/sum(nhanes_combined_white$MEC8YR)

#nhanes_combined_white$no_of_cigarettes = as.numeric(nhanes_combined_white$no_of_cigarettes)

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
#nhanes_combined_white = nhanes_combined_white %>% filter(bmi < 100)
#AA
#nhanes_combined_aa = nhanes_combined[which(nhanes_combined$race == "black"), ]
#---All-cause


#nhanes_combined_white = nhanes_combined_white %>% filter(smoking_status == "Never")
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")



#--- only obese & overweight
nhanes_combined_white = nhanes_combined[which(nhanes_combined$race_ethnicity == "white"), ]

#nhanes_combined_white = nhanes_combined_white %>% filter(bmi < 100)
#nhanes_combined_aa = nhanes_combined[which(nhanes_combined$race == "black"), ]
#---All-cause
MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_fullsample.csv")
BMI_quantiles = MR_vcov_file$bmi.quant

#Normal : BMI_quantiles[3]- BMI_quantiles[6], Overweight: BMI_quantiles[6]-BMI_quantiles[9], Obese: BMI_quantiles[9]:BMI_quantiles[13], Severe obese: >= BMI_quantiles[13]
delta_HR = exp(MR_vcov_file$est)
#MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[9]-delta_HR[6])/2, delta_HR[9] + (delta_HR[13]-delta_HR[9])/2, delta_HR[13] + (delta_HR[14]-delta_HR[13])/2))
MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[10]-delta_HR[6])/2, delta_HR[10] + (delta_HR[12]-delta_HR[10])/2, delta_HR[12] + (delta_HR[13]-delta_HR[12])/2, delta_HR[13] + (delta_HR[14]-delta_HR[13])/2))

#MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[10]-delta_HR[6])/2, delta_HR[10] + (delta_HR[14]-delta_HR[10])/2))


#---Pooled cohort
mid_point_normal = BMI_quantiles[3] + (BMI_quantiles[6] - BMI_quantiles[3])/2

nhanes_combined_white = nhanes_combined_white %>% mutate(bmi_cat = case_when(bmi < BMI_quantiles[3] ~ "Underweight",
                                                                             bmi >= BMI_quantiles[3] & bmi < BMI_quantiles[6] ~ "Normal",
                                                                             bmi >= BMI_quantiles[6] & bmi < BMI_quantiles[10] ~ "Overweight",
                                                                             bmi >= BMI_quantiles[10] & bmi < BMI_quantiles[12] ~ "SObese_grade1",
                                                                             bmi >= BMI_quantiles[12] & bmi < BMI_quantiles[13] ~ "SObese_grade2",
                                                                             bmi >= BMI_quantiles[13] ~ "SObese_grade3"))
#---Removing underweight people--#
nhanes_combined_white = nhanes_combined_white[-which(nhanes_combined_white$bmi_cat == "Underweight"), ]

nhanes_combined_white$bmi_shift = NA
nhanes_combined_white$bmi_shift[which(nhanes_combined_white$bmi_cat == "Normal")] = nhanes_combined_white$bmi[which(nhanes_combined_white$bmi_cat == "Normal")]
nhanes_combined_white$bmi_shift[which(nhanes_combined_white$bmi_cat != "Normal")] = mid_point_normal + 0.5*(nhanes_combined_white$bmi[which(nhanes_combined_white$bmi_cat != "Normal")] - mid_point_normal)
nhanes_combined_white = nhanes_combined_white %>% mutate(bmi_cat_shift = case_when(bmi_shift < BMI_quantiles[3] ~ "Underweight",
                                                                                   bmi_shift >= BMI_quantiles[3] & bmi_shift < BMI_quantiles[6] ~ "Normal",
                                                                                   bmi_shift >= BMI_quantiles[6] & bmi_shift < BMI_quantiles[10] ~ "Overweight",
                                                                                   bmi_shift >= BMI_quantiles[10] & bmi_shift < BMI_quantiles[12] ~ "SObese_grade1",
                                                                                   bmi_shift >= BMI_quantiles[12] & bmi_shift < BMI_quantiles[13] ~ "SObese_grade2",
                                                                                   bmi_shift >= BMI_quantiles[13] ~ "SObese_grade3"))

#nhanes_combined_white = nhanes_combined_white %>% mutate(bmi_cat = case_when(bmi < BMI_quantiles[3] ~ "Underweight",
#                                                                            bmi >= BMI_quantiles[3] & bmi < BMI_quantiles[6] ~ "Normal",
#                                                                           bmi >= BMI_quantiles[6] & bmi < BMI_quantiles[8] ~ "ObeseI",
#                                                                          bmi >= BMI_quantiles[8] & bmi < BMI_quantiles[10] ~ "ObeseII",
#                                                                         bmi >= BMI_quantiles[10] & bmi < BMI_quantiles[12] ~ "ObeseIII",
#                                                                        bmi >= BMI_quantiles[12] & bmi < BMI_quantiles[13] ~ "ObeseIV",
#                                                                       bmi >= BMI_quantiles[13] ~ "ObeseV"))


#nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(bmi_cat = case_when(BMI < 18.5 ~ "Underweight",
#                                                                          BMI >= 18.5 & BMI < 25 ~ "Normal",
#                                                                         BMI >= 25 & BMI < 30 ~ "ObeseI",
#                                                                        BMI >= 30 & BMI < 35 ~ "ObeseII",
#                                                                       BMI >= 35 ~"ObeseIII"))


nhanes_combined_white$bmi_cat  = as.factor(nhanes_combined_white$bmi_cat)

nhanes_combined_white$bmi_cat = relevel(nhanes_combined_white$bmi_cat, ref = "Normal")

#nhanes_combined_white$alcohol_status_new[which(is.na(nhanes_combined_white$alcohol_status_new) == T)] = "NA"
#nhanes_combined_white$smoking_status[which(is.na(nhanes_combined_white$smoking_status) == T)] = "NA"
#nhanes_combined_white$marital_status[which(is.na(nhanes_combined_white$marital_status) == T)] = "NA"
#nhanes_combined_white$education[which(is.na(nhanes_combined_white$education) == T)] = "NA"

#nhanes_combined_white$alcohol_status_new[which(is.na(nhanes_combined_white$alcohol_status_new) == T)] = "NA"
#nhanes_combined_white$smoking_status[which(is.na(nhanes_combined_white$smoking_status) == T)] = "NA"
#nhanes_combined_white$marital_status[which(is.na(nhanes_combined_white$marital_status) == T)] = "NA"
#nhanes_combined_white$education[which(is.na(nhanes_combined_white$education) == T)] = "NA"


nhanes_svydesign <- svydesign(data=nhanes_combined_white, id=~SDMVPSU, strata=~SDMVSTRA, weights=~MEC8YR, nest=TRUE)
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
fit_unadjusted = svyglm(mortstat ~ bmi_cat, design=nhanes_svydesign, family = quasibinomial())
prop_exposure = wpct(nhanes_combined_white$bmi_cat, weight = nhanes_combined_white$MEC8YR)
nhanes_combined_white = nhanes_combined_white %>% filter(bmi_cat == "Normal" | bmi_cat == "Overweight" | bmi_cat == "SObese_grade1" | bmi_cat == "SObese_grade2" | bmi_cat =="SObese_grade3")
#nhanes_combined_white$bmi_cat_shift[which(nhanes_combined_white$bmi_cat_shift == "Underweight")] = "Normal"

#---Relative Risk Matrix
RR = matrix(NA,5,5)
colnames(RR) = c("Normal", "Overweight", "SObese_grade1", "SObese_grade2", "SObese_grade3")
rownames(RR) = c("Normal", "Overweight", "SObese_grade1", "SObese_grade2", "SObese_grade3")
diag(RR) = 1
RR = RR[-1,]
RR[1,1] = exp(fit_unadjusted$coefficients[2])
RR[2,1] = exp(fit_unadjusted$coefficients[3])
RR[3,1] = exp(fit_unadjusted$coefficients[4])
RR[4,1] = exp(fit_unadjusted$coefficients[5])
RR[2,2] = exp(fit_unadjusted$coefficients[3] - fit_unadjusted$coefficients[2])
RR[3,2] = exp(fit_unadjusted$coefficients[4] - fit_unadjusted$coefficients[2])
RR[4,2] = exp(fit_unadjusted$coefficients[5] - fit_unadjusted$coefficients[2])
RR[3,3] = exp(fit_unadjusted$coefficients[4] - fit_unadjusted$coefficients[3])
RR[4,3] = exp(fit_unadjusted$coefficients[5] - fit_unadjusted$coefficients[3])
RR[4,4] = exp(fit_unadjusted$coefficients[5] - fit_unadjusted$coefficients[4])


pooled_estimates = c(0.1001688, 0.3510417, 0.6614678, 1.0031354)
MR_RR = RR
MR_RR[1,1] = exp(pooled_estimates[1])
MR_RR[2,1] = exp(pooled_estimates[2])
MR_RR[3,1] = exp(pooled_estimates[3])
MR_RR[4,1] = exp(pooled_estimates[4])
MR_RR[2,2] = exp(pooled_estimates[2] - pooled_estimates[1])
MR_RR[3,2] = exp(pooled_estimates[3] - pooled_estimates[1])
MR_RR[4,2] = exp(pooled_estimates[4] - pooled_estimates[1])
MR_RR[3,3] = exp(pooled_estimates[3] - pooled_estimates[2])
MR_RR[4,3] = exp(pooled_estimates[4] - pooled_estimates[2])
MR_RR[4,4] = exp(pooled_estimates[4] - pooled_estimates[3])

RR = rbind(c(1,NA,NA,NA,NA), RR)
rownames(RR)[1] = "Normal"
MR_RR = rbind(c(1,NA,NA,NA,NA), MR_RR)
rownames(MR_RR)[1] = "Normal"


colnames(nhanes_combined_white)[97] = "sampweight"


weight.mean = function(x,w)
{
  return(sum(x*w))
}


baseline_prob = rep(0, length(unique(nhanes_combined_white$bmi_cat_shift)))
bmi_coeff = c(0, fit_unadjusted$coefficients[2:5])
names(bmi_coeff) = c("bmi_catNormal", names(fit_unadjusted$coefficients[2:5]))
baseline_prob = locfit::expit(fit_unadjusted$coefficients[1] + bmi_coeff)

PAR_percent_shift = function(transition_matrix, baseline_probability, unadjusted_RR, adjusted_RR, prevalence)
{
  ratio_matrix = unadjusted_RR/adjusted_RR
  ratio_matrix[upper.tri(ratio_matrix)]  = 0
  baseline_prob_matrix = t(matrix(rep(baseline_probability, length(baseline_probability)), nrow = length(baseline_probability), ncol = length(baseline_probability)))
  print(ratio_matrix)
  print(prevalence)
  print(sum(transition_matrix*ratio_matrix*baseline_prob_matrix))
  return(1 - sum(transition_matrix*ratio_matrix*baseline_prob_matrix)/prevalence)
}

nhanes_combined_white_temp = nhanes_combined_white
nhanes_combined_white_temp$bmi_cat = as.factor(as.character(nhanes_combined_white_temp$bmi_cat))
hh = model.matrix(~ bmi_cat, data = nhanes_combined_white_temp)
#hh_obese = model.matrix(~ bmi_cat + sex + age + education + smoking_status + no_of_cigarettes + alcohol_status + marital_status, data = data_processed_complete)
hh = cbind(hh, nhanes_combined_white$sampweight)
hh = as.data.frame(hh)

colnames(hh)[6] = "sampweight"
hh = hh %>% mutate(bmi_cat = as.character(nhanes_combined_white$bmi_cat), bmi_cat_shift = as.character(nhanes_combined_white$bmi_cat_shift))


p_hat = weighted.mean(nhanes_combined_white$mortstat, w=nhanes_combined_white$sampweight)

PAR_percent_shift(prop.table(table(hh$bmi_cat, hh$bmi_cat_shift)), baseline_prob, RR, MR_RR, p_hat)

A1 = as.matrix(MR_vcov_file[, -c(1:3)][c(6,10,12,13,14), c(6,10,12,13,14)])
B1 = diag((delta_HR[c(6,10,12,13,14)])) %*% A1 %*% diag((delta_HR[c(6,10,12,13,14)]))
C1 = matrix(0, 4,5)
C1[1, 1:2] = 1/sum(delta_HR[c(6,10)])
C1[2, 2:3] = 1/sum(delta_HR[c(10,12)])
C1[3, 3:4] = 1/sum(delta_HR[c(12,13)])
C1[4, 4:5] = 1/sum(delta_HR[c(13,14)])
Omega = C1 %*% B1 %*% t(C1)

Omega = matrix(0, 4, 4)
diag_omega = readRDS("code/var_beta_all_cause_meta_pooled_6groups.rds")
diag(Omega) = diag_omega
p_hat_using_model = sum(exp(fit_unadjusted$coefficients[1] + bmi_coeff)*prop_exposure)
PAR_percent_shift_se = function(unadjusted_RR, MR_RR, p_hat, transition_matrix, Omega)
{
  k = 1/prop_exposure[1]
  data_processed = nhanes_combined_white %>% dplyr::select("mortstat", "bmi_cat", "bmi", "sampweight")
  #colnames(data_processed)[4] = "sampweight"
  Q = -matrix(rep(-k, 16), 4, 4) + diag(-1/prop_exposure[-1])
  Sigma_11 = vcov(fit_unadjusted) * nrow(data_processed)
  total_weights = sum(data_processed$sampweight)
  data_processed$bmi_cat = as.factor(data_processed$bmi_cat)
  
  data_processed_mm = as.data.frame(model.matrix(~ bmi_cat + mortstat + sampweight, data_processed))
  w1 = (data_processed_mm$mortstat - locfit::expit(as.matrix(data_processed_mm)[, c(1:5)] %*% fit_unadjusted$coefficients)) * data_processed_mm$sampweight
  psi = t(t(data_processed_mm[, c(2:5)])/prop_exposure[-1]) - ((1-data_processed_mm[,2]-data_processed_mm[,3]-data_processed_mm[,4]-data_processed_mm[,5])/prop_exposure[1])
  
  w2 = -dexpit(as.matrix(data_processed_mm)[, c(1:5)] %*% fit_unadjusted$coefficients) * data_processed_mm$sampweight
  P = t(as.matrix(data_processed_mm[, c(1:5)])) %*% (w2 * as.matrix(data_processed_mm[, c(1:5)]))
  P = P/total_weights
  Sigma_12 = solve(P) %*% ((t(as.matrix(data_processed_mm[, c(1:5)])) %*% (psi*as.numeric(w1)))/total_weights) %*% solve(Q)
  Sigma_22 = solve(Q) %*% ((t(psi) %*% (psi*data_processed_mm$sampweight))/total_weights) %*% solve(Q)
  
  Sigma_13 = -solve(P) %*% ((p_hat*(1-p_hat)*t(as.matrix(data_processed_mm)[, c(1:5)]) %*% ((data_processed_mm$mortstat/p_hat - ((1-data_processed_mm$mortstat)/(1-p_hat)))*w1))/total_weights)
  
  Sigma_23 = p_hat*(1-p_hat) * (-solve(Q)) %*% (t(psi) %*% ((data_processed_mm$mortstat/p_hat - ((1-data_processed_mm$mortstat)/(1-p_hat)))*data_processed_mm$sampweight) *(1/total_weights))
  
  Sigma_33 = ((p_hat*(1-p_hat))^2) * sum((data_processed_mm$mortstat/p_hat - ((1-data_processed_mm$mortstat)/(1-p_hat)))^2*data_processed_mm$sampweight)
  Sigma_33 = Sigma_33/total_weights
  
  Sigma = as.matrix(rbind(cbind(Sigma_11, Sigma_12, Sigma_13), cbind(t(Sigma_12), Sigma_22, Sigma_23), cbind(t(Sigma_13), t(Sigma_23), Sigma_33)))
  Sigma = Sigma/nrow(data_processed_mm)
  Sigma = Sigma[c(1:9), c(1:9)]
  #A = diag(5)
  #A[1,2:5] = exp(fit_unadjusted$coefficients[-1])/exp(MR_estimates_UK)
  #for(i in 2:4)
  #{
  # for(j in (i+1):5)
  # {
  #   A[i,j] = exp(MR_estimates_UK[i-1]-MR_estimates_UK[j-1])
  #}
  #}
  A = diag(5)
  A[1,2:5] = exp(fit_unadjusted$coefficients[-1])/exp(MR_estimates_UK)
  for(i in 2:4)
  {
    for(j in (i+1):5)
    {
      A[i,j] = exp(MR_estimates_UK[i-1]-MR_estimates_UK[j-1])*exp(fit_unadjusted$coefficients[j])
    }
  }
  diag(A)[2:5] = exp(fit_unadjusted$coefficients[2:5])
  
  diaonal_sum = sum(diag(A %*% transition_matrix))
  f = 1 - (exp(fit_unadjusted$coefficients[1])* diaonal_sum/p_hat)
  numerator_f = diaonal_sum
  denominator_f = p_hat
  p_derivative_vector = (exp(2*fit_unadjusted$coefficients[1])*numerator_f*(exp(fit_unadjusted$coefficients[-1])-exp(0)))/(p_hat^2)
  
  #print(f)
  #---construction of g-vector
  A_g = diag(5)
  A_g[1,1] = 0
  A_g[,1] = 0
  A_g[1, 2:5] = exp(-MR_estimates_UK)
  for(i in 2:4)
  {
    for(j in (i+1):5)
    {
      A_g[i,j] = exp(MR_estimates_UK[i-1]-MR_estimates_UK[j-1])
    }
  }
  
  g_vector = (-exp(fit_unadjusted$coefficients[1]+bmi_coeff)[-1]/p_hat^2)*(diag(transition_matrix[-1,] %*% A_g[,-1])*p_hat - prop_exposure_projected[-1]*numerator_f*exp(fit_unadjusted$coefficients[1]))
  #g0 = (-1/p_hat^2) * (exp(fit_unadjusted$coefficients[1])*numerator_f*(denominator_f - exp(fit_unadjusted$coefficients[1])*prop_exposure_projected[1]))
  g_vector = c(0, g_vector)
  
  beta_derivative_vector = rep(NA,4)
  beta_derivative_vector[1] = sum(transition_matrix[3:5,2]/exp(MR_estimates_UK[2:4] - MR_estimates_UK[1])*exp(fit_unadjusted$coefficients[3:5])) - (exp(fit_unadjusted$coefficients[2] - MR_estimates_UK[1])*transition_matrix[2,1])
  beta_derivative_vector[2] = sum(transition_matrix[4:5,3]/exp(MR_estimates_UK[3:4] - MR_estimates_UK[2])*exp(fit_unadjusted$coefficients[4:5])) - (exp(fit_unadjusted$coefficients[3] - MR_estimates_UK[2])*transition_matrix[3,1]) - (exp(MR_estimates_UK[1] - MR_estimates_UK[2])*exp(fit_unadjusted$coefficients[3])*transition_matrix[3,2])
  beta_derivative_vector[3] = (transition_matrix[5,4]/exp(MR_estimates_UK[4] - MR_estimates_UK[3]))*exp(fit_unadjusted$coefficients[5]) - (exp(fit_unadjusted$coefficients[4] - MR_estimates_UK[3])*transition_matrix[4,1]) - (exp(MR_estimates_UK[1] - MR_estimates_UK[3])*exp(fit_unadjusted$coefficients[4])*transition_matrix[4,2]) - (exp(MR_estimates_UK[2] - MR_estimates_UK[3])*exp(fit_unadjusted$coefficients[4])*transition_matrix[4,3])
  beta_derivative_vector[4] = -exp(fit_unadjusted$coefficients[5])*(transition_matrix[5,2]/(exp(MR_estimates_UK[4] - MR_estimates_UK[1])) + transition_matrix[5,3]/(exp(MR_estimates_UK[4] - MR_estimates_UK[2])) + transition_matrix[5,4]/(exp(MR_estimates_UK[4] - MR_estimates_UK[3])) + (exp(fit_unadjusted$coefficients[5] - MR_estimates_UK[4])*transition_matrix[5,1]))
  #print(beta_derivative_vector)
  beta_derivative_vector  = -(exp(fit_unadjusted$coefficients[1])/p_hat)*beta_derivative_vector  
  
  #print(t(c(g_vector, p_derivative_vector))%*% Sigma %*% c(g_vector, p_derivative_vector))
  #print(t(beta_derivative_vector) %*% Omega %*% beta_derivative_vector)
  return(sqrt(t(c(g_vector, p_derivative_vector))%*% Sigma %*% c(g_vector, p_derivative_vector) + t(beta_derivative_vector) %*% Omega %*% beta_derivative_vector))
}
PAR_percent_shift(prop.table(table(hh$bmi_cat, hh$bmi_cat_shift)), baseline_prob, RR, MR_RR, p_hat)
PAR_percent_shift(prop.table(table(hh$bmi_cat, hh$bmi_cat_shift)), baseline_prob, RR, MR_RR, p_hat) - 1.96*PAR_percent_shift_se(fit_unadjusted$coefficients, MR_estimates_UK, p_hat, prop.table(table(hh$bmi_cat, hh$bmi_cat_shift)), Omega = Omega)
PAR_percent_shift(prop.table(table(hh$bmi_cat, hh$bmi_cat_shift)), baseline_prob, RR, MR_RR, p_hat) + 1.96*PAR_percent_shift_se(fit_unadjusted$coefficients, MR_estimates_UK, p_hat, prop.table(table(hh$bmi_cat, hh$bmi_cat_shift)), Omega = Omega)


#Heart disease

nhanes_combined_white = nhanes_combined[which(nhanes_combined$race_ethnicity == "white"), ]

MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_cardiovascular.csv")
BMI_quantiles = MR_vcov_file$bmi.quant

#Normal : BMI_quantiles[3]- BMI_quantiles[6], Overweight: BMI_quantiles[6]-BMI_quantiles[9], Obese: BMI_quantiles[9]:BMI_quantiles[13], Severe obese: >= BMI_quantiles[13]
delta_HR = exp(MR_vcov_file$est)
#MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[9]-delta_HR[6])/2, delta_HR[9] + (delta_HR[13]-delta_HR[9])/2, delta_HR[13] + (delta_HR[14]-delta_HR[13])/2))
MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[10]-delta_HR[6])/2, delta_HR[10] + (delta_HR[12]-delta_HR[10])/2, delta_HR[12] + (delta_HR[13]-delta_HR[12])/2, delta_HR[13] + (delta_HR[14]-delta_HR[13])/2))

#MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[10]-delta_HR[6])/2, delta_HR[10] + (delta_HR[14]-delta_HR[10])/2))


#---Pooled cohort
mid_point_normal = BMI_quantiles[3] + (BMI_quantiles[6] - BMI_quantiles[3])/2

nhanes_combined_white = nhanes_combined_white %>% mutate(bmi_cat = case_when(bmi < BMI_quantiles[3] ~ "Underweight",
                                                                             bmi >= BMI_quantiles[3] & bmi < BMI_quantiles[6] ~ "Normal",
                                                                             bmi >= BMI_quantiles[6] & bmi < BMI_quantiles[10] ~ "Overweight",
                                                                             bmi >= BMI_quantiles[10] & bmi < BMI_quantiles[12] ~ "SObese_grade1",
                                                                             bmi >= BMI_quantiles[12] & bmi < BMI_quantiles[13] ~ "SObese_grade2",
                                                                             bmi >= BMI_quantiles[13] ~ "SObese_grade3"))
if(length(which(is.na(nhanes_combined_white$ucod_leading) == T & nhanes_combined_white$mortstat == 1)) > 0)
  nhanes_combined_white = nhanes_combined_white[-which(is.na(nhanes_combined_white$ucod_leading) == T & nhanes_combined_white$mortstat == 1),]
nhanes_combined_white = nhanes_combined_white %>% mutate(mortstat_rec = case_when(mortstat == 1 & ucod_leading == 1  ~ 1,
                                                                                  mortstat == 1 & ucod_leading !=1  ~ 0,
                                                                                  mortstat == 0 ~ 0))

#---Removing underweight people--#
nhanes_combined_white = nhanes_combined_white[-which(nhanes_combined_white$bmi_cat == "Underweight"), ]

nhanes_combined_white$bmi_shift = NA
nhanes_combined_white$bmi_shift[which(nhanes_combined_white$bmi_cat == "Normal")] = nhanes_combined_white$bmi[which(nhanes_combined_white$bmi_cat == "Normal")]
nhanes_combined_white$bmi_shift[which(nhanes_combined_white$bmi_cat != "Normal")] = mid_point_normal + 0.5*(nhanes_combined_white$bmi[which(nhanes_combined_white$bmi_cat != "Normal")] - mid_point_normal)
nhanes_combined_white = nhanes_combined_white %>% mutate(bmi_cat_shift = case_when(bmi_shift < BMI_quantiles[3] ~ "Underweight",
                                                                                   bmi_shift >= BMI_quantiles[3] & bmi_shift < BMI_quantiles[6] ~ "Normal",
                                                                                   bmi_shift >= BMI_quantiles[6] & bmi_shift < BMI_quantiles[10] ~ "Overweight",
                                                                                   bmi_shift >= BMI_quantiles[10] & bmi_shift < BMI_quantiles[12] ~ "SObese_grade1",
                                                                                   bmi_shift >= BMI_quantiles[12] & bmi_shift < BMI_quantiles[13] ~ "SObese_grade2",
                                                                                   bmi_shift >= BMI_quantiles[13] ~ "SObese_grade3"))

#nhanes_combined_white = nhanes_combined_white %>% mutate(bmi_cat = case_when(bmi < BMI_quantiles[3] ~ "Underweight",
#                                                                            bmi >= BMI_quantiles[3] & bmi < BMI_quantiles[6] ~ "Normal",
#                                                                           bmi >= BMI_quantiles[6] & bmi < BMI_quantiles[8] ~ "ObeseI",
#                                                                          bmi >= BMI_quantiles[8] & bmi < BMI_quantiles[10] ~ "ObeseII",
#                                                                         bmi >= BMI_quantiles[10] & bmi < BMI_quantiles[12] ~ "ObeseIII",
#                                                                        bmi >= BMI_quantiles[12] & bmi < BMI_quantiles[13] ~ "ObeseIV",
#                                                                       bmi >= BMI_quantiles[13] ~ "ObeseV"))


#nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(bmi_cat = case_when(BMI < 18.5 ~ "Underweight",
#                                                                          BMI >= 18.5 & BMI < 25 ~ "Normal",
#                                                                         BMI >= 25 & BMI < 30 ~ "ObeseI",
#                                                                        BMI >= 30 & BMI < 35 ~ "ObeseII",
#                                                                       BMI >= 35 ~"ObeseIII"))


nhanes_combined_white$bmi_cat  = as.factor(nhanes_combined_white$bmi_cat)

nhanes_combined_white$bmi_cat = relevel(nhanes_combined_white$bmi_cat, ref = "Normal")

#nhanes_combined_white$alcohol_status_new[which(is.na(nhanes_combined_white$alcohol_status_new) == T)] = "NA"
#nhanes_combined_white$smoking_status[which(is.na(nhanes_combined_white$smoking_status) == T)] = "NA"
#nhanes_combined_white$marital_status[which(is.na(nhanes_combined_white$marital_status) == T)] = "NA"
#nhanes_combined_white$education[which(is.na(nhanes_combined_white$education) == T)] = "NA"

#nhanes_combined_white$alcohol_status_new[which(is.na(nhanes_combined_white$alcohol_status_new) == T)] = "NA"
#nhanes_combined_white$smoking_status[which(is.na(nhanes_combined_white$smoking_status) == T)] = "NA"
#nhanes_combined_white$marital_status[which(is.na(nhanes_combined_white$marital_status) == T)] = "NA"
#nhanes_combined_white$education[which(is.na(nhanes_combined_white$education) == T)] = "NA"


nhanes_svydesign <- svydesign(data=nhanes_combined_white, id=~SDMVPSU, strata=~SDMVSTRA, weights=~MEC8YR, nest=TRUE)
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
fit_unadjusted = svyglm(mortstat_rec ~ bmi_cat, design=nhanes_svydesign, family = quasibinomial())
prop_exposure = wpct(nhanes_combined_white$bmi_cat, weight = nhanes_combined_white$MEC8YR)
nhanes_combined_white = nhanes_combined_white %>% filter(bmi_cat == "Normal" | bmi_cat == "Overweight" | bmi_cat == "SObese_grade1" | bmi_cat == "SObese_grade2" | bmi_cat =="SObese_grade3")
#nhanes_combined_white$bmi_cat_shift[which(nhanes_combined_white$bmi_cat_shift == "Underweight")] = "Normal"

#---Relative Risk Matrix
RR = matrix(NA,5,5)
colnames(RR) = c("Normal", "Overweight", "SObese_grade1", "SObese_grade2", "SObese_grade3")
rownames(RR) = c("Normal", "Overweight", "SObese_grade1", "SObese_grade2", "SObese_grade3")
diag(RR) = 1
RR = RR[-1,]
RR[1,1] = exp(fit_unadjusted$coefficients[2])
RR[2,1] = exp(fit_unadjusted$coefficients[3])
RR[3,1] = exp(fit_unadjusted$coefficients[4])
RR[4,1] = exp(fit_unadjusted$coefficients[5])
RR[2,2] = exp(fit_unadjusted$coefficients[3] - fit_unadjusted$coefficients[2])
RR[3,2] = exp(fit_unadjusted$coefficients[4] - fit_unadjusted$coefficients[2])
RR[4,2] = exp(fit_unadjusted$coefficients[5] - fit_unadjusted$coefficients[2])
RR[3,3] = exp(fit_unadjusted$coefficients[4] - fit_unadjusted$coefficients[3])
RR[4,3] = exp(fit_unadjusted$coefficients[5] - fit_unadjusted$coefficients[3])
RR[4,4] = exp(fit_unadjusted$coefficients[5] - fit_unadjusted$coefficients[4])


pooled_estimates = c(0.1873277, 0.5385081, 0.9910994, 1.3868846)
MR_RR = RR
MR_RR[1,1] = exp(pooled_estimates[1])
MR_RR[2,1] = exp(pooled_estimates[2])
MR_RR[3,1] = exp(pooled_estimates[3])
MR_RR[4,1] = exp(pooled_estimates[4])
MR_RR[2,2] = exp(pooled_estimates[2] - pooled_estimates[1])
MR_RR[3,2] = exp(pooled_estimates[3] - pooled_estimates[1])
MR_RR[4,2] = exp(pooled_estimates[4] - pooled_estimates[1])
MR_RR[3,3] = exp(pooled_estimates[3] - pooled_estimates[2])
MR_RR[4,3] = exp(pooled_estimates[4] - pooled_estimates[2])
MR_RR[4,4] = exp(pooled_estimates[4] - pooled_estimates[3])

RR = rbind(c(1,NA,NA,NA,NA), RR)
rownames(RR)[1] = "Normal"
MR_RR = rbind(c(1,NA,NA,NA,NA), MR_RR)
rownames(MR_RR)[1] = "Normal"


colnames(nhanes_combined_white)[97] = "sampweight"


weight.mean = function(x,w)
{
  return(sum(x*w))
}


baseline_prob = rep(0, length(unique(nhanes_combined_white$bmi_cat_shift)))
bmi_coeff = c(0, fit_unadjusted$coefficients[2:5])
names(bmi_coeff) = c("bmi_catNormal", names(fit_unadjusted$coefficients[2:5]))
baseline_prob = locfit::expit(fit_unadjusted$coefficients[1] + bmi_coeff)

PAR_percent_shift = function(transition_matrix, baseline_probability, unadjusted_RR, adjusted_RR, prevalence)
{
  ratio_matrix = unadjusted_RR/adjusted_RR
  ratio_matrix[upper.tri(ratio_matrix)]  = 0
  baseline_prob_matrix = t(matrix(rep(baseline_probability, length(baseline_probability)), nrow = length(baseline_probability), ncol = length(baseline_probability)))
  return(1 - sum(transition_matrix*ratio_matrix*baseline_prob_matrix)/prevalence)
}

nhanes_combined_white_temp = nhanes_combined_white
nhanes_combined_white_temp$bmi_cat = as.factor(as.character(nhanes_combined_white_temp$bmi_cat))
hh = model.matrix(~ bmi_cat, data = nhanes_combined_white_temp)
#hh_obese = model.matrix(~ bmi_cat + sex + age + education + smoking_status + no_of_cigarettes + alcohol_status + marital_status, data = data_processed_complete)
hh = cbind(hh, nhanes_combined_white$sampweight)
hh = as.data.frame(hh)

colnames(hh)[6] = "sampweight"
hh = hh %>% mutate(bmi_cat = as.character(nhanes_combined_white$bmi_cat), bmi_cat_shift = as.character(nhanes_combined_white$bmi_cat_shift))


p_hat = weighted.mean(nhanes_combined_white$mortstat_rec, w=nhanes_combined_white$sampweight)

A1 = as.matrix(MR_vcov_file[, -c(1:3)][c(6,10,12,13,14), c(6,10,12,13,14)])
B1 = diag((delta_HR[c(6,10,12,13,14)])) %*% A1 %*% diag((delta_HR[c(6,10,12,13,14)]))
C1 = matrix(0, 4,5)
C1[1, 1:2] = 1/sum(delta_HR[c(6,10)])
C1[2, 2:3] = 1/sum(delta_HR[c(10,12)])
C1[3, 3:4] = 1/sum(delta_HR[c(12,13)])
C1[4, 4:5] = 1/sum(delta_HR[c(13,14)])
Omega = C1 %*% B1 %*% t(C1)
Omega = matrix(0, 4, 4)
diag_omega = readRDS("code/var_cardio_meta_pooled_6groups.rds")
diag(Omega) = diag_omega
p_hat_using_model = sum(exp(fit_unadjusted$coefficients[1] + bmi_coeff)*prop_exposure)

#PAR_percent_shift(prop.table(table(hh$bmi_cat, hh$bmi_cat_shift)), baseline_prob, RR, MR_RR, p_hat)
PAR_percent_shift(prop.table(table(hh$bmi_cat, hh$bmi_cat_shift)), baseline_prob, RR, MR_RR, p_hat)
PAR_percent_shift(prop.table(table(hh$bmi_cat, hh$bmi_cat_shift)), baseline_prob, RR, MR_RR, p_hat) - 1.96*PAR_percent_shift_se(fit_unadjusted$coefficients, MR_estimates_UK, p_hat, prop.table(table(hh$bmi_cat, hh$bmi_cat_shift)), Omega = Omega)
PAR_percent_shift(prop.table(table(hh$bmi_cat, hh$bmi_cat_shift)), baseline_prob, RR, MR_RR, p_hat) + 1.96*PAR_percent_shift_se(fit_unadjusted$coefficients, MR_estimates_UK, p_hat, prop.table(table(hh$bmi_cat, hh$bmi_cat_shift)), Omega = Omega)



#---- Cancer--
nhanes_combined_white = nhanes_combined[which(nhanes_combined$race_ethnicity == "white"), ]

MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_cancer.csv")
BMI_quantiles = MR_vcov_file$bmi.quant

#Normal : BMI_quantiles[3]- BMI_quantiles[6], Overweight: BMI_quantiles[6]-BMI_quantiles[9], Obese: BMI_quantiles[9]:BMI_quantiles[13], Severe obese: >= BMI_quantiles[13]
delta_HR = exp(MR_vcov_file$est)
#MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[9]-delta_HR[6])/2, delta_HR[9] + (delta_HR[13]-delta_HR[9])/2, delta_HR[13] + (delta_HR[14]-delta_HR[13])/2))
MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[10]-delta_HR[6])/2, delta_HR[10] + (delta_HR[12]-delta_HR[10])/2, delta_HR[12] + (delta_HR[13]-delta_HR[12])/2, delta_HR[13] + (delta_HR[14]-delta_HR[13])/2))

#MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[10]-delta_HR[6])/2, delta_HR[10] + (delta_HR[14]-delta_HR[10])/2))


#---Pooled cohort
mid_point_normal = BMI_quantiles[3] + (BMI_quantiles[6] - BMI_quantiles[3])/2

nhanes_combined_white = nhanes_combined_white %>% mutate(bmi_cat = case_when(bmi < BMI_quantiles[3] ~ "Underweight",
                                                                             bmi >= BMI_quantiles[3] & bmi < BMI_quantiles[6] ~ "Normal",
                                                                             bmi >= BMI_quantiles[6] & bmi < BMI_quantiles[10] ~ "Overweight",
                                                                             bmi >= BMI_quantiles[10] & bmi < BMI_quantiles[12] ~ "SObese_grade1",
                                                                             bmi >= BMI_quantiles[12] & bmi < BMI_quantiles[13] ~ "SObese_grade2",
                                                                             bmi >= BMI_quantiles[13] ~ "SObese_grade3"))
if(length(which(is.na(nhanes_combined_white$ucod_leading) == T & nhanes_combined_white$mortstat == 1)) > 0)
  nhanes_combined_white = nhanes_combined_white[-which(is.na(nhanes_combined_white$ucod_leading) == T & nhanes_combined_white$mortstat == 1),]
nhanes_combined_white = nhanes_combined_white %>% mutate(mortstat_rec = case_when(mortstat == 1 & ucod_leading == 2  ~ 1,
                                                                                  mortstat == 1 & ucod_leading !=2  ~ 0,
                                                                                  mortstat == 0 ~ 0))

#---Removing underweight people--#
nhanes_combined_white = nhanes_combined_white[-which(nhanes_combined_white$bmi_cat == "Underweight"), ]

nhanes_combined_white$bmi_shift = NA
nhanes_combined_white$bmi_shift[which(nhanes_combined_white$bmi_cat == "Normal")] = nhanes_combined_white$bmi[which(nhanes_combined_white$bmi_cat == "Normal")]
nhanes_combined_white$bmi_shift[which(nhanes_combined_white$bmi_cat != "Normal")] = mid_point_normal + 0.5*(nhanes_combined_white$bmi[which(nhanes_combined_white$bmi_cat != "Normal")] - mid_point_normal)
nhanes_combined_white = nhanes_combined_white %>% mutate(bmi_cat_shift = case_when(bmi_shift < BMI_quantiles[3] ~ "Underweight",
                                                                                   bmi_shift >= BMI_quantiles[3] & bmi_shift < BMI_quantiles[6] ~ "Normal",
                                                                                   bmi_shift >= BMI_quantiles[6] & bmi_shift < BMI_quantiles[10] ~ "Overweight",
                                                                                   bmi_shift >= BMI_quantiles[10] & bmi_shift < BMI_quantiles[12] ~ "SObese_grade1",
                                                                                   bmi_shift >= BMI_quantiles[12] & bmi_shift < BMI_quantiles[13] ~ "SObese_grade2",
                                                                                   bmi_shift >= BMI_quantiles[13] ~ "SObese_grade3"))

#nhanes_combined_white = nhanes_combined_white %>% mutate(bmi_cat = case_when(bmi < BMI_quantiles[3] ~ "Underweight",
#                                                                            bmi >= BMI_quantiles[3] & bmi < BMI_quantiles[6] ~ "Normal",
#                                                                           bmi >= BMI_quantiles[6] & bmi < BMI_quantiles[8] ~ "ObeseI",
#                                                                          bmi >= BMI_quantiles[8] & bmi < BMI_quantiles[10] ~ "ObeseII",
#                                                                         bmi >= BMI_quantiles[10] & bmi < BMI_quantiles[12] ~ "ObeseIII",
#                                                                        bmi >= BMI_quantiles[12] & bmi < BMI_quantiles[13] ~ "ObeseIV",
#                                                                       bmi >= BMI_quantiles[13] ~ "ObeseV"))


#nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(bmi_cat = case_when(BMI < 18.5 ~ "Underweight",
#                                                                          BMI >= 18.5 & BMI < 25 ~ "Normal",
#                                                                         BMI >= 25 & BMI < 30 ~ "ObeseI",
#                                                                        BMI >= 30 & BMI < 35 ~ "ObeseII",
#                                                                       BMI >= 35 ~"ObeseIII"))


nhanes_combined_white$bmi_cat  = as.factor(nhanes_combined_white$bmi_cat)

nhanes_combined_white$bmi_cat = relevel(nhanes_combined_white$bmi_cat, ref = "Normal")

#nhanes_combined_white$alcohol_status_new[which(is.na(nhanes_combined_white$alcohol_status_new) == T)] = "NA"
#nhanes_combined_white$smoking_status[which(is.na(nhanes_combined_white$smoking_status) == T)] = "NA"
#nhanes_combined_white$marital_status[which(is.na(nhanes_combined_white$marital_status) == T)] = "NA"
#nhanes_combined_white$education[which(is.na(nhanes_combined_white$education) == T)] = "NA"

#nhanes_combined_white$alcohol_status_new[which(is.na(nhanes_combined_white$alcohol_status_new) == T)] = "NA"
#nhanes_combined_white$smoking_status[which(is.na(nhanes_combined_white$smoking_status) == T)] = "NA"
#nhanes_combined_white$marital_status[which(is.na(nhanes_combined_white$marital_status) == T)] = "NA"
#nhanes_combined_white$education[which(is.na(nhanes_combined_white$education) == T)] = "NA"


nhanes_svydesign <- svydesign(data=nhanes_combined_white, id=~SDMVPSU, strata=~SDMVSTRA, weights=~MEC8YR, nest=TRUE)
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
fit_unadjusted = svyglm(mortstat_rec ~ bmi_cat, design=nhanes_svydesign, family = quasibinomial())
prop_exposure = wpct(nhanes_combined_white$bmi_cat, weight = nhanes_combined_white$MEC8YR)
nhanes_combined_white = nhanes_combined_white %>% filter(bmi_cat == "Normal" | bmi_cat == "Overweight" | bmi_cat == "SObese_grade1" | bmi_cat == "SObese_grade2" | bmi_cat =="SObese_grade3")
#nhanes_combined_white$bmi_cat_shift[which(nhanes_combined_white$bmi_cat_shift == "Underweight")] = "Normal"

#---Relative Risk Matrix
RR = matrix(NA,5,5)
colnames(RR) = c("Normal", "Overweight", "SObese_grade1", "SObese_grade2", "SObese_grade3")
rownames(RR) = c("Normal", "Overweight", "SObese_grade1", "SObese_grade2", "SObese_grade3")
diag(RR) = 1
RR = RR[-1,]
RR[1,1] = exp(fit_unadjusted$coefficients[2])
RR[2,1] = exp(fit_unadjusted$coefficients[3])
RR[3,1] = exp(fit_unadjusted$coefficients[4])
RR[4,1] = exp(fit_unadjusted$coefficients[5])
RR[2,2] = exp(fit_unadjusted$coefficients[3] - fit_unadjusted$coefficients[2])
RR[3,2] = exp(fit_unadjusted$coefficients[4] - fit_unadjusted$coefficients[2])
RR[4,2] = exp(fit_unadjusted$coefficients[5] - fit_unadjusted$coefficients[2])
RR[3,3] = exp(fit_unadjusted$coefficients[4] - fit_unadjusted$coefficients[3])
RR[4,3] = exp(fit_unadjusted$coefficients[5] - fit_unadjusted$coefficients[3])
RR[4,4] = exp(fit_unadjusted$coefficients[5] - fit_unadjusted$coefficients[4])


pooled_estimates = c(0.07803658, 0.25648089, 0.45863808, 0.65864070)
MR_RR = RR
MR_RR[1,1] = exp(pooled_estimates[1])
MR_RR[2,1] = exp(pooled_estimates[2])
MR_RR[3,1] = exp(pooled_estimates[3])
MR_RR[4,1] = exp(pooled_estimates[4])
MR_RR[2,2] = exp(pooled_estimates[2] - pooled_estimates[1])
MR_RR[3,2] = exp(pooled_estimates[3] - pooled_estimates[1])
MR_RR[4,2] = exp(pooled_estimates[4] - pooled_estimates[1])
MR_RR[3,3] = exp(pooled_estimates[3] - pooled_estimates[2])
MR_RR[4,3] = exp(pooled_estimates[4] - pooled_estimates[2])
MR_RR[4,4] = exp(pooled_estimates[4] - pooled_estimates[3])

RR = rbind(c(1,NA,NA,NA,NA), RR)
rownames(RR)[1] = "Normal"
MR_RR = rbind(c(1,NA,NA,NA,NA), MR_RR)
rownames(MR_RR)[1] = "Normal"


colnames(nhanes_combined_white)[97] = "sampweight"


weight.mean = function(x,w)
{
  return(sum(x*w))
}


baseline_prob = rep(0, length(unique(nhanes_combined_white$bmi_cat_shift)))
bmi_coeff = c(0, fit_unadjusted$coefficients[2:5])
names(bmi_coeff) = c("bmi_catNormal", names(fit_unadjusted$coefficients[2:5]))
baseline_prob = locfit::expit(fit_unadjusted$coefficients[1] + bmi_coeff)

PAR_percent_shift = function(transition_matrix, baseline_probability, unadjusted_RR, adjusted_RR, prevalence)
{
  ratio_matrix = unadjusted_RR/adjusted_RR
  ratio_matrix[upper.tri(ratio_matrix)]  = 0
  baseline_prob_matrix = t(matrix(rep(baseline_probability, length(baseline_probability)), nrow = length(baseline_probability), ncol = length(baseline_probability)))
  return(1 - sum(transition_matrix*ratio_matrix*baseline_prob_matrix)/prevalence)
}

nhanes_combined_white_temp = nhanes_combined_white
nhanes_combined_white_temp$bmi_cat = as.factor(as.character(nhanes_combined_white_temp$bmi_cat))
hh = model.matrix(~ bmi_cat, data = nhanes_combined_white_temp)
#hh_obese = model.matrix(~ bmi_cat + sex + age + education + smoking_status + no_of_cigarettes + alcohol_status + marital_status, data = data_processed_complete)
hh = cbind(hh, nhanes_combined_white$sampweight)
hh = as.data.frame(hh)

colnames(hh)[6] = "sampweight"
hh = hh %>% mutate(bmi_cat = as.character(nhanes_combined_white$bmi_cat), bmi_cat_shift = as.character(nhanes_combined_white$bmi_cat_shift))


p_hat = weighted.mean(nhanes_combined_white$mortstat_rec, w=nhanes_combined_white$sampweight)
A1 = as.matrix(MR_vcov_file[, -c(1:3)][c(6,10,12,13,14), c(6,10,12,13,14)])
B1 = diag((delta_HR[c(6,10,12,13,14)])) %*% A1 %*% diag((delta_HR[c(6,10,12,13,14)]))
C1 = matrix(0, 4,5)
C1[1, 1:2] = 1/sum(delta_HR[c(6,10)])
C1[2, 2:3] = 1/sum(delta_HR[c(10,12)])
C1[3, 3:4] = 1/sum(delta_HR[c(12,13)])
C1[4, 4:5] = 1/sum(delta_HR[c(13,14)])
Omega = C1 %*% B1 %*% t(C1)
Omega = matrix(0, 4, 4)
diag_omega = readRDS("code/beta_cancer_meta_pooled_6groups.rds")
diag(Omega) = diag_omega
PAR_percent_shift(prop.table(table(hh$bmi_cat, hh$bmi_cat_shift)), baseline_prob, RR, MR_RR, p_hat)
PAR_percent_shift(prop.table(table(hh$bmi_cat, hh$bmi_cat_shift)), baseline_prob, RR, MR_RR, p_hat) - 1.96*PAR_percent_shift_se(fit_unadjusted$coefficients, MR_estimates_UK, p_hat, prop.table(table(hh$bmi_cat, hh$bmi_cat_shift)), Omega = Omega)
PAR_percent_shift(prop.table(table(hh$bmi_cat, hh$bmi_cat_shift)), baseline_prob, RR, MR_RR, p_hat) + 1.96*PAR_percent_shift_se(fit_unadjusted$coefficients, MR_estimates_UK, p_hat, prop.table(table(hh$bmi_cat, hh$bmi_cat_shift)), Omega = Omega)


