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


DEMO_2017 = foreign::read.xport("/Users/prosenjitkundu/Desktop/NCI_multi_cancer/Reference_data/NHANES/2017_2018/DEMO_J.XPT")
BODY_MEASURE_2017 = foreign::read.xport("/Users/prosenjitkundu/Desktop/NCI_multi_cancer/Reference_data/NHANES/2017_2018/BMX_J.XPT")
#ALCO_1999 = foreign::read.xport("/Users/prosenjitkundu/Desktop/NCI_multi_cancer/Reference_data/NHANES/1999_2000/ALQ.XPT")
SMOKE_2017 = foreign::read.xport("/Users/prosenjitkundu/Desktop/NCI_multi_cancer/Reference_data/NHANES/2017_2018/SMQ_J.XPT")


NHANES_2017 = merge(DEMO_2017, BODY_MEASURE_2017, by = "SEQN", all = T)
NHANES_2017 = NHANES_2017 %>% mutate(race_ethnicity = case_when(RIDRETH1 <= 2  ~ "hispanic",
                                                                RIDRETH1 == 3 ~ "white",
                                                                RIDRETH1 == 4 ~ "black",
                                                                RIDRETH1 == 6 ~ "asian"))
nhanes_white_2017 = NHANES_2017[which(NHANES_2017$race_ethnicity == "black"),]
#---18+
nhanes_white_2017 = nhanes_white_2017[which(nhanes_white_2017$RIDAGEYR >= 40 & nhanes_white_2017$RIDAGEYR < 70), ]
nhanes_white_2017$bmi = nhanes_white_2017$BMXBMI

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
nhanes_combined_white = nhanes_combined[which(nhanes_combined$race_ethnicity == "black"), ]
final_data = nhanes_combined_white
#AA
#nhanes_combined_aa = nhanes_combined[which(nhanes_combined$race == "black"), ]

#---All-cause
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


nhanes_white_2017 = nhanes_white_2017 %>% mutate(bmi_cat = case_when(bmi < BMI_quantiles[3] ~ "Underweight",
                                                                     bmi >= BMI_quantiles[3] & bmi < BMI_quantiles[6] ~ "Normal",
                                                                     bmi >= BMI_quantiles[6] & bmi < BMI_quantiles[8] ~ "ObeseI",
                                                                     bmi >= BMI_quantiles[8] & bmi < BMI_quantiles[10] ~ "ObeseII",
                                                                     bmi >= BMI_quantiles[10] & bmi < BMI_quantiles[12] ~ "ObeseIII",
                                                                     bmi >= BMI_quantiles[12] & bmi < BMI_quantiles[13] ~ "ObeseIV",
                                                                     bmi >= BMI_quantiles[13] ~ "ObeseV"))
nhanes_white_2017 = nhanes_white_2017[-which(nhanes_white_2017$bmi_cat == "Underweight"), ]

#nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(bmi_cat = case_when(BMI < 18.5 ~ "Underweight",
#                                                                          BMI >= 18.5 & BMI < 25 ~ "Normal",
#                                                                         BMI >= 25 & BMI < 30 ~ "ObeseI",
#                                                                        BMI >= 30 & BMI < 35 ~ "ObeseII",
#                                                                       BMI >= 35 ~"ObeseIII"))

#---Removing underweight people--#
nhanes_combined_white = nhanes_combined_white[-which(nhanes_combined_white$bmi_cat == "Underweight"), ]





#nhanes_combined_white = nhanes_combined_white %>% mutate(one = 1)
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
nhanes_svydesign <- svydesign(data=nhanes_combined_white, id=~SDMVPSU, strata=~SDMVSTRA, weights=~MEC8YR, nest=TRUE)
fit_unadjusted = svyglm(mortstat ~ bmi_cat, design=nhanes_svydesign , family = quasibinomial())




p_hat = sum(nhanes_combined_white$MEC8YR[which(nhanes_combined_white$mortstat == 1)])/sum(nhanes_combined_white$MEC8YR)
p_obs_D_given_E_equalto_0 = sum(nhanes_combined_white$MEC8YR[which(nhanes_combined_white$mortstat == 1 & nhanes_combined_white$bmi_cat == "Normal")])/sum(nhanes_combined_white$MEC8YR[which(nhanes_combined_white$bmi_cat == "Normal")])
p_obs_D_given_E_equalto_0 = locfit::expit(fit_unadjusted$coefficients[1])
#prop_exposure = wpct(nhanes_combined_white$bmi_cat, weight = nhanes_combined_white$MEC8YR)
#PAR_MR = 1 - (prop_exposure[1] + sum(prop_exposure[-1] * exp(fit_unadjusted$coefficients[2:6] - MR_estimates_UK))) * (p_obs_D_given_E_equalto_0/p_hat)
prop_exposure = wpct(nhanes_white_2017$bmi_cat, weight = nhanes_white_2017$WTMEC2YR)
PAR_MR = 1 - ((prop_exposure[1] + sum(prop_exposure[-1] * exp(fit_unadjusted$coefficients[2:6] - MR_estimates_UK)))/((prop_exposure[1] + sum(prop_exposure[-1] * exp(fit_unadjusted$coefficients[2:6]))))) 

#Normal 
#0.1778511 
PAR_observed = 1 - (p_obs_D_given_E_equalto_0/p_hat)


#Variance of PAR projected calculation
k = 1/prop_exposure[1]
data_processed = nhanes_white_2017 %>% dplyr::select("bmi_cat", "bmi", "WTMEC2YR")
colnames(data_processed)[3] = "sampweight"
Q = -matrix(rep(-k, 25), 5, 5) + diag(-1/prop_exposure[-1])
Sigma = vcov(fit_unadjusted)[-1,-1]
total_weights = sum(data_processed$sampweight)
data_processed$bmi_cat = as.factor(data_processed$bmi_cat)

data_processed_mm = as.data.frame(model.matrix(~ bmi_cat + sampweight, data_processed))
psi = t(t(data_processed_mm[, c(2:6)])/prop_exposure[-1]) - ((1-data_processed_mm[,2]-data_processed_mm[,3]-data_processed_mm[,4]-data_processed_mm[,5]-data_processed_mm[,6])/prop_exposure[1])

Delta = (solve(Q) %*% ((t(psi) %*% (psi*data_processed_mm$sampweight))/total_weights) %*% solve(Q))/nrow(data_processed_mm)

MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_fullsample.csv") 
delta = MR_vcov_file[, 3]
#delta = c(delta, log(7.5))
MR_vcov_file = MR_vcov_file[, -c(1,2,3)]
MR_vcov = MR_vcov_file[c(6, 8, 10, 12, 13,14), c(6, 8, 10, 12, 13,14)]
#MR_vcov = as.matrix(rbind(cbind(MR_vcov, 0),0))
#MR_vcov[6,6] = 1
R = matrix(0, 5, 6)
R[1, 1:2] = c(exp(delta[6]), exp(delta[8]))/2
R[2, 2:3] = c(exp(delta[8]), exp(delta[10]))/2
R[3, 3:4] = c(exp(delta[10]), exp(delta[12]))/2
R[4, 4:5] = c(exp(delta[12]), exp(delta[13]))/2
R[5, 5:6] = c(exp(delta[13]), exp(delta[14]))/2
Omega = R %*% as.matrix(MR_vcov) %*% t(R)
Omega_star = diag(1/exp(MR_estimates_UK)) %*% Omega %*% diag(1/exp(MR_estimates_UK))

Lambda = bdiag(bdiag(Sigma, Omega_star), Delta)

a = fit_unadjusted$coefficients[-1]
b = MR_estimates_UK
c = prop_exposure[-1]
f = 1 + sum(c*(exp(a-b) - 1))
h = 1 + sum(c*(exp(a) - 1)) 
g1 = (c*exp(a-b)/h) - ((c*exp(a))*(f/h^2))
g2 = -c*exp(a-b)/h
g3 = ((exp(a-b) - 1)/h) - ((exp(a) - 1)*(f/h^2))
g = c(g1,g2,g3)

PAR_proj_var = t(g) %*% Lambda %*% g
sqrt(PAR_proj_var)

L.CI = PAR_MR - 1.96*sqrt(PAR_proj_var)
R.CI = PAR_MR + 1.96*sqrt(PAR_proj_var)


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

#heart_data = heart_data %>% dplyr::select("bmi_cat", "alcohol_status", "sex", "age", "smoking_status", "MEC8YR", "mortstat_rec", "SDMVSTRA", "SDMVPSU")

nhanes_combined_white$MEC8YR = nhanes_combined_white$MEC8YR/sum(nhanes_combined_white$MEC8YR)


options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
nhanes_svydesign <- svydesign(data=nhanes_combined_white, id=~SDMVPSU, strata=~SDMVSTRA, weights=~MEC8YR, nest=TRUE)

fit_unadjusted = svyglm(mortstat_rec ~ bmi_cat, design=nhanes_svydesign , family = quasibinomial())



p_hat = sum(nhanes_combined_white$MEC8YR[which(nhanes_combined_white$mortstat_rec == 1)])/sum(nhanes_combined_white$MEC8YR)
p_obs_D_given_E_equalto_0 = sum(nhanes_combined_white$MEC8YR[which(nhanes_combined_white$mortstat_rec == 1 & nhanes_combined_white$bmi_cat == "Normal")])/sum(nhanes_combined_white$MEC8YR[which(nhanes_combined_white$bmi_cat == "Normal")])

#prop_exposure = wpct(nhanes_combined_white$bmi_cat, weight = nhanes_combined_white$MEC8YR)
#PAR_MR = 1 - (prop_exposure[1] + sum(prop_exposure[-1] * exp(fit_unadjusted$coefficients[2:6] - MR_estimates_UK))) * (p_obs_D_given_E_equalto_0/p_hat)
prop_exposure = wpct(nhanes_white_2017$bmi_cat, weight = nhanes_white_2017$WTMEC2YR)
PAR_MR = 1 - ((prop_exposure[1] + sum(prop_exposure[-1] * exp(fit_unadjusted$coefficients[2:6] - MR_estimates_UK)))/((prop_exposure[1] + sum(prop_exposure[-1] * exp(fit_unadjusted$coefficients[2:6]))))) 

#Normal 
#0.1778511 
PAR_observed = 1 - (p_obs_D_given_E_equalto_0/p_hat)

fit_minimal = svyglm(mortstat_rec ~ bmi_cat + sex + age, design=nhanes_svydesign, family = quasibinomial())


data_processed_complete_minimal = na.omit(nhanes_combined_white %>% dplyr::select("sex", "age","bmi", "MEC8YR", "mortstat_rec", "bmi_obese", "bmi_cat"))
indicator_normal = if_else(data_processed_complete_minimal$bmi_cat == "Normal", 1,0)
hh = model.matrix(~ bmi_cat + sex + age, data = data_processed_complete_minimal)
hh[, c(2:6)] = 0
PAR_minimally_adjusted = 1 - wtd.mean(exp(hh %*% as.numeric(fit_minimal$coefficients))/(1 + exp(hh %*% as.numeric(fit_minimal$coefficients))), data_processed_complete_minimal$MEC8YR)/p_hat
#[1]0.0249229
colnames(data_processed_complete_minimal)[4]="sampweight"
indicator_normal = if_else(data_processed_complete_minimal$bmi_cat == "Normal", 1, 0)
s = exp(hh %*% as.numeric(fit_minimal$coefficients))/(1 + exp(hh %*% as.numeric(fit_minimal$coefficients)))
l = data_processed_complete_minimal$mortstat_rec - s
w = (data_processed_complete_minimal$sampweight/sum(data_processed_complete_minimal$sampweight)) * as.numeric(l)^2
a_11 = wtd.mean(s^2, data_processed_complete_minimal$sampweight) - wtd.mean(s, data_processed_complete_minimal$sampweight)^2
a_22 = t(hh[, -c(2:6)]) %*% (hh[, -c(2:6)] * w)
a_33 = wtd.mean(indicator_normal^2, data_processed_complete_minimal$sampweight) - (wtd.mean(indicator_normal, data_processed_complete_minimal$sampweight))^2
a_13 = wtd.mean(s*indicator_normal, data_processed_complete_minimal$sampweight) - (wtd.mean(indicator_normal, data_processed_complete_minimal$sampweight) * wtd.mean(s, data_processed_complete_minimal$sampweight))
a_23 = apply(hh[, -c(2:6)] * indicator_normal * (data_processed_complete_minimal$sampweight/sum(data_processed_complete_minimal$sampweight)), 2, mean)
a_12 = apply(hh[, -c(2:6)] * as.numeric(s) * as.numeric(l) * (data_processed_complete_minimal$sampweight/sum(data_processed_complete_minimal$sampweight)), 2, mean)
c = apply(hh[, -c(2:6)] * as.numeric(s) * (data_processed_complete_minimal$sampweight/sum(data_processed_complete_minimal$sampweight)), 2, mean)
A = as.matrix(rbind(c(a_11, a_12,a_13), cbind(a_12, a_22, a_23), c(a_13, a_23, a_33)))
Gamma = solve(t(hh[, -c(2:6)]) %*% (hh[, -c(2:6)] * dexpit(hh %*% as.numeric(fit_minimal$coefficients)) * (data_processed_complete_minimal$sampweight/sum(data_processed_complete_minimal$sampweight))))
c_star = t(c) %*% Gamma
M = as.matrix(rbind(c(1, c_star, 0), c(0, rep(0, length(c_star)), 1)))
g = c(1/p_hat, -wtd.mean(exp(hh %*% as.numeric(fit_minimal$coefficients))/(1 + exp(hh %*% as.numeric(fit_minimal$coefficients))), data_processed_complete_minimal$sampweight)/p_hat)
sqrt((1/nrow(data_processed_complete_minimal)) * t(g) %*% M %*% A %*% t(M) %*% g)


fit_full = svyglm(mortstat_rec ~ bmi_cat + sex + age + smoking_status + alcohol_status_new + education + marital_status + no_of_cigarettes , design=nhanes_svydesign, family = quasibinomial())
data_processed_complete = na.omit(nhanes_combined_white %>% dplyr::select("sex", "age","bmi", "MEC8YR", "mortstat_rec", "bmi_obese", "bmi_cat","education","alcohol_status_new","marital_status", "smoking_status", "no_of_cigarettes"))
indicator_normal = if_else(data_processed_complete$bmi_cat == "Normal", 1,0)
hh = model.matrix(~  bmi_cat + sex + age + smoking_status + alcohol_status_new + education + marital_status + no_of_cigarettes, data = data_processed_complete)
hh[, c(2:6)] = 0
PAR_fully_adjusted = 1 - wtd.mean(exp(hh %*% as.numeric(fit_full$coefficients))/(1 + exp(hh %*% as.numeric(fit_full$coefficients))), data_processed_complete$MEC8YR)/p_hat
#[1] 0.02262194

colnames(data_processed_complete)[4] = "sampweight"
s = exp(hh %*% as.numeric(fit_full$coefficients))/(1 + exp(hh %*% as.numeric(fit_full$coefficients)))
l = data_processed_complete$mortstat_rec - s
w = (data_processed_complete$sampweight/sum(data_processed_complete$sampweight)) * as.numeric(l)^2
a_11 = wtd.mean(s^2, data_processed_complete$sampweight) - wtd.mean(s, data_processed_complete$sampweight)^2
a_22 = t(hh[, -c(2:6)]) %*% (hh[, -c(2:6)] * w)
a_33 = wtd.mean(indicator_normal^2, data_processed_complete$sampweight) - (wtd.mean(indicator_normal, data_processed_complete$sampweight))^2
a_13 = wtd.mean(s*indicator_normal, data_processed_complete$sampweight) - (wtd.mean(indicator_normal, data_processed_complete$sampweight) * wtd.mean(s, data_processed_complete$sampweight))
a_23 = apply(hh[, -c(2:6)] * indicator_normal * (data_processed_complete$sampweight/sum(data_processed_complete$sampweight)), 2, mean)
a_12 = apply(hh[, -c(2:6)] * as.numeric(s) * as.numeric(l) * (data_processed_complete$sampweight/sum(data_processed_complete$sampweight)), 2, mean)
c = apply(hh[, -c(2:6)] * as.numeric(s) * (data_processed_complete$sampweight/sum(data_processed_complete$sampweight)), 2, mean)
A = as.matrix(rbind(c(a_11, a_12,a_13), cbind(a_12, a_22, a_23), c(a_13, a_23, a_33)))
Gamma = solve(t(hh[, -c(2:6)]) %*% (hh[, -c(2:6)] * dexpit(hh %*% as.numeric(fit_full$coefficients)) * (data_processed_complete$sampweight/sum(data_processed_complete$sampweight))))
c_star = t(c) %*% Gamma
M = as.matrix(rbind(c(1, c_star, 0), c(0, rep(0, length(c_star)), 1)))
g = c(1/p_hat, -wtd.mean(exp(hh %*% as.numeric(fit_full$coefficients))/(1 + exp(hh %*% as.numeric(fit_full$coefficients))), data_processed_complete$sampweight)/p_hat)
sqrt((1/nrow(data_processed_complete)) * t(g) %*% M %*% A %*% t(M) %*% g)
#0.004545333

# Variance projected PAR
k = 1/prop_exposure[1]
data_processed = nhanes_white_2017 %>% dplyr::select("bmi_cat", "bmi", "WTMEC2YR")
colnames(data_processed)[3] = "sampweight"
Q = -matrix(rep(-k, 25), 5, 5) + diag(-1/prop_exposure[-1])
Sigma = vcov(fit_unadjusted)[-1,-1]
total_weights = sum(data_processed$sampweight)
data_processed$bmi_cat = as.factor(data_processed$bmi_cat)

data_processed_mm = as.data.frame(model.matrix(~ bmi_cat + sampweight, data_processed))
psi = t(t(data_processed_mm[, c(2:6)])/prop_exposure[-1]) - ((1-data_processed_mm[,2]-data_processed_mm[,3]-data_processed_mm[,4]-data_processed_mm[,5]-data_processed_mm[,6])/prop_exposure[1])

Delta = (solve(Q) %*% ((t(psi) %*% (psi*data_processed_mm$sampweight))/total_weights) %*% solve(Q))/nrow(data_processed_mm)

MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_cardiovascular.csv") 
delta = MR_vcov_file[, 3]
#delta = c(delta, log(7.5))
MR_vcov_file = MR_vcov_file[, -c(1,2,3)]
MR_vcov = MR_vcov_file[c(6, 8, 10, 12, 13,14), c(6, 8, 10, 12, 13,14)]
#MR_vcov = as.matrix(rbind(cbind(MR_vcov, 0),0))
#MR_vcov[6,6] = 1
R = matrix(0, 5, 6)
R[1, 1:2] = c(exp(delta[6]), exp(delta[8]))/2
R[2, 2:3] = c(exp(delta[8]), exp(delta[10]))/2
R[3, 3:4] = c(exp(delta[10]), exp(delta[12]))/2
R[4, 4:5] = c(exp(delta[12]), exp(delta[13]))/2
R[5, 5:6] = c(exp(delta[13]), exp(delta[14]))/2
Omega = R %*% as.matrix(MR_vcov) %*% t(R)
Omega_star = diag(1/exp(MR_estimates_UK)) %*% Omega %*% diag(1/exp(MR_estimates_UK))

Lambda = bdiag(bdiag(Sigma, Omega_star), Delta)

a = fit_unadjusted$coefficients[-1]
b = MR_estimates_UK
c = prop_exposure[-1]
f = 1 + sum(c*(exp(a-b) - 1))
h = 1 + sum(c*(exp(a) - 1)) 
g1 = (c*exp(a-b)/h) - ((c*exp(a))*(f/h^2))
g2 = -c*exp(a-b)/h
g3 = ((exp(a-b) - 1)/h) - ((exp(a) - 1)*(f/h^2))
g = c(g1,g2,g3)

PAR_proj_var = t(g) %*% Lambda %*% g
sqrt(PAR_proj_var)




#Cancer
MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_cancer.csv")
BMI_quantiles = MR_vcov_file$bmi.quant
delta_HR = exp(MR_vcov_file$est)
#MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[9]-delta_HR[6])/2, delta_HR[9] + (delta_HR[11]-delta_HR[9])/2, delta_HR[11] + (delta_HR[13]-delta_HR[11])/2, delta_HR[13] + (delta_HR[14]-delta_HR[13])/2))
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

#heart_data = heart_data %>% dplyr::select("bmi_cat", "alcohol_status", "sex", "age", "smoking_status", "MEC8YR", "mortstat_rec", "SDMVSTRA", "SDMVPSU")

nhanes_svydesign <- svydesign(data=nhanes_combined_white, id=~SDMVPSU, strata=~SDMVSTRA, weights=~MEC8YR, nest=TRUE)
fit_unadjusted = svyglm(mortstat_rec ~ bmi_cat, design=nhanes_svydesign , family = quasibinomial())
p_hat = sum(nhanes_combined_white$MEC8YR[which(nhanes_combined_white$mortstat_rec == 1)])/sum(nhanes_combined_white$MEC8YR)
p_obs_D_given_E_equalto_0 = sum(nhanes_combined_white$MEC8YR[which(nhanes_combined_white$mortstat_rec == 1 & nhanes_combined_white$bmi_cat == "Normal")])/sum(nhanes_combined_white$MEC8YR[which(nhanes_combined_white$bmi_cat == "Normal")])

#prop_exposure = wpct(nhanes_combined_white$bmi_cat, weight = nhanes_combined_white$MEC8YR)
#PAR_MR = 1 - (prop_exposure[1] + sum(prop_exposure[-1] * exp(fit_unadjusted$coefficients[2:6] - MR_estimates_UK))) * (p_obs_D_given_E_equalto_0/p_hat)
prop_exposure = wpct(nhanes_white_2017$bmi_cat, weight = nhanes_white_2017$WTMEC2YR)
PAR_MR = 1 - ((prop_exposure[1] + sum(prop_exposure[-1] * exp(fit_unadjusted$coefficients[2:6] - MR_estimates_UK)))/((prop_exposure[1] + sum(prop_exposure[-1] * exp(fit_unadjusted$coefficients[2:6]))))) 

#Normal 
#0.1778511 
PAR_observed = 1 - (p_obs_D_given_E_equalto_0/p_hat)

fit_minimal = svyglm(mortstat_rec ~ bmi_cat + sex + age, design=nhanes_svydesign, family = quasibinomial())


data_processed_complete_minimal = na.omit(nhanes_combined_white %>% dplyr::select("sex", "age","bmi", "MEC8YR", "mortstat_rec", "bmi_obese", "bmi_cat"))
indicator_normal = if_else(data_processed_complete_minimal$bmi_cat == "Normal", 1,0)
hh = model.matrix(~ bmi_cat + sex + age, data = data_processed_complete_minimal)
hh[, c(2:6)] = 0
PAR_minimally_adjusted = 1 - wtd.mean(exp(hh %*% as.numeric(fit_minimal$coefficients))/(1 + exp(hh %*% as.numeric(fit_minimal$coefficients))), data_processed_complete_minimal$MEC8YR)/p_hat
#[1]0.0249229
colnames(data_processed_complete_minimal)[4]="sampweight"
indicator_normal = if_else(data_processed_complete_minimal$bmi_cat == "Normal", 1, 0)
s = exp(hh %*% as.numeric(fit_minimal$coefficients))/(1 + exp(hh %*% as.numeric(fit_minimal$coefficients)))
l = data_processed_complete_minimal$mortstat_rec - s
w = (data_processed_complete_minimal$sampweight/sum(data_processed_complete_minimal$sampweight)) * as.numeric(l)^2
a_11 = wtd.mean(s^2, data_processed_complete_minimal$sampweight) - wtd.mean(s, data_processed_complete_minimal$sampweight)^2
a_22 = t(hh[, -c(2:6)]) %*% (hh[, -c(2:6)] * w)
a_33 = wtd.mean(indicator_normal^2, data_processed_complete_minimal$sampweight) - (wtd.mean(indicator_normal, data_processed_complete_minimal$sampweight))^2
a_13 = wtd.mean(s*indicator_normal, data_processed_complete_minimal$sampweight) - (wtd.mean(indicator_normal, data_processed_complete_minimal$sampweight) * wtd.mean(s, data_processed_complete_minimal$sampweight))
a_23 = apply(hh[, -c(2:6)] * indicator_normal * (data_processed_complete_minimal$sampweight/sum(data_processed_complete_minimal$sampweight)), 2, mean)
a_12 = apply(hh[, -c(2:6)] * as.numeric(s) * as.numeric(l) * (data_processed_complete_minimal$sampweight/sum(data_processed_complete_minimal$sampweight)), 2, mean)
c = apply(hh[, -c(2:6)] * as.numeric(s) * (data_processed_complete_minimal$sampweight/sum(data_processed_complete_minimal$sampweight)), 2, mean)
A = as.matrix(rbind(c(a_11, a_12,a_13), cbind(a_12, a_22, a_23), c(a_13, a_23, a_33)))
Gamma = solve(t(hh[, -c(2:6)]) %*% (hh[, -c(2:6)] * dexpit(hh %*% as.numeric(fit_minimal$coefficients)) * (data_processed_complete_minimal$sampweight/sum(data_processed_complete_minimal$sampweight))))
c_star = t(c) %*% Gamma
M = as.matrix(rbind(c(1, c_star, 0), c(0, rep(0, length(c_star)), 1)))
g = c(1/p_hat, -wtd.mean(exp(hh %*% as.numeric(fit_minimal$coefficients))/(1 + exp(hh %*% as.numeric(fit_minimal$coefficients))), data_processed_complete_minimal$sampweight)/p_hat)
sqrt((1/nrow(data_processed_complete_minimal)) * t(g) %*% M %*% A %*% t(M) %*% g)


fit_full = svyglm(mortstat_rec ~ bmi_cat + sex + age + smoking_status + alcohol_status_new + education + marital_status + no_of_cigarettes , design=nhanes_svydesign, family = quasibinomial())
data_processed_complete = na.omit(nhanes_combined_white %>% dplyr::select("sex", "age","bmi", "MEC8YR", "mortstat_rec", "bmi_obese", "bmi_cat","education","alcohol_status_new","marital_status", "smoking_status", "no_of_cigarettes"))
indicator_normal = if_else(data_processed_complete$bmi_cat == "Normal", 1,0)
hh = model.matrix(~  bmi_cat + sex + age + smoking_status + alcohol_status_new + education + marital_status + no_of_cigarettes, data = data_processed_complete)
hh[, c(2:6)] = 0
PAR_fully_adjusted = 1 - wtd.mean(exp(hh %*% as.numeric(fit_full$coefficients))/(1 + exp(hh %*% as.numeric(fit_full$coefficients))), data_processed_complete$MEC8YR)/p_hat
#[1] 0.02262194

colnames(data_processed_complete)[4] = "sampweight"
s = exp(hh %*% as.numeric(fit_full$coefficients))/(1 + exp(hh %*% as.numeric(fit_full$coefficients)))
l = data_processed_complete$mortstat_rec - s
w = (data_processed_complete$sampweight/sum(data_processed_complete$sampweight)) * as.numeric(l)^2
a_11 = wtd.mean(s^2, data_processed_complete$sampweight) - wtd.mean(s, data_processed_complete$sampweight)^2
a_22 = t(hh[, -c(2:6)]) %*% (hh[, -c(2:6)] * w)
a_33 = wtd.mean(indicator_normal^2, data_processed_complete$sampweight) - (wtd.mean(indicator_normal, data_processed_complete$sampweight))^2
a_13 = wtd.mean(s*indicator_normal, data_processed_complete$sampweight) - (wtd.mean(indicator_normal, data_processed_complete$sampweight) * wtd.mean(s, data_processed_complete$sampweight))
a_23 = apply(hh[, -c(2:6)] * indicator_normal * (data_processed_complete$sampweight/sum(data_processed_complete$sampweight)), 2, mean)
a_12 = apply(hh[, -c(2:6)] * as.numeric(s) * as.numeric(l) * (data_processed_complete$sampweight/sum(data_processed_complete$sampweight)), 2, mean)
c = apply(hh[, -c(2:6)] * as.numeric(s) * (data_processed_complete$sampweight/sum(data_processed_complete$sampweight)), 2, mean)
A = as.matrix(rbind(c(a_11, a_12,a_13), cbind(a_12, a_22, a_23), c(a_13, a_23, a_33)))
Gamma = solve(t(hh[, -c(2:6)]) %*% (hh[, -c(2:6)] * dexpit(hh %*% as.numeric(fit_full$coefficients)) * (data_processed_complete$sampweight/sum(data_processed_complete$sampweight))))
c_star = t(c) %*% Gamma
M = as.matrix(rbind(c(1, c_star, 0), c(0, rep(0, length(c_star)), 1)))
g = c(1/p_hat, -wtd.mean(exp(hh %*% as.numeric(fit_full$coefficients))/(1 + exp(hh %*% as.numeric(fit_full$coefficients))), data_processed_complete$sampweight)/p_hat)
sqrt((1/nrow(data_processed_complete)) * t(g) %*% M %*% A %*% t(M) %*% g)
#0.004545333

#Variance of projected PAR calculation
k = 1/prop_exposure[1]
data_processed = nhanes_white_2017 %>% dplyr::select("bmi_cat", "bmi", "WTMEC2YR")
colnames(data_processed)[3] = "sampweight"
Q = -matrix(rep(-k, 25), 5, 5) + diag(-1/prop_exposure[-1])
Sigma = vcov(fit_unadjusted)[-1,-1]
total_weights = sum(data_processed$sampweight)
data_processed$bmi_cat = as.factor(data_processed$bmi_cat)

data_processed_mm = as.data.frame(model.matrix(~ bmi_cat + sampweight, data_processed))
psi = t(t(data_processed_mm[, c(2:6)])/prop_exposure[-1]) - ((1-data_processed_mm[,2]-data_processed_mm[,3]-data_processed_mm[,4]-data_processed_mm[,5]-data_processed_mm[,6])/prop_exposure[1])

Delta = (solve(Q) %*% ((t(psi) %*% (psi*data_processed_mm$sampweight))/total_weights) %*% solve(Q))/nrow(data_processed_mm)
MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_cancer.csv") 
delta = MR_vcov_file[, 3]
#delta = c(delta, log(7.5))
MR_vcov_file = MR_vcov_file[, -c(1,2,3)]
MR_vcov = MR_vcov_file[c(6, 8, 10, 12, 13,14), c(6, 8, 10, 12, 13,14)]
#MR_vcov = as.matrix(rbind(cbind(MR_vcov, 0),0))
#MR_vcov[6,6] = 1
R = matrix(0, 5, 6)
R[1, 1:2] = c(exp(delta[6]), exp(delta[8]))/2
R[2, 2:3] = c(exp(delta[8]), exp(delta[10]))/2
R[3, 3:4] = c(exp(delta[10]), exp(delta[12]))/2
R[4, 4:5] = c(exp(delta[12]), exp(delta[13]))/2
R[5, 5:6] = c(exp(delta[13]), exp(delta[14]))/2
Omega = R %*% as.matrix(MR_vcov) %*% t(R)
Omega_star = diag(1/exp(MR_estimates_UK)) %*% Omega %*% diag(1/exp(MR_estimates_UK))

Lambda = bdiag(bdiag(Sigma, Omega_star), Delta)

a = fit_unadjusted$coefficients[-1]
b = MR_estimates_UK
c = prop_exposure[-1]
f = 1 + sum(c*(exp(a-b) - 1))
h = 1 + sum(c*(exp(a) - 1)) 
g1 = (c*exp(a-b)/h) - ((c*exp(a))*(f/h^2))
g2 = -c*exp(a-b)/h
g3 = ((exp(a-b) - 1)/h) - ((exp(a) - 1)*(f/h^2))
g = c(g1,g2,g3)

PAR_proj_var = t(g) %*% Lambda %*% g
sqrt(PAR_proj_var)



#Smokers vs Non-smokers
#--Non-smokers----#
MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_nonsmokers.csv")
BMI_quantiles = MR_vcov_file$bmi.quant
delta_HR = exp(MR_vcov_file$est)
MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[8]-delta_HR[6])/2, delta_HR[8] + (delta_HR[10]-delta_HR[8])/2, delta_HR[10] + (delta_HR[12]-delta_HR[10])/2, delta_HR[12] + (delta_HR[13]-delta_HR[12])/2, delta_HR[13] + (delta_HR[14]-delta_HR[13])/2))

NHANES_2017 = merge(merge(DEMO_2017, BODY_MEASURE_2017, by = "SEQN", all = T), SMOKE_2017, by = "SEQN", all = T)
NHANES_2017 = NHANES_2017 %>% mutate(race_ethnicity = case_when(RIDRETH1 <= 2  ~ "hispanic",
                                                                RIDRETH1 == 3 ~ "white",
                                                                RIDRETH1 == 4 ~ "black",
                                                                RIDRETH1 == 6 ~ "asian"))
nhanes_white_2017 = NHANES_2017[which(NHANES_2017$race_ethnicity == "black"),]
#---18+
nhanes_white_2017 = nhanes_white_2017[which(nhanes_white_2017$RIDAGEYR >= 40 & nhanes_white_2017$RIDAGEYR < 70), ]
nhanes_white_2017$bmi = nhanes_white_2017$BMXBMI
nhanes_white_2017 = nhanes_white_2017 %>% mutate(smoking_status = case_when(SMQ020 == 1 & SMQ040 <= 2 ~ "Current",
                                                                            SMQ020 == 1 & SMQ040 == 3 ~ "Former",
                                                                            SMQ020 == 2 ~ "Never"))
nhanes_white_2017 = nhanes_white_2017 %>% mutate(bmi_cat = case_when(bmi < BMI_quantiles[3] ~ "Underweight",
                                                                     bmi >= BMI_quantiles[3] & bmi < BMI_quantiles[6] ~ "Normal",
                                                                     bmi >= BMI_quantiles[6] & bmi < BMI_quantiles[8] ~ "ObeseI",
                                                                     bmi >= BMI_quantiles[8] & bmi < BMI_quantiles[10] ~ "ObeseII",
                                                                     bmi >= BMI_quantiles[10] & bmi < BMI_quantiles[12] ~ "ObeseIII",
                                                                     bmi >= BMI_quantiles[12] & bmi < BMI_quantiles[13] ~ "ObeseIV",
                                                                     bmi >= BMI_quantiles[13] ~ "ObeseV"))


nhanes_combined_white = final_data
nhanes_combined_white = nhanes_combined_white %>% mutate(bmi_cat = case_when(bmi < BMI_quantiles[3] ~ "Underweight",
                                                                             bmi >= BMI_quantiles[3] & bmi < BMI_quantiles[6] ~ "Normal",
                                                                             bmi >= BMI_quantiles[6] & bmi < BMI_quantiles[8] ~ "ObeseI",
                                                                             bmi >= BMI_quantiles[8] & bmi < BMI_quantiles[10] ~ "ObeseII",
                                                                             bmi >= BMI_quantiles[10] & bmi < BMI_quantiles[12] ~ "ObeseIII",
                                                                             bmi >= BMI_quantiles[12] & bmi < BMI_quantiles[13] ~ "ObeseIV",
                                                                             bmi >= BMI_quantiles[13] ~ "ObeseV"))
nhanes_combined_white = nhanes_combined_white[-which(nhanes_combined_white$bmi_cat == "Underweight"), ]
nhanes_combined_white = nhanes_combined_white[which(nhanes_combined_white$smoking_status == "Never"),]
nhanes_white_2017 = nhanes_white_2017[-which(nhanes_white_2017$bmi_cat == "Underweight"), ]
nhanes_white_2017 = nhanes_white_2017[which(nhanes_white_2017$smoking_status == "Never"),]
#heart_data = heart_data %>% dplyr::select("bmi_cat", "alcohol_status", "sex", "age", "smoking_status", "MEC8YR", "mortstat_rec", "SDMVSTRA", "SDMVPSU")

nhanes_combined_white$MEC8YR = nhanes_combined_white$MEC8YR/sum(nhanes_combined_white$MEC8YR)


options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
nhanes_svydesign <- svydesign(data=nhanes_combined_white, id=~SDMVPSU, strata=~SDMVSTRA, weights=~MEC8YR, nest=TRUE)

fit_unadjusted = svyglm(mortstat ~ bmi_cat, design=nhanes_svydesign , family = quasibinomial())



p_hat = sum(nhanes_combined_white$MEC8YR[which(nhanes_combined_white$mortstat == 1)])/sum(nhanes_combined_white$MEC8YR)
p_obs_D_given_E_equalto_0 = sum(nhanes_combined_white$MEC8YR[which(nhanes_combined_white$mortstat == 1 & nhanes_combined_white$bmi_cat == "Normal")])/sum(nhanes_combined_white$MEC8YR[which(nhanes_combined_white$bmi_cat == "Normal")])
p_obs_D_given_E_equalto_0 = locfit::expit(fit_unadjusted$coefficients[1])
#prop_exposure = wpct(nhanes_combined_white$bmi_cat, weight = nhanes_combined_white$MEC8YR)
#PAR_MR = 1 - (prop_exposure[1] + sum(prop_exposure[-1] * exp(fit_unadjusted$coefficients[2:6] - MR_estimates_UK))) * (p_obs_D_given_E_equalto_0/p_hat)
prop_exposure = wpct(nhanes_white_2017$bmi_cat, weight = nhanes_white_2017$WTMEC2YR)
PAR_MR = 1 - ((prop_exposure[1] + sum(prop_exposure[-1] * exp(fit_unadjusted$coefficients[2:6] - MR_estimates_UK)))/((prop_exposure[1] + sum(prop_exposure[-1] * exp(fit_unadjusted$coefficients[2:6]))))) 

#Normal 
#0.1778511 
PAR_observed = 1 - (p_obs_D_given_E_equalto_0/p_hat)
fit_minimal = svyglm(mortstat ~ bmi_cat + sex + age, design=nhanes_svydesign, family = quasibinomial())


data_processed_complete_minimal = na.omit(nhanes_combined_white %>% dplyr::select("sex", "age","bmi", "MEC8YR", "mortstat", "bmi_obese", "bmi_cat"))
indicator_normal = if_else(data_processed_complete_minimal$bmi_cat == "Normal", 1,0)
hh = model.matrix(~ bmi_cat + sex + age, data = data_processed_complete_minimal)
hh[, c(2:6)] = 0
PAR_minimally_adjusted = 1 - wtd.mean(exp(hh %*% as.numeric(fit_minimal$coefficients))/(1 + exp(hh %*% as.numeric(fit_minimal$coefficients))), data_processed_complete_minimal$MEC8YR)/p_hat
#[1]0.0249229
colnames(data_processed_complete_minimal)[4]="sampweight"
indicator_normal = if_else(data_processed_complete_minimal$bmi_cat == "Normal", 1, 0)
s = exp(hh %*% as.numeric(fit_minimal$coefficients))/(1 + exp(hh %*% as.numeric(fit_minimal$coefficients)))
l = data_processed_complete_minimal$mortstat - s
w = (data_processed_complete_minimal$sampweight/sum(data_processed_complete_minimal$sampweight)) * as.numeric(l)^2
a_11 = wtd.mean(s^2, data_processed_complete_minimal$sampweight) - wtd.mean(s, data_processed_complete_minimal$sampweight)^2
a_22 = t(hh[, -c(2:6)]) %*% (hh[, -c(2:6)] * w)
a_33 = wtd.mean(indicator_normal^2, data_processed_complete_minimal$sampweight) - (wtd.mean(indicator_normal, data_processed_complete_minimal$sampweight))^2
a_13 = wtd.mean(s*indicator_normal, data_processed_complete_minimal$sampweight) - (wtd.mean(indicator_normal, data_processed_complete_minimal$sampweight) * wtd.mean(s, data_processed_complete_minimal$sampweight))
a_23 = apply(hh[, -c(2:6)] * indicator_normal * (data_processed_complete_minimal$sampweight/sum(data_processed_complete_minimal$sampweight)), 2, mean)
a_12 = apply(hh[, -c(2:6)] * as.numeric(s) * as.numeric(l) * (data_processed_complete_minimal$sampweight/sum(data_processed_complete_minimal$sampweight)), 2, mean)
c = apply(hh[, -c(2:6)] * as.numeric(s) * (data_processed_complete_minimal$sampweight/sum(data_processed_complete_minimal$sampweight)), 2, mean)
A = as.matrix(rbind(c(a_11, a_12,a_13), cbind(a_12, a_22, a_23), c(a_13, a_23, a_33)))
Gamma = solve(t(hh[, -c(2:6)]) %*% (hh[, -c(2:6)] * dexpit(hh %*% as.numeric(fit_minimal$coefficients)) * (data_processed_complete_minimal$sampweight/sum(data_processed_complete_minimal$sampweight))))
c_star = t(c) %*% Gamma
M = as.matrix(rbind(c(1, c_star, 0), c(0, rep(0, length(c_star)), 1)))
g = c(1/p_hat, -wtd.mean(exp(hh %*% as.numeric(fit_minimal$coefficients))/(1 + exp(hh %*% as.numeric(fit_minimal$coefficients))), data_processed_complete_minimal$sampweight)/p_hat)
sqrt((1/nrow(data_processed_complete_minimal)) * t(g) %*% M %*% A %*% t(M) %*% g)


fit_full = svyglm(mortstat ~ bmi_cat + sex + age + alcohol_status_new + education + marital_status , design=nhanes_svydesign, family = quasibinomial())
data_processed_complete = na.omit(nhanes_combined_white %>% dplyr::select("sex", "age","bmi", "MEC8YR", "mortstat", "bmi_obese", "bmi_cat","education","alcohol_status_new","marital_status"))
indicator_normal = if_else(data_processed_complete$bmi_cat == "Normal", 1,0)
hh = model.matrix(~  bmi_cat + sex + age + alcohol_status_new + education + marital_status, data = data_processed_complete)
hh[, c(2:6)] = 0
PAR_fully_adjusted = 1 - wtd.mean(exp(hh %*% as.numeric(fit_full$coefficients))/(1 + exp(hh %*% as.numeric(fit_full$coefficients))), data_processed_complete$MEC8YR)/p_hat
#[1] 0.02262194

colnames(data_processed_complete)[4] = "sampweight"
s = exp(hh %*% as.numeric(fit_full$coefficients))/(1 + exp(hh %*% as.numeric(fit_full$coefficients)))
l = data_processed_complete$mortstat - s
w = (data_processed_complete$sampweight/sum(data_processed_complete$sampweight)) * as.numeric(l)^2
a_11 = wtd.mean(s^2, data_processed_complete$sampweight) - wtd.mean(s, data_processed_complete$sampweight)^2
a_22 = t(hh[, -c(2:6)]) %*% (hh[, -c(2:6)] * w)
a_33 = wtd.mean(indicator_normal^2, data_processed_complete$sampweight) - (wtd.mean(indicator_normal, data_processed_complete$sampweight))^2
a_13 = wtd.mean(s*indicator_normal, data_processed_complete$sampweight) - (wtd.mean(indicator_normal, data_processed_complete$sampweight) * wtd.mean(s, data_processed_complete$sampweight))
a_23 = apply(hh[, -c(2:6)] * indicator_normal * (data_processed_complete$sampweight/sum(data_processed_complete$sampweight)), 2, mean)
a_12 = apply(hh[, -c(2:6)] * as.numeric(s) * as.numeric(l) * (data_processed_complete$sampweight/sum(data_processed_complete$sampweight)), 2, mean)
c = apply(hh[, -c(2:6)] * as.numeric(s) * (data_processed_complete$sampweight/sum(data_processed_complete$sampweight)), 2, mean)
A = as.matrix(rbind(c(a_11, a_12,a_13), cbind(a_12, a_22, a_23), c(a_13, a_23, a_33)))
Gamma = solve(t(hh[, -c(2:6)]) %*% (hh[, -c(2:6)] * dexpit(hh %*% as.numeric(fit_full$coefficients)) * (data_processed_complete$sampweight/sum(data_processed_complete$sampweight))))
c_star = t(c) %*% Gamma
M = as.matrix(rbind(c(1, c_star, 0), c(0, rep(0, length(c_star)), 1)))
g = c(1/p_hat, -wtd.mean(exp(hh %*% as.numeric(fit_full$coefficients))/(1 + exp(hh %*% as.numeric(fit_full$coefficients))), data_processed_complete$sampweight)/p_hat)
sqrt((1/nrow(data_processed_complete)) * t(g) %*% M %*% A %*% t(M) %*% g)
#0.004545333

#Variance of projected PAR calculation
k = 1/prop_exposure[1]
data_processed = nhanes_white_2017 %>% dplyr::select("bmi_cat", "bmi", "WTMEC2YR")
colnames(data_processed)[3] = "sampweight"
Q = -matrix(rep(-k, 25), 5, 5) + diag(-1/prop_exposure[-1])
Sigma = vcov(fit_unadjusted)[-1,-1]
total_weights = sum(data_processed$sampweight)
data_processed$bmi_cat = as.factor(data_processed$bmi_cat)

data_processed_mm = as.data.frame(model.matrix(~ bmi_cat + sampweight, data_processed))
psi = t(t(data_processed_mm[, c(2:6)])/prop_exposure[-1]) - ((1-data_processed_mm[,2]-data_processed_mm[,3]-data_processed_mm[,4]-data_processed_mm[,5]-data_processed_mm[,6])/prop_exposure[1])

Delta = (solve(Q) %*% ((t(psi) %*% (psi*data_processed_mm$sampweight))/total_weights) %*% solve(Q))/nrow(data_processed_mm)
MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_nonsmokers.csv") 
delta = MR_vcov_file[, 3]
#delta = c(delta, log(7.5))
MR_vcov_file = MR_vcov_file[, -c(1,2,3)]
MR_vcov = MR_vcov_file[c(6, 8, 10, 12, 13,14), c(6, 8, 10, 12, 13,14)]
#MR_vcov = as.matrix(rbind(cbind(MR_vcov, 0),0))
#MR_vcov[6,6] = 1
R = matrix(0, 5, 6)
R[1, 1:2] = c(exp(delta[6]), exp(delta[8]))/2
R[2, 2:3] = c(exp(delta[8]), exp(delta[10]))/2
R[3, 3:4] = c(exp(delta[10]), exp(delta[12]))/2
R[4, 4:5] = c(exp(delta[12]), exp(delta[13]))/2
R[5, 5:6] = c(exp(delta[13]), exp(delta[14]))/2
Omega = R %*% as.matrix(MR_vcov) %*% t(R)
Omega_star = diag(1/exp(MR_estimates_UK)) %*% Omega %*% diag(1/exp(MR_estimates_UK))

Lambda = bdiag(bdiag(Sigma, Omega_star), Delta)

a = fit_unadjusted$coefficients[-1]
b = MR_estimates_UK
c = prop_exposure[-1]
f = 1 + sum(c*(exp(a-b) - 1))
h = 1 + sum(c*(exp(a) - 1)) 
g1 = (c*exp(a-b)/h) - ((c*exp(a))*(f/h^2))
g2 = -c*exp(a-b)/h
g3 = ((exp(a-b) - 1)/h) - ((exp(a) - 1)*(f/h^2))
g = c(g1,g2,g3)

PAR_proj_var = t(g) %*% Lambda %*% g
sqrt(PAR_proj_var)
L.CI = PAR_MR - 1.96*sqrt(PAR_proj_var)
R.CI = PAR_MR + 1.96*sqrt(PAR_proj_var)

#-Smokers-#
MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_smokers.csv")
BMI_quantiles = MR_vcov_file$bmi.quant
delta_HR = exp(MR_vcov_file$est)
MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[8]-delta_HR[6])/2, delta_HR[8] + (delta_HR[10]-delta_HR[8])/2, delta_HR[10] + (delta_HR[12]-delta_HR[10])/2, delta_HR[12] + (delta_HR[13]-delta_HR[12])/2, delta_HR[13] + (delta_HR[14]-delta_HR[13])/2))

NHANES_2017 = merge(merge(DEMO_2017, BODY_MEASURE_2017, by = "SEQN", all = T), SMOKE_2017, by = "SEQN", all = T)
NHANES_2017 = NHANES_2017 %>% mutate(race_ethnicity = case_when(RIDRETH1 <= 2  ~ "hispanic",
                                                                RIDRETH1 == 3 ~ "white",
                                                                RIDRETH1 == 4 ~ "black",
                                                                RIDRETH1 == 6 ~ "asian"))
nhanes_white_2017 = NHANES_2017[which(NHANES_2017$race_ethnicity == "black"),]
#---18+
nhanes_white_2017 = nhanes_white_2017[which(nhanes_white_2017$RIDAGEYR >= 40 & nhanes_white_2017$RIDAGEYR < 70), ]
nhanes_white_2017$bmi = nhanes_white_2017$BMXBMI
nhanes_white_2017 = nhanes_white_2017 %>% mutate(smoking_status = case_when(SMQ020 == 1 & SMQ040 <= 2 ~ "Current",
                                                                            SMQ020 == 1 & SMQ040 == 3 ~ "Former",
                                                                            SMQ020 == 2 ~ "Never"))
nhanes_white_2017 = nhanes_white_2017 %>% mutate(bmi_cat = case_when(bmi < BMI_quantiles[3] ~ "Underweight",
                                                                     bmi >= BMI_quantiles[3] & bmi < BMI_quantiles[6] ~ "Normal",
                                                                     bmi >= BMI_quantiles[6] & bmi < BMI_quantiles[8] ~ "ObeseI",
                                                                     bmi >= BMI_quantiles[8] & bmi < BMI_quantiles[10] ~ "ObeseII",
                                                                     bmi >= BMI_quantiles[10] & bmi < BMI_quantiles[12] ~ "ObeseIII",
                                                                     bmi >= BMI_quantiles[12] & bmi < BMI_quantiles[13] ~ "ObeseIV",
                                                                     bmi >= BMI_quantiles[13] ~ "ObeseV"))

nhanes_combined_white = final_data
nhanes_combined_white = nhanes_combined_white %>% mutate(bmi_cat = case_when(bmi < BMI_quantiles[3] ~ "Underweight",
                                                                             bmi >= BMI_quantiles[3] & bmi < BMI_quantiles[6] ~ "Normal",
                                                                             bmi >= BMI_quantiles[6] & bmi < BMI_quantiles[8] ~ "ObeseI",
                                                                             bmi >= BMI_quantiles[8] & bmi < BMI_quantiles[10] ~ "ObeseII",
                                                                             bmi >= BMI_quantiles[10] & bmi < BMI_quantiles[12] ~ "ObeseIII",
                                                                             bmi >= BMI_quantiles[12] & bmi < BMI_quantiles[13] ~ "ObeseIV",
                                                                             bmi >= BMI_quantiles[13] ~ "ObeseV"))
nhanes_combined_white = nhanes_combined_white[-which(nhanes_combined_white$bmi_cat == "Underweight"), ]
nhanes_combined_white = nhanes_combined_white[which(nhanes_combined_white$smoking_status == "Former" | nhanes_combined_white$smoking_status == "Current" ),]
nhanes_white_2017 = nhanes_white_2017[-which(nhanes_white_2017$bmi_cat == "Underweight"), ]
nhanes_white_2017 = nhanes_white_2017[which(nhanes_white_2017$smoking_status == "Former" | nhanes_white_2017$smoking_status == "Current" ),]

#heart_data = heart_data %>% dplyr::select("bmi_cat", "alcohol_status", "sex", "age", "smoking_status", "MEC8YR", "mortstat_rec", "SDMVSTRA", "SDMVPSU")

nhanes_combined_white$no_of_cigarettes = nhanes_combined_white$no_of_cigarettes + 1
#nhanes_combined_white$MEC8YR = nhanes_combined_white$MEC8YR/sum(nhanes_combined_white$MEC8YR)

nhanes_combined_white$no_of_cigarettes = as.numeric(nhanes_combined_white$no_of_cigarettes)
#nhanes_combined_white$age = scale(nhanes_combined_white$age)
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
nhanes_svydesign <- svydesign(data=nhanes_combined_white, id=~SDMVPSU, strata=~SDMVSTRA, weights=~MEC8YR, nest=TRUE)

fit_unadjusted = svyglm(mortstat ~ bmi_cat, design=nhanes_svydesign , family = quasibinomial())



p_hat = sum(nhanes_combined_white$MEC8YR[which(nhanes_combined_white$mortstat == 1)])/sum(nhanes_combined_white$MEC8YR)
p_obs_D_given_E_equalto_0 = sum(nhanes_combined_white$MEC8YR[which(nhanes_combined_white$mortstat == 1 & nhanes_combined_white$bmi_cat == "Normal")])/sum(nhanes_combined_white$MEC8YR[which(nhanes_combined_white$bmi_cat == "Normal")])
p_obs_D_given_E_equalto_0 = locfit::expit(fit_unadjusted$coefficients[1])

#prop_exposure = wpct(nhanes_combined_white$bmi_cat, weight = nhanes_combined_white$MEC8YR)
#PAR_MR = 1 - (prop_exposure[1] + sum(prop_exposure[-1] * exp(fit_unadjusted$coefficients[2:6] - MR_estimates_UK))) * (p_obs_D_given_E_equalto_0/p_hat)
prop_exposure = wpct(nhanes_white_2017$bmi_cat, weight = nhanes_white_2017$WTMEC2YR)
PAR_MR = 1 - ((prop_exposure[1] + sum(prop_exposure[-1] * exp(fit_unadjusted$coefficients[2:6] - MR_estimates_UK)))/((prop_exposure[1] + sum(prop_exposure[-1] * exp(fit_unadjusted$coefficients[2:6]))))) 

#Normal 
#0.1778511 
PAR_observed = 1 - (p_obs_D_given_E_equalto_0/p_hat)
fit_minimal = svyglm(mortstat ~ bmi_cat + sex + age, design=nhanes_svydesign, family = quasibinomial())


data_processed_complete_minimal = na.omit(nhanes_combined_white %>% dplyr::select("sex", "age","bmi", "MEC8YR", "mortstat", "bmi_obese", "bmi_cat"))
indicator_normal = if_else(data_processed_complete_minimal$bmi_cat == "Normal", 1,0)
hh = model.matrix(~ bmi_cat + sex + age, data = data_processed_complete_minimal)
hh[, c(2:6)] = 0
PAR_minimally_adjusted = 1 - wtd.mean(exp(hh %*% as.numeric(fit_minimal$coefficients))/(1 + exp(hh %*% as.numeric(fit_minimal$coefficients))), data_processed_complete_minimal$MEC8YR)/p_hat
#[1]0.0249229
colnames(data_processed_complete_minimal)[4]="sampweight"
indicator_normal = if_else(data_processed_complete_minimal$bmi_cat == "Normal", 1, 0)
s = exp(hh %*% as.numeric(fit_minimal$coefficients))/(1 + exp(hh %*% as.numeric(fit_minimal$coefficients)))
l = data_processed_complete_minimal$mortstat - s
w = (data_processed_complete_minimal$sampweight/sum(data_processed_complete_minimal$sampweight)) * as.numeric(l)^2
a_11 = wtd.mean(s^2, data_processed_complete_minimal$sampweight) - wtd.mean(s, data_processed_complete_minimal$sampweight)^2
a_22 = t(hh[, -c(2:6)]) %*% (hh[, -c(2:6)] * w)
a_33 = wtd.mean(indicator_normal^2, data_processed_complete_minimal$sampweight) - (wtd.mean(indicator_normal, data_processed_complete_minimal$sampweight))^2
a_13 = wtd.mean(s*indicator_normal, data_processed_complete_minimal$sampweight) - (wtd.mean(indicator_normal, data_processed_complete_minimal$sampweight) * wtd.mean(s, data_processed_complete_minimal$sampweight))
a_23 = apply(hh[, -c(2:6)] * indicator_normal * (data_processed_complete_minimal$sampweight/sum(data_processed_complete_minimal$sampweight)), 2, mean)
a_12 = apply(hh[, -c(2:6)] * as.numeric(s) * as.numeric(l) * (data_processed_complete_minimal$sampweight/sum(data_processed_complete_minimal$sampweight)), 2, mean)
c = apply(hh[, -c(2:6)] * as.numeric(s) * (data_processed_complete_minimal$sampweight/sum(data_processed_complete_minimal$sampweight)), 2, mean)
A = as.matrix(rbind(c(a_11, a_12,a_13), cbind(a_12, a_22, a_23), c(a_13, a_23, a_33)))
Gamma = solve(t(hh[, -c(2:6)]) %*% (hh[, -c(2:6)] * dexpit(hh %*% as.numeric(fit_minimal$coefficients)) * (data_processed_complete_minimal$sampweight/sum(data_processed_complete_minimal$sampweight))))
c_star = t(c) %*% Gamma
M = as.matrix(rbind(c(1, c_star, 0), c(0, rep(0, length(c_star)), 1)))
g = c(1/p_hat, -wtd.mean(exp(hh %*% as.numeric(fit_minimal$coefficients))/(1 + exp(hh %*% as.numeric(fit_minimal$coefficients))), data_processed_complete_minimal$sampweight)/p_hat)
sqrt((1/nrow(data_processed_complete_minimal)) * t(g) %*% M %*% A %*% t(M) %*% g)


fit_full = svyglm(mortstat ~ bmi_cat + sex + age + alcohol_status_new + education + marital_status + smoking_status + no_of_cigarettes , design=nhanes_svydesign, family = quasibinomial())
data_processed_complete = na.omit(nhanes_combined_white %>% dplyr::select("sex", "age","bmi", "MEC8YR", "mortstat", "bmi_obese", "bmi_cat","education","alcohol_status_new","marital_status", "no_of_cigarettes", "smoking_status"))
indicator_normal = if_else(data_processed_complete$bmi_cat == "Normal", 1,0)
hh = model.matrix(~  bmi_cat + sex + age + alcohol_status_new + education + marital_status + smoking_status + no_of_cigarettes, data = data_processed_complete)
hh[, c(2:6)] = 0
PAR_fully_adjusted = 1 - wtd.mean(exp(hh %*% as.numeric(fit_full$coefficients))/(1 + exp(hh %*% as.numeric(fit_full$coefficients))), data_processed_complete$MEC8YR)/p_hat
#[1] 0.02262194

colnames(data_processed_complete)[4] = "sampweight"
s = exp(hh %*% as.numeric(fit_full$coefficients))/(1 + exp(hh %*% as.numeric(fit_full$coefficients)))
l = data_processed_complete$mortstat - s
w = (data_processed_complete$sampweight/sum(data_processed_complete$sampweight)) * as.numeric(l)^2
a_11 = wtd.mean(s^2, data_processed_complete$sampweight) - wtd.mean(s, data_processed_complete$sampweight)^2
a_22 = t(hh[, -c(2:6)]) %*% (hh[, -c(2:6)] * w)
a_33 = wtd.mean(indicator_normal^2, data_processed_complete$sampweight) - (wtd.mean(indicator_normal, data_processed_complete$sampweight))^2
a_13 = wtd.mean(s*indicator_normal, data_processed_complete$sampweight) - (wtd.mean(indicator_normal, data_processed_complete$sampweight) * wtd.mean(s, data_processed_complete$sampweight))
a_23 = apply(hh[, -c(2:6)] * indicator_normal * (data_processed_complete$sampweight/sum(data_processed_complete$sampweight)), 2, mean)
a_12 = apply(hh[, -c(2:6)] * as.numeric(s) * as.numeric(l) * (data_processed_complete$sampweight/sum(data_processed_complete$sampweight)), 2, mean)
c = apply(hh[, -c(2:6)] * as.numeric(s) * (data_processed_complete$sampweight/sum(data_processed_complete$sampweight)), 2, mean)
A = as.matrix(rbind(c(a_11, a_12,a_13), cbind(a_12, a_22, a_23), c(a_13, a_23, a_33)))
Gamma = solve(t(hh[, -c(2:6)]) %*% (hh[, -c(2:6)] * dexpit(hh %*% as.numeric(fit_full$coefficients)) * (data_processed_complete$sampweight/sum(data_processed_complete$sampweight))))
c_star = t(c) %*% Gamma
M = as.matrix(rbind(c(1, c_star, 0), c(0, rep(0, length(c_star)), 1)))
g = c(1/p_hat, -wtd.mean(exp(hh %*% as.numeric(fit_full$coefficients))/(1 + exp(hh %*% as.numeric(fit_full$coefficients))), data_processed_complete$sampweight)/p_hat)
sqrt((1/nrow(data_processed_complete)) * t(g) %*% M %*% A %*% t(M) %*% g)
#0.004545333

#Variance of projected PAR calculation
k = 1/prop_exposure[1]
data_processed = nhanes_white_2017 %>% dplyr::select("bmi_cat", "bmi", "WTMEC2YR")
colnames(data_processed)[3] = "sampweight"
Q = -matrix(rep(-k, 25), 5, 5) + diag(-1/prop_exposure[-1])
Sigma = vcov(fit_unadjusted)[-1,-1]
total_weights = sum(data_processed$sampweight)
data_processed$bmi_cat = as.factor(data_processed$bmi_cat)

data_processed_mm = as.data.frame(model.matrix(~ bmi_cat + sampweight, data_processed))
psi = t(t(data_processed_mm[, c(2:6)])/prop_exposure[-1]) - ((1-data_processed_mm[,2]-data_processed_mm[,3]-data_processed_mm[,4]-data_processed_mm[,5]-data_processed_mm[,6])/prop_exposure[1])

Delta = (solve(Q) %*% ((t(psi) %*% (psi*data_processed_mm$sampweight))/total_weights) %*% solve(Q))/nrow(data_processed_mm)
MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_smokers.csv") 
delta = MR_vcov_file[, 3]
#delta = c(delta, log(7.5))
MR_vcov_file = MR_vcov_file[, -c(1,2,3)]
MR_vcov = MR_vcov_file[c(6, 8, 10, 12, 13,14), c(6, 8, 10, 12, 13,14)]
#MR_vcov = as.matrix(rbind(cbind(MR_vcov, 0),0))
#MR_vcov[6,6] = 1
R = matrix(0, 5, 6)
R[1, 1:2] = c(exp(delta[6]), exp(delta[8]))/2
R[2, 2:3] = c(exp(delta[8]), exp(delta[10]))/2
R[3, 3:4] = c(exp(delta[10]), exp(delta[12]))/2
R[4, 4:5] = c(exp(delta[12]), exp(delta[13]))/2
R[5, 5:6] = c(exp(delta[13]), exp(delta[14]))/2
Omega = R %*% as.matrix(MR_vcov) %*% t(R)
Omega_star = diag(1/exp(MR_estimates_UK)) %*% Omega %*% diag(1/exp(MR_estimates_UK))

Lambda = bdiag(bdiag(Sigma, Omega_star), Delta)

a = fit_unadjusted$coefficients[-1]
b = MR_estimates_UK
c = prop_exposure[-1]
f = 1 + sum(c*(exp(a-b) - 1))
h = 1 + sum(c*(exp(a) - 1)) 
g1 = (c*exp(a-b)/h) - ((c*exp(a))*(f/h^2))
g2 = -c*exp(a-b)/h
g3 = ((exp(a-b) - 1)/h) - ((exp(a) - 1)*(f/h^2))
g = c(g1,g2,g3)

PAR_proj_var = t(g) %*% Lambda %*% g
sqrt(PAR_proj_var)
L.CI = PAR_MR - 1.96*sqrt(PAR_proj_var)
R.CI = PAR_MR + 1.96*sqrt(PAR_proj_var)
