library(survey)
library(stringr)
library(weights)
library(dplyr)
library(readr)
library(pollster)
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

#nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(bmi_cat = case_when(BMI < 18.5 ~ "Underweight",
#                                                                          BMI >= 18.5 & BMI < 25 ~ "Normal",
#                                                                         BMI >= 25 & BMI < 30 ~ "ObeseI",
#                                                                        BMI >= 30 & BMI < 35 ~ "ObeseII",
#                                                                       BMI >= 35 ~"ObeseIII"))

#---Removing underweight people--#
nhanes_combined_white = nhanes_combined_white[-which(nhanes_combined_white$bmi_cat == "Underweight"), ]






nhanes_svydesign <- svydesign(data=nhanes_combined_white, id=~SDMVPSU, strata=~SDMVSTRA, weights=~MEC8YR, nest=TRUE)
fit_unadjusted = svyglm(mortstat ~ bmi_cat, design=nhanes_svydesign , family = quasibinomial())




p_hat = sum(nhanes_combined_white$MEC8YR[which(nhanes_combined_white$mortstat == 1)])/sum(nhanes_combined_white$MEC8YR)
p_obs_D_given_E_equalto_0 = sum(nhanes_combined_white$MEC8YR[which(nhanes_combined_white$mortstat == 1 & nhanes_combined_white$bmi_cat == "Normal")])/sum(nhanes_combined_white$MEC8YR[which(nhanes_combined_white$bmi_cat == "Normal")])

prop_exposure = wpct(nhanes_combined_white$bmi_cat, weight = nhanes_combined_white$MEC8YR)
PAR_MR = 1 - (prop_exposure[1] + sum(prop_exposure[-1] * exp(fit_unadjusted$coefficients[2:6] - MR_estimates_UK))) * (p_obs_D_given_E_equalto_0/p_hat)
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

fit_full = svyglm(mortstat ~ bmi_cat + sex + age + smoking_status + alcohol_status_new + education + marital_status + no_of_cigarettes , design=nhanes_svydesign, family = quasibinomial())
data_processed_complete = na.omit(nhanes_combined_white %>% dplyr::select("sex", "age","bmi", "MEC8YR", "mortstat", "bmi_obese", "bmi_cat","education","alcohol_status_new","marital_status", "smoking_status", "no_of_cigarettes"))
indicator_normal = if_else(data_processed_complete$bmi_cat == "Normal", 1,0)
hh = model.matrix(~ bmi_cat + sex + age + smoking_status + alcohol_status_new + education + marital_status + no_of_cigarettes, data = data_processed_complete)
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

#---- Overweight, Obese and Normal

MR_estimates_obese_overweight_normal = log(c(delta_HR[6] + (delta_HR[10]-delta_HR[6])/2, delta_HR[10] + (delta_HR[14]-delta_HR[10])/2))

nhanes_combined_white = nhanes_combined_white %>% mutate(bmi_cat_new = case_when(bmi_cat == "Normal" ~ "Normal",
                                                                                 bmi_cat == "ObeseI" | bmi_cat == "ObeseII" ~ "Moreweight",
                                                                                 bmi_cat == "ObeseIII" | bmi_cat == "ObeseIV" | bmi_cat == "ObeseV" ~ "Obese"))
nhanes_combined_white$bmi_cat_new = factor(nhanes_combined_white$bmi_cat_new, levels = c("Normal", "Moreweight", "Obese"))
nhanes_combined_white_obeseoverweight_and_normal_for_model_fit = nhanes_combined_white 
#nhanes_combined_white_obese_and_normal_for_model_fit$bmi_cat_new = factor(nhanes_combined_white_obese_and_normal_for_model_fit$bmi_cat_new, levels = c("Normal", "Overweight"))
nhanes_combined_white_obeseoverweight_and_normal_for_model_fit$smoking_status = as.factor(nhanes_combined_white_obeseoverweight_and_normal_for_model_fit$smoking_status)
nhanes_combined_white_obeseoverweight_and_normal_for_model_fit$smoking_status = relevel(nhanes_combined_white_obeseoverweight_and_normal_for_model_fit$smoking_status, ref = "Never")
nhanes_combined_white_obeseoverweight_and_normal_for_model_fit$alcohol_status_new = as.factor(nhanes_combined_white_obeseoverweight_and_normal_for_model_fit$alcohol_status_new)
nhanes_combined_white_obeseoverweight_and_normal_for_model_fit$alcohol_status_new = relevel(nhanes_combined_white_obeseoverweight_and_normal_for_model_fit$alcohol_status_new, ref = "Nondrinker")
nhanes_combined_white_obeseoverweight_and_normal_for_model_fit$education = as.factor(nhanes_combined_white_obeseoverweight_and_normal_for_model_fit$education)
nhanes_combined_white_obeseoverweight_and_normal_for_model_fit$marital_status = as.factor(nhanes_combined_white_obeseoverweight_and_normal_for_model_fit$marital_status)
nhanes_combined_white_obeseoverweight_and_normal_for_model_fit$bmi_cat = as.factor(nhanes_combined_white_obeseoverweight_and_normal_for_model_fit$bmi_cat)

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
nhanes_svydesign <- svydesign(data=nhanes_combined_white_obeseoverweight_and_normal_for_model_fit, id=~SDMVPSU, strata=~SDMVSTRA, weights=~MEC8YR, nest=TRUE)

fit_full = svyglm(mortstat ~ bmi_cat_new + sex + age + education + smoking_status + no_of_cigarettes + alcohol_status_new + marital_status, design=nhanes_svydesign, family = quasibinomial())
non_bmi_coeff = fit_full$coefficients[-c(1:3)]
data_processed_complete = na.omit(nhanes_combined_white_obeseoverweight_and_normal_for_model_fit %>% dplyr::select("sex", "age","bmi", "MEC8YR", "mortstat", "bmi_obese", "bmi_cat_new","education","alcohol_status_new","marital_status", "smoking_status", "no_of_cigarettes", "SDMVPSU", "SDMVSTRA"))
#probability_outcome = wtd.mean(fit_full$fitted.values, weights = data_processed_complete$MEC8YR )
data_processed_complete$bmi_obese = if_else(data_processed_complete$bmi >= 30, 1, 0)
data_processed_complete$smoking_status = as.factor(data_processed_complete$smoking_status)
data_processed_complete$smoking_status = relevel(data_processed_complete$smoking_status, ref = "Never")
data_processed_complete$alcohol_status_new = as.factor(data_processed_complete$alcohol_status_new)
data_processed_complete$alcohol_status_new = relevel(data_processed_complete$alcohol_status_new, ref = "Nondrinker")
data_processed_complete$education = as.factor(data_processed_complete$education)
data_processed_complete$marital_status = as.factor(data_processed_complete$marital_status)
data_processed_complete$bmi_cat = as.factor(data_processed_complete$bmi_cat)

AR_fully_adjusted_observed_model_with_e0 = matrix(NA, 2, 5)



#data_processed_complete$MEC8YR = data_processed_complete$MEC8YR/sum(data_processed_complete$MEC8YR)
probability_outcome = wpct(data_processed_complete$mortstat, weight = data_processed_complete$MEC8YR)[2]
#probability_outcome = mean(data_processed_complete$mortstat)

data_processed_complete$bmi_cat_new = relevel(data_processed_complete$bmi_cat_new, ref = "Normal")

hh = model.matrix(~ bmi_cat_new + sex + age + education + smoking_status + no_of_cigarettes + alcohol_status_new + marital_status + bmi_obese, data = data_processed_complete)
  #hh_obese = model.matrix(~ bmi_cat + sex + age + education + smoking_status + no_of_cigarettes + alcohol_status + marital_status, data = data_processed_complete)
hh = cbind(hh, data_processed_complete$MEC8YR)
  #hh_obese = cbind(hh, data_processed_complete$sampweight)
colnames(hh)[18] = "sampweight"

all.equal(colnames(hh[, -c(1:3,17:18)]), names(non_bmi_coeff))
hh_risk_score = as.data.frame(hh) %>% mutate(risk_score = as.numeric(hh[, -c(1:3,17:18)] %*% non_bmi_coeff))
hh_risk_score = hh_risk_score %>% mutate(quintile = ntile(risk_score, 5))
data_processed_complete = data_processed_complete %>% mutate(risk_score = hh_risk_score$risk_score) %>% mutate(quintile = ntile(risk_score, 5))
colnames(data_processed_complete)[4] = "sampweight"
data_processed_complete$sampweight = data_processed_complete$sampweight/sum(data_processed_complete$sampweight)

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
nhanes_svydesign <- svydesign(data=data_processed_complete, id=~SDMVPSU, strata=~SDMVSTRA, weights=~sampweight, nest=TRUE)

fit_quintile = svyglm(mortstat ~ bmi_cat_new + as.factor(quintile), design=nhanes_svydesign, family = quasibinomial())
fit_quintile$coefficients = c(fit_quintile$coefficients[1:3], 0, fit_quintile$coefficients[4:7])

hh_risk_score$sampweight = hh_risk_score$sampweight/sum(hh_risk_score$sampweight)
table_decile_bmicat = prop.table(table(data_processed_complete$bmi_cat_new, data_processed_complete$quintile))
wt_table_decile_bmicat = data.frame(crosstab(df = data_processed_complete, x = bmi_cat_new, y = quintile, weight = sampweight, pct_type = "cell"))[c(1:3), c(2:6)]/100
PAR_C_bmi_quintile = matrix(NA,2,5)
PAR_C_bmi_quintile_aliter = matrix(NA,2,5)
AR_C_fully_adjusted = matrix(NA, 2, 5)
AR_C_fully_adjusted_quintile = matrix(NA, 2, 5)
AR_fully_adjusted_observed_model = matrix(NA,2,5)
AR_fully_adjusted_observed_model_quintile = matrix(NA,2,5)
AR_fully_adjusted_observed_model_with_e0_quintile = matrix(NA,2,5)
prop_exposure_deciles = matrix(NA, 5, 2)

for(j in 1:2)
{
  for(i in 1:5)
  {
    temp = hh_risk_score[which(hh_risk_score$quintile == i), ]
    temp_1 = data_processed_complete[which(data_processed_complete$quintile == i), ]
    
    wpct(temp_1$mortstat, weight = temp_1$sampweight)[2]
    #prop_exposure_deciles[i,] = wpct(temp_1$bmi_cat_new, weight = temp_1$sampweight)
    
    
    #temp_2 = temp[, -c(16:19)]
    #temp_2[, c(2)] = 0
    
    AR_fully_adjusted_observed_model_with_e0[j,i] = wtd.mean(locfit::expit(fit_full$coefficients[1] + temp$risk_score), temp$sampweight)
    AR_fully_adjusted_observed_model_with_e0_quintile[j,i] = locfit::expit(fit_quintile$coefficients[1] + fit_quintile$coefficients[i+3])
    
    AR_fully_adjusted_observed_model[j,i] = wtd.mean(locfit::expit(fit_full$coefficients[1] + fit_full$coefficients[(j+1)] + temp$risk_score), temp$sampweight)
    AR_fully_adjusted_observed_model_quintile[j,i] = locfit::expit(fit_quintile$coefficients[1] + fit_quintile$coefficients[(j+1)] + fit_quintile$coefficients[i+3])
    
    AR_C_fully_adjusted[j,i] = AR_fully_adjusted_observed_model_with_e0[j,i] * exp(fit_full$coefficients[(j+1)] - MR_estimates_obese_overweight_normal[j])
    AR_C_fully_adjusted_quintile[j,i] = AR_fully_adjusted_observed_model_with_e0_quintile[j,i] * exp(fit_quintile$coefficients[(j+1)] - MR_estimates_obese_overweight_normal[j])
    
    print(AR_fully_adjusted_observed_model[j,i])
    print(AR_C_fully_adjusted[j,i])
    
    PAR_C_bmi_quintile[j,i] = (((AR_fully_adjusted_observed_model[j,i] - AR_C_fully_adjusted[j,i])*wt_table_decile_bmicat[(j+1), i])/probability_outcome)
    PAR_C_bmi_quintile_aliter[j,i] = (((AR_fully_adjusted_observed_model_quintile[j,i] - AR_C_fully_adjusted_quintile[j,i])*wt_table_decile_bmicat[(j+1), i])/probability_outcome)
    
    print(i)
  }
  
}
round(PAR_C_bmi_quintile/sum(PAR_C_bmi_quintile)*100, 2)
round(PAR_C_bmi_quintile_aliter/sum(PAR_C_bmi_quintile_aliter)*100, 2)
#Variance calculation
Sigma_1 = vcov(fit_quintile)
data_processed_complete$quintile = as.factor(data_processed_complete$quintile)
X = model.matrix(~ bmi_cat_new + quintile + sampweight, data = data_processed_complete)
W = dexpit(X[,-8] %*% fit_quintile$coefficients[-4]) * data_processed_complete$sampweight
Gamma = (1/nrow(data_processed_complete)) * solve(t(X[, -8]) %*% (X[, -8]*W))
s1 = apply(X[,-8] * as.numeric((data_processed_complete$mortstat - locfit::expit(X[,-8] %*% fit_quintile$coefficients[-4]))*data_processed_complete$mortstat * data_processed_complete$sampweight), 2, sum)
s2 = apply(X[,-8] * as.numeric((data_processed_complete$mortstat - locfit::expit(X[,-8] %*% fit_quintile$coefficients[-4]))* data_processed_complete$sampweight), 2, sum)
s3 = wtd.mean(data_processed_complete$mortstat, weights = data_processed_complete$sampweight)
Sigma_2 = Gamma %*% (s1 - s2*s3)
Sigma_3 = (1/nrow(data_processed_complete)) * probability_outcome * (1-probability_outcome)
Sigma = as.matrix(rbind(cbind(Sigma_1, Sigma_2), c(t(Sigma_2), Sigma_3)))

Omega_star = MR_vcov_file[, -c(1:3)][c(6,10,14), c(6,10,14)]
beta_mr = MR_vcov_file$est[c(6,10,14)]
delta_mr = exp(beta_mr)
var_delta = diag(exp(beta_mr)) %*% as.matrix(Omega_star) %*% diag(exp(beta_mr)) 
M = t(matrix(c((1/(delta_mr[1] + delta_mr[2])), (1/(delta_mr[1] + delta_mr[2])), 0, 0, (1/(delta_mr[2] + delta_mr[3])), (1/(delta_mr[2] + delta_mr[3]))), 3, 2))
omega_MR = M %*% var_delta %*% t(M)

big_vcov_matrix = bdiag(Sigma, omega_MR)
g = function(x)
{
  (locfit::expit(sum(x[1:3])) - (locfit::expit(sum(x[c(1,3)]))*exp(x[2] - x[5])))/x[4]
}
g_prime = function(x)
{
  g1 = (1/x[4]) * (dexpit(sum(x[1:3])) - (dexpit(sum(x[c(1,3)]))*exp(x[2] - x[5])))
  g2 = (1/x[4]) * (dexpit(sum(x[1:3])) - (locfit::expit(sum(x[c(1,3)]))*exp(x[2] - x[5])))
  g3 = g1
  g4 = -(1/x[4])*g(x)
  g5 = (1/x[4])*(locfit::expit(sum(x[c(1,3)])) * exp(x[2] - x[5]))
  return(c(g1,g2,g3,g4,g5))
}

var_PAR_quintile= matrix(NA,2,5)
for(j in 1:2)
{
  for(i in 1:4)
  {
    x = c(fit_quintile$coefficients[1], fit_quintile$coefficients[j+1], fit_quintile$coefficients[-4][(i+3)], probability_outcome, MR_estimates_obese_overweight_normal[j])
    var_sub = big_vcov_matrix[c(1, (j+1), (i+3),8,(j+8)), c(1, (j+1), (i+3),8,(j+8))]
    var_PAR_quintile[j,(i+1)] = as.numeric(t(g_prime(x)) %*% var_sub %*% g_prime(x))
  }
}
for(j in 1:2)
{
  x = c(fit_quintile$coefficients[1], fit_quintile$coefficients[j+1],0, probability_outcome, MR_estimates_obese_overweight_normal[j])
  var_sub = big_vcov_matrix[c(1, (j+1),8,(j+8)), c(1, (j+1),8,(j+8))]
  var_PAR_quintile[j,1] = as.numeric(t(g_prime(x)[-3]) %*% var_sub %*% g_prime(x)[-3])
}

g = rep(NA, 10)
for(i in 1:5)
{
  g[i] = (1/probability_outcome) * (locfit::expit(fit_quintile$coefficients[1] + fit_quintile$coefficients[2] + fit_quintile$coefficients[i+3]) - (locfit::expit(fit_quintile$coefficients[1] + fit_quintile$coefficients[i+3])*exp(fit_quintile$coefficients[2] - MR_estimates_obese_overweight_normal[1])))
}
for(i in 1:5)
{
  g[i+5] = (1/probability_outcome) * (locfit::expit(fit_quintile$coefficients[1] + fit_quintile$coefficients[3] + fit_quintile$coefficients[i+3]) - (locfit::expit(fit_quintile$coefficients[1] + fit_quintile$coefficients[i+3])*exp(fit_quintile$coefficients[3] - MR_estimates_obese_overweight_normal[2])))
}

a = rep(NA,5)
for(i in 1:5)
{
  a[i] = (1/probability_outcome) *(dexpit(fit_quintile$coefficients[1] + fit_quintile$coefficients[2] + fit_quintile$coefficients[i+3]) - (dexpit(fit_quintile$coefficients[1] + fit_quintile$coefficients[i+3])*exp(fit_quintile$coefficients[2] - MR_estimates_UK[1])))
}

b = rep(NA,5)
for(i in 1:5)
{
  b[i] = (1/probability_outcome) *(dexpit(fit_quintile$coefficients[1] + fit_quintile$coefficients[2] + fit_quintile$coefficients[i+3]) - (locfit::expit(fit_quintile$coefficients[1] + fit_quintile$coefficients[i+3])*exp(fit_quintile$coefficients[2] - MR_estimates_UK[1])))
}
c = a[2:5]
d = rep(NA,5)
for(i in 1:5)
{
  d[i] = (-1/probability_outcome)*g[i]
}

e = rep(NA,5)
for(i in 1:5)
{
  e[i] = (1/probability_outcome)*locfit::expit(fit_quintile$coefficients[1] + fit_quintile$coefficients[i+3]) * exp(fit_quintile$coefficients[2] - MR_estimates_obese_overweight_normal[1])
}

f = rep(NA,5)
for(i in 1:5)
{
  f[i] = (1/probability_outcome) *(dexpit(fit_quintile$coefficients[1] + fit_quintile$coefficients[3] + fit_quintile$coefficients[i+3]) - (dexpit(fit_quintile$coefficients[1] + fit_quintile$coefficients[i+3])*exp(fit_quintile$coefficients[3] - MR_estimates_UK[2])))
}

gg = rep(NA,5)
for(i in 1:5)
{
  gg[i] = (1/probability_outcome) *(dexpit(fit_quintile$coefficients[1] + fit_quintile$coefficients[3] + fit_quintile$coefficients[i+3]) - (locfit::expit(fit_quintile$coefficients[1] + fit_quintile$coefficients[i+3])*exp(fit_quintile$coefficients[3] - MR_estimates_UK[2])))
}
h = f[2:5]

k = rep(NA,5)
for(i in 1:5)
{
  k[i] = (-1/probability_outcome)*g[i+5]
}

l = rep(NA,5)
for(i in 1:5)
{
  l[i] = (1/probability_outcome)*locfit::expit(fit_quintile$coefficients[1] + fit_quintile$coefficients[i+3]) * exp(fit_quintile$coefficients[3] - MR_estimates_obese_overweight_normal[2])
}

N = matrix(0, 10,10)
N[,1]  = c(a,f)
N[1:5,2] = b
N[6:10,3] = gg
N[2,4] = c[1]
N[3,5] = c[2]
N[4,6] = c[3]
N[5,7] = c[4]
N[7,4] = h[1]
N[8,5] = h[2]
N[9,6] = h[3]
N[10,7] = h[4]
N[1:5, 8] = d
N[1:5,9] = e
N[6:10, 8] = k
N[6:10, 10] = l

p_variance = N %*% big_vcov_matrix %*% t(N)

O = matrix(NA, 10, 10)
p_aliter = c(PAR_C_bmi_quintile_aliter[1,], PAR_C_bmi_quintile_aliter[2, ])
for(i in 1:10)
{
  O[i, ] = -p_aliter[i]/(sum(PAR_C_bmi_quintile_aliter)^2)
}
diag(O) = (sum(PAR_C_bmi_quintile_aliter) - PAR_C_bmi_quintile_aliter)/((sum(PAR_C_bmi_quintile_aliter))^2)

var_final = O %*% p_variance %*% t(O) 
PAR_C_bmi_quintile_aliter_var = matrix(NA, 2, 5)
PAR_C_bmi_quintile_aliter_var[1,] = diag(var_final)[1:5]
PAR_C_bmi_quintile_aliter_var[2,] = diag(var_final)[6:10]
R.CI = (PAR_C_bmi_quintile_aliter/sum(PAR_C_bmi_quintile_aliter)) + 1.96* sqrt(PAR_C_bmi_quintile_aliter_var)
L.CI = (PAR_C_bmi_quintile_aliter/sum(PAR_C_bmi_quintile_aliter)) - 1.96* sqrt(PAR_C_bmi_quintile_aliter_var)


#--All BMI categories
nhanes_combined_white = nhanes_combined_white %>% mutate(bmi_cat_new = case_when(bmi_cat == "Normal" ~ "Normal",
                                                                                 bmi_cat == "ObeseI" | bmi_cat == "ObeseII" ~ "Moreweight",
                                                                                 bmi_cat == "ObeseIII" | bmi_cat == "ObeseIV" | bmi_cat == "ObeseV" ~ "Obese"))
nhanes_combined_white$bmi_cat_new = factor(nhanes_combined_white$bmi_cat_new, levels = c("Normal", "Moreweight", "Obese"))
nhanes_combined_white_obeseoverweight_and_normal_for_model_fit = nhanes_combined_white 
#nhanes_combined_white_obese_and_normal_for_model_fit$bmi_cat_new = factor(nhanes_combined_white_obese_and_normal_for_model_fit$bmi_cat_new, levels = c("Normal", "Overweight"))
nhanes_combined_white_obeseoverweight_and_normal_for_model_fit$smoking_status = as.factor(nhanes_combined_white_obeseoverweight_and_normal_for_model_fit$smoking_status)
nhanes_combined_white_obeseoverweight_and_normal_for_model_fit$smoking_status = relevel(nhanes_combined_white_obeseoverweight_and_normal_for_model_fit$smoking_status, ref = "Never")
nhanes_combined_white_obeseoverweight_and_normal_for_model_fit$alcohol_status_new = as.factor(nhanes_combined_white_obeseoverweight_and_normal_for_model_fit$alcohol_status_new)
nhanes_combined_white_obeseoverweight_and_normal_for_model_fit$alcohol_status_new = relevel(nhanes_combined_white_obeseoverweight_and_normal_for_model_fit$alcohol_status_new, ref = "Nondrinker")
nhanes_combined_white_obeseoverweight_and_normal_for_model_fit$education = as.factor(nhanes_combined_white_obeseoverweight_and_normal_for_model_fit$education)
nhanes_combined_white_obeseoverweight_and_normal_for_model_fit$marital_status = as.factor(nhanes_combined_white_obeseoverweight_and_normal_for_model_fit$marital_status)
nhanes_combined_white_obeseoverweight_and_normal_for_model_fit$bmi_cat = as.factor(nhanes_combined_white_obeseoverweight_and_normal_for_model_fit$bmi_cat)

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
nhanes_svydesign <- svydesign(data=nhanes_combined_white_obeseoverweight_and_normal_for_model_fit, id=~SDMVPSU, strata=~SDMVSTRA, weights=~MEC8YR, nest=TRUE)

fit_full = svyglm(mortstat ~ bmi_cat + sex + age + education + smoking_status + no_of_cigarettes + alcohol_status_new + marital_status, design=nhanes_svydesign, family = quasibinomial())
non_bmi_coeff = fit_full$coefficients[-c(1:6)]
data_processed_complete = na.omit(nhanes_combined_white_obeseoverweight_and_normal_for_model_fit %>% dplyr::select("sex", "age","bmi", "MEC8YR", "mortstat", "bmi_obese", "bmi_cat","education","alcohol_status_new","marital_status", "smoking_status", "no_of_cigarettes", "SDMVPSU", "SDMVSTRA"))
#probability_outcome = wtd.mean(fit_full$fitted.values, weights = data_processed_complete$MEC8YR )
data_processed_complete$bmi_obese = if_else(data_processed_complete$bmi >= 30, 1, 0)
data_processed_complete$smoking_status = as.factor(data_processed_complete$smoking_status)
data_processed_complete$smoking_status = relevel(data_processed_complete$smoking_status, ref = "Never")
data_processed_complete$alcohol_status_new = as.factor(data_processed_complete$alcohol_status_new)
data_processed_complete$alcohol_status_new = relevel(data_processed_complete$alcohol_status_new, ref = "Nondrinker")
data_processed_complete$education = as.factor(data_processed_complete$education)
data_processed_complete$marital_status = as.factor(data_processed_complete$marital_status)
data_processed_complete$bmi_cat = as.factor(data_processed_complete$bmi_cat)

AR_fully_adjusted_observed_model_with_e0 = matrix(NA, 5, 5)



#data_processed_complete$MEC8YR = data_processed_complete$MEC8YR/sum(data_processed_complete$MEC8YR)
probability_outcome = wpct(data_processed_complete$mortstat, weight = data_processed_complete$MEC8YR)[2]
#probability_outcome = mean(data_processed_complete$mortstat)

#data_processed_complete$bmi_cat_new = relevel(data_processed_complete$bmi_cat_new, ref = "Normal")

hh = model.matrix(~ bmi_cat + sex + age + education + smoking_status + no_of_cigarettes + alcohol_status_new + marital_status + bmi_obese, data = data_processed_complete)
#hh_obese = model.matrix(~ bmi_cat + sex + age + education + smoking_status + no_of_cigarettes + alcohol_status + marital_status, data = data_processed_complete)
hh = cbind(hh, data_processed_complete$MEC8YR)
#hh_obese = cbind(hh, data_processed_complete$sampweight)
colnames(hh)[21] = "sampweight"

all.equal(colnames(hh[, -c(1:6,20:21)]), names(non_bmi_coeff))
hh_risk_score = as.data.frame(hh) %>% mutate(risk_score = as.numeric(hh[, -c(1:6,20:21)] %*% non_bmi_coeff))
hh_risk_score = hh_risk_score %>% mutate(quintile = ntile(risk_score, 5))
data_processed_complete = data_processed_complete %>% mutate(risk_score = hh_risk_score$risk_score) %>% mutate(quintile = ntile(risk_score, 5))
colnames(data_processed_complete)[4] = "sampweight"
data_processed_complete$sampweight = data_processed_complete$sampweight/sum(data_processed_complete$sampweight)
hh_risk_score$sampweight = hh_risk_score$sampweight/sum(hh_risk_score$sampweight)
table_decile_bmicat = prop.table(table(data_processed_complete$bmi_cat, data_processed_complete$quintile))
wt_table_decile_bmicat = data.frame(crosstab(df = data_processed_complete, x = bmi_cat, y = quintile, weight = sampweight, pct_type = "cell"))[c(1:6), c(2:6)]/100
PAR_C_bmi_all_quintile = matrix(NA,5,5)
AR_C_fully_adjusted = matrix(NA, 5, 5)
AR_fully_adjusted_observed_model = matrix(NA,5,5)
prop_exposure_deciles = matrix(NA, 5, 2)
for(j in 1:5)
{
  for(i in 1:5)
  {
    temp = hh_risk_score[which(hh_risk_score$quintile == i), ]
    temp_1 = data_processed_complete[which(data_processed_complete$quintile == i), ]
    
    wpct(temp_1$mortstat, weight = temp_1$sampweight)[2]
    #prop_exposure_deciles[i,] = wpct(temp_1$bmi_cat_new, weight = temp_1$sampweight)
    
    
    #temp_2 = temp[, -c(16:19)]
    #temp_2[, c(2)] = 0
    
    AR_fully_adjusted_observed_model_with_e0[j,i] = wtd.mean(locfit::expit(fit_full$coefficients[1] + temp$risk_score), temp$sampweight)
    AR_fully_adjusted_observed_model[j,i] = wtd.mean(locfit::expit(fit_full$coefficients[1] + fit_full$coefficients[(j+1)] + temp$risk_score), temp$sampweight)
    AR_C_fully_adjusted[j,i] = AR_fully_adjusted_observed_model_with_e0[j,i] * exp(fit_full$coefficients[(j+1)] - MR_estimates_UK[j])
    print(AR_fully_adjusted_observed_model[j,i])
    print(AR_C_fully_adjusted[j,i])
    PAR_C_bmi_all_quintile[j,i] = (((AR_fully_adjusted_observed_model[j,i] - AR_C_fully_adjusted[j,i])*wt_table_decile_bmicat[(j+1), i])/probability_outcome)
    print(i)
  }
  
}



AR_C_fully_adjusted = as.numeric(na.omit(AR_C_fully_adjusted))
AR_fully_adjusted_observed_model = as.numeric(na.omit(AR_fully_adjusted_observed_model))
AR_fully_adjusted_observed_model_obese = as.numeric(na.omit(AR_fully_adjusted_observed_model_obese))
AR_fully_adjusted_observed_model_with_e0 = as.numeric(na.omit(AR_fully_adjusted_observed_model_with_e0))
AR_fully_adjusted_observed_model_with_e0_obese = as.numeric(na.omit(AR_fully_adjusted_observed_model_with_e0_obese))
AR_fully_adjusted_observed_model_with_e0_obese_severe = as.numeric(na.omit(AR_fully_adjusted_observed_model_with_e0_obese_severe))
#data_plot = c(c(AR_C_fully_adjusted[, 2] - AR_C_fully_adjusted[,1],
#                               AR_C_fully_adjusted[, 3] - AR_C_fully_adjusted[,1],
#                              AR_C_fully_adjusted[, 4] - AR_C_fully_adjusted[,1],
#                             AR_C_fully_adjusted[, 5] - AR_C_fully_adjusted[,1],
#                            AR_C_fully_adjusted[, 6] - AR_C_fully_adjusted[,1]), 
#                 c(AR_fully_adjusted[, 2] - AR_fully_adjusted[,1],
#                          AR_fully_adjusted[, 3] - AR_fully_adjusted[,1],
#                         AR_fully_adjusted[, 4] - AR_fully_adjusted[,1],
#                        AR_fully_adjusted[, 5] - AR_fully_adjusted[,1],
#                       AR_fully_adjusted[, 6] - AR_fully_adjusted[,1]))
data_plot = as.data.frame(cbind(AR_fully_adjusted_observed_model_with_e0_obese_severe, AR_fully_adjusted_observed_model_with_e0_obese, AR_fully_adjusted_observed_model_with_e0, AR_C_fully_adjusted))
data_plot$decile = rep(paste0(seq(1,10,1)), 1)
data_plot$decile <- factor(data_plot$decile, levels=paste0(seq(1,10,1)))
colnames(data_plot) = c("Observed risk with severe obese BMI category (BMI >= 40)", "Observed risk with obese BMI category (BMI >= 30)", "Observed risk with normal BMI (22-25.7)", "Counterfactual risk", "Decile")
data_plot.m <- reshape2::melt(data_plot, id.vars='Decile')
p1_all_white = ggplot(data_plot.m, aes(Decile, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
  scale_fill_manual(values=c("#00A1D5FF","#B24745FF","#79AF97FF", "#6A6599FF"), labels = c(expression(paste("Observed risk for severe obese BMI (","">=40, Kgm^{-2}, ")")), expression(paste("Observed risk for obese BMI (","">=30, Kgm^{-2}, ")")), expression(paste("Observed risk for normal BMI (22-25.7", Kgm^{-2}, ")")), "Counterfactual risk for normal BMI")) +
  theme_bw() + labs(y="Absolute risk", x="Deciles of risk score", title = "All-cause mortality") +
  theme(axis.text.x = element_text(color = "black", size = 15, face="bold"),
        axis.text.y = element_text(color = "black", size = 15, face="bold"),  
        axis.title.x = element_text(color = "black", size = 15, face="bold"),
        axis.title.y = element_text(color = "black", size = 15, face="bold"),
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        strip.text = element_text(size = 15, face = "bold"),
        legend.box = "none",
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(colour="black", size=15),
        legend.text.align = 0,
        legend.position = c(0.18,0.8))  


#Variance of PAR calculation
k = 1/prop_exposure[1]
data_processed = nhanes_combined_white %>% dplyr::select("mortstat", "bmi_cat", "bmi", "MEC8YR")
colnames(data_processed)[4] = "sampweight"
Q = -matrix(rep(-k, 25), 5, 5) + diag(-1/prop_exposure[-1])
Sigma_11 = vcov(fit_unadjusted) * nrow(data_processed)
total_weights = sum(data_processed$sampweight)
data_processed$bmi_cat = as.factor(data_processed$bmi_cat)

data_processed_mm = as.data.frame(model.matrix(~ bmi_cat + mortstat + sampweight, data_processed))
w1 = (data_processed_mm$mortstat - locfit::expit(as.matrix(data_processed_mm)[, c(1:6)] %*% fit_unadjusted$coefficients)) * data_processed_mm$sampweight
psi = t(t(data_processed_mm[, c(2:6)])/prop_exposure[-1]) - ((1-data_processed_mm[,2]-data_processed_mm[,3]-data_processed_mm[,4]-data_processed_mm[,5]-data_processed_mm[,6])/prop_exposure[1])

w2 = -dexpit(as.matrix(data_processed_mm)[, c(1:6)] %*% fit_unadjusted$coefficients) * data_processed_mm$sampweight
P = t(as.matrix(data_processed_mm[, c(1:6)])) %*% (w2 * as.matrix(data_processed_mm[, c(1:6)]))
P = P/total_weights
Sigma_12 = solve(P) %*% ((t(as.matrix(data_processed_mm[, c(1:6)])) %*% (psi*as.numeric(w1)))/total_weights) %*% solve(Q)
Sigma_22 = solve(Q) %*% ((t(psi) %*% (psi*data_processed_mm$sampweight))/total_weights) %*% solve(Q)

Sigma_13 = -solve(P) %*% ((p_hat*(1-p_hat)*t(as.matrix(data_processed_mm)[, c(1:6)]) %*% ((data_processed_mm$mortstat/p_hat - ((1-data_processed_mm$mortstat)/(1-p_hat)))*w1))/total_weights)

Sigma_23 = p_hat*(1-p_hat) * (-solve(Q)) %*% (t(psi) %*% ((data_processed_mm$mortstat/p_hat - ((1-data_processed_mm$mortstat)/(1-p_hat)))*data_processed_mm$sampweight) *(1/total_weights))

Sigma_33 = ((p_hat*(1-p_hat))^2) * sum((data_processed_mm$mortstat/p_hat - ((1-data_processed_mm$mortstat)/(1-p_hat)))^2*data_processed_mm$sampweight)
Sigma_33 = Sigma_33/total_weights

Sigma = as.matrix(rbind(cbind(Sigma_11, Sigma_12, Sigma_13), cbind(t(Sigma_12), Sigma_22, Sigma_23), cbind(t(Sigma_13), t(Sigma_23), Sigma_33)))
Sigma = Sigma/nrow(data_processed_mm)

a = rep(0, 12)
a[1] = (-1/p_hat) * (exp(fit_unadjusted$coefficients[1])/(1+exp(fit_unadjusted$coefficients[1]))^2)
temp = exp(fit_unadjusted$coefficients[1])/(1+exp(fit_unadjusted$coefficients[1]))
for(i in 2:6)
{
  a[i] = -(temp) * (prop_exposure[i]/p_hat) *exp(fit_unadjusted$coefficients[i]- MR_estimates_UK[i-1])
}
for(i in 7:11)
{
  a[i] = -(temp) * (1/p_hat) *(exp(fit_unadjusted$coefficients[i-5]- MR_estimates_UK[i-6])-1)
}
a[12] = (1/(p_hat^2)) * temp * (1 + sum(prop_exposure[-1]*(exp(fit_unadjusted$coefficients[-1] - MR_estimates_UK)-1)))

t(a) %*% Sigma %*% a
b = -a[2:6]

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

PAR_var = t(a) %*% Sigma %*% a + t(b) %*% Omega_star %*% b
sqrt(PAR_var)



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

prop_exposure = wpct(nhanes_combined_white$bmi_cat, weight = nhanes_combined_white$MEC8YR)
PAR_MR = 1 - (prop_exposure[1] + sum(prop_exposure[-1] * exp(fit_unadjusted$coefficients[2:6] - MR_estimates_UK))) * (p_obs_D_given_E_equalto_0/p_hat)
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
data_processed_complete = na.omit(nhanes_combined_white %>% dplyr::select("sex", "age","bmi", "MEC8YR", "mortstat_rec", "bmi_obese", "bmi_cat","education","alcohol_status_new","marital_status", "smoking_status", "no_of_cigarettes", "SDMVPSU", "SDMVSTRA"))
data_processed_complete$bmi_obese = if_else(data_processed_complete$bmi >= 30, 1, 0)
data_processed_complete$bmi_severe_obese = if_else(data_processed_complete$bmi >= 40, 1, 0)
data_processed_complete$bmi_cat = as.factor(data_processed_complete$bmi_cat)
data_processed_complete$smoking_status = as.factor(data_processed_complete$smoking_status)
data_processed_complete$alcohol_status = as.factor(data_processed_complete$alcohol_status)
data_processed_complete$education = as.factor(data_processed_complete$education)
data_processed_complete$marital_status = as.factor(data_processed_complete$marital_status)
data_processed_complete$bmi_cat = as.factor(data_processed_complete$bmi_cat)


indicator_normal = if_else(data_processed_complete$bmi_cat == "Normal", 1,0)
bmi_cat_names = c("Normal",   "ObeseI",   "ObeseII",  "ObeseIII", "ObeseIV",  "ObeseV")
MR_estimates_UK_matrix = t(matrix(rep(MR_estimates_UK, 6), 5, 6))
MR_estimates_UK_matrix[2,1] = -MR_estimates_UK_matrix[2,1]
MR_estimates_UK_matrix[3,2] = -MR_estimates_UK_matrix[3,2]
MR_estimates_UK_matrix[4,3] = -MR_estimates_UK_matrix[4,3]
MR_estimates_UK_matrix[5,4] = -MR_estimates_UK_matrix[5,4]
MR_estimates_UK_matrix[6,5] = -MR_estimates_UK_matrix[6,5]
MR_estimates_UK_matrix[2, 2:5] = MR_estimates_UK_matrix[2, 2:5]- MR_estimates_UK_matrix[1,1]
MR_estimates_UK_matrix[3, c(1, 3:5)] = MR_estimates_UK_matrix[3, c(3, 3:5)]- MR_estimates_UK_matrix[1,2]
MR_estimates_UK_matrix[4, c(1:2, 4:5)] = MR_estimates_UK_matrix[4, c(1:2, 4:5)]- MR_estimates_UK_matrix[1,3]
MR_estimates_UK_matrix[5, c(1:3, 5)] = MR_estimates_UK_matrix[5, c(1:3, 5)]- MR_estimates_UK_matrix[1,4]
MR_estimates_UK_matrix[6, c(1:4)] = MR_estimates_UK_matrix[6, c(1:4)]- MR_estimates_UK_matrix[1,5]

AR_fully_adjusted_observed_model_with_e0 = matrix(NA, 6, 10)
AR_fully_adjusted_observed_model = matrix(NA, 6, 10)
AR_fully_adjusted_observed_model_obese = matrix(NA, 6, 10)
AR_fully_adjusted_observed_model_with_e0_obese = matrix(NA,6, 10)
AR_fully_adjusted_observed_model_with_e0_obese_severe = matrix(NA, 6, 10)
AR_C_fully_adjusted = matrix(NA, 6, 10)
bmi_cat_names = bmi_cat_names[1]
for(j in 1:length(bmi_cat_names))
{
  data_processed_complete$bmi_cat = relevel(data_processed_complete$bmi_cat, ref = bmi_cat_names[j])
  options(survey.adjust.domain.lonely=TRUE)
  options(survey.lonely.psu="adjust")
  nhanes_svydesign <- svydesign(data=data_processed_complete, id=~SDMVPSU, strata=~SDMVSTRA, weights=~MEC8YR, nest=TRUE)
  
  fit_full = svyglm(mortstat_rec ~ bmi_cat + sex + age + education + smoking_status + no_of_cigarettes + alcohol_status_new + marital_status, design=nhanes_svydesign, family = quasibinomial())
  fit_full_obese = svyglm(mortstat_rec ~ bmi_obese + sex + age + education + smoking_status + no_of_cigarettes + alcohol_status_new + marital_status, design=nhanes_svydesign, family = quasibinomial())
  fit_full_obese_severe = svyglm(mortstat_rec ~ bmi_severe_obese + sex + age + education + smoking_status + no_of_cigarettes + alcohol_status_new + marital_status, design=nhanes_svydesign, family = quasibinomial())
  hh = model.matrix(~ bmi_cat + sex + age + education + smoking_status + no_of_cigarettes + alcohol_status_new + marital_status + bmi_obese + bmi_severe_obese, data = data_processed_complete)
  #hh_obese = model.matrix(~ bmi_cat + sex + age + education + smoking_status + no_of_cigarettes + alcohol_status + marital_status, data = data_processed_complete)
  hh = cbind(hh, data_processed_complete$MEC8YR)
  #hh_obese = cbind(hh, data_processed_complete$sampweight)
  colnames(hh)[22] = "sampweight"
  hh_risk_score = as.data.frame(hh) %>% mutate(risk_score = as.numeric(hh[, -c(2:6,20:22)] %*% fit_full$coefficients[-c(2:6)]))
  hh_risk_score = hh_risk_score %>% mutate(decile = ntile(risk_score, 10))
  data_processed_complete = data_processed_complete %>% mutate(risk_score = hh_risk_score$risk_score) %>% mutate(decile = ntile(risk_score, 10))
  colnames(data_processed_complete)[4] = "sampweight"
  prop_exposure_deciles = matrix(NA, 10, 6)
  for(i in 1:10)
  {
    temp = hh_risk_score[which(hh_risk_score$decile == i), ]
    temp_1 = data_processed_complete[which(data_processed_complete$decile == i), ]
    if(length(wpct(temp_1$bmi_cat, weight = temp_1$sampweight)) < 6)
    {
      prop_exposure_deciles[i,] = c(wpct(temp_1$bmi_cat, weight = temp_1$sampweight),rep(0, (6-length(wpct(temp_1$bmi_cat, weight = temp_1$sampweight)))))
    }else{
      prop_exposure_deciles[i,] = wpct(temp_1$bmi_cat, weight = temp_1$sampweight) 
    }
    
    
    
    temp_2 = temp[, -c(20:24)]
    temp_2[, c(2:6)] = 0
    temp3 = temp[, c(1,20,7:19)]
    temp4 = temp[, c(1,21,7:19)]
    temp3[, 2] = 1
    temp4[, 2] = 1
    AR_fully_adjusted_observed_model[j,i] = wtd.mean(exp(as.matrix(temp[, -c(20:24)]) %*% as.numeric(fit_full$coefficients))/(1 + exp(as.matrix(temp[, -c(20:24)]) %*% as.numeric(fit_full$coefficients))), temp$sampweight)
    AR_fully_adjusted_observed_model_obese[j,i] = wtd.mean(exp(as.matrix(temp[, c(1,20,7:19)]) %*% as.numeric(fit_full_obese$coefficients))/(1 + exp(as.matrix(temp[, c(1,20,7:19)]) %*% as.numeric(fit_full_obese$coefficients))), temp$sampweight)
    
    print(i)
    AR_fully_adjusted_observed_model_with_e0[j,i] = wtd.mean(exp(as.matrix(temp_2) %*% as.numeric(fit_full$coefficients))/(1 + exp(as.matrix(temp_2) %*% as.numeric(fit_full$coefficients))), temp$sampweight)
    AR_fully_adjusted_observed_model_with_e0_obese[j,i] = wtd.mean(exp(as.matrix(temp3) %*% as.numeric(fit_full_obese$coefficients))/(1 + exp(as.matrix(temp3) %*% as.numeric(fit_full_obese$coefficients))), temp3$sampweight)
    AR_fully_adjusted_observed_model_with_e0_obese_severe[j,i] = wtd.mean(exp(as.matrix(temp4) %*% as.numeric(fit_full_obese_severe$coefficients))/(1 + exp(as.matrix(temp4) %*% as.numeric(fit_full_obese_severe$coefficients))), temp4$sampweight)
    print(i)
    AR_C_fully_adjusted[j,i] = (prop_exposure_deciles[i,1] + sum(prop_exposure_deciles[i, -1] * exp(fit_full$coefficients[2:6] - MR_estimates_UK_matrix[j, ])))
    AR_C_fully_adjusted[j,i] = AR_C_fully_adjusted[j,i] * AR_fully_adjusted_observed_model_with_e0[j,i]
    print(i)
  }
}
AR_C_fully_adjusted = as.numeric(na.omit(AR_C_fully_adjusted))
AR_fully_adjusted_observed_model = as.numeric(na.omit(AR_fully_adjusted_observed_model))
AR_fully_adjusted_observed_model_obese = as.numeric(na.omit(AR_fully_adjusted_observed_model_obese))
AR_fully_adjusted_observed_model_with_e0 = as.numeric(na.omit(AR_fully_adjusted_observed_model_with_e0))
AR_fully_adjusted_observed_model_with_e0_obese = as.numeric(na.omit(AR_fully_adjusted_observed_model_with_e0_obese))
AR_fully_adjusted_observed_model_with_e0_obese_severe = as.numeric(na.omit(AR_fully_adjusted_observed_model_with_e0_obese_severe))
#data_plot = c(c(AR_C_fully_adjusted[, 2] - AR_C_fully_adjusted[,1],
#                               AR_C_fully_adjusted[, 3] - AR_C_fully_adjusted[,1],
#                              AR_C_fully_adjusted[, 4] - AR_C_fully_adjusted[,1],
#                             AR_C_fully_adjusted[, 5] - AR_C_fully_adjusted[,1],
#                            AR_C_fully_adjusted[, 6] - AR_C_fully_adjusted[,1]), 
#                 c(AR_fully_adjusted[, 2] - AR_fully_adjusted[,1],
#                          AR_fully_adjusted[, 3] - AR_fully_adjusted[,1],
#                         AR_fully_adjusted[, 4] - AR_fully_adjusted[,1],
#                        AR_fully_adjusted[, 5] - AR_fully_adjusted[,1],
#                       AR_fully_adjusted[, 6] - AR_fully_adjusted[,1]))
data_plot = as.data.frame(cbind(AR_fully_adjusted_observed_model_with_e0_obese_severe, AR_fully_adjusted_observed_model_with_e0_obese, AR_fully_adjusted_observed_model_with_e0, AR_C_fully_adjusted))
data_plot$decile = rep(paste0(seq(1,10,1)), 1)
data_plot$decile <- factor(data_plot$decile, levels=paste0(seq(1,10,1)))
colnames(data_plot) = c("Observed risk with severe obese BMI category (BMI >= 40)", "Observed risk with obese BMI category (BMI >= 30)", "Observed risk with normal BMI (22-25.7)", "Counterfactual risk", "Decile")
data_plot.m <- reshape2::melt(data_plot, id.vars='Decile')
p1_heart_white = ggplot(data_plot.m, aes(Decile, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
  scale_fill_manual(values=c("#00A1D5FF","#B24745FF","#79AF97FF", "#6A6599FF"), labels = c(expression(paste("Observed risk for severe obese BMI (","">=40, Kgm^{-2}, ")")), expression(paste("Observed risk for obese BMI (","">=30, Kgm^{-2}, ")")), expression(paste("Observed risk for normal BMI (22-25.7", Kgm^{-2}, ")")), "Counterfactual risk for normal BMI")) +
  theme_bw() + labs(y="Absolute risk", x="Deciles of risk score", title = "Heart disease mortality") +
  theme(axis.text.x = element_text(color = "black", size = 15, face="bold"),
        axis.text.y = element_text(color = "black", size = 15, face="bold"),  
        axis.title.x = element_text(color = "black", size = 15, face="bold"),
        axis.title.y = element_text(color = "black", size = 15, face="bold"),
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        strip.text = element_text(size = 15, face = "bold"),
        legend.box = "none",
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(colour="black", size=15),
        legend.text.align = 0,
        legend.position = c(0.18,0.8))  


#Variance of PAR calculation
k = 1/prop_exposure[1]
data_processed = nhanes_combined_white %>% dplyr::select("mortstat_rec", "bmi_cat", "bmi", "MEC8YR")
colnames(data_processed)[4] = "sampweight"
Q = -matrix(rep(-k, 25), 5, 5) + diag(-1/prop_exposure[-1])
Sigma_11 = vcov(fit_unadjusted) * nrow(data_processed)
total_weights = sum(data_processed$sampweight)
data_processed$bmi_cat = as.factor(data_processed$bmi_cat)

data_processed_mm = as.data.frame(model.matrix(~ bmi_cat + mortstat_rec + sampweight, data_processed))
w1 = (data_processed_mm$mortstat_rec - locfit::expit(as.matrix(data_processed_mm)[, c(1:6)] %*% fit_unadjusted$coefficients)) * data_processed_mm$sampweight
psi = t(t(data_processed_mm[, c(2:6)])/prop_exposure[-1]) - ((1-data_processed_mm[,2]-data_processed_mm[,3]-data_processed_mm[,4]-data_processed_mm[,5]-data_processed_mm[,6])/prop_exposure[1])

w2 = -dexpit(as.matrix(data_processed_mm)[, c(1:6)] %*% fit_unadjusted$coefficients) * data_processed_mm$sampweight
P = t(as.matrix(data_processed_mm[, c(1:6)])) %*% (w2 * as.matrix(data_processed_mm[, c(1:6)]))
P = P/total_weights
Sigma_12 = solve(P) %*% ((t(as.matrix(data_processed_mm[, c(1:6)])) %*% (psi*as.numeric(w1)))/total_weights) %*% solve(Q)
Sigma_22 = solve(Q) %*% ((t(psi) %*% (psi*data_processed_mm$sampweight))/total_weights) %*% solve(Q)

Sigma_13 = -solve(P) %*% ((p_hat*(1-p_hat)*t(as.matrix(data_processed_mm)[, c(1:6)]) %*% ((data_processed_mm$mortstat_rec/p_hat - ((1-data_processed_mm$mortstat_rec)/(1-p_hat)))*w1))/total_weights)

Sigma_23 = p_hat*(1-p_hat) * (-solve(Q)) %*% (t(psi) %*% ((data_processed_mm$mortstat_rec/p_hat - ((1-data_processed_mm$mortstat_rec)/(1-p_hat)))*data_processed_mm$sampweight) *(1/total_weights))

Sigma_33 = ((p_hat*(1-p_hat))^2) * sum((data_processed_mm$mortstat_rec/p_hat - ((1-data_processed_mm$mortstat_rec)/(1-p_hat)))^2*data_processed_mm$sampweight)
Sigma_33 = Sigma_33/total_weights

Sigma = as.matrix(rbind(cbind(Sigma_11, Sigma_12, Sigma_13), cbind(t(Sigma_12), Sigma_22, Sigma_23), cbind(t(Sigma_13), t(Sigma_23), Sigma_33)))
Sigma = Sigma/nrow(data_processed_mm)

a = rep(0, 12)
a[1] = (-1/p_hat) * (exp(fit_unadjusted$coefficients[1])/(1+exp(fit_unadjusted$coefficients[1]))^2)
temp = exp(fit_unadjusted$coefficients[1])/(1+exp(fit_unadjusted$coefficients[1]))
for(i in 2:6)
{
  a[i] = -(temp) * (prop_exposure[i]/p_hat) *exp(fit_unadjusted$coefficients[i]- MR_estimates_UK[i-1])
}
for(i in 7:11)
{
  a[i] = -(temp) * (1/p_hat) *(exp(fit_unadjusted$coefficients[i-5]- MR_estimates_UK[i-6])-1)
}
a[12] = (1/(p_hat^2)) * temp * (1 + sum(prop_exposure[-1]*(exp(fit_unadjusted$coefficients[-1] - MR_estimates_UK)-1)))

t(a) %*% Sigma %*% a
b = -a[2:6]

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

PAR_var = t(a) %*% Sigma %*% a + t(b) %*% Omega_star %*% b
sqrt(PAR_var)




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

prop_exposure = wpct(nhanes_combined_white$bmi_cat, weight = nhanes_combined_white$MEC8YR)
PAR_MR = 1 - (prop_exposure[1] + sum(prop_exposure[-1] * exp(fit_unadjusted$coefficients[2:6] - MR_estimates_UK))) * (p_obs_D_given_E_equalto_0/p_hat)
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
data_processed_complete = na.omit(nhanes_combined_white %>% dplyr::select("sex", "age","bmi", "MEC8YR", "mortstat_rec", "bmi_obese", "bmi_cat","education","alcohol_status_new","marital_status", "smoking_status", "no_of_cigarettes", "SDMVPSU", "SDMVSTRA"))
data_processed_complete$bmi_obese = if_else(data_processed_complete$bmi >= 30, 1, 0)
data_processed_complete$bmi_severe_obese = if_else(data_processed_complete$bmi >= 40, 1, 0)
data_processed_complete$bmi_cat = as.factor(data_processed_complete$bmi_cat)
data_processed_complete$smoking_status = as.factor(data_processed_complete$smoking_status)
data_processed_complete$alcohol_status = as.factor(data_processed_complete$alcohol_status)
data_processed_complete$education = as.factor(data_processed_complete$education)
data_processed_complete$marital_status = as.factor(data_processed_complete$marital_status)
data_processed_complete$bmi_cat = as.factor(data_processed_complete$bmi_cat)


indicator_normal = if_else(data_processed_complete$bmi_cat == "Normal", 1,0)
bmi_cat_names = c("Normal",   "ObeseI",   "ObeseII",  "ObeseIII", "ObeseIV",  "ObeseV")
MR_estimates_UK_matrix = t(matrix(rep(MR_estimates_UK, 6), 5, 6))
MR_estimates_UK_matrix[2,1] = -MR_estimates_UK_matrix[2,1]
MR_estimates_UK_matrix[3,2] = -MR_estimates_UK_matrix[3,2]
MR_estimates_UK_matrix[4,3] = -MR_estimates_UK_matrix[4,3]
MR_estimates_UK_matrix[5,4] = -MR_estimates_UK_matrix[5,4]
MR_estimates_UK_matrix[6,5] = -MR_estimates_UK_matrix[6,5]
MR_estimates_UK_matrix[2, 2:5] = MR_estimates_UK_matrix[2, 2:5]- MR_estimates_UK_matrix[1,1]
MR_estimates_UK_matrix[3, c(1, 3:5)] = MR_estimates_UK_matrix[3, c(3, 3:5)]- MR_estimates_UK_matrix[1,2]
MR_estimates_UK_matrix[4, c(1:2, 4:5)] = MR_estimates_UK_matrix[4, c(1:2, 4:5)]- MR_estimates_UK_matrix[1,3]
MR_estimates_UK_matrix[5, c(1:3, 5)] = MR_estimates_UK_matrix[5, c(1:3, 5)]- MR_estimates_UK_matrix[1,4]
MR_estimates_UK_matrix[6, c(1:4)] = MR_estimates_UK_matrix[6, c(1:4)]- MR_estimates_UK_matrix[1,5]

AR_fully_adjusted_observed_model_with_e0 = matrix(NA, 6, 10)
AR_fully_adjusted_observed_model = matrix(NA, 6, 10)
AR_fully_adjusted_observed_model_obese = matrix(NA, 6, 10)
AR_fully_adjusted_observed_model_with_e0_obese = matrix(NA,6, 10)
AR_fully_adjusted_observed_model_with_e0_obese_severe = matrix(NA, 6, 10)
AR_C_fully_adjusted = matrix(NA, 6, 10)
bmi_cat_names = bmi_cat_names[1]
for(j in 1:length(bmi_cat_names))
{
  data_processed_complete$bmi_cat = relevel(data_processed_complete$bmi_cat, ref = bmi_cat_names[j])
  options(survey.adjust.domain.lonely=TRUE)
  options(survey.lonely.psu="adjust")
  nhanes_svydesign <- svydesign(data=data_processed_complete, id=~SDMVPSU, strata=~SDMVSTRA, weights=~MEC8YR, nest=TRUE)
  
  fit_full = svyglm(mortstat_rec ~ bmi_cat + sex + age + education + smoking_status + no_of_cigarettes + alcohol_status_new + marital_status, design=nhanes_svydesign, family = quasibinomial())
  fit_full_obese = svyglm(mortstat_rec ~ bmi_obese + sex + age + education + smoking_status + no_of_cigarettes + alcohol_status_new + marital_status, design=nhanes_svydesign, family = quasibinomial())
  fit_full_obese_severe = svyglm(mortstat_rec ~ bmi_severe_obese + sex + age + education + smoking_status + no_of_cigarettes + alcohol_status_new + marital_status, design=nhanes_svydesign, family = quasibinomial())
  hh = model.matrix(~ bmi_cat + sex + age + education + smoking_status + no_of_cigarettes + alcohol_status_new + marital_status + bmi_obese + bmi_severe_obese, data = data_processed_complete)
  #hh_obese = model.matrix(~ bmi_cat + sex + age + education + smoking_status + no_of_cigarettes + alcohol_status + marital_status, data = data_processed_complete)
  hh = cbind(hh, data_processed_complete$MEC8YR)
  #hh_obese = cbind(hh, data_processed_complete$sampweight)
  colnames(hh)[22] = "sampweight"
  hh_risk_score = as.data.frame(hh) %>% mutate(risk_score = as.numeric(hh[, -c(2:6,20:22)] %*% fit_full$coefficients[-c(2:6)]))
  hh_risk_score = hh_risk_score %>% mutate(decile = ntile(risk_score, 10))
  data_processed_complete = data_processed_complete %>% mutate(risk_score = hh_risk_score$risk_score) %>% mutate(decile = ntile(risk_score, 10))
  colnames(data_processed_complete)[4] = "sampweight"
  prop_exposure_deciles = matrix(NA, 10, 6)
  for(i in 1:10)
  {
    temp = hh_risk_score[which(hh_risk_score$decile == i), ]
    temp_1 = data_processed_complete[which(data_processed_complete$decile == i), ]
    if(length(wpct(temp_1$bmi_cat, weight = temp_1$sampweight)) < 6)
    {
      prop_exposure_deciles[i,] = c(wpct(temp_1$bmi_cat, weight = temp_1$sampweight),rep(0, (6-length(wpct(temp_1$bmi_cat, weight = temp_1$sampweight)))))
    }else{
      prop_exposure_deciles[i,] = wpct(temp_1$bmi_cat, weight = temp_1$sampweight) 
    }
    
    
    
    temp_2 = temp[, -c(20:24)]
    temp_2[, c(2:6)] = 0
    temp3 = temp[, c(1,20,7:19)]
    temp4 = temp[, c(1,21,7:19)]
    temp3[, 2] = 1
    temp4[, 2] = 1
    AR_fully_adjusted_observed_model[j,i] = wtd.mean(exp(as.matrix(temp[, -c(20:24)]) %*% as.numeric(fit_full$coefficients))/(1 + exp(as.matrix(temp[, -c(20:24)]) %*% as.numeric(fit_full$coefficients))), temp$sampweight)
    AR_fully_adjusted_observed_model_obese[j,i] = wtd.mean(exp(as.matrix(temp[, c(1,20,7:19)]) %*% as.numeric(fit_full_obese$coefficients))/(1 + exp(as.matrix(temp[, c(1,20,7:19)]) %*% as.numeric(fit_full_obese$coefficients))), temp$sampweight)
    
    print(i)
    AR_fully_adjusted_observed_model_with_e0[j,i] = wtd.mean(exp(as.matrix(temp_2) %*% as.numeric(fit_full$coefficients))/(1 + exp(as.matrix(temp_2) %*% as.numeric(fit_full$coefficients))), temp$sampweight)
    AR_fully_adjusted_observed_model_with_e0_obese[j,i] = wtd.mean(exp(as.matrix(temp3) %*% as.numeric(fit_full_obese$coefficients))/(1 + exp(as.matrix(temp3) %*% as.numeric(fit_full_obese$coefficients))), temp3$sampweight)
    AR_fully_adjusted_observed_model_with_e0_obese_severe[j,i] = wtd.mean(exp(as.matrix(temp4) %*% as.numeric(fit_full_obese_severe$coefficients))/(1 + exp(as.matrix(temp4) %*% as.numeric(fit_full_obese_severe$coefficients))), temp4$sampweight)
    print(i)
    AR_C_fully_adjusted[j,i] = (prop_exposure_deciles[i,1] + sum(prop_exposure_deciles[i, -1] * exp(fit_full$coefficients[2:6] - MR_estimates_UK_matrix[j, ])))
    AR_C_fully_adjusted[j,i] = AR_C_fully_adjusted[j,i] * AR_fully_adjusted_observed_model_with_e0[j,i]
    print(i)
  }
}
AR_C_fully_adjusted = as.numeric(na.omit(AR_C_fully_adjusted))
AR_fully_adjusted_observed_model = as.numeric(na.omit(AR_fully_adjusted_observed_model))
AR_fully_adjusted_observed_model_obese = as.numeric(na.omit(AR_fully_adjusted_observed_model_obese))
AR_fully_adjusted_observed_model_with_e0 = as.numeric(na.omit(AR_fully_adjusted_observed_model_with_e0))
AR_fully_adjusted_observed_model_with_e0_obese = as.numeric(na.omit(AR_fully_adjusted_observed_model_with_e0_obese))
AR_fully_adjusted_observed_model_with_e0_obese_severe = as.numeric(na.omit(AR_fully_adjusted_observed_model_with_e0_obese_severe))
#data_plot = c(c(AR_C_fully_adjusted[, 2] - AR_C_fully_adjusted[,1],
#                               AR_C_fully_adjusted[, 3] - AR_C_fully_adjusted[,1],
#                              AR_C_fully_adjusted[, 4] - AR_C_fully_adjusted[,1],
#                             AR_C_fully_adjusted[, 5] - AR_C_fully_adjusted[,1],
#                            AR_C_fully_adjusted[, 6] - AR_C_fully_adjusted[,1]), 
#                 c(AR_fully_adjusted[, 2] - AR_fully_adjusted[,1],
#                          AR_fully_adjusted[, 3] - AR_fully_adjusted[,1],
#                         AR_fully_adjusted[, 4] - AR_fully_adjusted[,1],
#                        AR_fully_adjusted[, 5] - AR_fully_adjusted[,1],
#                       AR_fully_adjusted[, 6] - AR_fully_adjusted[,1]))
data_plot = as.data.frame(cbind(AR_fully_adjusted_observed_model_with_e0_obese_severe, AR_fully_adjusted_observed_model_with_e0_obese, AR_fully_adjusted_observed_model_with_e0, AR_C_fully_adjusted))
data_plot$decile = rep(paste0(seq(1,10,1)), 1)
data_plot$decile <- factor(data_plot$decile, levels=paste0(seq(1,10,1)))
colnames(data_plot) = c("Observed risk with severe obese BMI category (BMI >= 40)", "Observed risk with obese BMI category (BMI >= 30)", "Observed risk with normal BMI (22-25.7)", "Counterfactual risk", "Decile")
data_plot.m <- reshape2::melt(data_plot, id.vars='Decile')
p1_cancer_white = ggplot(data_plot.m, aes(Decile, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
  scale_fill_manual(values=c("#00A1D5FF","#B24745FF","#79AF97FF", "#6A6599FF"), labels = c(expression(paste("Observed risk for severe obese BMI (","">=40, Kgm^{-2}, ")")), expression(paste("Observed risk for obese BMI (","">=30, Kgm^{-2}, ")")), expression(paste("Observed risk for normal BMI (22-25.7", Kgm^{-2}, ")")), "Counterfactual risk for normal BMI")) +
  theme_bw() + labs(y="Absolute risk", x="Deciles of risk score", title = "Cancer mortality") +
  theme(axis.text.x = element_text(color = "black", size = 15, face="bold"),
        axis.text.y = element_text(color = "black", size = 15, face="bold"),  
        axis.title.x = element_text(color = "black", size = 15, face="bold"),
        axis.title.y = element_text(color = "black", size = 15, face="bold"),
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        strip.text = element_text(size = 15, face = "bold"),
        legend.box = "none",
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(colour="black", size=15),
        legend.text.align = 0,
        legend.position = c(0.18,0.8))  



#Variance of PAR calculation
k = 1/prop_exposure[1]
data_processed = nhanes_combined_white %>% dplyr::select("mortstat_rec", "bmi_cat", "bmi", "MEC8YR")
colnames(data_processed)[4] = "sampweight"
Q = -matrix(rep(-k, 25), 5, 5) + diag(-1/prop_exposure[-1])
Sigma_11 = vcov(fit_unadjusted) * nrow(data_processed)
total_weights = sum(data_processed$sampweight)
data_processed$bmi_cat = as.factor(data_processed$bmi_cat)

data_processed_mm = as.data.frame(model.matrix(~ bmi_cat + mortstat_rec + sampweight, data_processed))
w1 = (data_processed_mm$mortstat_rec - locfit::expit(as.matrix(data_processed_mm)[, c(1:6)] %*% fit_unadjusted$coefficients)) * data_processed_mm$sampweight
psi = t(t(data_processed_mm[, c(2:6)])/prop_exposure[-1]) - ((1-data_processed_mm[,2]-data_processed_mm[,3]-data_processed_mm[,4]-data_processed_mm[,5]-data_processed_mm[,6])/prop_exposure[1])

w2 = -dexpit(as.matrix(data_processed_mm)[, c(1:6)] %*% fit_unadjusted$coefficients) * data_processed_mm$sampweight
P = t(as.matrix(data_processed_mm[, c(1:6)])) %*% (w2 * as.matrix(data_processed_mm[, c(1:6)]))
P = P/total_weights
Sigma_12 = solve(P) %*% ((t(as.matrix(data_processed_mm[, c(1:6)])) %*% (psi*as.numeric(w1)))/total_weights) %*% solve(Q)
Sigma_22 = solve(Q) %*% ((t(psi) %*% (psi*data_processed_mm$sampweight))/total_weights) %*% solve(Q)

Sigma_13 = -solve(P) %*% ((p_hat*(1-p_hat)*t(as.matrix(data_processed_mm)[, c(1:6)]) %*% ((data_processed_mm$mortstat_rec/p_hat - ((1-data_processed_mm$mortstat_rec)/(1-p_hat)))*w1))/total_weights)

Sigma_23 = p_hat*(1-p_hat) * (-solve(Q)) %*% (t(psi) %*% ((data_processed_mm$mortstat_rec/p_hat - ((1-data_processed_mm$mortstat_rec)/(1-p_hat)))*data_processed_mm$sampweight) *(1/total_weights))

Sigma_33 = ((p_hat*(1-p_hat))^2) * sum((data_processed_mm$mortstat_rec/p_hat - ((1-data_processed_mm$mortstat_rec)/(1-p_hat)))^2*data_processed_mm$sampweight)
Sigma_33 = Sigma_33/total_weights

Sigma = as.matrix(rbind(cbind(Sigma_11, Sigma_12, Sigma_13), cbind(t(Sigma_12), Sigma_22, Sigma_23), cbind(t(Sigma_13), t(Sigma_23), Sigma_33)))
Sigma = Sigma/nrow(data_processed_mm)

a = rep(0, 12)
a[1] = (-1/p_hat) * (exp(fit_unadjusted$coefficients[1])/(1+exp(fit_unadjusted$coefficients[1]))^2)
temp = exp(fit_unadjusted$coefficients[1])/(1+exp(fit_unadjusted$coefficients[1]))
for(i in 2:6)
{
  a[i] = -(temp) * (prop_exposure[i]/p_hat) *exp(fit_unadjusted$coefficients[i]- MR_estimates_UK[i-1])
}
for(i in 7:11)
{
  a[i] = -(temp) * (1/p_hat) *(exp(fit_unadjusted$coefficients[i-5]- MR_estimates_UK[i-6])-1)
}
a[12] = (1/(p_hat^2)) * temp * (1 + sum(prop_exposure[-1]*(exp(fit_unadjusted$coefficients[-1] - MR_estimates_UK)-1)))

t(a) %*% Sigma %*% a
b = -a[2:6]

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

PAR_var = t(a) %*% Sigma %*% a + t(b) %*% Omega_star %*% b
sqrt(PAR_var)


#Smokers vs Non-smokers
#--Non-smokers----#
MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_nonsmokers.csv")
BMI_quantiles = MR_vcov_file$bmi.quant
delta_HR = exp(MR_vcov_file$est)
MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[8]-delta_HR[6])/2, delta_HR[8] + (delta_HR[10]-delta_HR[8])/2, delta_HR[10] + (delta_HR[12]-delta_HR[10])/2, delta_HR[12] + (delta_HR[13]-delta_HR[12])/2, delta_HR[13] + (delta_HR[14]-delta_HR[13])/2))

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
#heart_data = heart_data %>% dplyr::select("bmi_cat", "alcohol_status", "sex", "age", "smoking_status", "MEC8YR", "mortstat_rec", "SDMVSTRA", "SDMVPSU")

nhanes_combined_white$MEC8YR = nhanes_combined_white$MEC8YR/sum(nhanes_combined_white$MEC8YR)


options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
nhanes_svydesign <- svydesign(data=nhanes_combined_white, id=~SDMVPSU, strata=~SDMVSTRA, weights=~MEC8YR, nest=TRUE)

fit_unadjusted = svyglm(mortstat ~ bmi_cat, design=nhanes_svydesign , family = quasibinomial())



p_hat = sum(nhanes_combined_white$MEC8YR[which(nhanes_combined_white$mortstat == 1)])/sum(nhanes_combined_white$MEC8YR)
p_obs_D_given_E_equalto_0 = sum(nhanes_combined_white$MEC8YR[which(nhanes_combined_white$mortstat == 1 & nhanes_combined_white$bmi_cat == "Normal")])/sum(nhanes_combined_white$MEC8YR[which(nhanes_combined_white$bmi_cat == "Normal")])

prop_exposure = wpct(nhanes_combined_white$bmi_cat, weight = nhanes_combined_white$MEC8YR)
PAR_MR = 1 - (prop_exposure[1] + sum(prop_exposure[-1] * exp(fit_unadjusted$coefficients[2:6] - MR_estimates_UK))) * (p_obs_D_given_E_equalto_0/p_hat)
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

#Variance of PAR calculation
k = 1/prop_exposure[1]
data_processed = nhanes_combined_white %>% dplyr::select("mortstat", "bmi_cat", "bmi", "MEC8YR")
colnames(data_processed)[4] = "sampweight"
Q = -matrix(rep(-k, 25), 5, 5) + diag(-1/prop_exposure[-1])
Sigma_11 = vcov(fit_unadjusted) * nrow(data_processed)
total_weights = sum(data_processed$sampweight)
data_processed$bmi_cat = as.factor(data_processed$bmi_cat)

data_processed_mm = as.data.frame(model.matrix(~ bmi_cat + mortstat + sampweight, data_processed))
w1 = (data_processed_mm$mortstat - locfit::expit(as.matrix(data_processed_mm)[, c(1:6)] %*% fit_unadjusted$coefficients)) * data_processed_mm$sampweight
psi = t(t(data_processed_mm[, c(2:6)])/prop_exposure[-1]) - ((1-data_processed_mm[,2]-data_processed_mm[,3]-data_processed_mm[,4]-data_processed_mm[,5]-data_processed_mm[,6])/prop_exposure[1])

w2 = -dexpit(as.matrix(data_processed_mm)[, c(1:6)] %*% fit_unadjusted$coefficients) * data_processed_mm$sampweight
P = t(as.matrix(data_processed_mm[, c(1:6)])) %*% (w2 * as.matrix(data_processed_mm[, c(1:6)]))
P = P/total_weights
Sigma_12 = solve(P) %*% ((t(as.matrix(data_processed_mm[, c(1:6)])) %*% (psi*as.numeric(w1)))/total_weights) %*% solve(Q)
Sigma_22 = solve(Q) %*% ((t(psi) %*% (psi*data_processed_mm$sampweight))/total_weights) %*% solve(Q)

Sigma_13 = -solve(P) %*% ((p_hat*(1-p_hat)*t(as.matrix(data_processed_mm)[, c(1:6)]) %*% ((data_processed_mm$mortstat/p_hat - ((1-data_processed_mm$mortstat)/(1-p_hat)))*w1))/total_weights)

Sigma_23 = p_hat*(1-p_hat) * (-solve(Q)) %*% (t(psi) %*% ((data_processed_mm$mortstat/p_hat - ((1-data_processed_mm$mortstat)/(1-p_hat)))*data_processed_mm$sampweight) *(1/total_weights))

Sigma_33 = ((p_hat*(1-p_hat))^2) * sum((data_processed_mm$mortstat/p_hat - ((1-data_processed_mm$mortstat)/(1-p_hat)))^2*data_processed_mm$sampweight)
Sigma_33 = Sigma_33/total_weights

Sigma = as.matrix(rbind(cbind(Sigma_11, Sigma_12, Sigma_13), cbind(t(Sigma_12), Sigma_22, Sigma_23), cbind(t(Sigma_13), t(Sigma_23), Sigma_33)))
Sigma = Sigma/nrow(data_processed_mm)

a = rep(0, 12)
a[1] = (-1/p_hat) * (exp(fit_unadjusted$coefficients[1])/(1+exp(fit_unadjusted$coefficients[1]))^2)
temp = exp(fit_unadjusted$coefficients[1])/(1+exp(fit_unadjusted$coefficients[1]))
for(i in 2:6)
{
  a[i] = -(temp) * (prop_exposure[i]/p_hat) *exp(fit_unadjusted$coefficients[i]- MR_estimates_UK[i-1])
}
for(i in 7:11)
{
  a[i] = -(temp) * (1/p_hat) *(exp(fit_unadjusted$coefficients[i-5]- MR_estimates_UK[i-6])-1)
}
a[12] = (1/(p_hat^2)) * temp * (1 + sum(prop_exposure[-1]*(exp(fit_unadjusted$coefficients[-1] - MR_estimates_UK)-1)))

t(a) %*% Sigma %*% a
b = -a[2:6]

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

PAR_var = t(a) %*% Sigma %*% a + t(b) %*% Omega_star %*% b
sqrt(PAR_var)

#-Smokers-#
MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_smokers.csv")
BMI_quantiles = MR_vcov_file$bmi.quant
delta_HR = exp(MR_vcov_file$est)
MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[8]-delta_HR[6])/2, delta_HR[8] + (delta_HR[10]-delta_HR[8])/2, delta_HR[10] + (delta_HR[12]-delta_HR[10])/2, delta_HR[12] + (delta_HR[13]-delta_HR[12])/2, delta_HR[13] + (delta_HR[14]-delta_HR[13])/2))

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

prop_exposure = wpct(nhanes_combined_white$bmi_cat, weight = nhanes_combined_white$MEC8YR)
PAR_MR = 1 - (prop_exposure[1] + sum(prop_exposure[-1] * exp(fit_unadjusted$coefficients[2:6] - MR_estimates_UK))) * (p_obs_D_given_E_equalto_0/p_hat)
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

#Variance of PAR calculation
k = 1/prop_exposure[1]
data_processed = nhanes_combined_white %>% dplyr::select("mortstat", "bmi_cat", "bmi", "MEC8YR")
colnames(data_processed)[4] = "sampweight"
Q = -matrix(rep(-k, 25), 5, 5) + diag(-1/prop_exposure[-1])
Sigma_11 = vcov(fit_unadjusted) * nrow(data_processed)
total_weights = sum(data_processed$sampweight)
data_processed$bmi_cat = as.factor(data_processed$bmi_cat)

data_processed_mm = as.data.frame(model.matrix(~ bmi_cat + mortstat + sampweight, data_processed))
w1 = (data_processed_mm$mortstat - locfit::expit(as.matrix(data_processed_mm)[, c(1:6)] %*% fit_unadjusted$coefficients)) * data_processed_mm$sampweight
psi = t(t(data_processed_mm[, c(2:6)])/prop_exposure[-1]) - ((1-data_processed_mm[,2]-data_processed_mm[,3]-data_processed_mm[,4]-data_processed_mm[,5]-data_processed_mm[,6])/prop_exposure[1])

w2 = -dexpit(as.matrix(data_processed_mm)[, c(1:6)] %*% fit_unadjusted$coefficients) * data_processed_mm$sampweight
P = t(as.matrix(data_processed_mm[, c(1:6)])) %*% (w2 * as.matrix(data_processed_mm[, c(1:6)]))
P = P/total_weights
Sigma_12 = solve(P) %*% ((t(as.matrix(data_processed_mm[, c(1:6)])) %*% (psi*as.numeric(w1)))/total_weights) %*% solve(Q)
Sigma_22 = solve(Q) %*% ((t(psi) %*% (psi*data_processed_mm$sampweight))/total_weights) %*% solve(Q)

Sigma_13 = -solve(P) %*% ((p_hat*(1-p_hat)*t(as.matrix(data_processed_mm)[, c(1:6)]) %*% ((data_processed_mm$mortstat/p_hat - ((1-data_processed_mm$mortstat)/(1-p_hat)))*w1))/total_weights)

Sigma_23 = p_hat*(1-p_hat) * (-solve(Q)) %*% (t(psi) %*% ((data_processed_mm$mortstat/p_hat - ((1-data_processed_mm$mortstat)/(1-p_hat)))*data_processed_mm$sampweight) *(1/total_weights))

Sigma_33 = ((p_hat*(1-p_hat))^2) * sum((data_processed_mm$mortstat/p_hat - ((1-data_processed_mm$mortstat)/(1-p_hat)))^2*data_processed_mm$sampweight)
Sigma_33 = Sigma_33/total_weights

Sigma = as.matrix(rbind(cbind(Sigma_11, Sigma_12, Sigma_13), cbind(t(Sigma_12), Sigma_22, Sigma_23), cbind(t(Sigma_13), t(Sigma_23), Sigma_33)))
Sigma = Sigma/nrow(data_processed_mm)

a = rep(0, 12)
a[1] = (-1/p_hat) * (exp(fit_unadjusted$coefficients[1])/(1+exp(fit_unadjusted$coefficients[1]))^2)
temp = exp(fit_unadjusted$coefficients[1])/(1+exp(fit_unadjusted$coefficients[1]))
for(i in 2:6)
{
  a[i] = -(temp) * (prop_exposure[i]/p_hat) *exp(fit_unadjusted$coefficients[i]- MR_estimates_UK[i-1])
}
for(i in 7:11)
{
  a[i] = -(temp) * (1/p_hat) *(exp(fit_unadjusted$coefficients[i-5]- MR_estimates_UK[i-6])-1)
}
a[12] = (1/(p_hat^2)) * temp * (1 + sum(prop_exposure[-1]*(exp(fit_unadjusted$coefficients[-1] - MR_estimates_UK)-1)))

t(a) %*% Sigma %*% a
b = -a[2:6]

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

PAR_var = t(a) %*% Sigma %*% a + t(b) %*% Omega_star %*% b
sqrt(PAR_var)

p1 = ggarrange(p1_all_white,p1_heart_white,p1_cancer_white,ncol=1,nrow=3)
ggsave('/Users/prosenjitkundu/Dropbox/Attributable_risk/Absolute_and_attributable_risk/Plots/absolute_risk_NHANES_white.png', p1, width = 17,height=11, dpi = 300)
