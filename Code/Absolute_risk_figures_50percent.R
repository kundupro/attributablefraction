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

nhanes_combined = nhanes_combined[which(nhanes_combined$RIDAGEYR >= 40 & nhanes_combined$RIDAGEYR < 70), ]


AR_shift = function(transition_matrix, baseline_probability, unadjusted_RR, adjusted_RR)
{
  ratio_matrix = unadjusted_RR/adjusted_RR
  ratio_matrix[upper.tri(ratio_matrix)]  = 0
  baseline_prob_matrix = t(matrix(rep(baseline_probability, length(baseline_probability)), nrow = length(baseline_probability), ncol = length(baseline_probability)))
  return(sum(transition_matrix*ratio_matrix*baseline_prob_matrix))
}

#--- only obese
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

mid_point_normal = BMI_quantiles[3] + (BMI_quantiles[6] - BMI_quantiles[3])/2

nhanes_combined_white = nhanes_combined_white %>% mutate(bmi_cat = case_when(bmi < BMI_quantiles[3] ~ "Underweight",
                                                                             bmi >= BMI_quantiles[3] & bmi < BMI_quantiles[6] ~ "Normal",
                                                                             bmi >= BMI_quantiles[6] & bmi < BMI_quantiles[10] ~ "Overweight",
                                                                             bmi >= BMI_quantiles[10] & bmi < BMI_quantiles[12] ~ "SObese_grade1",
                                                                             bmi >= BMI_quantiles[12] & bmi < BMI_quantiles[13] ~ "SObese_grade2",
                                                                             bmi >= BMI_quantiles[13] ~ "SObese_grade3"))

#---Pooled cohort
beta_HR_pooled = log(c(1 + (1.07-1)/2, 1.07 + (1.45-1.07)/2, 1.45 + (2.76-1.45)/2))[-1]
#nhanes_combined_white = nhanes_combined_white %>% mutate(bmi_shift = 0.8*bmi)
#nhanes_combined_white = nhanes_combined_white %>% mutate(bmi_cat = case_when(bmi < BMI_quantiles[3] ~ "Underweight",
#                                                                             bmi >= BMI_quantiles[3] & bmi < BMI_quantiles[6] ~ "Normal",
 #                                                                            bmi >= BMI_quantiles[6] & bmi < BMI_quantiles[10] ~ "Overweight",
  #                                                                           bmi >= BMI_quantiles[10] & bmi < BMI_quantiles[12] ~ "SObese_grade1",
   #                                                                          bmi >= BMI_quantiles[12] & bmi < BMI_quantiles[13] ~ "SObese_grade2",
    #                                                                         bmi >= BMI_quantiles[13] ~ "SObese_grade3"))
#nhanes_combined_white = nhanes_combined_white %>% mutate(bmi_cat_shift = case_when(bmi_shift < BMI_quantiles[3] ~ "Underweight",
 #                                                                                  bmi_shift >= BMI_quantiles[3] & bmi_shift < BMI_quantiles[6] ~ "Normal",
  #                                                                                 bmi_shift >= BMI_quantiles[6] & bmi_shift < BMI_quantiles[10] ~ "Overweight",
   #                                                                                bmi_shift >= BMI_quantiles[10] & bmi_shift < BMI_quantiles[12] ~ "SObese_grade1",
    #                                                                               bmi_shift >= BMI_quantiles[12] & bmi_shift < BMI_quantiles[13] ~ "SObese_grade2",
     #                                                                              bmi_shift >= BMI_quantiles[13] ~ "SObese_grade3"))

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

#---Removing underweight people--#
nhanes_combined_white = nhanes_combined_white[-which(nhanes_combined_white$bmi_cat == "Underweight"), ]

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
fit_full = svyglm(mortstat ~ bmi_cat + sex + age + education + alcohol_status_new + marital_status + smoking_status + no_of_cigarettes, design=nhanes_svydesign, family = quasibinomial())

fit_unadjusted = svyglm(mortstat ~ bmi_cat, design=nhanes_svydesign, family = quasibinomial())
nhanes_combined_white = nhanes_combined_white %>% filter(bmi_cat == "SObese_grade1" | bmi_cat == "SObese_grade2" | bmi_cat =="SObese_grade3")


#---Relative Risk Matrix
RR = matrix(0,5,5)
colnames(RR) = c("Normal", "Overweight", "SObese_grade1", "SObese_grade2", "SObese_grade3")
rownames(RR) = c("Normal", "Overweight", "SObese_grade1", "SObese_grade2", "SObese_grade3")
diag(RR) = 1
RR = RR[-1,]
RR[1,1] = exp(fit_full$coefficients[2])
RR[2,1] = exp(fit_full$coefficients[3])
RR[3,1] = exp(fit_full$coefficients[4])
RR[4,1] = exp(fit_full$coefficients[5])
RR[2,2] = exp(fit_full$coefficients[3] - fit_full$coefficients[2])
RR[3,2] = exp(fit_full$coefficients[4] - fit_full$coefficients[2])
RR[4,2] = exp(fit_full$coefficients[5] - fit_full$coefficients[2])
RR[3,3] = exp(fit_full$coefficients[4] - fit_full$coefficients[3])
RR[4,3] = exp(fit_full$coefficients[5] - fit_full$coefficients[3])
RR[4,4] = exp(fit_full$coefficients[5] - fit_full$coefficients[4])

MR_RR = RR
MR_RR[1,1] = exp(MR_estimates_UK[1])
MR_RR[2,1] = exp(MR_estimates_UK[2])
MR_RR[3,1] = exp(MR_estimates_UK[3])
MR_RR[4,1] = exp(MR_estimates_UK[4])
MR_RR[2,2] = exp(MR_estimates_UK[2] - MR_estimates_UK[1])
MR_RR[3,2] = exp(MR_estimates_UK[3] - MR_estimates_UK[1])
MR_RR[4,2] = exp(MR_estimates_UK[4] - MR_estimates_UK[1])
MR_RR[3,3] = exp(MR_estimates_UK[3] - MR_estimates_UK[2])
MR_RR[4,3] = exp(MR_estimates_UK[4] - MR_estimates_UK[2])
MR_RR[4,4] = exp(MR_estimates_UK[4] - MR_estimates_UK[3])

pooled_RR_estimates = log(c(1.06 + (1.19-1.06)/2, 1.45, 1.96, 2.89))
pooled_RR_estimates = readRDS("/Users/prosenjitkundu/Dropbox/Attributable_risk/Absolute_and_attributable_risk/Code/beta_all_cause_meta_pooled_6groups.rds")

pooled_RR = RR
pooled_RR[1,1] = exp(pooled_RR_estimates[1])
pooled_RR[2,1] = exp(pooled_RR_estimates[2])
pooled_RR[3,1] = exp(pooled_RR_estimates[3])
pooled_RR[4,1] = exp(pooled_RR_estimates[4])
pooled_RR[2,2] = exp(pooled_RR_estimates[2] - pooled_RR_estimates[1])
pooled_RR[3,2] = exp(pooled_RR_estimates[3] - pooled_RR_estimates[1])
pooled_RR[4,2] = exp(pooled_RR_estimates[4] - pooled_RR_estimates[1])
pooled_RR[3,3] = exp(pooled_RR_estimates[3] - pooled_RR_estimates[2])
pooled_RR[4,3] = exp(pooled_RR_estimates[4] - pooled_RR_estimates[2])
pooled_RR[4,4] = exp(pooled_RR_estimates[4] - pooled_RR_estimates[3])

RR = rbind(c(1,0,0,0,0), RR)
rownames(RR)[1] = "Normal"
MR_RR = rbind(c(1,0,0,0,0), MR_RR)
rownames(MR_RR)[1] = "Normal"
pooled_RR = rbind(c(1,0,0,0,0), pooled_RR)
rownames(pooled_RR)[1] = "Normal"

data_processed_complete = na.omit(nhanes_combined_white %>% dplyr::select("sex", "age","bmi", "MEC8YR", "mortstat", "bmi_obese", "bmi_cat","education","alcohol_status_new","marital_status", "smoking_status", "no_of_cigarettes", "SDMVPSU", "SDMVSTRA", "bmi_cat_shift"))
#data_processed_complete_without_rf = nhanes_combined_white %>% dplyr::select("sex", "age","bmi", "MEC8YR", "mortstat", "bmi_obese", "bmi_cat","education","alcohol_status_new","marital_status", "smoking_status", "no_of_cigarettes", "SDMVPSU", "SDMVSTRA")
colnames(data_processed_complete)[4] = "sampweight"
data_processed_complete$sampweight = data_processed_complete$sampweight/sum(data_processed_complete$sampweight)
#colnames(data_processed_complete_without_rf)[4] = "sampweight"
ARD_K = matrix(NA,1,5)
ARD_K_pooled = matrix(NA,1,5)
observed_AR_K = matrix(NA,1,5) 
ARD_K_normal = matrix(NA,1,5)
ARD_K_normal_pooled = matrix(NA,1,5)

#data_processed_complete$bmi_cat = relevel(data_processed_complete$bmi_cat, ref = bmi_cat_names[j])
#nhanes_combined_white = nhanes_combined_white %>% filter(bmi_cat == "Overweight"| bmi_cat == "SObese_grade1" | bmi_cat == "SObese_grade2" | bmi_cat =="SObese_grade3")
data_processed_complete$bmi_cat = as.factor(as.character(data_processed_complete$bmi_cat))
data_processed_complete_obese = data_processed_complete
nhanes_svydesign <- svydesign(data=data_processed_complete_obese, id=~SDMVPSU, strata=~SDMVSTRA, weights=~sampweight, nest=TRUE)
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

fit_full_obese = svyglm(mortstat ~ bmi_cat + sex + age + education + alcohol_status_new + marital_status + smoking_status + no_of_cigarettes, design=nhanes_svydesign, family = quasibinomial())

hh = model.matrix(~ bmi_cat + sex + age + education + alcohol_status_new + marital_status + smoking_status + no_of_cigarettes + mortstat + bmi_cat_shift, data = data_processed_complete)
#hh_obese = model.matrix(~ bmi_cat + sex + age + education + smoking_status + no_of_cigarettes + alcohol_status + marital_status, data = data_processed_complete)
hh = cbind(hh, data_processed_complete$sampweight)
#hh_obese = cbind(hh, data_processed_complete$sampweight)
colnames(hh)[21] = "sampweight"
hh_risk_score = as.data.frame(hh) %>% mutate(risk_score = as.numeric(hh[, -c(1:3,17:21)] %*% fit_full$coefficients[-c(1:5)]))
hh_risk_score = hh_risk_score %>% mutate(decile = ntile(risk_score, 5), bmi_cat = as.character(data_processed_complete$bmi_cat), bmi_cat_shift = as.character(data_processed_complete$bmi_cat_shift))
data_processed_complete = data_processed_complete %>% mutate(risk_score = hh_risk_score$risk_score) %>% mutate(decile = ntile(risk_score, 5))
#colnames(data_processed_complete)[4] = "sampweight"
#prop_exposure_deciles = matrix(NA, 10, 4)
weight.mean = function(x,w)
{
  return(sum(x*w))
}
group = list()

baseline_prob = rep(0, length(fit_full$coefficients[2:5]) +1)
bmi_coeff = c(0, fit_full$coefficients[2:5])
names(bmi_coeff) = c("bmi_catNormal", names(fit_full$coefficients[2:5]))
#baseline_prob = locfit::expit(fit_full$coefficients[1] + bmi_coeff)


for(i in 1:5)
{
  temp = hh_risk_score[which(hh_risk_score$decile == i), ]
  
  #print(sum(temp$mortstat))
  #print(table(temp$mortstat, temp$bmi_catSObese_grade2))
  #print(table(temp$mortstat, temp$bmi_catSObese_grade3))
  #table(temp$mortstat, temp$bmi_catSObese_grade1)
  #beta = c(fit_full$coefficients[1], fit_full$coefficients[3:15])
  #observed_AR = mean(as.numeric(locfit::expit(as.numeric(as.matrix(temp[,-c(16:19)]) %*% fit_full$coefficients))))
  #prop_exposure_decile = wpct(temp_1$bmi_cat, weight = temp_1$sampweight)
  #combinations = expand.grid(c(0,1),c(0,1))[-4, ]
  n_temp = nrow(temp)
   
  for(l in 1:length(baseline_prob))
  {
    baseline_prob[l] = weighted.mean(locfit::expit(fit_full$coefficients[1] + bmi_coeff[l] + temp$risk_score), w = temp$sampweight)
  }
  names(baseline_prob) = str_replace(names(bmi_coeff), "bmi_cat", "")
  
  AR_C = 0
  AR_C_pooled = 0
  propbmi.table = table(temp$bmi_cat, temp$bmi_cat_shift)/n_temp
  #combinations = cbind(combinations, bmi_cat_name = c("SObese_grade1", "SObese_grade2", "SObese_grade3"))

    for(m in 1:nrow(propbmi.table))
    {
      for(n in 1:min(m, ncol(propbmi.table)))
      {
        index_row_RR = which(rownames(RR) %in% rownames(propbmi.table)[m] == T)
        index_col_RR = which(colnames(RR) %in% colnames(propbmi.table)[n] == T)
        rr = RR[index_row_RR, index_col_RR]
        mr = MR_RR[index_row_RR, index_col_RR]
        pooled_rr = pooled_RR[index_row_RR, index_col_RR]
        index_baseline_prob = which(names(baseline_prob) %in% colnames(propbmi.table)[n] == T)
        AR_C = AR_C + propbmi.table[m,n]*(rr/mr)*baseline_prob[index_baseline_prob]
        #print(AR_C)
        print(n)
        print(m)
        AR_C_pooled = AR_C_pooled + propbmi.table[m,n]*(rr/pooled_rr)*baseline_prob[index_baseline_prob]
      }
      #print(k)
     
      
    }
    
  index_full_model_coeff = as.numeric(na.omit(match(colnames(temp)[-c(17:25)], names(fit_full$coefficients)[1:5])))[-1]
  observed_AR = weighted.mean(locfit::expit(as.matrix(temp[, grep("bmi", colnames(temp)[-c(17:25)])]) %*% fit_full$coefficients[index_full_model_coeff]  + temp$risk_score + fit_full$coefficients[1]), w = temp$sampweight)
  observed_AR_normal = weighted.mean(locfit::expit(temp$risk_score + fit_full$coefficients[1]), w = temp$sampweight)
  
  print(wpct(temp$bmi_cat,weight = temp$sampweight))
  ARD_K_normal[1,i] = observed_AR_normal*sum(wpct(temp$bmi_cat,weight = temp$sampweight)*(RR[3:5,1]/MR_RR[3:5,1]))
  ARD_K_normal_pooled[1,i] = observed_AR_normal*sum(wpct(temp$bmi_cat,weight = temp$sampweight)*(RR[3:5,1]/pooled_RR[3:5,1]))
  
  ARD_K[1,i] = AR_C
  ARD_K_pooled[1,i] = AR_C_pooled
  observed_AR_K[1,i] = observed_AR
  print(i)
}

#---collapsing over stratum

quintile_proportions =  questionr::wtd.table(hh_risk_score$decile, weights = hh_risk_score$sampweight)
#ARD_K_collapsed = sum(quintile_proportions*ARD_K[1,])
ARD_K_collapsed = sum(ARD_K[1,])/5
#ARD_K_normal_collapsed = sum(quintile_proportions*ARD_K_normal[1,])
ARD_K_normal_collapsed = sum(ARD_K_normal[1,])/5
observed_AR_K_collapsed = locfit::expit(fit_unadjusted$coefficients[1])
observed_AR_K_collapsed = sum(observed_AR_K[1, ])/5
hh_risk_score_obese = as.data.frame(model.matrix(~ bmi_cat + risk_score + sampweight, data = hh_risk_score))
hh_risk_score_obese$bmi_catSObese_grade1 = 0
hh_risk_score_obese$bmi_catSObese_grade1[which(hh_risk_score_obese$bmi_catSObese_grade2 == 0 & hh_risk_score_obese$bmi_catSObese_grade3 == 0)] =1
hh_risk_score_obese = hh_risk_score_obese[, c(1,6,2:5)]
observed_AR_K_collapsed = weighted.mean(locfit::expit(fit_full$coefficients[1] + as.matrix(hh_risk_score_obese[, 2:4])%*%fit_full$coefficients[3:5] + hh_risk_score_obese$risk_score), w = hh_risk_score_obese$sampweight)
AR_C_obese = c(ARD_K[1,],ARD_K_collapsed)
AR_C_normal = c(ARD_K_normal[1,],ARD_K_normal_collapsed)
#AR_C_severe = ARD_K[3,]
AR_O_obese = c(observed_AR_K[1,], observed_AR_K_collapsed)
#AR_O_severe = observed_AR_K[3,]
data_plot = as.data.frame(cbind(AR_O_obese, AR_C_obese, AR_C_normal))
data_plot = data_plot[c(6,1:5),]
#data_plot = rbind(data_plot, c(0, 0, 0))
#data_plot = as.data.frame(cbind(AR_O_overweight, AR_C_overweight,  AR_O_obese, AR_C_obese, AR_O_severe, AR_C_severe))
data_plot$decile = c("Overall", rep(paste0("Q", seq(1,5,1))))
data_plot$decile <- factor(data_plot$decile, levels = c("Overall", rep(paste0("Q", seq(1,5,1)))))
colnames(data_plot) = c("observed risk obese", "counterfactual risk obese", "counterfactual risk normal",  "decile")
#colnames(data_plot) = c("observed risk overweight", "counterfactual risk overweight", "observed risk obese", "counterfactual risk obese", "observed risk severe", "counterfactual risk severe", "decile")

data_plot.m <- reshape2::melt(data_plot, id.vars='decile')

#data_plot.m.total = data_plot.m[c(6,12,18), ]

#data_plot.m.wo.total = data_plot.m[-c(6,12,18),]

#data_plot.m$index_order = c(rep(1,10), rep(2,10), rep(3,10), rep(4,10))
obs_count = factor(c(rep("observed", 6), rep("counterfactual_bmi_20per", 6), rep("counterfactual_bmi_normal", 6)), levels = c("observed","counterfactual_bmi_20per", "counterfactual_bmi_normal"))
status2 = rep("Quintile of risk score", 18)
status2[c(1,7,13)] = c("Overall")
status2 = as.factor(status2)
#bmi = factor(c(rep("overweight (25.7-30.7 Kg/m*2)", 10), rep("overweight (25.7-30.7 Kg/m*2)", 10), rep("obese (>30.7 Kg/m*2)", 10), rep("obese (>30.7 Kg/m*2)", 10)), levels = c("overweight (25.7-30.7 Kg/m*2)","obese (>30.7 Kg/m*2)"))
data_plot.m2 = data_plot.m %>% mutate(state = obs_count, status2 = status2)
p1_MR_white_all_10_percent = ggplot(data = data_plot.m2, aes(x=decile, y=value, pattern = state, fill = status2)) +
  geom_bar_pattern(stat = "identity", position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) +
  scale_fill_manual(values = colorRampPalette(c("lightsalmon3","deepskyblue3"))(2)) +
  scale_pattern_manual(values = c(observed = "none", counterfactual_bmi_20per = "stripe", counterfactual_bmi_normal = "weave"), labels = c("Observed","Counterfactual (50% reduction of excess BMI)","Counterfactual (100% reduction in excess BMI)")) +
  guides(pattern = guide_legend(override.aes = list(fill = c("white"))),
         fill = guide_legend(override.aes = list(pattern = "none"))) + 
  theme_bw() + labs(y="Absolute risk of 10-year mortality", x="Risk-score Quintiles") +
  theme(axis.text.x = element_text(color = "black", size = 15, face="bold"),
        axis.text.y = element_text(color = "black", size = 15, face="bold"),  
        axis.title.x = element_text(color = "black", size = 15, face="bold"),
        axis.title.y = element_text(color = "black", size = 15, face="bold"),
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        #strip.text = element_text(size = 15, face = "bold"),
        #legend.box = "none",
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(colour="black", size=15),
        #legend.text.align = 0,
        legend.position = c(0.3,0.8))
p1_MR_white_all_10_percent
ggsave("all_cause_mortality_MR_estimates.png", plot = last_plot(), dpi = 300)

#All-cause pooled 
ARD_K_pooled_collapsed = sum(ARD_K_pooled[1,])/5
ARD_K_normal_pooled_collapsed = sum(ARD_K_normal_pooled[1,])/5
AR_C_obese = c(ARD_K_pooled[1,],ARD_K_pooled_collapsed)
AR_C_normal = c(ARD_K_normal_pooled[1,],ARD_K_normal_pooled_collapsed)
#AR_C_severe = ARD_K[3,]
AR_O_obese = c(observed_AR_K[1,], observed_AR_K_collapsed)
#AR_O_severe = observed_AR_K[3,]
data_plot = as.data.frame(cbind(AR_O_obese, AR_C_obese, AR_C_normal))
data_plot = data_plot[c(6,1:5),]
#data_plot = as.data.frame(cbind(AR_O_overweight, AR_C_overweight,  AR_O_obese, AR_C_obese, AR_O_severe, AR_C_severe))
data_plot$decile = c("Overall", rep(paste0("Q", seq(1,5,1))))
data_plot$decile <- factor(data_plot$decile, levels = c("Overall", rep(paste0("Q", seq(1,5,1)))))
colnames(data_plot) = c("observed risk obese", "counterfactual risk obese", "counterfactual risk normal",  "decile")
#colnames(data_plot) = c("observed risk overweight", "counterfactual risk overweight", "observed risk obese", "counterfactual risk obese", "observed risk severe", "counterfactual risk severe", "decile")
data_plot.m <- reshape2::melt(data_plot, id.vars='decile')


#data_plot.m$index_order = c(rep(1,10), rep(2,10), rep(3,10), rep(4,10))
obs_count = factor(c(rep("observed", 6), rep("counterfactual_bmi_20per", 6), rep("counterfactual_bmi_normal", 6)), levels = c("observed","counterfactual_bmi_20per", "counterfactual_bmi_normal"))
status2 = rep("Quintile of risk score", 18)
status2[c(1,7,13)] = c("Overall")
status2 = as.factor(status2)
data_plot.m2 = data_plot.m %>% mutate(state = obs_count, status2 = status2)
p1_RR_pooled_white_all_10_percent = ggplot(data = data_plot.m2, aes(x=decile, y=value, pattern = state, fill = status2)) +
  geom_bar_pattern(stat = "identity", position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) +
  scale_fill_manual(values = colorRampPalette(c("lightsalmon3","deepskyblue3"))(2)) +
  scale_pattern_manual(values = c(observed = "none", counterfactual_bmi_20per = "stripe", counterfactual_bmi_normal = "weave"), labels = c("Observed","Counterfactual (50% reduction of excess BMI)","Counterfactual (100% reduction in excess BMI)")) +
  guides(pattern = guide_legend(override.aes = list(fill = c("white"))),
         fill = guide_legend(override.aes = list(pattern = "none"))) + 
  theme_bw() + labs(y="Absolute risk of 10-year mortality", x="Risk-score Quintiles") +
  theme(axis.text.x = element_text(color = "black", size = 15, face="bold"),
        axis.text.y = element_text(color = "black", size = 15, face="bold"),  
        axis.title.x = element_text(color = "black", size = 15, face="bold"),
        axis.title.y = element_text(color = "black", size = 15, face="bold"),
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        #strip.text = element_text(size = 15, face = "bold"),
        #legend.box = "none",
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(colour="black", size=15),
        #legend.text.align = 0,
        legend.position = c(0.3,0.8))

p1_RR_pooled_white_all_10_percent
ggsave("all_cause_mortality_RR_estimates.png", plot = last_plot(), dpi = 300)



#only obese MR heart disease
nhanes_combined_white = nhanes_combined[which(nhanes_combined$race_ethnicity == "white"), ]

#nhanes_combined_white = nhanes_combined_white %>% filter(bmi < 100)
#nhanes_combined_aa = nhanes_combined[which(nhanes_combined$race == "black"), ]

MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_cardiovascular.csv")
BMI_quantiles = MR_vcov_file$bmi.quant

#Normal : BMI_quantiles[3]- BMI_quantiles[6], Overweight: BMI_quantiles[6]-BMI_quantiles[9], Obese: BMI_quantiles[9]:BMI_quantiles[13], Severe obese: >= BMI_quantiles[13]
delta_HR = exp(MR_vcov_file$est)
#MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[9]-delta_HR[6])/2, delta_HR[9] + (delta_HR[13]-delta_HR[9])/2, delta_HR[13] + (delta_HR[14]-delta_HR[13])/2))
MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[10]-delta_HR[6])/2, delta_HR[10] + (delta_HR[12]-delta_HR[10])/2, delta_HR[12] + (delta_HR[13]-delta_HR[12])/2, delta_HR[13] + (delta_HR[14]-delta_HR[13])/2))

#MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[10]-delta_HR[6])/2, delta_HR[10] + (delta_HR[14]-delta_HR[10])/2))


#---Pooled cohort
#beta_HR_pooled = log(c(1 + (1.07-1)/2, 1.07 + (1.45-1.07)/2, 1.45 + (2.76-1.45)/2))[-1]
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
mid_point_normal = BMI_quantiles[3] + (BMI_quantiles[6] - BMI_quantiles[3])/2
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

#---Removing underweight people--#
nhanes_combined_white = nhanes_combined_white[-which(nhanes_combined_white$bmi_cat == "Underweight"), ]

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
fit_full = svyglm(mortstat_rec ~ bmi_cat + sex + age + education + alcohol_status_new + marital_status + smoking_status + no_of_cigarettes, design=nhanes_svydesign, family = quasibinomial())
nhanes_combined_white = nhanes_combined_white %>% filter(bmi_cat == "SObese_grade1" | bmi_cat == "SObese_grade2" | bmi_cat =="SObese_grade3")


#---Relative Risk Matrix
RR = matrix(0,5,5)
colnames(RR) = c("Normal", "Overweight", "SObese_grade1", "SObese_grade2", "SObese_grade3")
rownames(RR) = c("Normal", "Overweight", "SObese_grade1", "SObese_grade2", "SObese_grade3")
diag(RR) = 1
RR = RR[-1,]
RR[1,1] = exp(fit_full$coefficients[2])
RR[2,1] = exp(fit_full$coefficients[3])
RR[3,1] = exp(fit_full$coefficients[4])
RR[4,1] = exp(fit_full$coefficients[5])
RR[2,2] = exp(fit_full$coefficients[3] - fit_full$coefficients[2])
RR[3,2] = exp(fit_full$coefficients[4] - fit_full$coefficients[2])
RR[4,2] = exp(fit_full$coefficients[5] - fit_full$coefficients[2])
RR[3,3] = exp(fit_full$coefficients[4] - fit_full$coefficients[3])
RR[4,3] = exp(fit_full$coefficients[5] - fit_full$coefficients[3])
RR[4,4] = exp(fit_full$coefficients[5] - fit_full$coefficients[4])

MR_RR = RR
MR_RR[1,1] = exp(MR_estimates_UK[1])
MR_RR[2,1] = exp(MR_estimates_UK[2])
MR_RR[3,1] = exp(MR_estimates_UK[3])
MR_RR[4,1] = exp(MR_estimates_UK[4])
MR_RR[2,2] = exp(MR_estimates_UK[2] - MR_estimates_UK[1])
MR_RR[3,2] = exp(MR_estimates_UK[3] - MR_estimates_UK[1])
MR_RR[4,2] = exp(MR_estimates_UK[4] - MR_estimates_UK[1])
MR_RR[3,3] = exp(MR_estimates_UK[3] - MR_estimates_UK[2])
MR_RR[4,3] = exp(MR_estimates_UK[4] - MR_estimates_UK[2])
MR_RR[4,4] = exp(MR_estimates_UK[4] - MR_estimates_UK[3])

#pooled_RR_estimates = log(c(1.11, 1.44, 1.92, 2.71))

pooled_RR_estimates = readRDS("/Users/prosenjitkundu/Dropbox/Attributable_risk/Absolute_and_attributable_risk/Code/beta_cardio_meta_pooled_6groups.rds")
#pooled_RR_estimates = log(c(1.12 + (1.34-1.12)/2, 1.72, 2.67, 3.71))
pooled_RR = RR
pooled_RR[1,1] = exp(pooled_RR_estimates[1])
pooled_RR[2,1] = exp(pooled_RR_estimates[2])
pooled_RR[3,1] = exp(pooled_RR_estimates[3])
pooled_RR[4,1] = exp(pooled_RR_estimates[4])
pooled_RR[2,2] = exp(pooled_RR_estimates[2] - pooled_RR_estimates[1])
pooled_RR[3,2] = exp(pooled_RR_estimates[3] - pooled_RR_estimates[1])
pooled_RR[4,2] = exp(pooled_RR_estimates[4] - pooled_RR_estimates[1])
pooled_RR[3,3] = exp(pooled_RR_estimates[3] - pooled_RR_estimates[2])
pooled_RR[4,3] = exp(pooled_RR_estimates[4] - pooled_RR_estimates[2])
pooled_RR[4,4] = exp(pooled_RR_estimates[4] - pooled_RR_estimates[3])

RR = rbind(c(1,0,0,0,0), RR)
rownames(RR)[1] = "Normal"
MR_RR = rbind(c(1,0,0,0,0), MR_RR)
rownames(MR_RR)[1] = "Normal"
pooled_RR = rbind(c(1,0,0,0,0), pooled_RR)
rownames(pooled_RR)[1] = "Normal"

data_processed_complete = na.omit(nhanes_combined_white %>% dplyr::select("sex", "age","bmi", "MEC8YR", "mortstat_rec", "bmi_obese", "bmi_cat","education","alcohol_status_new","marital_status", "smoking_status", "no_of_cigarettes", "SDMVPSU", "SDMVSTRA", "bmi_cat_shift"))
#data_processed_complete_without_rf = nhanes_combined_white %>% dplyr::select("sex", "age","bmi", "MEC8YR", "mortstat", "bmi_obese", "bmi_cat","education","alcohol_status_new","marital_status", "smoking_status", "no_of_cigarettes", "SDMVPSU", "SDMVSTRA")
colnames(data_processed_complete)[4] = "sampweight"
data_processed_complete$sampweight = data_processed_complete$sampweight/sum(data_processed_complete$sampweight)
#colnames(data_processed_complete_without_rf)[4] = "sampweight"
ARD_K = matrix(NA,1,5)
ARD_K_pooled = matrix(NA,1,5)
observed_AR_K = matrix(NA,1,5) 
ARD_K_normal = matrix(NA,1,5)
ARD_K_normal_pooled = matrix(NA,1,5)

#data_processed_complete$bmi_cat = relevel(data_processed_complete$bmi_cat, ref = bmi_cat_names[j])
#nhanes_combined_white = nhanes_combined_white %>% filter(bmi_cat == "Overweight"| bmi_cat == "SObese_grade1" | bmi_cat == "SObese_grade2" | bmi_cat =="SObese_grade3")
data_processed_complete$bmi_cat = as.factor(as.character(data_processed_complete$bmi_cat))
hh = model.matrix(~ bmi_cat + sex + age + education + alcohol_status_new + marital_status + smoking_status + no_of_cigarettes + mortstat_rec + bmi_cat_shift, data = data_processed_complete)
#hh_obese = model.matrix(~ bmi_cat + sex + age + education + smoking_status + no_of_cigarettes + alcohol_status + marital_status, data = data_processed_complete)
hh = cbind(hh, data_processed_complete$sampweight)
#hh_obese = cbind(hh, data_processed_complete$sampweight)
colnames(hh)[21] = "sampweight"
hh_risk_score = as.data.frame(hh) %>% mutate(risk_score = as.numeric(hh[, -c(1:3,17:21)] %*% fit_full$coefficients[-c(1:5)]))
hh_risk_score = hh_risk_score %>% mutate(decile = ntile(risk_score, 5), bmi_cat = as.character(data_processed_complete$bmi_cat), bmi_cat_shift = as.character(data_processed_complete$bmi_cat_shift))
data_processed_complete = data_processed_complete %>% mutate(risk_score = hh_risk_score$risk_score) %>% mutate(decile = ntile(risk_score, 5))
#colnames(data_processed_complete)[4] = "sampweight"
#prop_exposure_deciles = matrix(NA, 10, 4)
weight.mean = function(x,w)
{
  return(sum(x*w))
}
group = list()

baseline_prob = rep(0, length(fit_full$coefficients[2:5]) +1)
bmi_coeff = c(0, fit_full$coefficients[2:5])
names(bmi_coeff) = c("bmi_catNormal", names(fit_full$coefficients[2:5]))
#baseline_prob = locfit::expit(fit_full$coefficients[1] + bmi_coeff)


for(i in 1:5)
{
  temp = hh_risk_score[which(hh_risk_score$decile == i), ]
  n_temp = nrow(temp)
  
  for(l in 1:length(baseline_prob))
  {
    baseline_prob[l] = weighted.mean(locfit::expit(fit_full$coefficients[1] + bmi_coeff[l] + temp$risk_score), w = temp$sampweight)
  }
  names(baseline_prob) = str_replace(names(bmi_coeff), "bmi_cat", "")
  
  AR_C = 0
  AR_C_pooled = 0
  propbmi.table = table(temp$bmi_cat, temp$bmi_cat_shift)/n_temp
  #combinations = cbind(combinations, bmi_cat_name = c("SObese_grade1", "SObese_grade2", "SObese_grade3"))
  
  for(m in 1:nrow(propbmi.table))
  {
    for(n in 1:min(m, ncol(propbmi.table)))
    {
      index_row_RR = which(rownames(RR) %in% rownames(propbmi.table)[m] == T)
      index_col_RR = which(colnames(RR) %in% colnames(propbmi.table)[n] == T)
      rr = RR[index_row_RR, index_col_RR]
      mr = MR_RR[index_row_RR, index_col_RR]
      pooled_rr = pooled_RR[index_row_RR, index_col_RR]
      index_baseline_prob = which(names(baseline_prob) %in% colnames(propbmi.table)[n] == T)
      AR_C = AR_C + propbmi.table[m,n]*(rr/mr)*baseline_prob[index_baseline_prob]
      #print(AR_C)
      print(n)
      print(m)
      AR_C_pooled = AR_C_pooled + propbmi.table[m,n]*(rr/pooled_rr)*baseline_prob[index_baseline_prob]
    }
    #print(k)
    
    
  }
  
  index_full_model_coeff = as.numeric(na.omit(match(colnames(temp)[-c(17:25)], names(fit_full$coefficients)[1:5])))[-1]
  observed_AR = weighted.mean(locfit::expit(as.matrix(temp[, grep("bmi", colnames(temp)[-c(17:25)])]) %*% fit_full$coefficients[index_full_model_coeff]  + temp$risk_score + fit_full$coefficients[1]), w = temp$sampweight)
  observed_AR_normal = weighted.mean(locfit::expit(temp$risk_score + fit_full$coefficients[1]), w = temp$sampweight)
  
  print(wpct(temp$bmi_cat,weight = temp$sampweight))
  ARD_K_normal[1,i] = observed_AR_normal*sum(wpct(temp$bmi_cat,weight = temp$sampweight)*(RR[3:5,1]/MR_RR[3:5,1]))
  ARD_K_normal_pooled[1,i] = observed_AR_normal*sum(wpct(temp$bmi_cat,weight = temp$sampweight)*(RR[3:5,1]/pooled_RR[3:5,1]))
  
  ARD_K[1,i] = AR_C
  ARD_K_pooled[1,i] = AR_C_pooled
  observed_AR_K[1,i] = observed_AR
  print(i)
}

#---collapsing over stratum

temp = hh_risk_score


#ARD_K_collapsed = sum(quintile_proportions*ARD_K[1,])
ARD_K_collapsed = sum(ARD_K[1,])/5
#ARD_K_normal_collapsed = sum(quintile_proportions*ARD_K_normal[1,])
ARD_K_normal_collapsed = sum(ARD_K_normal[1,])/5

observed_AR_K_collapsed = locfit::expit(fit_unadjusted$coefficients[1])
hh_risk_score_obese = as.data.frame(model.matrix(~ bmi_cat + risk_score + sampweight, data = hh_risk_score))
hh_risk_score_obese$bmi_catSObese_grade1 = 0
hh_risk_score_obese$bmi_catSObese_grade1[which(hh_risk_score_obese$bmi_catSObese_grade2 == 0 & hh_risk_score_obese$bmi_catSObese_grade3 == 0)] =1
hh_risk_score_obese = hh_risk_score_obese[, c(1,6,2:5)]
observed_AR_K_collapsed = weighted.mean(locfit::expit(fit_full$coefficients[1] + as.matrix(hh_risk_score_obese[, 2:4])%*%fit_full$coefficients[3:5] + hh_risk_score_obese$risk_score), w = hh_risk_score_obese$sampweight)

AR_C_obese = c(ARD_K[1,],ARD_K_collapsed)
AR_C_normal = c(ARD_K_normal[1,],ARD_K_normal_collapsed)
#AR_C_severe = ARD_K[3,]
AR_O_obese = c(observed_AR_K[1,], observed_AR_K_collapsed)
#AR_O_severe = observed_AR_K[3,]
data_plot = as.data.frame(cbind(AR_O_obese, AR_C_obese, AR_C_normal))
data_plot = data_plot[c(6,1:5),]
#data_plot = rbind(data_plot, c(0, 0, 0))
#data_plot = as.data.frame(cbind(AR_O_overweight, AR_C_overweight,  AR_O_obese, AR_C_obese, AR_O_severe, AR_C_severe))
data_plot$decile = c("Overall", rep(paste0("Q", seq(1,5,1))))
data_plot$decile <- factor(data_plot$decile, levels = c("Overall", rep(paste0("Q", seq(1,5,1)))))
colnames(data_plot) = c("observed risk obese", "counterfactual risk obese", "counterfactual risk normal",  "decile")
#colnames(data_plot) = c("observed risk overweight", "counterfactual risk overweight", "observed risk obese", "counterfactual risk obese", "observed risk severe", "counterfactual risk severe", "decile")

data_plot.m <- reshape2::melt(data_plot, id.vars='decile')

#data_plot.m.total = data_plot.m[c(6,12,18), ]

#data_plot.m.wo.total = data_plot.m[-c(6,12,18),]

#data_plot.m$index_order = c(rep(1,10), rep(2,10), rep(3,10), rep(4,10))
obs_count = factor(c(rep("observed", 6), rep("counterfactual_bmi_20per", 6), rep("counterfactual_bmi_normal", 6)), levels = c("observed","counterfactual_bmi_20per", "counterfactual_bmi_normal"))
status2 = rep("Quintile of risk score", 18)
status2[c(1,7,13)] = c("Overall")
status2 = as.factor(status2)
#bmi = factor(c(rep("overweight (25.7-30.7 Kg/m*2)", 10), rep("overweight (25.7-30.7 Kg/m*2)", 10), rep("obese (>30.7 Kg/m*2)", 10), rep("obese (>30.7 Kg/m*2)", 10)), levels = c("overweight (25.7-30.7 Kg/m*2)","obese (>30.7 Kg/m*2)"))
data_plot.m2 = data_plot.m %>% mutate(state = obs_count, status2 = status2)
p1_MR_white_heartdisease_10_percent = ggplot(data = data_plot.m2, aes(x=decile, y=value, pattern = state, fill = status2)) +
  geom_bar_pattern(stat = "identity", position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) +
  scale_fill_manual(values = colorRampPalette(c("lightsalmon3","deepskyblue3"))(2)) +
  scale_pattern_manual(values = c(observed = "none", counterfactual_bmi_20per = "stripe", counterfactual_bmi_normal = "weave"), labels = c("Observed","Counterfactual (50% reduction of excess BMI)","Counterfactual (100% reduction in excess BMI)")) +
  guides(pattern = guide_legend(override.aes = list(fill = c("white"))),
         fill = guide_legend(override.aes = list(pattern = "none"))) + 
  theme_bw() + labs(y="Absolute risk of 10-year mortality", x="Risk-score Quintiles") +
  theme(axis.text.x = element_text(color = "black", size = 15, face="bold"),
        axis.text.y = element_text(color = "black", size = 15, face="bold"),  
        axis.title.x = element_text(color = "black", size = 15, face="bold"),
        axis.title.y = element_text(color = "black", size = 15, face="bold"),
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        #strip.text = element_text(size = 15, face = "bold"),
        #legend.box = "none",
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(colour="black", size=15),
        #legend.text.align = 0,
        legend.position = c(0.3,0.8))


p1_MR_white_heartdisease_10_percent
ggsave("all_heartdisease_mortality_MR_estimates.png", plot = last_plot(), dpi = 300)

ARD_K_pooled_collapsed = sum(ARD_K_pooled[1,])/5
ARD_K_normal_pooled_collapsed = sum(ARD_K_normal_pooled[1,])/5
AR_C_obese = c(ARD_K_pooled[1,],ARD_K_pooled_collapsed)
AR_C_normal = c(ARD_K_normal_pooled[1,],ARD_K_normal_pooled_collapsed)
#AR_C_severe = ARD_K[3,]
AR_O_obese = c(observed_AR_K[1,], observed_AR_K_collapsed)
#AR_O_severe = observed_AR_K[3,]
data_plot = as.data.frame(cbind(AR_O_obese, AR_C_obese, AR_C_normal))
data_plot = data_plot[c(6,1:5),]
#data_plot = as.data.frame(cbind(AR_O_overweight, AR_C_overweight,  AR_O_obese, AR_C_obese, AR_O_severe, AR_C_severe))
data_plot$decile = c("Overall", rep(paste0("Q", seq(1,5,1))))
data_plot$decile <- factor(data_plot$decile, levels = c("Overall", rep(paste0("Q", seq(1,5,1)))))
colnames(data_plot) = c("observed risk obese", "counterfactual risk obese", "counterfactual risk normal",  "decile")
#colnames(data_plot) = c("observed risk overweight", "counterfactual risk overweight", "observed risk obese", "counterfactual risk obese", "observed risk severe", "counterfactual risk severe", "decile")
data_plot.m <- reshape2::melt(data_plot, id.vars='decile')


#data_plot.m$index_order = c(rep(1,10), rep(2,10), rep(3,10), rep(4,10))
obs_count = factor(c(rep("observed", 6), rep("counterfactual_bmi_20per", 6), rep("counterfactual_bmi_normal", 6)), levels = c("observed","counterfactual_bmi_20per", "counterfactual_bmi_normal"))
status2 = rep("Quintile of risk score", 18)
status2[c(1,7,13)] = c("Overall")
status2 = as.factor(status2)
data_plot.m2 = data_plot.m %>% mutate(state = obs_count, status2 = status2)
p1_RR_pooled_white_heartdisease_10_percent = ggplot(data = data_plot.m2, aes(x=decile, y=value, pattern = state, fill = status2)) +
  geom_bar_pattern(stat = "identity", position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) +
  scale_fill_manual(values = colorRampPalette(c("lightsalmon3","deepskyblue3"))(2)) +
  scale_pattern_manual(values = c(observed = "none", counterfactual_bmi_20per = "stripe", counterfactual_bmi_normal = "weave"), labels = c("Observed","Counterfactual (50% reduction of excess BMI)","Counterfactual (100% reduction in excess BMI)")) +
  guides(pattern = guide_legend(override.aes = list(fill = c("white"))),
         fill = guide_legend(override.aes = list(pattern = "none"))) + 
  theme_bw() + labs(y="Absolute risk of 10-year mortality", x="Risk-score Quintiles") +
  theme(axis.text.x = element_text(color = "black", size = 15, face="bold"),
        axis.text.y = element_text(color = "black", size = 15, face="bold"),  
        axis.title.x = element_text(color = "black", size = 15, face="bold"),
        axis.title.y = element_text(color = "black", size = 15, face="bold"),
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        #strip.text = element_text(size = 15, face = "bold"),
        #legend.box = "none",
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(colour="black", size=15),
        #legend.text.align = 0,
        legend.position = c(0.3,0.8))

p1_RR_pooled_white_heartdisease_10_percent
ggsave("all_heartdisease_mortality_RR_estimates.png", plot = last_plot(), dpi = 300)







#only obese MR cancer disease
nhanes_combined_white = nhanes_combined[which(nhanes_combined$race_ethnicity == "white"), ]

#nhanes_combined_white = nhanes_combined_white %>% filter(bmi < 100)
#nhanes_combined_aa = nhanes_combined[which(nhanes_combined$race == "black"), ]

MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_cancer.csv")
BMI_quantiles = MR_vcov_file$bmi.quant

#Normal : BMI_quantiles[3]- BMI_quantiles[6], Overweight: BMI_quantiles[6]-BMI_quantiles[9], Obese: BMI_quantiles[9]:BMI_quantiles[13], Severe obese: >= BMI_quantiles[13]
delta_HR = exp(MR_vcov_file$est)
#MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[9]-delta_HR[6])/2, delta_HR[9] + (delta_HR[13]-delta_HR[9])/2, delta_HR[13] + (delta_HR[14]-delta_HR[13])/2))
MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[10]-delta_HR[6])/2, delta_HR[10] + (delta_HR[12]-delta_HR[10])/2, delta_HR[12] + (delta_HR[13]-delta_HR[12])/2, delta_HR[13] + (delta_HR[14]-delta_HR[13])/2))

#MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[10]-delta_HR[6])/2, delta_HR[10] + (delta_HR[14]-delta_HR[10])/2))


#---Pooled cohort
#beta_HR_pooled = log(c(1 + (1.07-1)/2, 1.07 + (1.45-1.07)/2, 1.45 + (2.76-1.45)/2))[-1]
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

mid_point_normal = BMI_quantiles[3] + (BMI_quantiles[6] - BMI_quantiles[3])/2
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

#---Removing underweight people--#
nhanes_combined_white = nhanes_combined_white[-which(nhanes_combined_white$bmi_cat == "Underweight"), ]

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
fit_unadjusted = svyglm(mortstat_rec ~ bmi_cat , design=nhanes_svydesign, family = quasibinomial())
fit_full = svyglm(mortstat_rec ~ bmi_cat + sex + age + education + alcohol_status_new + marital_status + smoking_status + no_of_cigarettes, design=nhanes_svydesign, family = quasibinomial())
nhanes_combined_white = nhanes_combined_white %>% filter(bmi_cat == "SObese_grade1" | bmi_cat == "SObese_grade2" | bmi_cat =="SObese_grade3")


#---Relative Risk Matrix
RR = matrix(NA,5,5)
colnames(RR) = c("Normal", "Overweight", "SObese_grade1", "SObese_grade2", "SObese_grade3")
rownames(RR) = c("Normal", "Overweight", "SObese_grade1", "SObese_grade2", "SObese_grade3")
diag(RR) = 1
RR = RR[-1,]
RR[1,1] = exp(fit_full$coefficients[2])
RR[2,1] = exp(fit_full$coefficients[3])
RR[3,1] = exp(fit_full$coefficients[4])
RR[4,1] = exp(fit_full$coefficients[5])
RR[2,2] = exp(fit_full$coefficients[3] - fit_full$coefficients[2])
RR[3,2] = exp(fit_full$coefficients[4] - fit_full$coefficients[2])
RR[4,2] = exp(fit_full$coefficients[5] - fit_full$coefficients[2])
RR[3,3] = exp(fit_full$coefficients[4] - fit_full$coefficients[3])
RR[4,3] = exp(fit_full$coefficients[5] - fit_full$coefficients[3])
RR[4,4] = exp(fit_full$coefficients[5] - fit_full$coefficients[4])

MR_RR = RR
MR_RR[1,1] = exp(MR_estimates_UK[1])
MR_RR[2,1] = exp(MR_estimates_UK[2])
MR_RR[3,1] = exp(MR_estimates_UK[3])
MR_RR[4,1] = exp(MR_estimates_UK[4])
MR_RR[2,2] = exp(MR_estimates_UK[2] - MR_estimates_UK[1])
MR_RR[3,2] = exp(MR_estimates_UK[3] - MR_estimates_UK[1])
MR_RR[4,2] = exp(MR_estimates_UK[4] - MR_estimates_UK[1])
MR_RR[3,3] = exp(MR_estimates_UK[3] - MR_estimates_UK[2])
MR_RR[4,3] = exp(MR_estimates_UK[4] - MR_estimates_UK[2])
MR_RR[4,4] = exp(MR_estimates_UK[4] - MR_estimates_UK[3])

#pooled_RR_estimates = log(c(1.11, 1.44, 1.92, 2.71))
pooled_RR_estimates = readRDS("/Users/prosenjitkundu/Dropbox/Attributable_risk/Absolute_and_attributable_risk/Code/beta_cancer_meta_pooled_6groups.rds")

#pooled_RR_estimates = log(c(1.04 + (1.12-1.04)/2, 1.28, 1.56, 1.86))
pooled_RR = RR
pooled_RR[1,1] = exp(pooled_RR_estimates[1])
pooled_RR[2,1] = exp(pooled_RR_estimates[2])
pooled_RR[3,1] = exp(pooled_RR_estimates[3])
pooled_RR[4,1] = exp(pooled_RR_estimates[4])
pooled_RR[2,2] = exp(pooled_RR_estimates[2] - pooled_RR_estimates[1])
pooled_RR[3,2] = exp(pooled_RR_estimates[3] - pooled_RR_estimates[1])
pooled_RR[4,2] = exp(pooled_RR_estimates[4] - pooled_RR_estimates[1])
pooled_RR[3,3] = exp(pooled_RR_estimates[3] - pooled_RR_estimates[2])
pooled_RR[4,3] = exp(pooled_RR_estimates[4] - pooled_RR_estimates[2])
pooled_RR[4,4] = exp(pooled_RR_estimates[4] - pooled_RR_estimates[3])

RR = rbind(c(1,0,0,0,0), RR)
rownames(RR)[1] = "Normal"
MR_RR = rbind(c(1,0,0,0,0), MR_RR)
rownames(MR_RR)[1] = "Normal"
pooled_RR = rbind(c(1,0,0,0,0), pooled_RR)
rownames(pooled_RR)[1] = "Normal"

data_processed_complete = na.omit(nhanes_combined_white %>% dplyr::select("sex", "age","bmi", "MEC8YR", "mortstat_rec", "bmi_obese", "bmi_cat","education","alcohol_status_new","marital_status", "smoking_status", "no_of_cigarettes", "SDMVPSU", "SDMVSTRA", "bmi_cat_shift"))
#data_processed_complete_without_rf = nhanes_combined_white %>% dplyr::select("sex", "age","bmi", "MEC8YR", "mortstat", "bmi_obese", "bmi_cat","education","alcohol_status_new","marital_status", "smoking_status", "no_of_cigarettes", "SDMVPSU", "SDMVSTRA")
colnames(data_processed_complete)[4] = "sampweight"
data_processed_complete$sampweight = data_processed_complete$sampweight/sum(data_processed_complete$sampweight)
#colnames(data_processed_complete_without_rf)[4] = "sampweight"
ARD_K = matrix(NA,1,5)
ARD_K_pooled = matrix(NA,1,5)
observed_AR_K = matrix(NA,1,5) 
ARD_K_normal = matrix(NA,1,5)
ARD_K_normal_pooled = matrix(NA,1,5)

#data_processed_complete$bmi_cat = relevel(data_processed_complete$bmi_cat, ref = bmi_cat_names[j])
#nhanes_combined_white = nhanes_combined_white %>% filter(bmi_cat == "Overweight"| bmi_cat == "SObese_grade1" | bmi_cat == "SObese_grade2" | bmi_cat =="SObese_grade3")
data_processed_complete$bmi_cat = as.factor(as.character(data_processed_complete$bmi_cat))
hh = model.matrix(~ bmi_cat + sex + age + education + alcohol_status_new + marital_status + smoking_status + no_of_cigarettes + mortstat_rec + bmi_cat_shift, data = data_processed_complete)

#hh = model.matrix(~ bmi_cat + sex + age + education + smoking_status + no_of_cigarettes + alcohol_status_new + marital_status + mortstat_rec + bmi_cat_shift, data = data_processed_complete)
#hh_obese = model.matrix(~ bmi_cat + sex + age + education + smoking_status + no_of_cigarettes + alcohol_status + marital_status, data = data_processed_complete)
hh = cbind(hh, data_processed_complete$sampweight)
#hh_obese = cbind(hh, data_processed_complete$sampweight)
colnames(hh)[21] = "sampweight"
hh_risk_score = as.data.frame(hh) %>% mutate(risk_score = as.numeric(hh[, -c(1:3,17:21)] %*% fit_full$coefficients[-c(1:5)]))
hh_risk_score = hh_risk_score %>% mutate(decile = ntile(risk_score, 5), bmi_cat = as.character(data_processed_complete$bmi_cat), bmi_cat_shift = as.character(data_processed_complete$bmi_cat_shift))
data_processed_complete = data_processed_complete %>% mutate(risk_score = hh_risk_score$risk_score) %>% mutate(decile = ntile(risk_score, 5))
#colnames(data_processed_complete)[4] = "sampweight"
#prop_exposure_deciles = matrix(NA, 10, 4)
weight.mean = function(x,w)
{
  return(sum(x*w))
}
group = list()

baseline_prob = rep(0, length(fit_full$coefficients[2:5]) +1)
bmi_coeff = c(0, fit_full$coefficients[2:5])
names(bmi_coeff) = c("bmi_catNormal", names(fit_full$coefficients[2:5]))
#baseline_prob = locfit::expit(fit_full$coefficients[1] + bmi_coeff)


for(i in 1:5)
{
  temp = hh_risk_score[which(hh_risk_score$decile == i), ]
  n_temp = nrow(temp)
  
  for(l in 1:length(baseline_prob))
  {
    baseline_prob[l] = weighted.mean(locfit::expit(fit_full$coefficients[1] + bmi_coeff[l] + temp$risk_score), w = temp$sampweight)
  }
  names(baseline_prob) = str_replace(names(bmi_coeff), "bmi_cat", "")
  
  AR_C = 0
  AR_C_pooled = 0
  propbmi.table = table(temp$bmi_cat, temp$bmi_cat_shift)/n_temp
  #combinations = cbind(combinations, bmi_cat_name = c("SObese_grade1", "SObese_grade2", "SObese_grade3"))
  
  for(m in 1:nrow(propbmi.table))
  {
    for(n in 1:min(m, ncol(propbmi.table)))
    {
      index_row_RR = which(rownames(RR) %in% rownames(propbmi.table)[m] == T)
      index_col_RR = which(colnames(RR) %in% colnames(propbmi.table)[n] == T)
      rr = RR[index_row_RR, index_col_RR]
      mr = MR_RR[index_row_RR, index_col_RR]
      pooled_rr = pooled_RR[index_row_RR, index_col_RR]
      index_baseline_prob = which(names(baseline_prob) %in% colnames(propbmi.table)[n] == T)
      AR_C = AR_C + propbmi.table[m,n]*(rr/mr)*baseline_prob[index_baseline_prob]
      #print(AR_C)
      print(n)
      print(m)
      AR_C_pooled = AR_C_pooled + propbmi.table[m,n]*(rr/pooled_rr)*baseline_prob[index_baseline_prob]
    }
    #print(k)
    
    
  }
  
  index_full_model_coeff = as.numeric(na.omit(match(colnames(temp)[-c(17:25)], names(fit_full$coefficients)[1:5])))[-1]
  observed_AR = weighted.mean(locfit::expit(as.matrix(temp[, grep("bmi", colnames(temp)[-c(17:25)])]) %*% fit_full$coefficients[index_full_model_coeff]  + temp$risk_score + fit_full$coefficients[1]), w = temp$sampweight)
  observed_AR_normal = weighted.mean(locfit::expit(temp$risk_score + fit_full$coefficients[1]), w = temp$sampweight)
  
  print(wpct(temp$bmi_cat,weight = temp$sampweight))
  ARD_K_normal[1,i] = observed_AR_normal*sum(wpct(temp$bmi_cat,weight = temp$sampweight)*(RR[3:5,1]/MR_RR[3:5,1]))
  ARD_K_normal_pooled[1,i] = observed_AR_normal*sum(wpct(temp$bmi_cat,weight = temp$sampweight)*(RR[3:5,1]/pooled_RR[3:5,1]))
  
  ARD_K[1,i] = AR_C
  ARD_K_pooled[1,i] = AR_C_pooled
  observed_AR_K[1,i] = observed_AR
  print(i)
}

#---collapsing over stratum

temp = hh_risk_score

ARD_K_collapsed = sum(ARD_K[1,])/5
#ARD_K_normal_collapsed = sum(quintile_proportions*ARD_K_normal[1,])
ARD_K_normal_collapsed = sum(ARD_K_normal[1,])/5

observed_AR_K_collapsed = locfit::expit(fit_unadjusted$coefficients[1])
hh_risk_score_obese = as.data.frame(model.matrix(~ bmi_cat + risk_score + sampweight, data = hh_risk_score))
hh_risk_score_obese$bmi_catSObese_grade1 = 0
hh_risk_score_obese$bmi_catSObese_grade1[which(hh_risk_score_obese$bmi_catSObese_grade2 == 0 & hh_risk_score_obese$bmi_catSObese_grade3 == 0)] =1
hh_risk_score_obese = hh_risk_score_obese[, c(1,6,2:5)]
observed_AR_K_collapsed = weighted.mean(locfit::expit(fit_full$coefficients[1] + as.matrix(hh_risk_score_obese[, 2:4])%*%fit_full$coefficients[3:5] + hh_risk_score_obese$risk_score), w = hh_risk_score_obese$sampweight)


AR_C_obese = c(ARD_K[1,],ARD_K_collapsed)
AR_C_normal = c(ARD_K_normal[1,],ARD_K_normal_collapsed)
#AR_C_severe = ARD_K[3,]
AR_O_obese = c(observed_AR_K[1,], observed_AR_K_collapsed)
#AR_O_severe = observed_AR_K[3,]
data_plot = as.data.frame(cbind(AR_O_obese, AR_C_obese, AR_C_normal))
data_plot = data_plot[c(6,1:5),]
#data_plot = rbind(data_plot, c(0, 0, 0))
#data_plot = as.data.frame(cbind(AR_O_overweight, AR_C_overweight,  AR_O_obese, AR_C_obese, AR_O_severe, AR_C_severe))
data_plot$decile = c("Overall", rep(paste0("Q", seq(1,5,1))))
data_plot$decile <- factor(data_plot$decile, levels = c("Overall", rep(paste0("Q", seq(1,5,1)))))
colnames(data_plot) = c("observed risk obese", "counterfactual risk obese", "counterfactual risk normal",  "decile")
#colnames(data_plot) = c("observed risk overweight", "counterfactual risk overweight", "observed risk obese", "counterfactual risk obese", "observed risk severe", "counterfactual risk severe", "decile")

data_plot.m <- reshape2::melt(data_plot, id.vars='decile')

#data_plot.m.total = data_plot.m[c(6,12,18), ]

#data_plot.m.wo.total = data_plot.m[-c(6,12,18),]

#data_plot.m$index_order = c(rep(1,10), rep(2,10), rep(3,10), rep(4,10))
obs_count = factor(c(rep("observed", 6), rep("counterfactual_bmi_20per", 6), rep("counterfactual_bmi_normal", 6)), levels = c("observed","counterfactual_bmi_20per", "counterfactual_bmi_normal"))
status2 = rep("Quintile of risk score", 18)
status2[c(1,7,13)] = c("Overall")
status2 = as.factor(status2)
#bmi = factor(c(rep("overweight (25.7-30.7 Kg/m*2)", 10), rep("overweight (25.7-30.7 Kg/m*2)", 10), rep("obese (>30.7 Kg/m*2)", 10), rep("obese (>30.7 Kg/m*2)", 10)), levels = c("overweight (25.7-30.7 Kg/m*2)","obese (>30.7 Kg/m*2)"))
data_plot.m2 = data_plot.m %>% mutate(state = obs_count, status2 = status2)
p1_MR_white_cancer_10_percent = ggplot(data = data_plot.m2, aes(x=decile, y=value, pattern = state, fill = status2)) +
  geom_bar_pattern(stat = "identity", position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) +
  scale_fill_manual(values = colorRampPalette(c("lightsalmon3","deepskyblue3"))(2)) +
  scale_pattern_manual(values = c(observed = "none", counterfactual_bmi_20per = "stripe", counterfactual_bmi_normal = "weave"), labels = c("Observed","Counterfactual (50% reduction of excess BMI)","Counterfactual (100% reduction in excess BMI)")) +
  guides(pattern = guide_legend(override.aes = list(fill = c("white"))),
         fill = guide_legend(override.aes = list(pattern = "none"))) + 
  theme_bw() + labs(y="Absolute risk of 10-year mortality", x="Risk-score Quintiles") +
  theme(axis.text.x = element_text(color = "black", size = 15, face="bold"),
        axis.text.y = element_text(color = "black", size = 15, face="bold"),  
        axis.title.x = element_text(color = "black", size = 15, face="bold"),
        axis.title.y = element_text(color = "black", size = 15, face="bold"),
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        #strip.text = element_text(size = 15, face = "bold"),
        #legend.box = "none",
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(colour="black", size=15),
        #legend.text.align = 0,
        legend.position = c(0.3,0.8))

p1_MR_white_cancer_10_percent
ggsave("all_cancer_mortality_MR_estimates.png", plot = last_plot(), dpi = 300)

ARD_K_pooled_collapsed = sum(ARD_K_pooled[1,])/5
ARD_K_normal_pooled_collapsed = sum(ARD_K_normal_pooled[1,])/5

AR_C_obese = c(ARD_K_pooled[1,],ARD_K_pooled_collapsed)
AR_C_normal = c(ARD_K_normal_pooled[1,],ARD_K_normal_pooled_collapsed)
#AR_C_severe = ARD_K[3,]
AR_O_obese = c(observed_AR_K[1,], observed_AR_K_collapsed)
#AR_O_severe = observed_AR_K[3,]
data_plot = as.data.frame(cbind(AR_O_obese, AR_C_obese, AR_C_normal))
data_plot = data_plot[c(6,1:5),]
#data_plot = as.data.frame(cbind(AR_O_overweight, AR_C_overweight,  AR_O_obese, AR_C_obese, AR_O_severe, AR_C_severe))
data_plot$decile = c("Overall", rep(paste0("Q", seq(1,5,1))))
data_plot$decile <- factor(data_plot$decile, levels = c("Overall", rep(paste0("Q", seq(1,5,1)))))
colnames(data_plot) = c("observed risk obese", "counterfactual risk obese", "counterfactual risk normal",  "decile")
#colnames(data_plot) = c("observed risk overweight", "counterfactual risk overweight", "observed risk obese", "counterfactual risk obese", "observed risk severe", "counterfactual risk severe", "decile")
data_plot.m <- reshape2::melt(data_plot, id.vars='decile')


#data_plot.m$index_order = c(rep(1,10), rep(2,10), rep(3,10), rep(4,10))
obs_count = factor(c(rep("observed", 6), rep("counterfactual_bmi_20per", 6), rep("counterfactual_bmi_normal", 6)), levels = c("observed","counterfactual_bmi_20per", "counterfactual_bmi_normal"))
status2 = rep("Quintile of risk score", 18)
status2[c(1,7,13)] = c("Overall")
status2 = as.factor(status2)
data_plot.m2 = data_plot.m %>% mutate(state = obs_count, status2 = status2)
p1_RR_pooled_white_cancer_10_percent = ggplot(data = data_plot.m2, aes(x=decile, y=value, pattern = state, fill = status2)) +
  geom_bar_pattern(stat = "identity", position = position_dodge(preserve = "single"),
                   color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) +
  scale_fill_manual(values = colorRampPalette(c("lightsalmon3","deepskyblue3"))(2)) +
  scale_pattern_manual(values = c(observed = "none", counterfactual_bmi_20per = "stripe", counterfactual_bmi_normal = "weave"), labels = c("Observed","Counterfactual (50% reduction of excess BMI)","Counterfactual (100% reduction in excess BMI)")) +
  guides(pattern = guide_legend(override.aes = list(fill = c("white"))),
         fill = guide_legend(override.aes = list(pattern = "none"))) + 
  theme_bw() + labs(y="Absolute risk of 10-year mortality", x="Risk-score Quintiles") +
  theme(axis.text.x = element_text(color = "black", size = 15, face="bold"),
        axis.text.y = element_text(color = "black", size = 15, face="bold"),  
        axis.title.x = element_text(color = "black", size = 15, face="bold"),
        axis.title.y = element_text(color = "black", size = 15, face="bold"),
        plot.title = element_text(color = "black", size = 15, face = "bold"),
        #strip.text = element_text(size = 15, face = "bold"),
        #legend.box = "none",
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(colour="black", size=15),
        #legend.text.align = 0,
        legend.position = c(0.3,0.8))



p1_RR_pooled_white_cancer_10_percent
ggsave("all_cancer_mortality_RR_estimates.png", plot = last_plot(), dpi = 300)




