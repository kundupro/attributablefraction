MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_fullsample.csv")
BMI_quantiles = MR_vcov_file$bmi.quant

nhis_ipums_2000_2005 = fread("~/Downloads/nhis_00002.csv")
#---Selecting individuals who are sample adults---#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$ASTATFLG == 1), ]


#--- Including with mortstat eligibe status == 1 ---#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$MORTELIG == 1), ]


#Removing individuals whose age is ceiled at 85 years
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$AGE < 85), ]

#----Removing unknown BMI----#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[-which(nhis_ipums_2000_2005$BMI == 99.99), ]


#---Keeping white individuals---#
nhis_ipums_2000_2005  = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$RACENEW == 100), ]




nhis_ipums_2000_2005$MORTDODQ[which(nhis_ipums_2000_2005$MORTDODQ == 9)] = NA

nhis_ipums_2000_2005$MORTDODY[which(nhis_ipums_2000_2005$MORTDODY == 9999)] = NA
#Rmoving those individuals with unknown death data
if(length(nhis_ipums_2000_2005$MORTUCODLD == 96 & nhis_ipums_2000_2005$MORTSTAT == 1) > 0)
{
  nhis_ipums_2000_2005 = nhis_ipums_2000_2005[-which(nhis_ipums_2000_2005$MORTUCODLD == 96 & nhis_ipums_2000_2005$MORTSTAT == 1), ]
}
#---Redefining morstat for 10 years follow-up---#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005  %>% mutate(mortstat_rec = case_when(MORTSTAT == 2 ~ 0,
                                                                                 YEAR == 2000 & MORTDODY <= 2010 ~ 1,
                                                                                 YEAR == 2000 & MORTDODY > 2010 ~ 0,
                                                                                 YEAR == 2001 & MORTDODY <= 2011 ~ 1,
                                                                                 YEAR == 2001 & MORTDODY > 2011 ~ 0,
                                                                                 YEAR == 2002 & MORTDODY <= 2012 ~ 1,
                                                                                 YEAR == 2002 & MORTDODY > 2012 ~ 0,
                                                                                 YEAR == 2003 & MORTDODY <= 2013 ~ 1,
                                                                                 YEAR == 2003 & MORTDODY > 2013 ~ 0,
                                                                                 YEAR == 2004 & MORTDODY <= 2014 ~ 1,
                                                                                 YEAR == 2004 & MORTDODY > 2014 ~ 0,
                                                                                 YEAR == 2005 & MORTDODY <= 2015 ~ 1,
                                                                                 YEAR == 2005 & MORTDODY > 2015 ~ 0))
length(which(is.na(nhis_ipums_2000_2005$MORTDODY) == T & nhis_ipums_2000_2005$MORTSTAT == 1))
#0
length(which(is.na(nhis_ipums_2000_2005$MORTDODY) == F & nhis_ipums_2000_2005$MORTSTAT == 2))
#0
#nhis_ipums_2000_2005$mortstat_rec[which(nhis_ipums_2000_2005$MORTSTAT == 1 & is.na(nhis_ipums_2000_2005$MORTDODY) == T)] = 1


nhis_ipums_2000_2005 = nhis_ipums_2000_2005  %>% mutate(censoring_date = case_when(YEAR == 2000 ~ "12-31-2010",
                                                                                   YEAR == 2001 ~ "12-31-2011",
                                                                                   YEAR == 2002 ~ "12-31-2012",
                                                                                   YEAR == 2003 ~ "12-31-2013",
                                                                                   YEAR == 2004 ~ "12-31-2014",
                                                                                   YEAR == 2005 ~ "12-31-2015"))
nhis_ipums_2000_2005$censoring_date = mdy(nhis_ipums_2000_2005$censoring_date)


nhis_ipums_2000_2005 = nhis_ipums_2000_2005  %>% mutate(MORTDODM = case_when(MORTDODQ == 1 ~ "02",
                                                                             MORTDODQ == 2 ~ "05",
                                                                             MORTDODQ == 3 ~ "08",
                                                                             MORTDODQ == 4 ~ "11"))


nhis_ipums_2000_2005$date_of_death = paste0("15-", nhis_ipums_2000_2005$MORTDODM, "-", nhis_ipums_2000_2005$MORTDODY) 
nhis_ipums_2000_2005$date_of_death[grep("NA", nhis_ipums_2000_2005$date_of_death)] = NA
nhis_ipums_2000_2005$date_of_death = dmy(nhis_ipums_2000_2005$date_of_death)

nhis_ipums_2000_2005$BIRTHMO[which(nhis_ipums_2000_2005$BIRTHMO >= 97)] = NA
nhis_ipums_2000_2005$BIRTHYR[which(nhis_ipums_2000_2005$BIRTHYR >= 9997)] = NA
nhis_ipums_2000_2005$date_of_birth = paste0("15-", str_pad(nhis_ipums_2000_2005$BIRTHMO, 2, pad ="0"), "-", nhis_ipums_2000_2005$BIRTHYR) 
nhis_ipums_2000_2005$date_of_birth[grep("NA", nhis_ipums_2000_2005$date_of_birth)] = NA
nhis_ipums_2000_2005$date_of_birth = dmy(nhis_ipums_2000_2005$date_of_birth)

nhis_ipums_2000_2005 = nhis_ipums_2000_2005  %>% mutate(age_at_exit = if_else(date_of_death < censoring_date, (date_of_death-date_of_birth)/365, (censoring_date-date_of_birth)/365))

all.equal(sum(unique(nhis_ipums_2000_2005$mortstat_rec[which(is.na(nhis_ipums_2000_2005$date_of_death) == T)])), 0)

nhis_ipums_2000_2005$age_at_exit[which(is.na(nhis_ipums_2000_2005$date_of_death) == T)] = (nhis_ipums_2000_2005$censoring_date[which(is.na(nhis_ipums_2000_2005$date_of_death) == T)] - nhis_ipums_2000_2005$date_of_birth[which(is.na(nhis_ipums_2000_2005$date_of_death) == T)])/365





nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(bmi_cat = case_when(BMI < BMI_quantiles[3] ~ "Underweight",
                                                                           BMI >= BMI_quantiles[3] & BMI < BMI_quantiles[6] ~ "Normal",
                                                                           BMI >= BMI_quantiles[6] & BMI < BMI_quantiles[8] ~ "ObeseI",
                                                                           BMI >= BMI_quantiles[8] & BMI < BMI_quantiles[10] ~ "ObeseII",
                                                                           BMI >= BMI_quantiles[10] & BMI < BMI_quantiles[12] ~ "ObeseIII",
                                                                           BMI >= BMI_quantiles[12] & BMI < BMI_quantiles[13] ~ "ObeseIV",
                                                                           BMI >= BMI_quantiles[13] ~ "ObeseV"))


#---Removing underweight people--#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[-which(nhis_ipums_2000_2005$bmi_cat == "Underweight"), ]




#alcohol
nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(alcohol_status = case_when(ALCSTAT1 == 1 ~ "Never",
                                                                                  ALCSTAT1 == 2 ~ "Former",
                                                                                  ALCSTAT1 == 3 ~ "Current"))


#Education

nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(education = case_when(EDUC >= 100 & EDUC < 200 ~ "less_than_high_school",
                                                                             EDUC >= 200 & EDUC <= 202 ~ "high_school",
                                                                             EDUC > 202 & EDUC <= 504 ~ "college_graduate"))

#Smoking
#nhis_ipums_2000_2005 = nhis_ipums_2000_2005[-which(nhis_ipums_2000_2005$SMOKESTATUS2 == 40 | nhis_ipums_2000_2005$SMOKESTATUS2 == 90), ]
nhis_ipums_2000_2005= nhis_ipums_2000_2005 %>% mutate(smoking_status = case_when(SMOKESTATUS2 == 30 ~ "Never",
                                                                                 SMOKESTATUS2 == 20 ~ "Former",
                                                                                 SMOKESTATUS2 == 11 | SMOKESTATUS2 == 12 ~ "Current"))

#Marital Status
nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(marital_status = case_when(MARST >= 10 & MARST <= 13 ~ "married",
                                                                                  MARST == 20 | MARST == 30 | MARST == 40 ~ "divorce",
                                                                                  MARST == 50 ~ "never_married"))
colnames(nhis_ipums_2000_2005) = tolower(colnames(nhis_ipums_2000_2005))
data_processed = nhis_ipums_2000_2005 %>% dplyr::select("mortstat_rec", "bmi_cat", "age", "sex", "education", "smoking_status",
                                                        "strata", "psu", "sampweight", "alcohol_status", "marital_status", "year", "mortucodld")  
#Keeping White, Black/AfricanAmerican,  American_Indian/Alaskan_Native, Asian
#data_processed = data_processed[which(data_processed$racenew <= 400), ]
#data_processed = data_processed  %>% mutate(racenew = case_when(racenew == 100 ~ "White",
 #                                                               racenew == 200 ~ "African_american",
  #                                                              racenew == 300 ~ "AI_alaskan_native",
   #                                                             racenew == 400 ~ "Asian"))
#bmi_quantile = quantile(data_processed$bmi, probs = seq(0, 1, 0.01))

#data_processed = data_processed %>% mutate(bmi_cat = case_when(bmi < bmi_quantile[11] ~ "Underweight",
 #                                                              bmi >= bmi_quantile[11] & bmi < bmi_quantile[41] ~ "Normal",
  #                                                             bmi >= bmi_quantile[41] & bmi < bmi_quantile[61] ~ "ObeseI",
   #                                                            bmi >= bmi_quantile[61] & bmi < bmi_quantile[81] ~ "ObeseII",
    #                                                           bmi >= bmi_quantile[81] & bmi < bmi_quantile[96] ~ "ObeseIII",
     #                                                          bmi >= bmi_quantile[96] & bmi < bmi_quantile[100] ~ "ObeseIV",
      #                                                         bmi >= bmi_quantile[100] ~ "ObeseV"))
myVars = c("bmi_cat", "age", "sex", "education", "smoking_status", "alcohol_status", "marital_status")
catVars = c("sex", "bmi_cat", "education", "smoking_status", "alcohol_status", "marital_status")
#data_processed$sampweight = data_processed$sampweight/(sum(data_processed$sampweight))
#options(survey.adjust.domain.lonely=TRUE)
#options(survey.lonely.psu="adjust")
#data_svy = svydesign(id = ~0, data = data_processed, weights = ~sampweight)

tab3 <- CreateTableOne(vars = myVars, strata = "mortstat_rec" , data = data_processed, factorVars = catVars)


#--heart disease
MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_cardiovascular.csv")
BMI_quantiles = MR_vcov_file$bmi.quant

#MR_estimates_UK = log(c(1.09, 1.195, 2.065, 2.915))

nhis_ipums_2000_2005 = fread("~/Downloads/nhis_00002.csv")
#---Selecting individuals who are sample adults---#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$ASTATFLG == 1), ]


#---Keeping AA individuals---#
nhis_ipums_2000_2005  = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$RACENEW == 100), ]


#----Removing unknown BMI----#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[-which(nhis_ipums_2000_2005$BMI == 99.99), ]


#--- Including with mortstat eligibe status == 1 ---#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$MORTELIG == 1), ]


#Removing individuals whose age is ceiled at 85 years
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$AGE < 85), ]

nhis_ipums_2000_2005$MORTDODQ[which(nhis_ipums_2000_2005$MORTDODQ == 9)] = NA

nhis_ipums_2000_2005$MORTDODY[which(nhis_ipums_2000_2005$MORTDODY == 9999)] = NA

#Rmoving those individuals with unknown death data
if(length(which(nhis_ipums_2000_2005$MORTUCODLD == 96 & nhis_ipums_2000_2005$MORTSTAT == 1)) > 0)
{
  nhis_ipums_2000_2005 = nhis_ipums_2000_2005[-which(nhis_ipums_2000_2005$MORTUCODLD == 96 & nhis_ipums_2000_2005$MORTSTAT == 1), ]
}
#---Redefining morstat for 10 years follow-up---#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005  %>% mutate(mortstat_rec = case_when(MORTSTAT == 2 ~ 0,
                                                                                 YEAR == 2000 & MORTDODY <= 2010 & MORTUCODLD == 1 ~ 1,
                                                                                 YEAR == 2000 & MORTDODY <= 2010 & MORTUCODLD != 1 ~ 0,
                                                                                 YEAR == 2000 & MORTDODY > 2010 ~ 0,
                                                                                 YEAR == 2001 & MORTDODY <= 2011 & MORTUCODLD == 1 ~ 1,
                                                                                 YEAR == 2001 & MORTDODY <= 2011 & MORTUCODLD != 1 ~ 0,
                                                                                 YEAR == 2001 & MORTDODY > 2011 ~ 0,
                                                                                 YEAR == 2002 & MORTDODY <= 2012 & MORTUCODLD == 1 ~ 1,
                                                                                 YEAR == 2002 & MORTDODY <= 2012 & MORTUCODLD != 1 ~ 0,
                                                                                 YEAR == 2002 & MORTDODY > 2012 ~ 0,
                                                                                 YEAR == 2003 & MORTDODY <= 2013 & MORTUCODLD == 1 ~ 1,
                                                                                 YEAR == 2003 & MORTDODY <= 2013 & MORTUCODLD != 1 ~ 0,
                                                                                 YEAR == 2003 & MORTDODY > 2013 ~ 0,
                                                                                 YEAR == 2004 & MORTDODY <= 2014 & MORTUCODLD == 1 ~ 1,
                                                                                 YEAR == 2004 & MORTDODY <= 2014 & MORTUCODLD != 1 ~ 0,
                                                                                 YEAR == 2004 & MORTDODY > 2014 ~ 0,
                                                                                 YEAR == 2005 & MORTDODY <= 2015 & MORTUCODLD == 1 ~ 1,
                                                                                 YEAR == 2005 & MORTDODY <= 2015 & MORTUCODLD != 1 ~ 0,
                                                                                 YEAR == 2005 & MORTDODY > 2015 ~ 0))
length(which(is.na(nhis_ipums_2000_2005$MORTDODY) == T & nhis_ipums_2000_2005$MORTSTAT == 1))
#0
length(which(is.na(nhis_ipums_2000_2005$MORTDODY) == F & nhis_ipums_2000_2005$MORTSTAT == 2))
#0
#nhis_ipums_2000_2005$mortstat_rec[which(nhis_ipums_2000_2005$MORTSTAT == 1 & is.na(nhis_ipums_2000_2005$MORTDODY) == T)] = 1


nhis_ipums_2000_2005 = nhis_ipums_2000_2005  %>% mutate(censoring_date = case_when(YEAR == 2000 ~ "12-31-2010",
                                                                                   YEAR == 2001 ~ "12-31-2011",
                                                                                   YEAR == 2002 ~ "12-31-2012",
                                                                                   YEAR == 2003 ~ "12-31-2013",
                                                                                   YEAR == 2004 ~ "12-31-2014",
                                                                                   YEAR == 2005 ~ "12-31-2015"))
nhis_ipums_2000_2005$censoring_date = mdy(nhis_ipums_2000_2005$censoring_date)


nhis_ipums_2000_2005 = nhis_ipums_2000_2005  %>% mutate(MORTDODM = case_when(MORTDODQ == 1 ~ "02",
                                                                             MORTDODQ == 2 ~ "05",
                                                                             MORTDODQ == 3 ~ "08",
                                                                             MORTDODQ == 4 ~ "11"))


nhis_ipums_2000_2005$date_of_death = paste0("15-", nhis_ipums_2000_2005$MORTDODM, "-", nhis_ipums_2000_2005$MORTDODY) 
nhis_ipums_2000_2005$date_of_death[grep("NA", nhis_ipums_2000_2005$date_of_death)] = NA
nhis_ipums_2000_2005$date_of_death = dmy(nhis_ipums_2000_2005$date_of_death)

nhis_ipums_2000_2005$BIRTHMO[which(nhis_ipums_2000_2005$BIRTHMO >= 97)] = NA
nhis_ipums_2000_2005$BIRTHYR[which(nhis_ipums_2000_2005$BIRTHYR >= 9997)] = NA
nhis_ipums_2000_2005$date_of_birth = paste0("15-", str_pad(nhis_ipums_2000_2005$BIRTHMO, 2, pad ="0"), "-", nhis_ipums_2000_2005$BIRTHYR) 
nhis_ipums_2000_2005$date_of_birth[grep("NA", nhis_ipums_2000_2005$date_of_birth)] = NA
nhis_ipums_2000_2005$date_of_birth = dmy(nhis_ipums_2000_2005$date_of_birth)

nhis_ipums_2000_2005 = nhis_ipums_2000_2005  %>% mutate(age_at_exit = if_else(date_of_death < censoring_date, (date_of_death-date_of_birth)/365, (censoring_date-date_of_birth)/365))

all.equal(sum(unique(nhis_ipums_2000_2005$mortstat_rec[which(is.na(nhis_ipums_2000_2005$date_of_death) == T)])), 0)

nhis_ipums_2000_2005$age_at_exit[which(is.na(nhis_ipums_2000_2005$date_of_death) == T)] = (nhis_ipums_2000_2005$censoring_date[which(is.na(nhis_ipums_2000_2005$date_of_death) == T)] - nhis_ipums_2000_2005$date_of_birth[which(is.na(nhis_ipums_2000_2005$date_of_death) == T)])/365





nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(bmi_cat = case_when(BMI < BMI_quantiles[3] ~ "Underweight",
                                                                           BMI >= BMI_quantiles[3] & BMI < BMI_quantiles[6] ~ "Normal",
                                                                           BMI >= BMI_quantiles[6] & BMI < BMI_quantiles[8] ~ "ObeseI",
                                                                           BMI >= BMI_quantiles[8] & BMI < BMI_quantiles[10] ~ "ObeseII",
                                                                           BMI >= BMI_quantiles[10] & BMI < BMI_quantiles[12] ~ "ObeseIII",
                                                                           BMI >= BMI_quantiles[12] & BMI < BMI_quantiles[13]~ "ObeseIV",
                                                                           BMI >= BMI_quantiles[13] ~ "ObeseV"))

#---Removing underweight people--#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[-which(nhis_ipums_2000_2005$bmi_cat == "Underweight"), ]




#alcohol
nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(alcohol_status = case_when(ALCSTAT1 == 1 ~ "Never",
                                                                                  ALCSTAT1 == 2 ~ "Former",
                                                                                  ALCSTAT1 == 3 ~ "Current"))


#Education

nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(education = case_when(EDUC >= 100 & EDUC < 200 ~ "less_than_high_school",
                                                                             EDUC >= 200 & EDUC <= 202 ~ "high_school",
                                                                             EDUC > 202 & EDUC <= 504 ~ "college_graduate"))

#Smoking
#nhis_ipums_2000_2005 = nhis_ipums_2000_2005[-which(nhis_ipums_2000_2005$SMOKESTATUS2 == 40 | nhis_ipums_2000_2005$SMOKESTATUS2 == 90), ]
nhis_ipums_2000_2005= nhis_ipums_2000_2005 %>% mutate(smoking_status = case_when(SMOKESTATUS2 == 30 ~ "Never",
                                                                                 SMOKESTATUS2 == 20 ~ "Former",
                                                                                 SMOKESTATUS2 == 11 | SMOKESTATUS2 == 12 ~ "Current"))

#Marital Status
nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(marital_status = case_when(MARST >= 10 & MARST <= 13 ~ "married",
                                                                                  MARST == 20 | MARST == 30 | MARST == 40 ~ "divorce",
                                                                                  MARST == 50 ~ "never_married"))

#nhis_ipums_2000_2005 = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$MORTUCODLD == 1), ]
colnames(nhis_ipums_2000_2005) = tolower(colnames(nhis_ipums_2000_2005))


#data_processed = nhis_ipums_2000_2005 %>% dplyr::select("mortstat_rec", "bmi_cat", "age", "sex", "education", "smoking_status",
 #                                                       "strata", "psu", "sampweight", "alcohol_status", "marital_status", "age_at_exit", "year")  
data_processed = nhis_ipums_2000_2005 %>% dplyr::select("mortstat_rec", "bmi_cat", "age", "sex", "education", "smoking_status",
                                                        "strata", "psu", "sampweight", "alcohol_status", "marital_status", "year", "mortucodld")
myVars = c("bmi_cat", "age", "sex", "education", "smoking_status", "alcohol_status", "marital_status")
catVars = c("sex", "bmi_cat", "education", "smoking_status", "alcohol_status", "marital_status")
tab_heart <- CreateTableOne(vars = myVars, strata = "mortstat_rec" , data = data_processed, factorVars = catVars)

MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_cancer.csv")
BMI_quantiles = MR_vcov_file$bmi.quant
delta_HR = exp(MR_vcov_file$est)
#MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[9]-delta_HR[6])/2, delta_HR[9] + (delta_HR[11]-delta_HR[9])/2, delta_HR[11] + (delta_HR[13]-delta_HR[11])/2, delta_HR[13] + (delta_HR[14]-delta_HR[13])/2))
MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[8]-delta_HR[6])/2, delta_HR[8] + (delta_HR[10]-delta_HR[8])/2, delta_HR[10] + (delta_HR[12]-delta_HR[10])/2, delta_HR[12] + (delta_HR[13]-delta_HR[12])/2, delta_HR[13] + (delta_HR[14]-delta_HR[13])/2))

#MR_estimates_UK = log(c(1.09, 1.195, 2.065, 2.915))

nhis_ipums_2000_2005 = fread("~/Downloads/nhis_00002.csv")
#---Selecting individuals who are sample adults---#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$ASTATFLG == 1), ]


#---Keeping white individuals---#
nhis_ipums_2000_2005  = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$RACENEW == 100), ]


#----Removing unknown BMI----#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[-which(nhis_ipums_2000_2005$BMI == 99.99), ]


#--- Including with mortstat eligibe status == 1 ---#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$MORTELIG == 1), ]


#Removing individuals whose age is ceiled at 85 years
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$AGE < 85), ]

nhis_ipums_2000_2005$MORTDODQ[which(nhis_ipums_2000_2005$MORTDODQ == 9)] = NA

nhis_ipums_2000_2005$MORTDODY[which(nhis_ipums_2000_2005$MORTDODY == 9999)] = NA

#Rmoving those individuals with unknown death data
if(length(which(nhis_ipums_2000_2005$MORTUCODLD == 96 & nhis_ipums_2000_2005$MORTSTAT == 1)) > 0)
{
  nhis_ipums_2000_2005 = nhis_ipums_2000_2005[-which(nhis_ipums_2000_2005$MORTUCODLD == 96 & nhis_ipums_2000_2005$MORTSTAT == 1), ]
}
#---Redefining morstat for 10 years follow-up---#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005  %>% mutate(mortstat_rec = case_when(MORTSTAT == 2 ~ 0,
                                                                                 YEAR == 2000 & MORTDODY <= 2010 & MORTUCODLD == 2 ~ 1,
                                                                                 YEAR == 2000 & MORTDODY <= 2010 & MORTUCODLD != 2 ~ 0,
                                                                                 YEAR == 2000 & MORTDODY > 2010 ~ 0,
                                                                                 YEAR == 2001 & MORTDODY <= 2011 & MORTUCODLD == 2 ~ 1,
                                                                                 YEAR == 2001 & MORTDODY <= 2011 & MORTUCODLD != 2 ~ 0,
                                                                                 YEAR == 2001 & MORTDODY > 2011 ~ 0,
                                                                                 YEAR == 2002 & MORTDODY <= 2012 & MORTUCODLD == 2 ~ 1,
                                                                                 YEAR == 2002 & MORTDODY <= 2012 & MORTUCODLD != 2 ~ 0,
                                                                                 YEAR == 2002 & MORTDODY > 2012 ~ 0,
                                                                                 YEAR == 2003 & MORTDODY <= 2013 & MORTUCODLD == 2 ~ 1,
                                                                                 YEAR == 2003 & MORTDODY <= 2013 & MORTUCODLD != 2 ~ 0,
                                                                                 YEAR == 2003 & MORTDODY > 2013 ~ 0,
                                                                                 YEAR == 2004 & MORTDODY <= 2014 & MORTUCODLD == 2 ~ 1,
                                                                                 YEAR == 2004 & MORTDODY <= 2014 & MORTUCODLD != 2 ~ 0,
                                                                                 YEAR == 2004 & MORTDODY > 2014 ~ 0,
                                                                                 YEAR == 2005 & MORTDODY <= 2015 & MORTUCODLD == 2 ~ 1,
                                                                                 YEAR == 2005 & MORTDODY <= 2015 & MORTUCODLD != 2 ~ 0,
                                                                                 YEAR == 2005 & MORTDODY > 2015 ~ 0))
length(which(is.na(nhis_ipums_2000_2005$MORTDODY) == T & nhis_ipums_2000_2005$MORTSTAT == 1))
#0
length(which(is.na(nhis_ipums_2000_2005$MORTDODY) == F & nhis_ipums_2000_2005$MORTSTAT == 2))
#0
#nhis_ipums_2000_2005$mortstat_rec[which(nhis_ipums_2000_2005$MORTSTAT == 1 & is.na(nhis_ipums_2000_2005$MORTDODY) == T)] = 1


nhis_ipums_2000_2005 = nhis_ipums_2000_2005  %>% mutate(censoring_date = case_when(YEAR == 2000 ~ "12-31-2010",
                                                                                   YEAR == 2001 ~ "12-31-2011",
                                                                                   YEAR == 2002 ~ "12-31-2012",
                                                                                   YEAR == 2003 ~ "12-31-2013",
                                                                                   YEAR == 2004 ~ "12-31-2014",
                                                                                   YEAR == 2005 ~ "12-31-2015"))
nhis_ipums_2000_2005$censoring_date = mdy(nhis_ipums_2000_2005$censoring_date)


nhis_ipums_2000_2005 = nhis_ipums_2000_2005  %>% mutate(MORTDODM = case_when(MORTDODQ == 1 ~ "02",
                                                                             MORTDODQ == 2 ~ "05",
                                                                             MORTDODQ == 3 ~ "08",
                                                                             MORTDODQ == 4 ~ "11"))


nhis_ipums_2000_2005$date_of_death = paste0("15-", nhis_ipums_2000_2005$MORTDODM, "-", nhis_ipums_2000_2005$MORTDODY) 
nhis_ipums_2000_2005$date_of_death[grep("NA", nhis_ipums_2000_2005$date_of_death)] = NA
nhis_ipums_2000_2005$date_of_death = dmy(nhis_ipums_2000_2005$date_of_death)

nhis_ipums_2000_2005$BIRTHMO[which(nhis_ipums_2000_2005$BIRTHMO >= 97)] = NA
nhis_ipums_2000_2005$BIRTHYR[which(nhis_ipums_2000_2005$BIRTHYR >= 9997)] = NA
nhis_ipums_2000_2005$date_of_birth = paste0("15-", str_pad(nhis_ipums_2000_2005$BIRTHMO, 2, pad ="0"), "-", nhis_ipums_2000_2005$BIRTHYR) 
nhis_ipums_2000_2005$date_of_birth[grep("NA", nhis_ipums_2000_2005$date_of_birth)] = NA
nhis_ipums_2000_2005$date_of_birth = dmy(nhis_ipums_2000_2005$date_of_birth)

nhis_ipums_2000_2005 = nhis_ipums_2000_2005  %>% mutate(age_at_exit = if_else(date_of_death < censoring_date, (date_of_death-date_of_birth)/365, (censoring_date-date_of_birth)/365))

all.equal(sum(unique(nhis_ipums_2000_2005$mortstat_rec[which(is.na(nhis_ipums_2000_2005$date_of_death) == T)])), 0)

nhis_ipums_2000_2005$age_at_exit[which(is.na(nhis_ipums_2000_2005$date_of_death) == T)] = (nhis_ipums_2000_2005$censoring_date[which(is.na(nhis_ipums_2000_2005$date_of_death) == T)] - nhis_ipums_2000_2005$date_of_birth[which(is.na(nhis_ipums_2000_2005$date_of_death) == T)])/365





#nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(bmi_cat = case_when(BMI < BMI_quantiles[2] ~ "Underweight",
#                                                                          BMI >= BMI_quantiles[2] & BMI < BMI_quantiles[6] ~ "Normal",
#                                                                         BMI >= BMI_quantiles[6] & BMI < BMI_quantiles[9] ~ "ObeseI",
#                                                                        BMI >= BMI_quantiles[9] & BMI < BMI_quantiles[11] ~ "ObeseII",
#                                                                       BMI >= BMI_quantiles[11] & BMI < BMI_quantiles[13] ~ "ObeseIII",
#                                                                      BMI >= BMI_quantiles[13] ~ "ObeseV"))

nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(bmi_cat = case_when(BMI < BMI_quantiles[3] ~ "Underweight",
                                                                           BMI >= BMI_quantiles[3] & BMI < BMI_quantiles[6] ~ "Normal",
                                                                           BMI >= BMI_quantiles[6] & BMI < BMI_quantiles[8] ~ "ObeseI",
                                                                           BMI >= BMI_quantiles[8] & BMI < BMI_quantiles[10] ~ "ObeseII",
                                                                           BMI >= BMI_quantiles[10] & BMI < BMI_quantiles[12] ~ "ObeseIII",
                                                                           BMI >= BMI_quantiles[12] & BMI < BMI_quantiles[13]~ "ObeseIV",
                                                                           BMI >= BMI_quantiles[13] ~ "ObeseV"))

#---Removing underweight people--#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[-which(nhis_ipums_2000_2005$bmi_cat == "Underweight"), ]




#alcohol
nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(alcohol_status = case_when(ALCSTAT1 == 1 ~ "Never",
                                                                                  ALCSTAT1 == 2 ~ "Former",
                                                                                  ALCSTAT1 == 3 ~ "Current"))


#Education

nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(education = case_when(EDUC >= 100 & EDUC < 200 ~ "less_than_high_school",
                                                                             EDUC >= 200 & EDUC <= 202 ~ "high_school",
                                                                             EDUC > 202 & EDUC <= 504 ~ "college_graduate"))

#Smoking
#nhis_ipums_2000_2005 = nhis_ipums_2000_2005[-which(nhis_ipums_2000_2005$SMOKESTATUS2 == 40 | nhis_ipums_2000_2005$SMOKESTATUS2 == 90), ]
nhis_ipums_2000_2005= nhis_ipums_2000_2005 %>% mutate(smoking_status = case_when(SMOKESTATUS2 == 30 ~ "Never",
                                                                                 SMOKESTATUS2 == 20 ~ "Former",
                                                                                 SMOKESTATUS2 == 11 | SMOKESTATUS2 == 12 ~ "Current"))

#Marital Status
nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(marital_status = case_when(MARST >= 10 & MARST <= 13 ~ "married",
                                                                                  MARST == 20 | MARST == 30 | MARST == 40 ~ "divorce",
                                                                                  MARST == 50 ~ "never_married"))

#nhis_ipums_2000_2005 = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$MORTUCODLD == 1), ]
colnames(nhis_ipums_2000_2005) = tolower(colnames(nhis_ipums_2000_2005))
data_processed = nhis_ipums_2000_2005 %>% dplyr::select("mortstat_rec", "bmi_cat", "age", "sex", "education", "smoking_status",
                                                        "strata", "psu", "sampweight", "alcohol_status", "marital_status", "year", "mortucodld")

myVars = c("bmi_cat", "age", "sex", "education", "smoking_status", "alcohol_status", "marital_status")
catVars = c("sex", "bmi_cat", "education", "smoking_status", "alcohol_status", "marital_status")
tab_cancer <- CreateTableOne(vars = myVars, strata = "mortstat_rec" , data = data_processed, factorVars = catVars)




#African-american
MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_fullsample.csv")
BMI_quantiles = MR_vcov_file$bmi.quant

nhis_ipums_2000_2005 = fread("~/Downloads/nhis_00002.csv")
#---Selecting individuals who are sample adults---#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$ASTATFLG == 1), ]


#--- Including with mortstat eligibe status == 1 ---#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$MORTELIG == 1), ]


#Removing individuals whose age is ceiled at 85 years
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$AGE < 85), ]

#----Removing unknown BMI----#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[-which(nhis_ipums_2000_2005$BMI == 99.99), ]


#---Keeping AA individuals---#
nhis_ipums_2000_2005  = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$RACENEW == 200), ]




nhis_ipums_2000_2005$MORTDODQ[which(nhis_ipums_2000_2005$MORTDODQ == 9)] = NA

nhis_ipums_2000_2005$MORTDODY[which(nhis_ipums_2000_2005$MORTDODY == 9999)] = NA
#Rmoving those individuals with unknown death data
if(length(nhis_ipums_2000_2005$MORTUCODLD == 96 & nhis_ipums_2000_2005$MORTSTAT == 1) > 0)
{
  nhis_ipums_2000_2005 = nhis_ipums_2000_2005[-which(nhis_ipums_2000_2005$MORTUCODLD == 96 & nhis_ipums_2000_2005$MORTSTAT == 1), ]
}
#---Redefining morstat for 10 years follow-up---#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005  %>% mutate(mortstat_rec = case_when(MORTSTAT == 2 ~ 0,
                                                                                 YEAR == 2000 & MORTDODY <= 2010 ~ 1,
                                                                                 YEAR == 2000 & MORTDODY > 2010 ~ 0,
                                                                                 YEAR == 2001 & MORTDODY <= 2011 ~ 1,
                                                                                 YEAR == 2001 & MORTDODY > 2011 ~ 0,
                                                                                 YEAR == 2002 & MORTDODY <= 2012 ~ 1,
                                                                                 YEAR == 2002 & MORTDODY > 2012 ~ 0,
                                                                                 YEAR == 2003 & MORTDODY <= 2013 ~ 1,
                                                                                 YEAR == 2003 & MORTDODY > 2013 ~ 0,
                                                                                 YEAR == 2004 & MORTDODY <= 2014 ~ 1,
                                                                                 YEAR == 2004 & MORTDODY > 2014 ~ 0,
                                                                                 YEAR == 2005 & MORTDODY <= 2015 ~ 1,
                                                                                 YEAR == 2005 & MORTDODY > 2015 ~ 0))
length(which(is.na(nhis_ipums_2000_2005$MORTDODY) == T & nhis_ipums_2000_2005$MORTSTAT == 1))
#0
length(which(is.na(nhis_ipums_2000_2005$MORTDODY) == F & nhis_ipums_2000_2005$MORTSTAT == 2))
#0
#nhis_ipums_2000_2005$mortstat_rec[which(nhis_ipums_2000_2005$MORTSTAT == 1 & is.na(nhis_ipums_2000_2005$MORTDODY) == T)] = 1


nhis_ipums_2000_2005 = nhis_ipums_2000_2005  %>% mutate(censoring_date = case_when(YEAR == 2000 ~ "12-31-2010",
                                                                                   YEAR == 2001 ~ "12-31-2011",
                                                                                   YEAR == 2002 ~ "12-31-2012",
                                                                                   YEAR == 2003 ~ "12-31-2013",
                                                                                   YEAR == 2004 ~ "12-31-2014",
                                                                                   YEAR == 2005 ~ "12-31-2015"))
nhis_ipums_2000_2005$censoring_date = mdy(nhis_ipums_2000_2005$censoring_date)


nhis_ipums_2000_2005 = nhis_ipums_2000_2005  %>% mutate(MORTDODM = case_when(MORTDODQ == 1 ~ "02",
                                                                             MORTDODQ == 2 ~ "05",
                                                                             MORTDODQ == 3 ~ "08",
                                                                             MORTDODQ == 4 ~ "11"))


nhis_ipums_2000_2005$date_of_death = paste0("15-", nhis_ipums_2000_2005$MORTDODM, "-", nhis_ipums_2000_2005$MORTDODY) 
nhis_ipums_2000_2005$date_of_death[grep("NA", nhis_ipums_2000_2005$date_of_death)] = NA
nhis_ipums_2000_2005$date_of_death = dmy(nhis_ipums_2000_2005$date_of_death)

nhis_ipums_2000_2005$BIRTHMO[which(nhis_ipums_2000_2005$BIRTHMO >= 97)] = NA
nhis_ipums_2000_2005$BIRTHYR[which(nhis_ipums_2000_2005$BIRTHYR >= 9997)] = NA
nhis_ipums_2000_2005$date_of_birth = paste0("15-", str_pad(nhis_ipums_2000_2005$BIRTHMO, 2, pad ="0"), "-", nhis_ipums_2000_2005$BIRTHYR) 
nhis_ipums_2000_2005$date_of_birth[grep("NA", nhis_ipums_2000_2005$date_of_birth)] = NA
nhis_ipums_2000_2005$date_of_birth = dmy(nhis_ipums_2000_2005$date_of_birth)

nhis_ipums_2000_2005 = nhis_ipums_2000_2005  %>% mutate(age_at_exit = if_else(date_of_death < censoring_date, (date_of_death-date_of_birth)/365, (censoring_date-date_of_birth)/365))

all.equal(sum(unique(nhis_ipums_2000_2005$mortstat_rec[which(is.na(nhis_ipums_2000_2005$date_of_death) == T)])), 0)

nhis_ipums_2000_2005$age_at_exit[which(is.na(nhis_ipums_2000_2005$date_of_death) == T)] = (nhis_ipums_2000_2005$censoring_date[which(is.na(nhis_ipums_2000_2005$date_of_death) == T)] - nhis_ipums_2000_2005$date_of_birth[which(is.na(nhis_ipums_2000_2005$date_of_death) == T)])/365





nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(bmi_cat = case_when(BMI < BMI_quantiles[3] ~ "Underweight",
                                                                           BMI >= BMI_quantiles[3] & BMI < BMI_quantiles[6] ~ "Normal",
                                                                           BMI >= BMI_quantiles[6] & BMI < BMI_quantiles[8] ~ "ObeseI",
                                                                           BMI >= BMI_quantiles[8] & BMI < BMI_quantiles[10] ~ "ObeseII",
                                                                           BMI >= BMI_quantiles[10] & BMI < BMI_quantiles[12] ~ "ObeseIII",
                                                                           BMI >= BMI_quantiles[12] & BMI < BMI_quantiles[13] ~ "ObeseIV",
                                                                           BMI >= BMI_quantiles[13] ~ "ObeseV"))


#---Removing underweight people--#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[-which(nhis_ipums_2000_2005$bmi_cat == "Underweight"), ]




#alcohol
nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(alcohol_status = case_when(ALCSTAT1 == 1 ~ "Never",
                                                                                  ALCSTAT1 == 2 ~ "Former",
                                                                                  ALCSTAT1 == 3 ~ "Current"))


#Education

nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(education = case_when(EDUC >= 100 & EDUC < 200 ~ "less_than_high_school",
                                                                             EDUC >= 200 & EDUC <= 202 ~ "high_school",
                                                                             EDUC > 202 & EDUC <= 504 ~ "college_graduate"))

#Smoking
#nhis_ipums_2000_2005 = nhis_ipums_2000_2005[-which(nhis_ipums_2000_2005$SMOKESTATUS2 == 40 | nhis_ipums_2000_2005$SMOKESTATUS2 == 90), ]
nhis_ipums_2000_2005= nhis_ipums_2000_2005 %>% mutate(smoking_status = case_when(SMOKESTATUS2 == 30 ~ "Never",
                                                                                 SMOKESTATUS2 == 20 ~ "Former",
                                                                                 SMOKESTATUS2 == 11 | SMOKESTATUS2 == 12 ~ "Current"))

#Marital Status
nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(marital_status = case_when(MARST >= 10 & MARST <= 13 ~ "married",
                                                                                  MARST == 20 | MARST == 30 | MARST == 40 ~ "divorce",
                                                                                  MARST == 50 ~ "never_married"))
colnames(nhis_ipums_2000_2005) = tolower(colnames(nhis_ipums_2000_2005))
data_processed = nhis_ipums_2000_2005 %>% dplyr::select("mortstat_rec", "bmi_cat", "age", "sex", "education", "smoking_status",
                                                        "strata", "psu", "sampweight", "alcohol_status", "marital_status", "year", "mortucodld")  
#Keeping White, Black/AfricanAmerican,  American_Indian/Alaskan_Native, Asian
#data_processed = data_processed[which(data_processed$racenew <= 400), ]
#data_processed = data_processed  %>% mutate(racenew = case_when(racenew == 100 ~ "White",
#                                                               racenew == 200 ~ "African_american",
#                                                              racenew == 300 ~ "AI_alaskan_native",
#                                                             racenew == 400 ~ "Asian"))
#bmi_quantile = quantile(data_processed$bmi, probs = seq(0, 1, 0.01))

#data_processed = data_processed %>% mutate(bmi_cat = case_when(bmi < bmi_quantile[11] ~ "Underweight",
#                                                              bmi >= bmi_quantile[11] & bmi < bmi_quantile[41] ~ "Normal",
#                                                             bmi >= bmi_quantile[41] & bmi < bmi_quantile[61] ~ "ObeseI",
#                                                            bmi >= bmi_quantile[61] & bmi < bmi_quantile[81] ~ "ObeseII",
#                                                           bmi >= bmi_quantile[81] & bmi < bmi_quantile[96] ~ "ObeseIII",
#                                                          bmi >= bmi_quantile[96] & bmi < bmi_quantile[100] ~ "ObeseIV",
#                                                         bmi >= bmi_quantile[100] ~ "ObeseV"))
myVars = c("bmi_cat", "age", "sex", "education", "smoking_status", "alcohol_status", "marital_status")
catVars = c("sex", "bmi_cat", "education", "smoking_status", "alcohol_status", "marital_status")
tab3_AA <- CreateTableOne(vars = myVars, strata = "mortstat_rec" , data = data_processed, factorVars = catVars)


#--heart disease
MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_cardiovascular.csv")
BMI_quantiles = MR_vcov_file$bmi.quant

#MR_estimates_UK = log(c(1.09, 1.195, 2.065, 2.915))

nhis_ipums_2000_2005 = fread("~/Downloads/nhis_00002.csv")
#---Selecting individuals who are sample adults---#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$ASTATFLG == 1), ]


#---Keeping AA individuals---#
nhis_ipums_2000_2005  = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$RACENEW == 200), ]


#----Removing unknown BMI----#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[-which(nhis_ipums_2000_2005$BMI == 99.99), ]


#--- Including with mortstat eligibe status == 1 ---#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$MORTELIG == 1), ]


#Removing individuals whose age is ceiled at 85 years
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$AGE < 85), ]

nhis_ipums_2000_2005$MORTDODQ[which(nhis_ipums_2000_2005$MORTDODQ == 9)] = NA

nhis_ipums_2000_2005$MORTDODY[which(nhis_ipums_2000_2005$MORTDODY == 9999)] = NA

#Rmoving those individuals with unknown death data
if(length(which(nhis_ipums_2000_2005$MORTUCODLD == 96 & nhis_ipums_2000_2005$MORTSTAT == 1)) > 0)
{
  nhis_ipums_2000_2005 = nhis_ipums_2000_2005[-which(nhis_ipums_2000_2005$MORTUCODLD == 96 & nhis_ipums_2000_2005$MORTSTAT == 1), ]
}
#---Redefining morstat for 10 years follow-up---#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005  %>% mutate(mortstat_rec = case_when(MORTSTAT == 2 ~ 0,
                                                                                 YEAR == 2000 & MORTDODY <= 2010 & MORTUCODLD == 1 ~ 1,
                                                                                 YEAR == 2000 & MORTDODY <= 2010 & MORTUCODLD != 1 ~ 0,
                                                                                 YEAR == 2000 & MORTDODY > 2010 ~ 0,
                                                                                 YEAR == 2001 & MORTDODY <= 2011 & MORTUCODLD == 1 ~ 1,
                                                                                 YEAR == 2001 & MORTDODY <= 2011 & MORTUCODLD != 1 ~ 0,
                                                                                 YEAR == 2001 & MORTDODY > 2011 ~ 0,
                                                                                 YEAR == 2002 & MORTDODY <= 2012 & MORTUCODLD == 1 ~ 1,
                                                                                 YEAR == 2002 & MORTDODY <= 2012 & MORTUCODLD != 1 ~ 0,
                                                                                 YEAR == 2002 & MORTDODY > 2012 ~ 0,
                                                                                 YEAR == 2003 & MORTDODY <= 2013 & MORTUCODLD == 1 ~ 1,
                                                                                 YEAR == 2003 & MORTDODY <= 2013 & MORTUCODLD != 1 ~ 0,
                                                                                 YEAR == 2003 & MORTDODY > 2013 ~ 0,
                                                                                 YEAR == 2004 & MORTDODY <= 2014 & MORTUCODLD == 1 ~ 1,
                                                                                 YEAR == 2004 & MORTDODY <= 2014 & MORTUCODLD != 1 ~ 0,
                                                                                 YEAR == 2004 & MORTDODY > 2014 ~ 0,
                                                                                 YEAR == 2005 & MORTDODY <= 2015 & MORTUCODLD == 1 ~ 1,
                                                                                 YEAR == 2005 & MORTDODY <= 2015 & MORTUCODLD != 1 ~ 0,
                                                                                 YEAR == 2005 & MORTDODY > 2015 ~ 0))
length(which(is.na(nhis_ipums_2000_2005$MORTDODY) == T & nhis_ipums_2000_2005$MORTSTAT == 1))
#0
length(which(is.na(nhis_ipums_2000_2005$MORTDODY) == F & nhis_ipums_2000_2005$MORTSTAT == 2))
#0
#nhis_ipums_2000_2005$mortstat_rec[which(nhis_ipums_2000_2005$MORTSTAT == 1 & is.na(nhis_ipums_2000_2005$MORTDODY) == T)] = 1


nhis_ipums_2000_2005 = nhis_ipums_2000_2005  %>% mutate(censoring_date = case_when(YEAR == 2000 ~ "12-31-2010",
                                                                                   YEAR == 2001 ~ "12-31-2011",
                                                                                   YEAR == 2002 ~ "12-31-2012",
                                                                                   YEAR == 2003 ~ "12-31-2013",
                                                                                   YEAR == 2004 ~ "12-31-2014",
                                                                                   YEAR == 2005 ~ "12-31-2015"))
nhis_ipums_2000_2005$censoring_date = mdy(nhis_ipums_2000_2005$censoring_date)


nhis_ipums_2000_2005 = nhis_ipums_2000_2005  %>% mutate(MORTDODM = case_when(MORTDODQ == 1 ~ "02",
                                                                             MORTDODQ == 2 ~ "05",
                                                                             MORTDODQ == 3 ~ "08",
                                                                             MORTDODQ == 4 ~ "11"))


nhis_ipums_2000_2005$date_of_death = paste0("15-", nhis_ipums_2000_2005$MORTDODM, "-", nhis_ipums_2000_2005$MORTDODY) 
nhis_ipums_2000_2005$date_of_death[grep("NA", nhis_ipums_2000_2005$date_of_death)] = NA
nhis_ipums_2000_2005$date_of_death = dmy(nhis_ipums_2000_2005$date_of_death)

nhis_ipums_2000_2005$BIRTHMO[which(nhis_ipums_2000_2005$BIRTHMO >= 97)] = NA
nhis_ipums_2000_2005$BIRTHYR[which(nhis_ipums_2000_2005$BIRTHYR >= 9997)] = NA
nhis_ipums_2000_2005$date_of_birth = paste0("15-", str_pad(nhis_ipums_2000_2005$BIRTHMO, 2, pad ="0"), "-", nhis_ipums_2000_2005$BIRTHYR) 
nhis_ipums_2000_2005$date_of_birth[grep("NA", nhis_ipums_2000_2005$date_of_birth)] = NA
nhis_ipums_2000_2005$date_of_birth = dmy(nhis_ipums_2000_2005$date_of_birth)

nhis_ipums_2000_2005 = nhis_ipums_2000_2005  %>% mutate(age_at_exit = if_else(date_of_death < censoring_date, (date_of_death-date_of_birth)/365, (censoring_date-date_of_birth)/365))

all.equal(sum(unique(nhis_ipums_2000_2005$mortstat_rec[which(is.na(nhis_ipums_2000_2005$date_of_death) == T)])), 0)

nhis_ipums_2000_2005$age_at_exit[which(is.na(nhis_ipums_2000_2005$date_of_death) == T)] = (nhis_ipums_2000_2005$censoring_date[which(is.na(nhis_ipums_2000_2005$date_of_death) == T)] - nhis_ipums_2000_2005$date_of_birth[which(is.na(nhis_ipums_2000_2005$date_of_death) == T)])/365





nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(bmi_cat = case_when(BMI < BMI_quantiles[3] ~ "Underweight",
                                                                           BMI >= BMI_quantiles[3] & BMI < BMI_quantiles[6] ~ "Normal",
                                                                           BMI >= BMI_quantiles[6] & BMI < BMI_quantiles[8] ~ "ObeseI",
                                                                           BMI >= BMI_quantiles[8] & BMI < BMI_quantiles[10] ~ "ObeseII",
                                                                           BMI >= BMI_quantiles[10] & BMI < BMI_quantiles[12] ~ "ObeseIII",
                                                                           BMI >= BMI_quantiles[12] & BMI < BMI_quantiles[13]~ "ObeseIV",
                                                                           BMI >= BMI_quantiles[13] ~ "ObeseV"))

#---Removing underweight people--#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[-which(nhis_ipums_2000_2005$bmi_cat == "Underweight"), ]




#alcohol
nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(alcohol_status = case_when(ALCSTAT1 == 1 ~ "Never",
                                                                                  ALCSTAT1 == 2 ~ "Former",
                                                                                  ALCSTAT1 == 3 ~ "Current"))


#Education

nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(education = case_when(EDUC >= 100 & EDUC < 200 ~ "less_than_high_school",
                                                                             EDUC >= 200 & EDUC <= 202 ~ "high_school",
                                                                             EDUC > 202 & EDUC <= 504 ~ "college_graduate"))

#Smoking
#nhis_ipums_2000_2005 = nhis_ipums_2000_2005[-which(nhis_ipums_2000_2005$SMOKESTATUS2 == 40 | nhis_ipums_2000_2005$SMOKESTATUS2 == 90), ]
nhis_ipums_2000_2005= nhis_ipums_2000_2005 %>% mutate(smoking_status = case_when(SMOKESTATUS2 == 30 ~ "Never",
                                                                                 SMOKESTATUS2 == 20 ~ "Former",
                                                                                 SMOKESTATUS2 == 11 | SMOKESTATUS2 == 12 ~ "Current"))

#Marital Status
nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(marital_status = case_when(MARST >= 10 & MARST <= 13 ~ "married",
                                                                                  MARST == 20 | MARST == 30 | MARST == 40 ~ "divorce",
                                                                                  MARST == 50 ~ "never_married"))

#nhis_ipums_2000_2005 = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$MORTUCODLD == 1), ]
colnames(nhis_ipums_2000_2005) = tolower(colnames(nhis_ipums_2000_2005))


#data_processed = nhis_ipums_2000_2005 %>% dplyr::select("mortstat_rec", "bmi_cat", "age", "sex", "education", "smoking_status",
#                                                       "strata", "psu", "sampweight", "alcohol_status", "marital_status", "age_at_exit", "year")  
data_processed = nhis_ipums_2000_2005 %>% dplyr::select("mortstat_rec", "bmi_cat", "age", "sex", "education", "smoking_status",
                                                        "strata", "psu", "sampweight", "alcohol_status", "marital_status", "year", "mortucodld")
myVars = c("bmi_cat", "age", "sex", "education", "smoking_status", "alcohol_status", "marital_status")
catVars = c("sex", "bmi_cat", "education", "smoking_status", "alcohol_status", "marital_status")
tab_heart_AA <- CreateTableOne(vars = myVars, strata = "mortstat_rec" , data = data_processed, factorVars = catVars)

MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_cancer.csv")
BMI_quantiles = MR_vcov_file$bmi.quant
delta_HR = exp(MR_vcov_file$est)
#MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[9]-delta_HR[6])/2, delta_HR[9] + (delta_HR[11]-delta_HR[9])/2, delta_HR[11] + (delta_HR[13]-delta_HR[11])/2, delta_HR[13] + (delta_HR[14]-delta_HR[13])/2))
MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[8]-delta_HR[6])/2, delta_HR[8] + (delta_HR[10]-delta_HR[8])/2, delta_HR[10] + (delta_HR[12]-delta_HR[10])/2, delta_HR[12] + (delta_HR[13]-delta_HR[12])/2, delta_HR[13] + (delta_HR[14]-delta_HR[13])/2))

#MR_estimates_UK = log(c(1.09, 1.195, 2.065, 2.915))

nhis_ipums_2000_2005 = fread("~/Downloads/nhis_00002.csv")
#---Selecting individuals who are sample adults---#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$ASTATFLG == 1), ]


#---Keeping white individuals---#
nhis_ipums_2000_2005  = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$RACENEW == 200), ]


#----Removing unknown BMI----#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[-which(nhis_ipums_2000_2005$BMI == 99.99), ]


#--- Including with mortstat eligibe status == 1 ---#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$MORTELIG == 1), ]


#Removing individuals whose age is ceiled at 85 years
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$AGE < 85), ]

nhis_ipums_2000_2005$MORTDODQ[which(nhis_ipums_2000_2005$MORTDODQ == 9)] = NA

nhis_ipums_2000_2005$MORTDODY[which(nhis_ipums_2000_2005$MORTDODY == 9999)] = NA

#Rmoving those individuals with unknown death data
if(length(which(nhis_ipums_2000_2005$MORTUCODLD == 96 & nhis_ipums_2000_2005$MORTSTAT == 1)) > 0)
{
  nhis_ipums_2000_2005 = nhis_ipums_2000_2005[-which(nhis_ipums_2000_2005$MORTUCODLD == 96 & nhis_ipums_2000_2005$MORTSTAT == 1), ]
}
#---Redefining morstat for 10 years follow-up---#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005  %>% mutate(mortstat_rec = case_when(MORTSTAT == 2 ~ 0,
                                                                                 YEAR == 2000 & MORTDODY <= 2010 & MORTUCODLD == 2 ~ 1,
                                                                                 YEAR == 2000 & MORTDODY <= 2010 & MORTUCODLD != 2 ~ 0,
                                                                                 YEAR == 2000 & MORTDODY > 2010 ~ 0,
                                                                                 YEAR == 2001 & MORTDODY <= 2011 & MORTUCODLD == 2 ~ 1,
                                                                                 YEAR == 2001 & MORTDODY <= 2011 & MORTUCODLD != 2 ~ 0,
                                                                                 YEAR == 2001 & MORTDODY > 2011 ~ 0,
                                                                                 YEAR == 2002 & MORTDODY <= 2012 & MORTUCODLD == 2 ~ 1,
                                                                                 YEAR == 2002 & MORTDODY <= 2012 & MORTUCODLD != 2 ~ 0,
                                                                                 YEAR == 2002 & MORTDODY > 2012 ~ 0,
                                                                                 YEAR == 2003 & MORTDODY <= 2013 & MORTUCODLD == 2 ~ 1,
                                                                                 YEAR == 2003 & MORTDODY <= 2013 & MORTUCODLD != 2 ~ 0,
                                                                                 YEAR == 2003 & MORTDODY > 2013 ~ 0,
                                                                                 YEAR == 2004 & MORTDODY <= 2014 & MORTUCODLD == 2 ~ 1,
                                                                                 YEAR == 2004 & MORTDODY <= 2014 & MORTUCODLD != 2 ~ 0,
                                                                                 YEAR == 2004 & MORTDODY > 2014 ~ 0,
                                                                                 YEAR == 2005 & MORTDODY <= 2015 & MORTUCODLD == 2 ~ 1,
                                                                                 YEAR == 2005 & MORTDODY <= 2015 & MORTUCODLD != 2 ~ 0,
                                                                                 YEAR == 2005 & MORTDODY > 2015 ~ 0))
length(which(is.na(nhis_ipums_2000_2005$MORTDODY) == T & nhis_ipums_2000_2005$MORTSTAT == 1))
#0
length(which(is.na(nhis_ipums_2000_2005$MORTDODY) == F & nhis_ipums_2000_2005$MORTSTAT == 2))
#0
#nhis_ipums_2000_2005$mortstat_rec[which(nhis_ipums_2000_2005$MORTSTAT == 1 & is.na(nhis_ipums_2000_2005$MORTDODY) == T)] = 1


nhis_ipums_2000_2005 = nhis_ipums_2000_2005  %>% mutate(censoring_date = case_when(YEAR == 2000 ~ "12-31-2010",
                                                                                   YEAR == 2001 ~ "12-31-2011",
                                                                                   YEAR == 2002 ~ "12-31-2012",
                                                                                   YEAR == 2003 ~ "12-31-2013",
                                                                                   YEAR == 2004 ~ "12-31-2014",
                                                                                   YEAR == 2005 ~ "12-31-2015"))
nhis_ipums_2000_2005$censoring_date = mdy(nhis_ipums_2000_2005$censoring_date)


nhis_ipums_2000_2005 = nhis_ipums_2000_2005  %>% mutate(MORTDODM = case_when(MORTDODQ == 1 ~ "02",
                                                                             MORTDODQ == 2 ~ "05",
                                                                             MORTDODQ == 3 ~ "08",
                                                                             MORTDODQ == 4 ~ "11"))


nhis_ipums_2000_2005$date_of_death = paste0("15-", nhis_ipums_2000_2005$MORTDODM, "-", nhis_ipums_2000_2005$MORTDODY) 
nhis_ipums_2000_2005$date_of_death[grep("NA", nhis_ipums_2000_2005$date_of_death)] = NA
nhis_ipums_2000_2005$date_of_death = dmy(nhis_ipums_2000_2005$date_of_death)

nhis_ipums_2000_2005$BIRTHMO[which(nhis_ipums_2000_2005$BIRTHMO >= 97)] = NA
nhis_ipums_2000_2005$BIRTHYR[which(nhis_ipums_2000_2005$BIRTHYR >= 9997)] = NA
nhis_ipums_2000_2005$date_of_birth = paste0("15-", str_pad(nhis_ipums_2000_2005$BIRTHMO, 2, pad ="0"), "-", nhis_ipums_2000_2005$BIRTHYR) 
nhis_ipums_2000_2005$date_of_birth[grep("NA", nhis_ipums_2000_2005$date_of_birth)] = NA
nhis_ipums_2000_2005$date_of_birth = dmy(nhis_ipums_2000_2005$date_of_birth)

nhis_ipums_2000_2005 = nhis_ipums_2000_2005  %>% mutate(age_at_exit = if_else(date_of_death < censoring_date, (date_of_death-date_of_birth)/365, (censoring_date-date_of_birth)/365))

all.equal(sum(unique(nhis_ipums_2000_2005$mortstat_rec[which(is.na(nhis_ipums_2000_2005$date_of_death) == T)])), 0)

nhis_ipums_2000_2005$age_at_exit[which(is.na(nhis_ipums_2000_2005$date_of_death) == T)] = (nhis_ipums_2000_2005$censoring_date[which(is.na(nhis_ipums_2000_2005$date_of_death) == T)] - nhis_ipums_2000_2005$date_of_birth[which(is.na(nhis_ipums_2000_2005$date_of_death) == T)])/365





#nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(bmi_cat = case_when(BMI < BMI_quantiles[2] ~ "Underweight",
#                                                                          BMI >= BMI_quantiles[2] & BMI < BMI_quantiles[6] ~ "Normal",
#                                                                         BMI >= BMI_quantiles[6] & BMI < BMI_quantiles[9] ~ "ObeseI",
#                                                                        BMI >= BMI_quantiles[9] & BMI < BMI_quantiles[11] ~ "ObeseII",
#                                                                       BMI >= BMI_quantiles[11] & BMI < BMI_quantiles[13] ~ "ObeseIII",
#                                                                      BMI >= BMI_quantiles[13] ~ "ObeseV"))

nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(bmi_cat = case_when(BMI < BMI_quantiles[3] ~ "Underweight",
                                                                           BMI >= BMI_quantiles[3] & BMI < BMI_quantiles[6] ~ "Normal",
                                                                           BMI >= BMI_quantiles[6] & BMI < BMI_quantiles[8] ~ "ObeseI",
                                                                           BMI >= BMI_quantiles[8] & BMI < BMI_quantiles[10] ~ "ObeseII",
                                                                           BMI >= BMI_quantiles[10] & BMI < BMI_quantiles[12] ~ "ObeseIII",
                                                                           BMI >= BMI_quantiles[12] & BMI < BMI_quantiles[13]~ "ObeseIV",
                                                                           BMI >= BMI_quantiles[13] ~ "ObeseV"))

#---Removing underweight people--#
nhis_ipums_2000_2005 = nhis_ipums_2000_2005[-which(nhis_ipums_2000_2005$bmi_cat == "Underweight"), ]




#alcohol
nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(alcohol_status = case_when(ALCSTAT1 == 1 ~ "Never",
                                                                                  ALCSTAT1 == 2 ~ "Former",
                                                                                  ALCSTAT1 == 3 ~ "Current"))


#Education

nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(education = case_when(EDUC >= 100 & EDUC < 200 ~ "less_than_high_school",
                                                                             EDUC >= 200 & EDUC <= 202 ~ "high_school",
                                                                             EDUC > 202 & EDUC <= 504 ~ "college_graduate"))

#Smoking
#nhis_ipums_2000_2005 = nhis_ipums_2000_2005[-which(nhis_ipums_2000_2005$SMOKESTATUS2 == 40 | nhis_ipums_2000_2005$SMOKESTATUS2 == 90), ]
nhis_ipums_2000_2005= nhis_ipums_2000_2005 %>% mutate(smoking_status = case_when(SMOKESTATUS2 == 30 ~ "Never",
                                                                                 SMOKESTATUS2 == 20 ~ "Former",
                                                                                 SMOKESTATUS2 == 11 | SMOKESTATUS2 == 12 ~ "Current"))

#Marital Status
nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(marital_status = case_when(MARST >= 10 & MARST <= 13 ~ "married",
                                                                                  MARST == 20 | MARST == 30 | MARST == 40 ~ "divorce",
                                                                                  MARST == 50 ~ "never_married"))

#nhis_ipums_2000_2005 = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$MORTUCODLD == 1), ]
colnames(nhis_ipums_2000_2005) = tolower(colnames(nhis_ipums_2000_2005))
data_processed = nhis_ipums_2000_2005 %>% dplyr::select("mortstat_rec", "bmi_cat", "age", "sex", "education", "smoking_status",
                                                        "strata", "psu", "sampweight", "alcohol_status", "marital_status", "year", "mortucodld")

myVars = c("bmi_cat", "age", "sex", "education", "smoking_status", "alcohol_status", "marital_status")
catVars = c("sex", "bmi_cat", "education", "smoking_status", "alcohol_status", "marital_status")
tab_cancer_AA <- CreateTableOne(vars = myVars, strata = "mortstat_rec" , data = data_processed, factorVars = catVars)


