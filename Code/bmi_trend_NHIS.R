nhis_ipums_2000_2005 = fread("~/Downloads/nhis_00004.csv")
nhis_ipums_2000_2010 = fread("~/Downloads/nhis_00005.csv")
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





#nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(bmi_cat = case_when(BMI < 22.1 ~ "Underweight",
#                                                                          BMI >= 22.1 & BMI < 25.7 ~ "Normal",
#                                                                         BMI >= 25.7 & BMI < 29.1 ~ "ObeseI",
#                                                                        BMI >= 29.1 & BMI < 33.4 ~ "ObeseII",
#                                                                       BMI >= 33.4 & BMI < 36.1 ~ "ObeseIII",
#                                                                      BMI >= 36.1 & BMI < 42.3 ~ "ObeseIV",
#                                                                     BMI >= 42.3 ~ "ObeseV"))

nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(bmi_cat = case_when(BMI < BMI_quantiles[3] ~ "Underweight",
                                                                           BMI >= BMI_quantiles[3] & BMI < BMI_quantiles[6] ~ "Normal",
                                                                           BMI >= BMI_quantiles[6] & BMI < BMI_quantiles[8] ~ "ObeseI",
                                                                           BMI >= BMI_quantiles[8] & BMI < BMI_quantiles[10] ~ "ObeseII",
                                                                           BMI >= BMI_quantiles[10] & BMI < BMI_quantiles[12] ~ "ObeseIII",
                                                                           BMI >= BMI_quantiles[12] & BMI < BMI_quantiles[13] ~ "ObeseIV",
                                                                           BMI >= BMI_quantiles[13] ~ "ObeseV"))

#nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(bmi_cat = case_when(BMI < 18.5 ~ "Underweight",
#                                                                          BMI >= 18.5 & BMI < 25 ~ "Normal",
#                                                                         BMI >= 25 & BMI < 30 ~ "ObeseI",
#                                                                        BMI >= 30 & BMI < 35 ~ "ObeseII",
#                                                                       BMI >= 35 ~"ObeseIII"))

#nhis_ipums_2000_2005 = nhis_ipums_2000_2005[-which(nhis_ipums_2000_2005$bmi_cat == "Underweight"), ]
nhis_ipums_2000_2005$bmi_obese = if_else(nhis_ipums_2000_2005$BMI >= 30, 1, 0)
nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% dplyr::select("YEAR", "STRATA", "PSU", "SAMPWEIGHT", "bmi_cat", "bmi_obese")
trend_bmi = matrix(NA, nrow = length(unique(nhis_ipums_2000_2005$YEAR)), ncol = length(unique(nhis_ipums_2000_2005$bmi_cat)) )
trend_obese = matrix(NA, nrow = length(unique(nhis_ipums_2000_2005$YEAR)), ncol = length(unique(nhis_ipums_2000_2005$bmi_obese)) )
for(i in 1:length(unique(nhis_ipums_2000_2005$YEAR)))
{
  temp = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$YEAR == 1999+i), ]
  trend_bmi[i, ] = wpct(temp$bmi_cat, weight=temp$SAMPWEIGHT)
}

for(i in 1:length(unique(nhis_ipums_2000_2005$YEAR)))
{
  temp = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$YEAR == 1999+i), ]
  trend_obese[i, ] = wpct(temp$bmi_obese, weight=temp$SAMPWEIGHT)
}





nhis_ipums_2000_2010 = fread("~/Downloads/nhis_00005.csv")
#---Selecting individuals who are sample adults---#
nhis_ipums_2000_2010 = nhis_ipums_2000_2010[which(nhis_ipums_2000_2010$ASTATFLG == 1), ]



#Removing individuals whose age is ceiled at 85 years
nhis_ipums_2000_2010 = nhis_ipums_2000_2010[which(nhis_ipums_2000_2010$AGE < 900), ]

#----Removing unknown BMI----#
if(length(which(nhis_ipums_2000_2010$BMICALC == 996)) > 0)
{ 
  nhis_ipums_2000_2010 = nhis_ipums_2000_2010[-which(nhis_ipums_2000_2010$BMICALC == 996), ]
}


#---Keeping white individuals---#
nhis_ipums_2000_2010  = nhis_ipums_2000_2010[which(nhis_ipums_2000_2010$RACENEW == 100), ]


#nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(bmi_cat = case_when(BMI < 22.1 ~ "Underweight",
#                                                                          BMI >= 22.1 & BMI < 25.7 ~ "Normal",
#                                                                         BMI >= 25.7 & BMI < 29.1 ~ "ObeseI",
#                                                                        BMI >= 29.1 & BMI < 33.4 ~ "ObeseII",
#                                                                       BMI >= 33.4 & BMI < 36.1 ~ "ObeseIII",
#                                                                      BMI >= 36.1 & BMI < 42.3 ~ "ObeseIV",
#                                                                     BMI >= 42.3 ~ "ObeseV"))

nhis_ipums_2000_2010 = nhis_ipums_2000_2010 %>% mutate(bmi_cat = case_when(BMICALC < BMI_quantiles[3] ~ "Underweight",
                                                                           BMICALC >= BMI_quantiles[3] & BMICALC < BMI_quantiles[6] ~ "Normal",
                                                                           BMICALC >= BMI_quantiles[6] & BMICALC < BMI_quantiles[8] ~ "ObeseI",
                                                                           BMICALC >= BMI_quantiles[8] & BMICALC < BMI_quantiles[10] ~ "ObeseII",
                                                                           BMICALC >= BMI_quantiles[10] & BMICALC < BMI_quantiles[12] ~ "ObeseIII",
                                                                           BMICALC >= BMI_quantiles[12] & BMICALC < BMI_quantiles[13] ~ "ObeseIV",
                                                                           BMICALC >= BMI_quantiles[13] ~ "ObeseV"))

#nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(bmi_cat = case_when(BMI < 18.5 ~ "Underweight",
#                                                                          BMI >= 18.5 & BMI < 25 ~ "Normal",
#                                                                         BMI >= 25 & BMI < 30 ~ "ObeseI",
#                                                                        BMI >= 30 & BMI < 35 ~ "ObeseII",
#                                                                       BMI >= 35 ~"ObeseIII"))

#nhis_ipums_2000_2005 = nhis_ipums_2000_2005[-which(nhis_ipums_2000_2005$bmi_cat == "Underweight"), ]
nhis_ipums_2000_2010$bmi_obese = if_else(nhis_ipums_2000_2010$BMICALC >= 30, 1, 0)
nhis_ipums_2000_2010 = nhis_ipums_2000_2010 %>% dplyr::select("YEAR", "STRATA", "PSU", "SAMPWEIGHT", "bmi_cat", "bmi_obese")
trend_bmi_2000_2020 = matrix(NA, nrow = length(unique(nhis_ipums_2000_2010$YEAR)), ncol = length(unique(nhis_ipums_2000_2010$bmi_cat)) )
trend_obese_2000_2020 = matrix(NA, nrow = length(unique(nhis_ipums_2000_2010$YEAR)), ncol = length(unique(nhis_ipums_2000_2010$bmi_obese)) )
for(i in 1:length(unique(nhis_ipums_2000_2010$YEAR)))
{
  temp = nhis_ipums_2000_2010[which(nhis_ipums_2000_2010$YEAR == 1999+i), ]
  trend_bmi_2000_2020[i, ] = wpct(temp$bmi_cat, weight=temp$SAMPWEIGHT)
}

for(i in 1:length(unique(nhis_ipums_2000_2010$YEAR)))
{
  temp = nhis_ipums_2000_2010[which(nhis_ipums_2000_2010$YEAR == 1999+i), ]
  trend_obese_2000_2020[i, ] = wpct(temp$bmi_obese, weight=temp$SAMPWEIGHT)
}
