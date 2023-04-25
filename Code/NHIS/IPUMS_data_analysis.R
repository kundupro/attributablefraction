library(dplyr)
library(readr)
library(Hmisc)
library(rvest)
library(XML)
library(stringr)
library(survey)
library(GDAtools)
library(weights)
library(rnhanesdata)
library(data.table)
library(dplyr)
library(lubridate)

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


#MR_estimates_UK = log(c(1.12, 1.41, 1.91, 3.49, 5.155))

MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_fullsample.csv")
BMI_quantiles = MR_vcov_file$bmi.quant
delta_HR = exp(MR_vcov_file$est)
MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[8]-delta_HR[6])/2, delta_HR[8] + (delta_HR[10]-delta_HR[8])/2, delta_HR[10] + (delta_HR[12]-delta_HR[10])/2, delta_HR[12] + (delta_HR[13]-delta_HR[12])/2, delta_HR[13] + (delta_HR[14]-delta_HR[13])/2))



nhis_ipums_2000_2005 = fread("~/Downloads/nhis_00004.csv")
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
nhis_ipums_2000_2005= nhis_ipums_2000_2005 %>% mutate(no_of_cigarettes = case_when(CIGSDAY1 < 10 ~ "N_less_than_10",
                                                                                   CIGSDAY1 >= 10 & CIGSDAY1 < 20 ~ "N_10_20",
                                                                                   CIGSDAY1 >= 20 & CIGSDAY1 < 40 ~ "N_20_40",
                                                                                   CIGSDAY1 >= 40 & CIGSDAY1 <= 90 ~ "N_40_90"))

#Marital Status
nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(marital_status = case_when(MARST >= 10 & MARST <= 13 ~ "married",
                                                                                  MARST == 20 | MARST == 30 | MARST == 40 ~ "divorce",
                                                                                  MARST == 50 ~ "never_married"))
colnames(nhis_ipums_2000_2005) = tolower(colnames(nhis_ipums_2000_2005))
data_processed = nhis_ipums_2000_2005 %>% dplyr::select("mortstat_rec", "bmi_cat", "age", "sex", "education", "smoking_status",
                                                        "strata", "psu", "sampweight", "alcohol_status", "marital_status", "age_at_exit", "year", "no_of_cigarettes")  


nrow(na.omit(data_processed[,-12]))
#[1] 107362

data_processed$bmi_cat = as.factor(data_processed$bmi_cat)
data_processed$smoking_status = as.factor(data_processed$smoking_status)
data_processed$alcohol_status = as.factor(data_processed$alcohol_status)
data_processed$education = as.factor(data_processed$education)
data_processed$marital_status = as.factor(data_processed$marital_status)

#table(data_processed$mortstat_rec, data_processed$bmi_cat)

#Normal ObeseI ObeseII ObeseIII ObeseIV ObeseV
#0  34258  29687   19647     5813    5228   2065
#1   4188   3824    2654      795     753    371
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
nhis_samadult_design <-
  svydesign(
    id = ~psu ,
    strata = ~strata ,
    nest = TRUE ,
    weights = ~sampweight ,
    data = data_processed
  )

fit_unadjusted = svyglm(mortstat_rec ~ bmi_cat, design=nhis_samadult_design, family = quasibinomial())

#Removing individuals whose age is less than age_of_exit
data_processed_cox_ph = data_processed[-which(data_processed$age > data_processed$age_at_exit), ]
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
nhis_samadult_design_cox_ph <-
  svydesign(
    id = ~psu ,
    strata = ~strata ,
    nest = TRUE ,
    weights = ~sampweight ,
    data = data_processed_cox_ph
  )
fit_unadjusted_coxph = svycoxph(Surv(age, age_at_exit, mortstat_rec, type = "counting") ~ bmi_cat, design=nhis_samadult_design_cox_ph)
fit_minimal_coxph = svycoxph(Surv(age, age_at_exit, mortstat_rec, type = "counting") ~ bmi_cat + sex, design=nhis_samadult_design_cox_ph)
fit_full_coxph = svycoxph(Surv(age, age_at_exit, mortstat_rec, type = "counting") ~ bmi_cat + sex + education + smoking_status + alcohol_status + marital_status, design=nhis_samadult_design_cox_ph)



p_hat = sum(data_processed$sampweight[which(data_processed$mortstat_rec == 1)])/sum(data_processed$sampweight)
p_obs_D_given_E_equalto_0 = sum(data_processed$sampweight[which(data_processed$mortstat_rec == 1 & data_processed$bmi_cat == "Normal")])/sum(data_processed$sampweight[which(data_processed$bmi_cat == "Normal")])

prop_exposure = wpct(data_processed$bmi_cat, weight = data_processed$sampweight)
PAR_MR = 1 - (prop_exposure[1] + sum(prop_exposure[-1] * exp(fit_unadjusted$coefficients[2:6] - MR_estimates_UK))) * (p_obs_D_given_E_equalto_0/p_hat)
#Normal 
#0.1778511 
PAR_observed = 1 - (p_obs_D_given_E_equalto_0/p_hat)
#0.06917391
p_hat_cox_ph = sum(data_processed_cox_ph$sampweight[which(data_processed_cox_ph$mortstat_rec == 1)])/sum(data_processed_cox_ph$sampweight)
p_obs_D_given_E_equalto_0_cox_ph = sum(data_processed_cox_ph$sampweight[which(data_processed_cox_ph$mortstat_rec == 1 & data_processed_cox_ph$bmi_cat == "Normal")])/sum(data_processed_cox_ph$sampweight[which(data_processed_cox_ph$bmi_cat == "Normal")])
prop_exposure_cox_ph = wpct(data_processed_cox_ph$bmi_cat, weight = data_processed_cox_ph$sampweight)
PAR_MR_cox_ph = 1 - (prop_exposure_cox_ph[1] + sum(prop_exposure_cox_ph[-1] * exp(fit_unadjusted_coxph$coefficients[1:5] - MR_estimates_UK))) * (p_obs_D_given_E_equalto_0_cox_ph/p_hat_cox_ph)
#0.1956391
PAR_observed_cox_ph = 1 - (p_obs_D_given_E_equalto_0_cox_ph/p_hat_cox_ph)
#[1] 0.06915655



data_processed_complete = na.omit(data_processed[, -12])
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
nhis_samadult_design <-
  svydesign(
    id = ~psu ,
    strata = ~strata ,
    nest = TRUE ,
    weights = ~sampweight ,
    data = data_processed_complete
  )

fit_minimal = svyglm(mortstat_rec ~ bmi_cat + sex + age, design=nhis_samadult_design, family = quasibinomial())
fit_full = svyglm(mortstat_rec ~ bmi_cat + sex + age + education + smoking_status + no_of_cigarettes + alcohol_status + marital_status, design=nhis_samadult_design, family = quasibinomial())

indicator_normal = if_else(data_processed_complete$bmi_cat == "Normal", 1,0)
hh = model.matrix(~ bmi_cat + sex + age, data = data_processed_complete)
hh[, c(2:6)] = 0
PAR_minimally_adjusted = 1 - wtd.mean(exp(hh %*% as.numeric(fit_minimal$coefficients))/(1 + exp(hh %*% as.numeric(fit_minimal$coefficients))), data_processed_complete$sampweight)/p_hat
#[1]0.0249229
indicator_normal = if_else(data_processed_complete$bmi_cat == "Normal", 1, 0)
s = exp(hh %*% as.numeric(fit_minimal$coefficients))/(1 + exp(hh %*% as.numeric(fit_minimal$coefficients)))
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
Gamma = solve(t(hh[, -c(2:6)]) %*% (hh[, -c(2:6)] * dexpit(hh %*% as.numeric(fit_minimal$coefficients)) * (data_processed_complete$sampweight/sum(data_processed_complete$sampweight))))
c_star = t(c) %*% Gamma
M = as.matrix(rbind(c(1, c_star, 0), c(0, rep(0, length(c_star)), 1)))
g = c(1/p_hat, -wtd.mean(exp(hh %*% as.numeric(fit_minimal$coefficients))/(1 + exp(hh %*% as.numeric(fit_minimal$coefficients))), data_processed_complete$sampweight)/p_hat)
sqrt((1/nrow(data_processed_complete)) * t(g) %*% M %*% A %*% t(M) %*% g)
#0.004258147

hh = model.matrix(~ bmi_cat + sex + age + education + smoking_status + no_of_cigarettes + alcohol_status + marital_status, data = data_processed_complete)
hh[, c(2:6)] = 0
PAR_fully_adjusted = 1 - wtd.mean(exp(hh %*% as.numeric(fit_full$coefficients))/(1 + exp(hh %*% as.numeric(fit_full$coefficients))), data_processed_complete$sampweight)/p_hat
#[1] 0.02262194

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

#Variance of PAR calculation
k = 1/prop_exposure[1]
Q = -matrix(rep(-k, 25), 5, 5) + diag(-1/prop_exposure[-1])
Sigma_11 = vcov(fit_unadjusted) * nrow(data_processed)
total_weights = sum(data_processed$sampweight)
data_processed$bmi_cat = as.factor(data_processed$bmi_cat)

data_processed_mm = as.data.frame(model.matrix(~ bmi_cat + mortstat_rec + sampweight, data_processed))
w1 = (data_processed_mm$mortstat_rec - expit(as.matrix(data_processed_mm)[, c(1:6)] %*% fit_unadjusted$coefficients)) * data_processed_mm$sampweight
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
#0.04368677




#-----Stratification based on smoking status-- IPUMS 2000-2005#
#Smokers
MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_smokers.csv")
BMI_quantiles = MR_vcov_file$bmi.quant
delta_HR = exp(MR_vcov_file$est)
MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[8]-delta_HR[6])/2, delta_HR[8] + (delta_HR[10]-delta_HR[8])/2, delta_HR[10] + (delta_HR[12]-delta_HR[10])/2, delta_HR[12] + (delta_HR[13]-delta_HR[12])/2, delta_HR[13] + (delta_HR[14]-delta_HR[13])/2))

#MR_estimates_UK = log(c(1.08, 1.38, 3.44, 6.65))

nhis_ipums_2000_2005 = fread("~/Downloads/nhis_00004.csv")
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





#nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(bmi_cat = case_when(BMI < 23.8 ~ "Underweight",
 #                                                                          BMI >= 23.8 & BMI < 27.1 ~ "Normal",
  #                                                                         BMI >= 27.1 & BMI < 29.4 ~ "ObeseI",
   #                                                                        BMI >= 29.4 & BMI < 33.7 ~ "ObeseII",
    #                                                                       BMI >= 33.7 & BMI < 42.6 ~ "ObeseIII",
     #                                                                      BMI >= 42.6 ~ "ObeseIV"))

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
nhis_ipums_2000_2005= nhis_ipums_2000_2005 %>% mutate(no_of_cigarettes = case_when(CIGSDAY1 < 10 ~ "N_less_than_10",
                                                                                   CIGSDAY1 >= 10 & CIGSDAY1 < 20 ~ "N_10_20",
                                                                                   CIGSDAY1 >= 20 & CIGSDAY1 < 40 ~ "N_20_40",
                                                                                   CIGSDAY1 >= 40 & CIGSDAY1 <= 90 ~ "N_40_90"))

#Marital Status
nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(marital_status = case_when(MARST >= 10 & MARST <= 13 ~ "married",
                                                                                  MARST == 20 | MARST == 30 | MARST == 40 ~ "divorce",
                                                                                  MARST == 50 ~ "never_married"))
colnames(nhis_ipums_2000_2005) = tolower(colnames(nhis_ipums_2000_2005))
data_processed = nhis_ipums_2000_2005 %>% dplyr::select("mortstat_rec", "bmi_cat", "age", "sex", "education", "smoking_status",
                                                        "strata", "psu", "sampweight", "alcohol_status", "marital_status", "age_at_exit", "year", "no_of_cigarettes")  


nrow(na.omit(data_processed[, -12]))
#[1]89130

data_processed$bmi_cat = as.factor(data_processed$bmi_cat)
data_processed$smoking_status = as.factor(data_processed$smoking_status)
data_processed$alcohol_status = as.factor(data_processed$alcohol_status)
data_processed$education = as.factor(data_processed$education)
data_processed$marital_status = as.factor(data_processed$marital_status)

#data_processed_nonsmokers = data_processed[which(as.character(data_processed$smoking_status) == "Never"), ]
data_processed_smokers = data_processed[which(as.character(data_processed$smoking_status) == "Former" | as.character(data_processed$smoking_status) == "Current"), ]


nrow(na.omit(data_processed_smokers))
#[1] 41946
data_processed_smokers$bmi_cat = as.factor(data_processed_smokers$bmi_cat)
data_processed_smokers$smoking_status = as.factor(data_processed_smokers$smoking_status)
data_processed_smokers$alcohol_status = as.factor(data_processed_smokers$alcohol_status)
data_processed_smokers$education = as.factor(data_processed_smokers$education)
data_processed_smokers$marital_status = as.factor(data_processed_smokers$marital_status)

#table(data_processed$mortstat_rec, data_processed$bmi_cat)

#Normal ObeseI ObeseII ObeseIII ObeseIV ObeseV
#0  34258  29687   19647     5813    5228   2065
#1   4188   3824    2654      795     753    371
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
nhis_samadult_design <-
  svydesign(
    id = ~psu ,
    strata = ~strata ,
    nest = TRUE ,
    weights = ~sampweight ,
    data = data_processed_smokers
  )

fit_unadjusted = svyglm(mortstat_rec ~ bmi_cat, design=nhis_samadult_design, family = quasibinomial())

#Removing individuals whose age is less than age_of_exit
if(length(which(data_processed_smokers$age > data_processed_smokers$age_at_exit)) > 0)
{
  data_processed_cox_ph_smokers = data_processed_smokers[-which(data_processed_smokers$age > data_processed_smokers$age_at_exit), ]
}else{
  data_processed_cox_ph_smokers = data_processed_smokers
}

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
nhis_samadult_design_cox_ph_smokers <-
  svydesign(
    id = ~psu ,
    strata = ~strata ,
    nest = TRUE ,
    weights = ~sampweight ,
    data = data_processed_cox_ph_smokers
  )
fit_unadjusted_coxph = svycoxph(Surv(age, age_at_exit, mortstat_rec, type = "counting") ~ bmi_cat, design=nhis_samadult_design_cox_ph_smokers)
fit_minimal_coxph = svycoxph(Surv(age, age_at_exit, mortstat_rec, type = "counting") ~ bmi_cat + sex, design=nhis_samadult_design_cox_ph_smokers)
fit_full_coxph = svycoxph(Surv(age, age_at_exit, mortstat_rec, type = "counting") ~ bmi_cat + sex + education + alcohol_status + marital_status, design=nhis_samadult_design_cox_ph_smokers)



p_hat = sum(data_processed_smokers$sampweight[which(data_processed_smokers$mortstat_rec == 1)])/sum(data_processed_smokers$sampweight)
p_obs_D_given_E_equalto_0 = sum(data_processed_smokers$sampweight[which(data_processed_smokers$mortstat_rec == 1 & data_processed_smokers$bmi_cat == "Normal")])/sum(data_processed_smokers$sampweight[which(data_processed_smokers$bmi_cat == "Normal")])

prop_exposure = wpct(data_processed_smokers$bmi_cat, weight = data_processed_smokers$sampweight)
PAR_MR = 1 - (prop_exposure[1] + sum(prop_exposure[-1] * exp(fit_unadjusted$coefficients[2:6] - MR_estimates_UK))) * (p_obs_D_given_E_equalto_0/p_hat)
#Normal 
#0.1589423 

PAR_observed = 1 - (p_obs_D_given_E_equalto_0/p_hat)
#0.04213775
p_hat_cox_ph = sum(data_processed_cox_ph_smokers$sampweight[which(data_processed_cox_ph_smokers$mortstat_rec == 1)])/sum(data_processed_cox_ph_smokers$sampweight)
p_obs_D_given_E_equalto_0_cox_ph = sum(data_processed_cox_ph_smokers$sampweight[which(data_processed_cox_ph_smokers$mortstat_rec == 1 & data_processed_cox_ph_smokers$bmi_cat == "Normal")])/sum(data_processed_cox_ph_smokers$sampweight[which(data_processed_cox_ph_smokers$bmi_cat == "Normal")])
prop_exposure_cox_ph = wpct(data_processed_cox_ph_smokers$bmi_cat, weight = data_processed_cox_ph_smokers$sampweight)
PAR_MR_cox_ph = 1 - (prop_exposure_cox_ph[1] + sum(prop_exposure_cox_ph[-1] * exp(fit_unadjusted_coxph$coefficients[1:5] - MR_estimates_UK))) * (p_obs_D_given_E_equalto_0_cox_ph/p_hat_cox_ph)
#0.1764778 
PAR_observed_cox_ph = 1 - (p_obs_D_given_E_equalto_0_cox_ph/p_hat_cox_ph)
#[1] 0.02533969



data_processed_complete = na.omit(data_processed_smokers[, -12])
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
nhis_samadult_design <-
  svydesign(
    id = ~psu ,
    strata = ~strata ,
    nest = TRUE ,
    weights = ~sampweight ,
    data = data_processed_complete
  )

fit_minimal = svyglm(mortstat_rec ~ bmi_cat + sex + age, design=nhis_samadult_design, family = quasibinomial())
fit_full = svyglm(mortstat_rec ~ bmi_cat + sex + age + education + smoking_status + alcohol_status + marital_status, design=nhis_samadult_design, family = quasibinomial())

hh = model.matrix(~ bmi_cat + sex + age, data = data_processed_complete)
hh[, c(2:6)] = 0
PAR_minimally_adjusted = 1 - wtd.mean(exp(hh %*% as.numeric(fit_minimal$coefficients))/(1 + exp(hh %*% as.numeric(fit_minimal$coefficients))), data_processed_complete$sampweight)/p_hat
#[1]0.01148301

indicator_normal = if_else(data_processed_complete$bmi_cat == "Normal", 1, 0)
s = exp(hh %*% as.numeric(fit_minimal$coefficients))/(1 + exp(hh %*% as.numeric(fit_minimal$coefficients)))
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
Gamma = solve(t(hh[, -c(2:6)]) %*% (hh[, -c(2:6)] * dexpit(hh %*% as.numeric(fit_minimal$coefficients)) * (data_processed_complete$sampweight/sum(data_processed_complete$sampweight))))
c_star = t(c) %*% Gamma
M = as.matrix(rbind(c(1, c_star, 0), c(0, rep(0, length(c_star)), 1)))
g = c(1/p_hat, -wtd.mean(exp(hh %*% as.numeric(fit_minimal$coefficients))/(1 + exp(hh %*% as.numeric(fit_minimal$coefficients))), data_processed_complete$sampweight)/p_hat)
sqrt((1/nrow(data_processed_complete)) * t(g) %*% M %*% A %*% t(M) %*% g)

hh = model.matrix(~ bmi_cat + sex + age + education + smoking_status + alcohol_status + marital_status, data = data_processed_complete)
hh = hh[,-12]
hh[, c(2:6)] = 0
PAR_fully_adjusted = 1 - wtd.mean(exp(hh %*% as.numeric(fit_full$coefficients))/(1 + exp(hh %*% as.numeric(fit_full$coefficients))), data_processed_complete$sampweight)/p_hat
#[1] 8.51002e-05
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



#Variance of PAR calculation
k = 1/prop_exposure[1]
Q = -matrix(rep(-k, 25), 5, 5) + diag(-1/prop_exposure[-1])
Sigma_11 = vcov(fit_unadjusted) * nrow(data_processed_smokers)
total_weights = sum(data_processed_smokers$sampweight)
data_processed_smokers$bmi_cat = as.factor(data_processed_smokers$bmi_cat)

data_processed_mm = as.data.frame(model.matrix(~ bmi_cat + mortstat_rec + sampweight, data_processed_smokers))
w1 = (data_processed_mm$mortstat_rec - expit(as.matrix(data_processed_mm)[, c(1:6)] %*% fit_unadjusted$coefficients)) * data_processed_mm$sampweight
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

#MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_smokers.csv") 
#delta = MR_vcov_file[, 3]
#MR_vcov_file = MR_vcov_file[, -c(1,2,3)]
#MR_vcov = MR_vcov_file[c(4, 9, 11, 13,14), c(4, 9, 11, 13,14)]
#R = matrix(0, 4, 5)
#R[1, 1:2] = c(exp(delta[4]), exp(delta[9]))/2
#R[2, 2:3] = c(exp(delta[9]), exp(delta[11]))/2
#R[3, 3:4] = c(exp(delta[11]), exp(delta[13]))/2
#R[4, 4:5] = c(exp(delta[13]), exp(delta[14]))/2

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


#--Non-smokers#
MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_nonsmokers.csv")
BMI_quantiles = MR_vcov_file$bmi.quant
delta_HR = exp(MR_vcov_file$est)
MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[8]-delta_HR[6])/2, delta_HR[8] + (delta_HR[10]-delta_HR[8])/2, delta_HR[10] + (delta_HR[12]-delta_HR[10])/2, delta_HR[12] + (delta_HR[13]-delta_HR[12])/2, delta_HR[13] + (delta_HR[14]-delta_HR[13])/2))

#MR_estimates_UK = log(c(1.09, 1.195, 2.065, 2.915))

nhis_ipums_2000_2005 = fread("~/Downloads/nhis_00004.csv")
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





#nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(bmi_cat = case_when(BMI < 19 ~ "Underweight",
 #                                                                          BMI >= 19 & BMI < 26.4 ~ "Normal",
  #                                                                         BMI >= 26.4 & BMI < 28.7 ~ "ObeseI",
   #                                                                        BMI >= 28.7 & BMI < 33.2 ~ "ObeseII",
    #                                                                       BMI >= 33.2 & BMI < 42.2 ~ "ObeseIII",
     #                                                                      BMI >= 42.2 ~ "ObeseIV"))
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
                                                        "strata", "psu", "sampweight", "alcohol_status", "marital_status", "age_at_exit", "year")  


nrow(na.omit(data_processed[, -12]))
#[1] 126573

data_processed$bmi_cat = as.factor(data_processed$bmi_cat)
data_processed$smoking_status = as.factor(data_processed$smoking_status)
data_processed$alcohol_status = as.factor(data_processed$alcohol_status)
data_processed$education = as.factor(data_processed$education)
data_processed$marital_status = as.factor(data_processed$marital_status)

data_processed_nonsmokers = data_processed[which(as.character(data_processed$smoking_status) == "Never"), ]
#data_processed_smokers = data_processed[which(as.character(data_processed$smoking_status) == "Former" | as.character(data_processed$smoking_status) == "Current"), ]


nrow(na.omit(data_processed_nonsmokers[, -12]))
#[1] 41692
data_processed_nonsmokers$bmi_cat = as.factor(data_processed_nonsmokers$bmi_cat)
data_processed_nonsmokers$smoking_status = as.factor(data_processed_nonsmokers$smoking_status)
data_processed_nonsmokers$alcohol_status = as.factor(data_processed_nonsmokers$alcohol_status)
data_processed_nonsmokers$education = as.factor(data_processed_nonsmokers$education)
data_processed_nonsmokers$marital_status = as.factor(data_processed_nonsmokers$marital_status)

#table(data_processed$mortstat_rec, data_processed$bmi_cat)

#Normal ObeseI ObeseII ObeseIII ObeseIV ObeseV
#0  34258  29687   19647     5813    5228   2065
#1   4188   3824    2654      795     753    371
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
nhis_samadult_design <-
  svydesign(
    id = ~psu ,
    strata = ~strata ,
    nest = TRUE ,
    weights = ~sampweight ,
    data = data_processed_nonsmokers
  )

fit_unadjusted = svyglm(mortstat_rec ~ bmi_cat, design=nhis_samadult_design, family = quasibinomial())

#Removing individuals whose age is less than age_of_exit
if(length(which(data_processed_nonsmokers$age > data_processed_nonsmokers$age_at_exit)) > 0)
{
  data_processed_cox_ph_nonsmokers = data_processed_nonsmokers[-which(data_processed_nonsmokers$age > data_processed_nonsmokers$age_at_exit), ]
}else{
  data_processed_cox_ph_nonsmokers = data_processed_nonsmokers
}

options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
nhis_samadult_design_cox_ph_nonsmokers <-
  svydesign(
    id = ~psu ,
    strata = ~strata ,
    nest = TRUE ,
    weights = ~sampweight ,
    data = data_processed_cox_ph_nonsmokers
  )
fit_unadjusted_coxph = svycoxph(Surv(age, age_at_exit, mortstat_rec, type = "counting") ~ bmi_cat, design=nhis_samadult_design_cox_ph_nonsmokers)
fit_minimal_coxph = svycoxph(Surv(age, age_at_exit, mortstat_rec, type = "counting") ~ bmi_cat + sex, design=nhis_samadult_design_cox_ph_nonsmokers)
fit_full_coxph = svycoxph(Surv(age, age_at_exit, mortstat_rec, type = "counting") ~ bmi_cat + sex + education + alcohol_status + marital_status, design=nhis_samadult_design_cox_ph_nonsmokers)



p_hat = sum(data_processed_nonsmokers$sampweight[which(data_processed_nonsmokers$mortstat_rec == 1)])/sum(data_processed_nonsmokers$sampweight)
p_obs_D_given_E_equalto_0 = sum(data_processed_nonsmokers$sampweight[which(data_processed_nonsmokers$mortstat_rec == 1 & data_processed_nonsmokers$bmi_cat == "Normal")])/sum(data_processed_nonsmokers$sampweight[which(data_processed_nonsmokers$bmi_cat == "Normal")])

prop_exposure = wpct(data_processed_nonsmokers$bmi_cat, weight = data_processed_nonsmokers$sampweight)
PAR_MR = 1 - (prop_exposure[1] + sum(prop_exposure[-1] * exp(fit_unadjusted$coefficients[2:6] - MR_estimates_UK))) * (p_obs_D_given_E_equalto_0/p_hat)
#Normal 
#0.1510221 

PAR_observed = 1 - (p_obs_D_given_E_equalto_0/p_hat)
# 0.1077974
p_hat_cox_ph = sum(data_processed_cox_ph_nonsmokers$sampweight[which(data_processed_cox_ph_nonsmokers$mortstat_rec == 1)])/sum(data_processed_cox_ph_nonsmokers$sampweight)
p_obs_D_given_E_equalto_0_cox_ph = sum(data_processed_cox_ph_nonsmokers$sampweight[which(data_processed_cox_ph_nonsmokers$mortstat_rec == 1 & data_processed_cox_ph_nonsmokers$bmi_cat == "Normal")])/sum(data_processed_cox_ph_nonsmokers$sampweight[which(data_processed_cox_ph_nonsmokers$bmi_cat == "Normal")])
prop_exposure_cox_ph = wpct(data_processed_cox_ph_nonsmokers$bmi_cat, weight = data_processed_cox_ph_nonsmokers$sampweight)
PAR_MR_cox_ph = 1 - (prop_exposure_cox_ph[1] + sum(prop_exposure_cox_ph[-1] * exp(fit_unadjusted_coxph$coefficients[1:4] - MR_estimates_UK))) * (p_obs_D_given_E_equalto_0_cox_ph/p_hat_cox_ph)
#0.1399601
PAR_observed_cox_ph = 1 - (p_obs_D_given_E_equalto_0_cox_ph/p_hat_cox_ph)
#[1] 0.1076844



data_processed_complete = na.omit(data_processed_nonsmokers[, -12])
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
nhis_samadult_design <-
  svydesign(
    id = ~psu ,
    strata = ~strata ,
    nest = TRUE ,
    weights = ~sampweight ,
    data = data_processed_complete
  )

fit_minimal = svyglm(mortstat_rec ~ bmi_cat + sex + age, design=nhis_samadult_design, family = quasibinomial())
fit_full = svyglm(mortstat_rec ~ bmi_cat + sex + age + education + alcohol_status + marital_status, design=nhis_samadult_design, family = quasibinomial())

hh = model.matrix(~ bmi_cat + sex + age, data = data_processed_complete)
hh[, c(2:6)] = 0
PAR_minimally_adjusted = 1 - wtd.mean(exp(hh %*% as.numeric(fit_minimal$coefficients))/(1 + exp(hh %*% as.numeric(fit_minimal$coefficients))), data_processed_complete$sampweight)/p_hat
#[1]0.05918836

indicator_normal = if_else(data_processed_complete$bmi_cat == "Normal", 1, 0)
s = exp(hh %*% as.numeric(fit_minimal$coefficients))/(1 + exp(hh %*% as.numeric(fit_minimal$coefficients)))
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
Gamma = solve(t(hh[, -c(2:6)]) %*% (hh[, -c(2:6)] * dexpit(hh %*% as.numeric(fit_minimal$coefficients)) * (data_processed_complete$sampweight/sum(data_processed_complete$sampweight))))
c_star = t(c) %*% Gamma
M = as.matrix(rbind(c(1, c_star, 0), c(0, rep(0, length(c_star)), 1)))
g = c(1/p_hat, -wtd.mean(exp(hh %*% as.numeric(fit_minimal$coefficients))/(1 + exp(hh %*% as.numeric(fit_minimal$coefficients))), data_processed_complete$sampweight)/p_hat)
sqrt((1/nrow(data_processed_complete)) * t(g) %*% M %*% A %*% t(M) %*% g)


hh = model.matrix(~ bmi_cat + sex + age + education + alcohol_status + marital_status, data = data_processed_complete)
hh[, c(2:6)] = 0
PAR_fully_adjusted = 1 - wtd.mean(exp(hh %*% as.numeric(fit_full$coefficients))/(1 + exp(hh %*% as.numeric(fit_full$coefficients))), data_processed_complete$sampweight)/p_hat
#[1] 0.04183644

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


#Variance of PAR calculation
k = 1/prop_exposure[1]
Q = -matrix(rep(-k, 25), 5, 5) + diag(-1/prop_exposure[-1])
Sigma_11 = vcov(fit_unadjusted) * nrow(data_processed_nonsmokers)
total_weights = sum(data_processed_nonsmokers$sampweight)
data_processed_nonsmokers$bmi_cat = as.factor(data_processed_nonsmokers$bmi_cat)

data_processed_mm = as.data.frame(model.matrix(~ bmi_cat + mortstat_rec + sampweight, data_processed_nonsmokers))
w1 = (data_processed_mm$mortstat_rec - expit(as.matrix(data_processed_mm)[, c(1:6)] %*% fit_unadjusted$coefficients)) * data_processed_mm$sampweight
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

#MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_nonsmokers.csv") 
#delta = MR_vcov_file[, 3]
#MR_vcov_file = MR_vcov_file[, -c(1,2,3)]
#MR_vcov = MR_vcov_file[c(7, 9, 11, 13,14), c(7, 9, 11, 13,14)]

#R = matrix(0, 4, 5)
#R[1, 1:2] = c(exp(delta[7]), exp(delta[9]))/2
#R[2, 2:3] = c(exp(delta[9]), exp(delta[11]))/2
#R[3, 3:4] = c(exp(delta[11]), exp(delta[13]))/2
#R[4, 4:5] = c(exp(delta[13]), exp(delta[14]))/2
#Omega = R %*% as.matrix(MR_vcov) %*% t(R)


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

#Omega_star = diag(1/exp(MR_estimates_UK)) %*% Omega %*% diag(1/exp(MR_estimates_UK))

#PAR_var = t(a) %*% Sigma %*% a + t(b) %*% Omega_star %*% b





#---- Heart disease---#
MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_cardiovascular.csv")
BMI_quantiles = MR_vcov_file$bmi.quant
delta_HR = exp(MR_vcov_file$est)
MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[8]-delta_HR[6])/2, delta_HR[8] + (delta_HR[10]-delta_HR[8])/2, delta_HR[10] + (delta_HR[12]-delta_HR[10])/2, delta_HR[12] + (delta_HR[13]-delta_HR[12])/2, delta_HR[13] + (delta_HR[14]-delta_HR[13])/2))

#MR_estimates_UK = log(c(1.09, 1.195, 2.065, 2.915))

nhis_ipums_2000_2005 = fread("~/Downloads/nhis_00004.csv")
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
nhis_ipums_2000_2005= nhis_ipums_2000_2005 %>% mutate(no_of_cigarettes = case_when(CIGSDAY1 < 10 ~ "N_less_than_10",
                                                                                   CIGSDAY1 >= 10 & CIGSDAY1 < 20 ~ "N_10_20",
                                                                                   CIGSDAY1 >= 20 & CIGSDAY1 < 40 ~ "N_20_40",
                                                                                   CIGSDAY1 >= 40 & CIGSDAY1 <= 90 ~ "N_40_90"))

#Marital Status
nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(marital_status = case_when(MARST >= 10 & MARST <= 13 ~ "married",
                                                                                  MARST == 20 | MARST == 30 | MARST == 40 ~ "divorce",
                                                                                  MARST == 50 ~ "never_married"))

#nhis_ipums_2000_2005 = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$MORTUCODLD == 1), ]
colnames(nhis_ipums_2000_2005) = tolower(colnames(nhis_ipums_2000_2005))


data_processed = nhis_ipums_2000_2005 %>% dplyr::select("mortstat_rec", "bmi_cat", "age", "sex", "education", "smoking_status",
                                                        "strata", "psu", "sampweight", "alcohol_status", "marital_status", "age_at_exit", "year", "no_of_cigarettes")  


nrow(na.omit(data_processed[, -12]))
#[1] 126242

data_processed$bmi_cat = as.factor(data_processed$bmi_cat)
data_processed$smoking_status = as.factor(data_processed$smoking_status)
data_processed$alcohol_status = as.factor(data_processed$alcohol_status)
data_processed$education = as.factor(data_processed$education)
data_processed$marital_status = as.factor(data_processed$marital_status)


options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
nhis_samadult_design <-
  svydesign(
    id = ~psu ,
    strata = ~strata ,
    nest = TRUE ,
    weights = ~sampweight ,
    data = data_processed
  )

fit_unadjusted = svyglm(mortstat_rec ~ bmi_cat, design=nhis_samadult_design, family = quasibinomial())

#Removing individuals whose age is less than age_of_exit
data_processed_cox_ph = data_processed[-which(data_processed$age > data_processed$age_at_exit), ]
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
nhis_samadult_design_cox_ph <-
  svydesign(
    id = ~psu ,
    strata = ~strata ,
    nest = TRUE ,
    weights = ~sampweight ,
    data = data_processed_cox_ph
  )
fit_unadjusted_coxph = svycoxph(Surv(age, age_at_exit, mortstat_rec, type = "counting") ~ bmi_cat, design=nhis_samadult_design_cox_ph)
fit_minimal_coxph = svycoxph(Surv(age, age_at_exit, mortstat_rec, type = "counting") ~ bmi_cat + sex, design=nhis_samadult_design_cox_ph)
fit_full_coxph = svycoxph(Surv(age, age_at_exit, mortstat_rec, type = "counting") ~ bmi_cat + sex + education + smoking_status + alcohol_status + marital_status, design=nhis_samadult_design_cox_ph)



p_hat = sum(data_processed$sampweight[which(data_processed$mortstat_rec == 1)])/sum(data_processed$sampweight)
p_obs_D_given_E_equalto_0 = sum(data_processed$sampweight[which(data_processed$mortstat_rec == 1 & data_processed$bmi_cat == "Normal")])/sum(data_processed$sampweight[which(data_processed$bmi_cat == "Normal")])

prop_exposure = wpct(data_processed$bmi_cat, weight = data_processed$sampweight)
PAR_MR = 1 - (prop_exposure[1] + sum(prop_exposure[-1] * exp(fit_unadjusted$coefficients[2:6] - MR_estimates_UK))) * (p_obs_D_given_E_equalto_0/p_hat)
#Normal 
#0.2293703   

PAR_observed = 1 - (p_obs_D_given_E_equalto_0/p_hat)
#0.1253837
p_hat_cox_ph = sum(data_processed_cox_ph$sampweight[which(data_processed_cox_ph$mortstat_rec == 1)])/sum(data_processed_cox_ph$sampweight)
p_obs_D_given_E_equalto_0_cox_ph = sum(data_processed_cox_ph$sampweight[which(data_processed_cox_ph$mortstat_rec == 1 & data_processed_cox_ph$bmi_cat == "Normal")])/sum(data_processed_cox_ph$sampweight[which(data_processed_cox_ph$bmi_cat == "Normal")])
prop_exposure_cox_ph = wpct(data_processed_cox_ph$bmi_cat, weight = data_processed_cox_ph$sampweight)
PAR_MR_cox_ph = 1 - (prop_exposure_cox_ph[1] + sum(prop_exposure_cox_ph[-1] * exp(fit_unadjusted_coxph$coefficients[1:5] - MR_estimates_UK))) * (p_obs_D_given_E_equalto_0_cox_ph/p_hat_cox_ph)
#0.245041 
PAR_observed_cox_ph = 1 - (p_obs_D_given_E_equalto_0_cox_ph/p_hat_cox_ph)
#[1] 0.1257216



data_processed_complete = na.omit(data_processed[, -12])
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
nhis_samadult_design <-
  svydesign(
    id = ~psu ,
    strata = ~strata ,
    nest = TRUE ,
    weights = ~sampweight ,
    data = data_processed_complete
  )

fit_minimal = svyglm(mortstat_rec ~ bmi_cat + sex + age, design=nhis_samadult_design, family = quasibinomial())
fit_full = svyglm(mortstat_rec ~ bmi_cat + sex + age + education + smoking_status + no_of_cigarettes + alcohol_status + marital_status, design=nhis_samadult_design, family = quasibinomial())

hh = model.matrix(~ bmi_cat + sex + age, data = data_processed_complete)
hh[, c(2:6)] = 0
PAR_minimally_adjusted = 1 - wtd.mean(exp(hh %*% as.numeric(fit_minimal$coefficients))/(1 + exp(hh %*% as.numeric(fit_minimal$coefficients))), data_processed_complete$sampweight)/p_hat
#[1]0.08235997

indicator_normal = if_else(data_processed_complete$bmi_cat == "Normal", 1, 0)
s = exp(hh %*% as.numeric(fit_minimal$coefficients))/(1 + exp(hh %*% as.numeric(fit_minimal$coefficients)))
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
Gamma = solve(t(hh[, -c(2:6)]) %*% (hh[, -c(2:6)] * dexpit(hh %*% as.numeric(fit_minimal$coefficients)) * (data_processed_complete$sampweight/sum(data_processed_complete$sampweight))))
c_star = t(c) %*% Gamma
M = as.matrix(rbind(c(1, c_star, 0), c(0, rep(0, length(c_star)), 1)))
g = c(1/p_hat, -wtd.mean(exp(hh %*% as.numeric(fit_minimal$coefficients))/(1 + exp(hh %*% as.numeric(fit_minimal$coefficients))), data_processed_complete$sampweight)/p_hat)
sqrt((1/nrow(data_processed_complete)) * t(g) %*% M %*% A %*% t(M) %*% g)


hh = model.matrix(~ bmi_cat + sex + age + education + smoking_status  + no_of_cigarettes + alcohol_status + marital_status, data = data_processed_complete)
hh[, c(2:6)] = 0
PAR_fully_adjusted = 1 - wtd.mean(exp(hh %*% as.numeric(fit_full$coefficients))/(1 + exp(hh %*% as.numeric(fit_full$coefficients))), data_processed_complete$sampweight)/p_hat
#[1] 0.1037972

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

#Variance of PAR calculation
k = 1/prop_exposure[1]
Q = -matrix(rep(-k, 25), 5, 5) + diag(-1/prop_exposure[-1])
Sigma_11 = vcov(fit_unadjusted) * nrow(data_processed)
total_weights = sum(data_processed$sampweight)
data_processed$bmi_cat = as.factor(data_processed$bmi_cat)

data_processed_mm = as.data.frame(model.matrix(~ bmi_cat + mortstat_rec + sampweight, data_processed))
w1 = (data_processed_mm$mortstat_rec - expit(as.matrix(data_processed_mm)[, c(1:6)] %*% fit_unadjusted$coefficients)) * data_processed_mm$sampweight
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

delta = MR_vcov_file[, 3]
var_delta = diag(as.matrix(MR_vcov_file[, -c(1:3)]))
L_CI = delta - 1.96*sqrt(var_delta)
U_CI = delta + 1.96*sqrt(var_delta)
#View(cbind(MR_vcov_file[, 2], exp(delta), exp(L_CI), exp(U_CI)))
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


#Cancer----#
MR_vcov_file = read.csv("~/Dropbox/Attributable_risk/jhu_cancer.csv")
BMI_quantiles = MR_vcov_file$bmi.quant
delta_HR = exp(MR_vcov_file$est)
#MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[9]-delta_HR[6])/2, delta_HR[9] + (delta_HR[11]-delta_HR[9])/2, delta_HR[11] + (delta_HR[13]-delta_HR[11])/2, delta_HR[13] + (delta_HR[14]-delta_HR[13])/2))
MR_estimates_UK = log(c(delta_HR[6] + (delta_HR[8]-delta_HR[6])/2, delta_HR[8] + (delta_HR[10]-delta_HR[8])/2, delta_HR[10] + (delta_HR[12]-delta_HR[10])/2, delta_HR[12] + (delta_HR[13]-delta_HR[12])/2, delta_HR[13] + (delta_HR[14]-delta_HR[13])/2))

#MR_estimates_UK = log(c(1.09, 1.195, 2.065, 2.915))

nhis_ipums_2000_2005 = fread("~/Downloads/nhis_00004.csv")
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
nhis_ipums_2000_2005= nhis_ipums_2000_2005 %>% mutate(no_of_cigarettes = case_when(CIGSDAY1 < 10 ~ "N_less_than_10",
                                                                                   CIGSDAY1 >= 10 & CIGSDAY1 < 20 ~ "N_10_20",
                                                                                   CIGSDAY1 >= 20 & CIGSDAY1 < 40 ~ "N_20_40",
                                                                                   CIGSDAY1 >= 40 & CIGSDAY1 <= 90 ~ "N_40_90"))

#Marital Status
nhis_ipums_2000_2005 = nhis_ipums_2000_2005 %>% mutate(marital_status = case_when(MARST >= 10 & MARST <= 13 ~ "married",
                                                                                  MARST == 20 | MARST == 30 | MARST == 40 ~ "divorce",
                                                                                  MARST == 50 ~ "never_married"))

#nhis_ipums_2000_2005 = nhis_ipums_2000_2005[which(nhis_ipums_2000_2005$MORTUCODLD == 1), ]
colnames(nhis_ipums_2000_2005) = tolower(colnames(nhis_ipums_2000_2005))


data_processed = nhis_ipums_2000_2005 %>% dplyr::select("mortstat_rec", "bmi_cat", "age", "sex", "education", "smoking_status",
                                                        "strata", "psu", "sampweight", "alcohol_status", "marital_status", "age_at_exit", "year", "no_of_cigarettes")  


nrow(na.omit(data_processed[, -12]))
#[1] 115815

data_processed$bmi_cat = as.factor(data_processed$bmi_cat)
data_processed$smoking_status = as.factor(data_processed$smoking_status)
data_processed$alcohol_status = as.factor(data_processed$alcohol_status)
data_processed$education = as.factor(data_processed$education)
data_processed$marital_status = as.factor(data_processed$marital_status)


options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
nhis_samadult_design <-
  svydesign(
    id = ~psu ,
    strata = ~strata ,
    nest = TRUE ,
    weights = ~sampweight ,
    data = data_processed
  )

fit_unadjusted = svyglm(mortstat_rec ~ bmi_cat, design=nhis_samadult_design, family = quasibinomial())

#Removing individuals whose age is less than age_of_exit
data_processed_cox_ph = data_processed[-which(data_processed$age > data_processed$age_at_exit), ]
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
nhis_samadult_design_cox_ph <-
  svydesign(
    id = ~psu ,
    strata = ~strata ,
    nest = TRUE ,
    weights = ~sampweight ,
    data = data_processed_cox_ph
  )
fit_unadjusted_coxph = svycoxph(Surv(age, age_at_exit, mortstat_rec, type = "counting") ~ bmi_cat, design=nhis_samadult_design_cox_ph)
fit_minimal_coxph = svycoxph(Surv(age, age_at_exit, mortstat_rec, type = "counting") ~ bmi_cat + sex, design=nhis_samadult_design_cox_ph)
fit_full_coxph = svycoxph(Surv(age, age_at_exit, mortstat_rec, type = "counting") ~ bmi_cat + sex + education + smoking_status + alcohol_status + marital_status, design=nhis_samadult_design_cox_ph)



p_hat = sum(data_processed$sampweight[which(data_processed$mortstat_rec == 1)])/sum(data_processed$sampweight)
p_obs_D_given_E_equalto_0 = sum(data_processed$sampweight[which(data_processed$mortstat_rec == 1 & data_processed$bmi_cat == "Normal")])/sum(data_processed$sampweight[which(data_processed$bmi_cat == "Normal")])

prop_exposure = wpct(data_processed$bmi_cat, weight = data_processed$sampweight)
PAR_MR = 1 - (prop_exposure[1] + sum(prop_exposure[-1] * exp(fit_unadjusted$coefficients[2:6] - MR_estimates_UK))) * (p_obs_D_given_E_equalto_0/p_hat)
#Normal 
#0.08706198  

PAR_observed = 1 - (p_obs_D_given_E_equalto_0/p_hat)
#0.06089223
p_hat_cox_ph = sum(data_processed_cox_ph$sampweight[which(data_processed_cox_ph$mortstat_rec == 1)])/sum(data_processed_cox_ph$sampweight)
p_obs_D_given_E_equalto_0_cox_ph = sum(data_processed_cox_ph$sampweight[which(data_processed_cox_ph$mortstat_rec == 1 & data_processed_cox_ph$bmi_cat == "Normal")])/sum(data_processed_cox_ph$sampweight[which(data_processed_cox_ph$bmi_cat == "Normal")])
prop_exposure_cox_ph = wpct(data_processed_cox_ph$bmi_cat, weight = data_processed_cox_ph$sampweight)
PAR_MR_cox_ph = 1 - (prop_exposure_cox_ph[1] + sum(prop_exposure_cox_ph[-1] * exp(fit_unadjusted_coxph$coefficients[1:5] - MR_estimates_UK))) * (p_obs_D_given_E_equalto_0_cox_ph/p_hat_cox_ph)
#0.1592571 
PAR_observed_cox_ph = 1 - (p_obs_D_given_E_equalto_0_cox_ph/p_hat_cox_ph)
#[1] 0.05904235



data_processed_complete = na.omit(data_processed[, -12])
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
nhis_samadult_design <-
  svydesign(
    id = ~psu ,
    strata = ~strata ,
    nest = TRUE ,
    weights = ~sampweight ,
    data = data_processed_complete
  )

fit_minimal = svyglm(mortstat_rec ~ bmi_cat + sex + age, design=nhis_samadult_design, family = quasibinomial())
fit_full = svyglm(mortstat_rec ~ bmi_cat + sex + age + education + smoking_status + no_of_cigarettes + alcohol_status + marital_status, design=nhis_samadult_design, family = quasibinomial())

hh = model.matrix(~ bmi_cat + sex + age, data = data_processed_complete)
hh[, c(2:6)] = 0
PAR_minimally_adjusted = 1 - wtd.mean(exp(hh %*% as.numeric(fit_minimal$coefficients))/(1 + exp(hh %*% as.numeric(fit_minimal$coefficients))), data_processed_complete$sampweight)/p_hat
#[1]-0.0130734

indicator_normal = if_else(data_processed_complete$bmi_cat == "Normal", 1, 0)
s = exp(hh %*% as.numeric(fit_minimal$coefficients))/(1 + exp(hh %*% as.numeric(fit_minimal$coefficients)))
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
Gamma = solve(t(hh[, -c(2:6)]) %*% (hh[, -c(2:6)] * dexpit(hh %*% as.numeric(fit_minimal$coefficients)) * (data_processed_complete$sampweight/sum(data_processed_complete$sampweight))))
c_star = t(c) %*% Gamma
M = as.matrix(rbind(c(1, c_star, 0), c(0, rep(0, length(c_star)), 1)))
g = c(1/p_hat, -wtd.mean(exp(hh %*% as.numeric(fit_minimal$coefficients))/(1 + exp(hh %*% as.numeric(fit_minimal$coefficients))), data_processed_complete$sampweight)/p_hat)
sqrt((1/nrow(data_processed_complete)) * t(g) %*% M %*% A %*% t(M) %*% g)
#0.004047349
hh = model.matrix(~ bmi_cat + sex + age + education + smoking_status + no_of_cigarettes + alcohol_status + marital_status, data = data_processed_complete)
hh[, c(2:6)] = 0
PAR_fully_adjusted = 1 - wtd.mean(exp(hh %*% as.numeric(fit_full$coefficients))/(1 + exp(hh %*% as.numeric(fit_full$coefficients))), data_processed_complete$sampweight)/p_hat
#[1] 0.002738155

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
#0.004255071

#Variance of PAR calculation
k = 1/prop_exposure[1]
Q = -matrix(rep(-k, 25), 5, 5) + diag(-1/prop_exposure[-1])
Sigma_11 = vcov(fit_unadjusted) * nrow(data_processed)
total_weights = sum(data_processed$sampweight)
data_processed$bmi_cat = as.factor(data_processed$bmi_cat)

data_processed_mm = as.data.frame(model.matrix(~ bmi_cat + mortstat_rec + sampweight, data_processed))
w1 = (data_processed_mm$mortstat_rec - expit(as.matrix(data_processed_mm)[, c(1:6)] %*% fit_unadjusted$coefficients)) * data_processed_mm$sampweight
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

delta = MR_vcov_file[, 3]
var_delta = diag(as.matrix(MR_vcov_file[, -c(1:3)]))
L_CI = delta - 1.96*sqrt(var_delta)
U_CI = delta + 1.96*sqrt(var_delta)

#MR_vcov_file = MR_vcov_file[, -c(1,2,3)]
#MR_vcov = MR_vcov_file[c(6, 9, 11, 13,14), c(6, 9, 11, 13,14)]

#R = matrix(0, 4, 5)
#R[1, 1:2] = c(exp(delta[6]), exp(delta[9]))/2
#R[2, 2:3] = c(exp(delta[9]), exp(delta[11]))/2
#R[3, 3:4] = c(exp(delta[11]), exp(delta[13]))/2
#R[4, 4:5] = c(exp(delta[13]), exp(delta[14]))/2

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




