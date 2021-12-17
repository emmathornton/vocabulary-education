#ANALYSES
library(tidyverse)
library(miceadds)
library(mice)
library(swfscMisc)
library(imputools)
library(haven)
library(sjmisc)
library(Hmisc)
library(psych)
library(lavaan)
library(mice)
library(miceadds)
#load in imputed data
#load imputed dataset code here

#RQ1 & 2 - DOES VOCAB PREDICT EDUCATIONAL ATTAINMENT?

#1. Binary Outcome variable ####
# 1a. does vocabulary predict reaching a functional level in core subjects?
#unadjusted relationship

binary_unadjusted = with(imputed_mcs2, glm(benchmark_binary ~ age5_standardised,
                                        family = binomial,  weights = weight))

binary_unadjusted_results = summary(pool(binary_unadjusted), conf.int = TRUE, exponentiate = TRUE)

# adjusted relationship
binary_model1 = with(imputed_mcs2, glm(benchmark_binary ~ sex + ethnicity + EAL + country +
                                         highest_nvq + oecd_income + occupational_status + wealth_quintiles + imd, 
                                       family = binomial, weights = weight))

binary_model2 = with(imputed_mcs2, glm(benchmark_binary ~ sex + ethnicity + EAL + country +
                                         highest_nvq + oecd_income + occupational_status + wealth_quintiles + 
                                         imd + caregiver_vocabStandardised, 
                                       family = binomial, weights = weight))

binary_model3 = with(imputed_mcs2, glm(benchmark_binary ~ sex + ethnicity + EAL + country +
                                         highest_nvq + oecd_income + occupational_status + wealth_quintiles + 
                                         imd + caregiver_vocabStandardised + age5_standardised, 
                                       family = binomial, weights = weight))

binary_model1Results = summary(pool(binary_model1), conf.int = TRUE, exponentiate = TRUE)
binary_model2Results = summary(pool(binary_model2), conf.int = TRUE, exponentiate = TRUE)
binary_model3Results = summary(pool(binary_model3), conf.int = TRUE, exponentiate = TRUE)


#1b. does vocabulary predict reaching a functional level of education above and beyond SEC/caregiver vocab factors?
#compare model with only sociodemographic factors to a model with caregiver vocab
D1(binary_model2, binary_model1) #check if need to use a specific method ?
#compare model with all confounders to model with vocab
D1(binary_model3, binary_model2) 

#2. Continuous outcome variable####
#2a does vocabulary predict level of achievement in core subjects regardless of pass/ fail? 
#unadjusted relationship

continuous_unadjusted = with(imputed_mcs2, lm(standardised_core_subjects ~ age5_standardised, weights = weight))

continuous_unadjusted_results = summary(pool(continuous_unadjusted), conf.int = TRUE)

#adjusted relationship 
continuous_model1 = with(imputed_mcs2, lm(standardised_core_subjects ~ sex + ethnicity + EAL + country +
                                         highest_nvq + oecd_income + occupational_status + wealth_quintiles + imd, 
                                       weights = weight))

continuous_model2 = with(imputed_mcs2, lm(standardised_core_subjects ~ sex + ethnicity + EAL + country +
                                         highest_nvq + oecd_income + occupational_status + wealth_quintiles + 
                                         imd + caregiver_vocabStandardised, 
                                        weights = weight))

continuous_model3 = with(imputed_mcs2, lm(standardised_core_subjects ~ sex + ethnicity + EAL + country +
                                         highest_nvq + oecd_income + occupational_status + wealth_quintiles + 
                                         imd + caregiver_vocabStandardised + age5_standardised, 
                                        weights = weight))

continuous_model1Results = summary(pool(continuous_model1), conf.int = TRUE)
continuous_model2Results = summary(pool(continuous_model2), conf.int = TRUE)
continuous_model3Results = summary(pool(continuous_model3), conf.int = TRUE)


#2b. does vocabulary predict level of achievement in core subjects above and beyond SEC/caregiver vocab factors?
#compare model with only sociodemographic factors to a model with caregiver vocab
D1(continuous_model2, continuous_model1) #check if need to use a specific method ?
#compare model with all confounders to model with vocab
D1(continuous_model3, continuous_model2) 

round(pool.r.squared(continuous_unadjusted),4)*100
round(pool.r.squared(continuous_model1),4)*100
round(pool.r.squared(continuous_model2),4)*100
round(pool.r.squared(continuous_model3),4)*100

#could also get partial r squareds for vocabulary too - as did in inequalities paper? 

#RQ3 - IS ANY RELATION MODERATED BY SEC? 
#use composite SEC as the main moderator = run CFA to get composite factor score. 
#create dataset of each individual imputed data
imputed_mcs2_1 <- complete(imputed_mcs2,1)
imputed_mcs2_2 <- complete(imputed_mcs2,2)
imputed_mcs2_3 <- complete(imputed_mcs2,3)
imputed_mcs2_4 <- complete(imputed_mcs2,4)
imputed_mcs2_5 <- complete(imputed_mcs2,5)
imputed_mcs2_6 <- complete(imputed_mcs2,6)
imputed_mcs2_7 <- complete(imputed_mcs2,7)
imputed_mcs2_8 <- complete(imputed_mcs2,8)
imputed_mcs2_9 <- complete(imputed_mcs2,9)
imputed_mcs2_10 <- complete(imputed_mcs2,10)
imputed_mcs2_11 <- complete(imputed_mcs2,11)
imputed_mcs2_12 <- complete(imputed_mcs2,12)
imputed_mcs2_13 <- complete(imputed_mcs2,13)
imputed_mcs2_14 <- complete(imputed_mcs2,14)
imputed_mcs2_15 <- complete(imputed_mcs2,15)
imputed_mcs2_16 <- complete(imputed_mcs2,16)
imputed_mcs2_17 <- complete(imputed_mcs2,17)
imputed_mcs2_18 <- complete(imputed_mcs2,18)
imputed_mcs2_19 <- complete(imputed_mcs2,19)
imputed_mcs2_20<- complete(imputed_mcs2,20)
imputed_mcs2_21<- complete(imputed_mcs2,21)
imputed_mcs2_22<- complete(imputed_mcs2,22)
imputed_mcs2_23<- complete(imputed_mcs2,23)
imputed_mcs2_24<- complete(imputed_mcs2,24)
imputed_mcs2_25<- complete(imputed_mcs2,25)


#factor analysis to get factor score####
#factor analysis model


#create imputed datasets as a list - this is so can run CFA over each dataset
imputed_datasets = list(imputed_mcs2_1, imputed_mcs2_2, imputed_mcs2_3, imputed_mcs2_4, imputed_mcs2_5, 
                        imputed_mcs2_6, imputed_mcs2_7, imputed_mcs2_8, imputed_mcs2_9, imputed_mcs2_10, 
                        imputed_mcs2_11, imputed_mcs2_12, imputed_mcs2_13, imputed_mcs2_14, imputed_mcs2_15,
                        imputed_mcs2_16, imputed_mcs2_17, imputed_mcs2_18, imputed_mcs2_19, imputed_mcs2_20, 
                        imputed_mcs2_21, imputed_mcs2_22, imputed_mcs2_23, imputed_mcs2_24, imputed_mcs2_25)

#define CFA model to create SEP
SEP_model <- 'SEP =~ highest_nvq + oecd_income + wealth_quintiles + occupational_status + imd'

#run CFA across 25 imputed datasets
cfa_imputed = function(ert){
  fit = cfa(SEP_model, 
            data = ert,
            ordered = c("highest_nvq", "oecd_income","wealth_quintiles", "occupational_status", "imd"), 
            std.lv=TRUE, 
            estimator="WLSMV")
  ses_latent = lavPredict(fit, type = "lv")
}

#create and add SES latent variable to each dataset
imputed_datasets$ses_latent = lapply(imputed_datasets, cfa_imputed)

#get individual datasets from list - these will now have SEP composite (pull this out for each dataset). 
#this is so can run the regression over each dataset. 
imputed_data1 = imputed_datasets[[1]]
imputed_data1$ses_latent = imputed_datasets$ses_latent[[1]]
imputed_data2 = imputed_datasets[[2]]
imputed_data2$ses_latent = imputed_datasets$ses_latent[[2]]
imputed_data3 = imputed_datasets[[3]]
imputed_data3$ses_latent = imputed_datasets$ses_latent[[3]]
imputed_data4 = imputed_datasets[[4]]
imputed_data4$ses_latent = imputed_datasets$ses_latent[[4]]
imputed_data5 = imputed_datasets[[5]]
imputed_data5$ses_latent = imputed_datasets$ses_latent[[5]]
imputed_data6 = imputed_datasets[[6]]
imputed_data6$ses_latent = imputed_datasets$ses_latent[[6]]
imputed_data7 = imputed_datasets[[7]]
imputed_data7$ses_latent = imputed_datasets$ses_latent[[7]]
imputed_data8 = imputed_datasets[[8]]
imputed_data8$ses_latent = imputed_datasets$ses_latent[[8]]
imputed_data9 = imputed_datasets[[9]]
imputed_data9$ses_latent = imputed_datasets$ses_latent[[9]]
imputed_data10 = imputed_datasets[[10]]
imputed_data10$ses_latent = imputed_datasets$ses_latent[[10]]
imputed_data11 = imputed_datasets[[11]]
imputed_data11$ses_latent = imputed_datasets$ses_latent[[11]]
imputed_data12 = imputed_datasets[[12]]
imputed_data12$ses_latent = imputed_datasets$ses_latent[[12]]
imputed_datq13 = imputed_datasets[[13]]
imputed_data13$ses_latent = imputed_datasets$ses_latent[[13]]
imputed_data14 = imputed_datasets[[14]]
imputed_data14$ses_latent = imputed_datasets$ses_latent[[14]]
imputed_data15 = imputed_datasets[[15]]
imputed_data15$ses_latent = imputed_datasets$ses_latent[[15]]
imputed_data16 = imputed_datasets[[16]]
imputed_data16$ses_latent = imputed_datasets$ses_latent[[16]]
imputed_data17 = imputed_datasets[[17]]
imputed_data17$ses_latent = imputed_datasets$ses_latent[[17]]
imputed_data18 = imputed_datasets[[18]]
imputed_data18$ses_latent = imputed_datasets$ses_latent[[18]]
imputed_data19 = imputed_datasets[[19]]
imputed_data19$ses_latent = imputed_datasets$ses_latent[[19]]
imputed_data20 = imputed_datasets[[20]]
imputed_data20$ses_latent = imputed_datasets$ses_latent[[20]]
imputed_data21 = imputed_datasets[[21]]
imputed_data21$ses_latent = imputed_datasets$ses_latent[[21]]
imputed_data22 = imputed_datasets[[22]]
imputed_data22$ses_latent = imputed_datasets$ses_latent[[22]]
imputed_data23 = imputed_datasets[[23]]
imputed_data23$ses_latent = imputed_datasets$ses_latent[[23]]
imputed_data24 = imputed_datasets[[24]]
imputed_data24$ses_latent = imputed_datasets$ses_latent[[24]]
imputed_data25 = imputed_datasets[[25]]
imputed_data25$ses_latent = imputed_datasets$ses_latent[[25]]

#regression models with composite as the mdoerator. run this across 25 imputed datasets that have the ses_latent variable in. 

compositeModerator_model <- function(df) {
  fit <- glm(benchmark_binary ~ sex + ethnicity + EAL + country +
               + caregiver_vocabStandardised + age5_standardised*ses_latent, 
             family = binomial, weights = weight, data=df)
  return(fit)
}

#put new imputed datasets into lists (i.e. ones with the latent variable in them) and apply regression model to each
compositeModerator <- lapply(list(imputed_data1, imputed_data2, imputed_data3, imputed_data4, imputed_data5,
                                  imputed_data6, imputed_data7, imputed_data8, imputed_data9, imputed_data10,
                                  imputed_data11, imputed_data12, imputed_data13, imputed_data14, imputed_data15,
                                  imputed_data16, imputed_data17, imputed_data18, imputed_data19, imputed_data20,
                                  imputed_data21, imputed_data22, imputed_data23, imputed_data24, imputed_data25),
                             compositeModerator_model)

#pool results
#moderator_composite <- summary(pool(as.mira(compositeModerator)))
#get odds ratios
#moderator_composite <- exp(moderator_composite$estimate) 
#round
#moderator_composite <- round(moderator_composite,2)
#get 95% CIs
#lower CI
#lower_moderator_composite  = exp(summary(pool(as.mira(compositeModerator)))$estimate-(summary(pool(as.mira(compositeModerator)))$std.error*1.96))
#exponentiate and round
#lower_moderator_composite = exp(lower_moderator_composite)
#lower_moderator_composite <- round(lower_moderator_composite ,2)
#upper CI
#upper_moderator_composite  = exp(summary(pool(as.mira(compositeModerator)))$estimate+(summary(pool(as.mira(compositeModerator)))$std.error*1.96))
#exponentiate and round
#upper_moderator_composite = exp(upper_moderator_composite)
#upper_moderator_composite <- round(upper_moderator_composite ,2)

#alternative way to calculate:
moderator_composite1 <- summary(pool(as.mira(compositeModerator)),conf.int = TRUE, conf.level = 0.95,  exponentiate = TRUE) 

#model without interaction term - for model comparison -will be run over 25 imputed datasets in the list
noCompositeModerator_model <- function(df) {
  fit1 <- glm(benchmark_binary ~ sex + ethnicity + EAL + country +
                + caregiver_vocabStandardised + age5_standardised + ses_latent, 
              family = binomial, weights = weight, data=df)
  return(fit1)
}

#run regression across list of 25 imputed datasets that have latent variable in 
noCompositeModerator <- lapply(list(imputed_data1, imputed_data2, imputed_data3, imputed_data4, imputed_data5,
                                    imputed_data6, imputed_data7, imputed_data8, imputed_data9, imputed_data10,
                                    imputed_data11, imputed_data12, imputed_data13, imputed_data14, imputed_data15,
                                    imputed_data16, imputed_data17, imputed_data18, imputed_data19, imputed_data20,
                                    imputed_data21, imputed_data22, imputed_data23, imputed_data24, imputed_data25),
                               noCompositeModerator_model)

#convert into mira objects so that D1 function works to compare the nested models (mira - multiple imputation repeated analyses)
#The as.mira() function takes the results of repeated complete-data analysis stored as a list, and turns it into a mira object that can be pooled.
moderator = as.mira(compositeModerator)
noModerator = as.mira(noCompositeModerator)
#nested model comparison
D1(moderator, noModerator)

#MODERATOR ANALYSIS ON EACH INDIVIDUAL SEC PREDICTOR ####

#1. parent education as the moderator 
#vocabulary*NVQ interaction term
NVQ_moderator = with(imputed_mcs2, glm(benchmark_binary ~ sex + ethnicity + EAL + country +
                                                          oecd_income + occupational_status + wealth_quintiles + imd + 
                                         caregiver_vocabStandardised + age5_standardised*highest_nvq, 
                                         family = binomial, weights = weight))
#no interaction term
NVQ_noModerator = with(imputed_mcs2, glm(benchmark_binary ~ sex + ethnicity + EAL + country +
                                           oecd_income + occupational_status + wealth_quintiles + imd + 
                                           caregiver_vocabStandardised + highest_nvq + age5_standardised, 
                                         family = binomial, weights = weight))
#pooled results of models 
NVQ_moderatorResults = summary(pool(NVQ_moderator), conf.int = TRUE, exponentiate = TRUE)
NVQ_noModeratorResults = summary(pool(NVQ_noModerator), conf.int = TRUE, exponentiate = TRUE)
#compare model with moderator to model without moderator 
D1(NVQ_moderator, NVQ_noModerator)

#2. occupational status as the moderator 
#vocabulary*occupation interaction term
occupation_moderator = with(imputed_mcs2, glm(benchmark_binary ~ sex + ethnicity + EAL + country +
                                                highest_nvq+ oecd_income  + wealth_quintiles + imd + 
                                         caregiver_vocabStandardised + age5_standardised*occupational_status, 
                                       family = binomial, weights = weight))
#no interaction term
occupation_noModerator = with(imputed_mcs2, glm(benchmark_binary ~ sex + ethnicity + EAL + country +
                                           highest_nvq + oecd_income  + wealth_quintiles + imd + 
                                           caregiver_vocabStandardised + occupational_status + age5_standardised, 
                                         family = binomial, weights = weight))
#pooled results of models 
occupation_moderatorResults = summary(pool(occupation_moderator), conf.int = TRUE, exponentiate = TRUE)
occupation_noModeratorResults = summary(pool(occupation_noModerator), conf.int = TRUE, exponentiate = TRUE)
#compare model with moderator to model without moderator 
D1(occupation_moderator, occupation_noModerator)

#3. income as the moderator 
#vocabulary*income interaction term
income_moderator = with(imputed_mcs2, glm(benchmark_binary ~ sex + ethnicity + EAL + country +
                                                highest_nvq+ occupational_status  + wealth_quintiles + imd + 
                                                caregiver_vocabStandardised + age5_standardised*oecd_income, 
                                              family = binomial, weights = weight))
#no interaction term
income_noModerator = with(imputed_mcs2, glm(benchmark_binary ~ sex + ethnicity + EAL + country +
                                                  highest_nvq + occupational_status  + wealth_quintiles + imd + 
                                                  caregiver_vocabStandardised + oecd_income + age5_standardised, 
                                                family = binomial, weights = weight))
#pooled results of models 
income_moderatorResults = summary(pool(income_moderator), conf.int = TRUE, exponentiate = TRUE)
income_noModeratorResults = summary(pool(income_noModerator), conf.int = TRUE, exponentiate = TRUE)
#compare model with moderator to model without moderator 
D1(income_moderator, income_noModerator)

#4. wealth as the moderator 
#vocabulary*wealth interaction term
wealth_moderator = with(imputed_mcs2, glm(benchmark_binary ~ sex + ethnicity + EAL + country +
                                            highest_nvq+ oecd_income + occupational_status   + imd + 
                                            caregiver_vocabStandardised + age5_standardised*wealth_quintiles, 
                                          family = binomial, weights = weight))
#no interaction term
wealth_noModerator = with(imputed_mcs2, glm(benchmark_binary ~ sex + ethnicity + EAL + country +
                                              highest_nvq + oecd_income + occupational_status  + wealth_quintiles + imd + 
                                              caregiver_vocabStandardised + wealth_quintiles + age5_standardised, 
                                            family = binomial, weights = weight))
#pooled results of models 
wealth_moderatorResults = summary(pool(wealth_moderator), conf.int = TRUE, exponentiate = TRUE)
wealth_noModeratorResults = summary(pool(wealth_noModerator), conf.int = TRUE, exponentiate = TRUE)
#compare model with moderator to model without moderator 
D1(wealth_moderator, wealth_noModerator)

#5. imd as the moderator 
#vocabulary*wealth interaction term
imd_moderator = with(imputed_mcs2, glm(benchmark_binary ~ sex + ethnicity + EAL + country +
                                            highest_nvq+ oecd_income + occupational_status + wealth_quintiles + 
                                            caregiver_vocabStandardised + age5_standardised*imd, 
                                          family = binomial, weights = weight))
#no interaction term
imd_noModerator = with(imputed_mcs2, glm(benchmark_binary ~ sex + ethnicity + EAL + country +
                                              highest_nvq + oecd_income + occupational_status  + wealth_quintiles + 
                                              caregiver_vocabStandardised + imd + age5_standardised, 
                                            family = binomial, weights = weight))
#pooled results of models 
imd_moderatorResults = summary(pool(imd_moderator), conf.int = TRUE, exponentiate = TRUE)
imd_noModeratorResults = summary(pool(imd_noModerator), conf.int = TRUE, exponentiate = TRUE)
#compare model with moderator to model without moderator 
D1(imd_moderator, imd_noModerator)
