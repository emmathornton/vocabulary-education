#load necessary packages####
library(haven)
require(swfscMisc)
require(sjmisc)
require(Hmisc)
require(psych)
library(dplyr) 
library(tidyr) 
library(tidyverse)
#load in data####
mcs3_child_assessment <- read_sav("mcs3_cm_cognitive_assessment.sav")
mcs1_hh <- read_sav("mcs1_hhgrid.sav")
mcs2_hh <- read_sav("mcs2_hhgrid.sav")
mcs_family <- read_sav("mcs_longitudinal_family_file.sav")
mcs1_parent <- read_sav("mcs1_parent_interview.sav")
mcs2_parent <- read_sav("mcs2_parent_interview.sav")
mcs2_derived <- read_sav("mcs2_parent_derived.sav")
mcs2_derived_family <- read_sav("mcs2_family_derived.sav")
mcs1_derived_family <- read_sav("mcs1_family_derived.sav")
mcs1_derived <- read_sav("mcs1_parent_derived.sav")
mcs5_parent<- read_sav("mcs5_parent_interview.sav")
mcs2_geography <- read_sav("mcs2_geographically_linked_data.sav")
mcs1_geography <- read_sav("mcs1_geographically_linked_data.sav")
mcs6_parent_assessment <- read_sav("mcs6_parent_assessment.sav")
#convert all to lowercase####
names(mcs3_child_assessment) <- tolower(names(mcs3_child_assessment))
names(mcs_family) <- tolower(names(mcs_family))
names(mcs5_parent) <- tolower(names(mcs5_parent))
names(mcs1_parent) <- tolower(names(mcs1_parent))
names(mcs2_parent) <- tolower(names(mcs2_parent))
names(mcs2_derived) <- tolower(names(mcs2_derived))
names(mcs2_derived_family) <- tolower(names(mcs2_derived_family))
names(mcs1_derived_family) <- tolower(names(mcs1_derived_family))
names(mcs1_derived) <- tolower(names(mcs1_derived))
names(mcs1_hh) <- tolower(names(mcs1_hh))
names(mcs2_hh) <- tolower(names(mcs2_hh))
names(mcs2_geography) <- tolower(names(mcs2_geography))
names(mcs1_geography) <- tolower(names(mcs1_geography))
names(mcs6_parent_assessment) <- tolower(names(mcs6_parent_assessment))
#create sweep entry variable to identify second entry families later####
sweep_entry <- c("mcsid", "sentry")
sweep_entry <- mcs_family[sweep_entry]
sweep_entry$sentry = as.character(sweep_entry$sentry)


#create weight variable####
#attrition and sample weight age 9 months sweep 
mcs1_weight <- c("mcsid", "aovwt2")
mcs1_weight <- mcs_family[mcs1_weight]
mcs1_weight [mcs1_weight  ==-1] <- NA
#attrition and sample weight age 3 sweep 
mcs2_weight <- c("mcsid", "bovwt2")
mcs2_weight <- mcs_family[mcs2_weight]
mcs2_weight [mcs2_weight  ==-1] <- NA
#mcs2_weight1<- merge (all=TRUE, mcs2_weight, sweep_entry, by="mcsid")
#mcs2_weight2<- mcs2_weight1[which(mcs2_weight1$sentry == "2"),]
weight <- merge(all=TRUE, mcs1_weight, mcs2_weight,by="mcsid")
weight$weight1 <- ifelse(!is.na(weight$bovwt2), weight$bovwt2, weight$aovwt2)
mcs_weight <- c("mcsid", "weight1")
mcs_weight <- weight[mcs_weight] #check which weight need here when meet

respondent_key = c(`1` = "main",
                   `2` = "partner", 
                   `3` = "partner_proxy",
                   `4` = "not_eligible")


#PREDICTOR VARIABLE _AGE 5 VOCAB ####

age5_vocab = mcs3_child_assessment %>% select(mcsid, ccnvtscore, cccmno00) %>% 
  filter(cccmno00 == 1) %>% 
  select(mcsid, ccnvtscore) %>% 
  filter(!is.na(ccnvtscore)) %>% 
  rename(age5_vocab = ccnvtscore)

#SES VARIABLES#### 

#occupational status####
#OCCUPATION 4 CLASSES - AGE 3.
#occupation at age 3

age3_occupation = mcs2_derived %>% select(mcsid, bdd05s00, belig00) %>% 
  mutate(occupation = case_when(bdd05s00 ==1 ~1, #recode to be 3 level variable. 
                                bdd05s00 ==2 ~2,  
                                bdd05s00 ==3 ~2, 
                                bdd05s00 ==4 ~3, 
                                bdd05s00 ==5 ~3))
age3_occupation$belig00 = as.factor(age3_occupation$belig00)
age3_occupation$belig00 = recode(age3_occupation$belig00, !!!respondent_key)

age3_occupation_wide = age3_occupation %>% select(!bdd05s00) %>% 
  group_by(mcsid) %>%
  pivot_wider(names_from = belig00, values_from = occupation) %>% 
  rename("main_occupation" = main, 
         "partner_occupation" = partner, 
         "proxy_partner_occupation" = partner_proxy) %>% 
  mutate(partner_occupation = case_when(!is.na(partner_occupation) ~ partner_occupation, 
                                        is.na(partner_occupation) ~ proxy_partner_occupation)) %>% 
  mutate(highest_occupation = pmin(main_occupation, partner_occupation, na.rm = TRUE)) %>% 
  select(mcsid, highest_occupation)

#add unemployment as 4th category 
age3_employment = mcs2_derived_family %>% select(mcsid, bdcwrk00)
age3_occupation = merge(all=TRUE, age3_occupation_wide, age3_employment, by="mcsid")
age3_occupation[is.na(age3_occupation$highest_occupation) & age3_occupation$bdcwrk00 %in% c(4,6,10),]$highest_occupation= 4

#occupation at 9 months
months9_occupation = mcs1_derived %>% select(mcsid, add05s00, aelig00) %>% 
  mutate(occupation = case_when(add05s00 ==1 ~1, #recode to be 3 level variable. 
                                add05s00 ==2 ~2,  
                                add05s00 ==3 ~2, 
                                add05s00 ==4 ~3, 
                                add05s00 ==5 ~3))
months9_occupation$aelig00 = as.factor(months9_occupation$aelig00)
months9_occupation$aelig00 = recode(months9_occupation$aelig00, !!!respondent_key)

months9_occupation_wide = months9_occupation %>% select(!add05s00) %>% 
  group_by(mcsid) %>%
  pivot_wider(names_from = aelig00, values_from = occupation) %>% 
  rename("main_occupation" = main, 
         "partner_occupation" = partner, 
         "proxy_partner_occupation" = partner_proxy) %>% 
  mutate(partner_occupation = case_when(!is.na(partner_occupation) ~ partner_occupation, 
                                        is.na(partner_occupation) ~ proxy_partner_occupation)) %>% 
  mutate(highest_occupation = pmin(main_occupation, partner_occupation, na.rm = TRUE)) %>% 
  select(mcsid, highest_occupation)

#add unemployment as 4th category 
months9_employment = mcs1_derived_family %>% select(mcsid, adcwrk00)
months9_occupation = merge(all=TRUE, months9_occupation_wide, months9_employment, by="mcsid")
months9_occupation[is.na(months9_occupation$highest_occupation) & months9_occupation$adcwrk00 %in% c(4,6,10),]$highest_occupation= 4
months9_occupation = months9_occupation %>% rename("highest_occupation_s1" = highest_occupation)

#replace NA at age 3 with values from 9 months
occupational_status = merge(all=TRUE, age3_occupation, months9_occupation, by = "mcsid") %>% 
  select(mcsid, highest_occupation, highest_occupation_s1) %>% 
  mutate(highest_household_occupation = case_when(!is.na(highest_occupation) ~ highest_occupation, 
                                                  is.na(highest_occupation) ~ highest_occupation_s1)) %>% 
  select(mcsid, highest_household_occupation) %>% 
  rec(highest_household_occupation,  rec = "1=4; 2=3; 3=2; 4=1", #reverse code variable
      as.num = TRUE, var.label = NULL, 
      val.labels = NULL, append = TRUE, suffix = "_r") %>% 
  select(mcsid, highest_household_occupation_r)

#income####
#INCOME AT AGE 3. OECD weighted quintiles
#oecd income at age 3
oecd_income = mcs2_derived_family %>% select(mcsid, boecduk0)
#oecd income at 9 months to replace NA
oecd_income_9months = mcs1_derived_family %>% select(mcsid, aoecduk0)
#combine together
income = merge(all=TRUE, oecd_income, oecd_income_9months, by="mcsid") %>% 
  mutate(oecd_income = case_when(!is.na(boecduk0) ~ boecduk0, 
                                 is.na(boecduk0) ~ aoecduk0))

#NVQ education variable####
#NVQ qualifications

#RESPONDENT VARIABLE
#parent education variable ####
#first need to identify mother and father figures from main and partner respondents
#do this separately for sweep 1 and 2 as can change between sweeps

#sweep 1 respondent identity
respondent_identity_sweep1 = mcs1_derived %>% select(mcsid, amdres00, apdres00) 
mother_respondent_main = respondent_identity_sweep1 %>% filter(amdres00 == 1 | amdres00 == 3 |amdres00 ==5|
                                                                 amdres00==7 | amdres00 == 9 | amdres00 == 11 |
                                                                 amdres00 == 13 |amdres00 == 15) %>% 
  select(mcsid, amdres00) %>% 
  rename(mother_mainRespondent = "amdres00")

mother_respondent_partner = respondent_identity_sweep1 %>% filter(apdres00 == 1 | apdres00 == 3 |apdres00 ==5|
                                                                    apdres00==7 | apdres00 == 9 | apdres00 == 11 |
                                                                    apdres00 == 13 |apdres00 == 15 | apdres00 == 21 ) %>% 
  select(mcsid, apdres00) %>% 
  rename(mother_partnerRespondent = "apdres00")

father_respondent_main = respondent_identity_sweep1 %>% filter(amdres00 == 2 | amdres00 == 4 |amdres00 ==6|
                                                                 amdres00==8 | amdres00 == 10 | amdres00 == 12 |
                                                                 amdres00 == 14 |amdres00 == 16) %>% 
  select(mcsid, amdres00) %>% 
  rename(father_mainRespondent = "amdres00")

father_respondent_partner = respondent_identity_sweep1 %>% filter(apdres00 == 2 | apdres00 == 4 |apdres00 ==6|
                                                                    apdres00==8 | apdres00 == 10 | apdres00 == 12 |
                                                                    apdres00 == 14 |apdres00 == 16 | apdres00 == 22 | apdres00 == 24) %>% 
  select(mcsid, apdres00) %>% 
  rename(father_partnerRespondent = "apdres00")

#sweep 2 respondent identity

respondent_identity_sweep2  = mcs2_derived %>% select(mcsid, bmdres00, bpdres00)
#split sweep 2 into original families and families that joined in second sweep. 
respondent_sweep2_original = merge(all=TRUE, respondent_identity_sweep2, sweep_entry, by = "mcsid") %>% 
  filter(sentry == 1)
mother_respondent_main_sweep2_original = respondent_sweep2_original %>% filter(bmdres00 == 1 | bmdres00 == 3 |bmdres00 ==5|
                                                                                 bmdres00==7 | bmdres00 == 9 | bmdres00 == 11 |
                                                                                 bmdres00 == 13 |bmdres00 == 15) %>% 
  select(mcsid, bmdres00) %>% 
  rename(mother_mainRespondent_2 = "bmdres00")

mother_respondent_partner_sweep2_original = respondent_sweep2_original %>% filter(bpdres00 == 1 | bpdres00 == 3 |bpdres00 ==5|
                                                                                    bpdres00==7 | bpdres00 == 9 | bpdres00 == 11 |
                                                                                    bpdres00 == 13 |bpdres00 == 15 | bpdres00 == 21 ) %>% 
  select(mcsid, bpdres00) %>% 
  rename(mother_partnerRespondent_2 = "bpdres00")

father_respondent_main_sweep2_original = respondent_sweep2_original%>% filter(bmdres00 == 2 | bmdres00 == 4 |bmdres00 ==6|
                                                                                bmdres00==8 | bmdres00 == 10 | bmdres00 == 12 |
                                                                                bmdres00 == 14 |bmdres00 == 16) %>% 
  select(mcsid, bmdres00) %>% 
  rename(father_mainRespondent_2 = "bmdres00")

father_respondent_partner_sweep2_original = respondent_sweep2_original %>% filter(bpdres00 == 2 | bpdres00 == 4 |bpdres00 ==6|
                                                                                    bpdres00==8 | bpdres00 == 10 | bpdres00 == 12 |
                                                                                    bpdres00 == 14 |bpdres00 == 16 |bpdres00 == 18 | bpdres00 == 22 | bpdres00 == 24) %>% 
  select(mcsid, bpdres00) %>% 
  rename(father_partnerRespondent_2 = "bpdres00")

#families that joined in second sweep - new families
respondent_sweep2_new = merge(all=TRUE, respondent_identity_sweep2, sweep_entry, by = "mcsid") %>% 
  filter(sentry == 2)
mother_respondent_main_sweep2_new = respondent_sweep2_new %>% filter(bmdres00 == 1 | bmdres00 == 3 |bmdres00 ==5|
                                                                       bmdres00==7 | bmdres00 == 9 | bmdres00 == 11 |
                                                                       bmdres00 == 13 |bmdres00 == 15) %>% 
  select(mcsid, bmdres00) %>% 
  rename(mother_mainRespondent_2_new = "bmdres00")

mother_respondent_partner_sweep2_new = respondent_sweep2_new %>% filter(bpdres00 == 1 | bpdres00 == 3 |bpdres00 ==5|
                                                                          bpdres00==7 | bpdres00 == 9 | bpdres00 == 11 |
                                                                          bpdres00 == 13 |bpdres00 == 15 | bpdres00 == 21 ) %>% 
  select(mcsid, bpdres00) %>% 
  rename(mother_partnerRespondent_2_new = "bpdres00")

father_respondent_main_sweep2_new = respondent_sweep2_new%>% filter(bmdres00 == 2 | bmdres00 == 4 |bmdres00 ==6|
                                                                      bmdres00==8 | bmdres00 == 10 | bmdres00 == 12 |
                                                                      bmdres00 == 14 |bmdres00 == 16) %>% 
  select(mcsid, bmdres00) %>% 
  rename(father_mainRespondent_2_new = "bmdres00")

father_respondent_partner_sweep2_new = respondent_sweep2_new %>% filter(bpdres00 == 2 | bpdres00 == 4 |bpdres00 ==6|
                                                                          bpdres00==8 | bpdres00 == 10 | bpdres00 == 12 |
                                                                          bpdres00 == 14 |bpdres00 == 16 | bpdres00 == 18 |bpdres00 == 22 | bpdres00 == 24) %>% 
  select(mcsid, bpdres00) %>% 
  rename(father_partnerRespondent_2_new = "bpdres00")


#parent NVQ at age 3 - for original families
#recode education variables
mcs2_derived$bmdnvq00[mcs2_derived$bmdnvq00==-1] <- NA
mcs2_derived$bmdnvq00[mcs2_derived$bmdnvq00==-1] <- NA
mcs2_derived$bmdnvq00[mcs2_derived$bmdnvq00==95] <- 0 #overseas qual only - convert into an NVQ level 0
mcs2_derived$bmdnvq00[mcs2_derived$bmdnvq00==96] <- 0
#partner
mcs2_derived$bpdnvq00[mcs2_derived$bpdnvq00==-1] <- NA
mcs2_derived$bpdnvq00[mcs2_derived$bpdnvq00==-1] <- NA
mcs2_derived$bpdnvq00[mcs2_derived$bpdnvq00==95] <- 0 #overseas qual only - convert into an NVQ level 0
mcs2_derived$bpdnvq00[mcs2_derived$bpdnvq00==96] <- 0
#mother
maternal_NVQ_age3_original_main = mcs2_derived %>% select(mcsid, bmdnvq00) %>% 
  filter(mcsid %in% mother_respondent_main_sweep2_original$mcsid) 
maternal_NVQ_age3_original_partner = mcs2_derived %>% select(mcsid, bpdnvq00) %>% 
  filter(mcsid %in% mother_respondent_partner_sweep2_original$mcsid) 
maternal_NVQ_age3_original = merge(all=TRUE, maternal_NVQ_age3_original_main, maternal_NVQ_age3_original_partner, by="mcsid") %>% 
  mutate(maternal_education_age3 = case_when(!is.na(bmdnvq00) ~ bmdnvq00, 
                                             is.na(bmdnvq00) ~ bpdnvq00)) %>% 
  select(mcsid, maternal_education_age3)
#father
paternal_NVQ_age3_original_main = mcs2_derived %>%  select(mcsid, bmdnvq00) %>% 
  filter(mcsid %in% father_respondent_main_sweep2_original$mcsid) 
paternal_NVQ_age3_original_partner = mcs2_derived %>%  select(mcsid, bpdnvq00) %>% 
  filter(mcsid %in% father_respondent_partner_sweep2_original$mcsid) 
paternal_NVQ_age3_original = merge(all=TRUE, paternal_NVQ_age3_original_main,paternal_NVQ_age3_original_partner, by = "mcsid") %>% 
  mutate(paternal_education_age3 = case_when(!is.na(bpdnvq00) ~ bpdnvq00, 
                                             is.na(bpdnvq00) ~ bmdnvq00)) %>% 
  select(mcsid, paternal_education_age3)

#original families - parent NVQ at 9 months 
mcs1_derived$amdnvq00[mcs1_derived$amdnvq00==-1] <- NA
mcs1_derived$amdnvq00[mcs1_derived$amdnvq00==-1] <- NA
mcs1_derived$amdnvq00[mcs1_derived$amdnvq00==95] <- 0 #overseas qual only - convert into an NVQ level 0
mcs1_derived$amdnvq00[mcs1_derived$amdnvq00==96] <- 0
#partner
mcs1_derived$apdnvq00[mcs1_derived$apdnvq00==-1] <- NA
mcs1_derived$apdnvq00[mcs1_derived$apdnvq00==-1] <- NA
mcs1_derived$apdnvq00[mcs1_derived$apdnvq00==95] <- 0 #overseas qual only - convert into an NVQ level 0
mcs1_derived$apdnvq00[mcs1_derived$apdnvq00==96] <- 0
#mother
maternal_NVQ_9months_main = mcs1_derived %>% select(mcsid, amdnvq00) %>% 
  filter(mcsid %in% mother_respondent_main$mcsid)
maternal_NVQ_9months_partner = mcs1_derived %>% select(mcsid, apdnvq00) %>% 
  filter(mcsid %in% mother_respondent_partner$mcsid) 
maternal_NVQ_9months = merge(all=TRUE, maternal_NVQ_9months_main, maternal_NVQ_9months_partner, by="mcsid") %>% 
  mutate(maternal_education_9months = case_when(!is.na(amdnvq00) ~ amdnvq00, 
                                                is.na(amdnvq00) ~ apdnvq00)) %>% 
  select(mcsid, maternal_education_9months)
#father
paternal_NVQ_9months_main = mcs1_derived %>% select(mcsid, amdnvq00) %>% 
  filter(mcsid %in% father_respondent_main$mcsid) 
paternal_NVQ_9months_partner = mcs1_derived %>% select(mcsid, apdnvq00) %>% 
  filter(mcsid %in% father_respondent_partner$mcsid) 
paternal_NVQ_9months = merge(all=TRUE,paternal_NVQ_9months_main, paternal_NVQ_9months_partner, by="mcsid") %>% 
  mutate(paternal_education_9months = case_when(!is.na(apdnvq00) ~ apdnvq00, 
                                                is.na(apdnvq00) ~ amdnvq00)) %>% 
  select(mcsid, paternal_education_9months)

#merge and replace NA at age 3 with response at 9 months instead
#mother
maternal_original_nvq = merge(all=TRUE, maternal_NVQ_age3_original, maternal_NVQ_9months, by="mcsid") %>% 
  mutate(maternal_nvq = case_when(!is.na(maternal_education_age3) ~ maternal_education_age3, 
                                  is.na(maternal_education_age3) ~ maternal_education_9months)) %>% 
  select(mcsid,maternal_nvq)
#father
paternal_original_nvq = merge(all=TRUE, paternal_NVQ_age3_original, paternal_NVQ_9months, by="mcsid") %>% 
  mutate(paternal_nvq = case_when(!is.na(paternal_education_age3) ~ paternal_education_age3, 
                                  is.na(paternal_education_age3) ~ paternal_education_9months)) %>% 
  select(mcsid,paternal_nvq)

#families who joined in sweep 2 - NVQ for new families

maternal_NVQ_age3_new_main = mcs2_derived %>% select(mcsid, bmdnvq00) %>% 
  filter(mcsid %in% mother_respondent_main_sweep2_new$mcsid)
maternal_NVQ_age3_partner = mcs2_derived %>% select(mcsid,  bpdnvq00) %>% 
  filter(mcsid %in% mother_respondent_partner_sweep2_new$mcsid) 
maternal_NVQ_age3_new = merge(all=TRUE, maternal_NVQ_age3_new_main, maternal_NVQ_age3_partner, by="mcsid") %>% 
  mutate(maternal_education_age3_new = case_when(!is.na(bmdnvq00) ~ bmdnvq00, 
                                                 is.na(bmdnvq00) ~ bpdnvq00)) %>% 
  select(mcsid, maternal_education_age3_new)
#father
paternal_NVQ_age3_new_main = mcs2_derived %>%  select(mcsid, bmdnvq00) %>% 
  filter(mcsid %in% father_respondent_main_sweep2_new$mcsid) 
paternal_NVQ_age3_new_partner = mcs2_derived %>%  select(mcsid,bpdnvq00) %>% 
  filter(mcsid %in% father_respondent_partner_sweep2_new$mcsid)
paternal_NVQ_age3_new = merge(all=TRUE, paternal_NVQ_age3_new_main, paternal_NVQ_age3_new_partner, by="mcsid") %>% 
  mutate(paternal_education_age3_new = case_when(!is.na(bpdnvq00) ~ bpdnvq00, 
                                                 is.na(bpdnvq00) ~ bmdnvq00)) %>% 
  select(mcsid, paternal_education_age3_new)

#combine original and new families together to give 1 overall variable 
#mother
maternal_nvq = merge(all=TRUE, maternal_original_nvq, maternal_NVQ_age3_new, by = "mcsid") %>% 
  mutate(maternal_nvq_variable = case_when(!is.na(maternal_nvq) ~ maternal_nvq, 
                                           is.na(maternal_nvq) ~ maternal_education_age3_new)) %>% 
  select(mcsid, maternal_nvq_variable)
#father
paternal_nvq = merge(all=TRUE, paternal_original_nvq, paternal_NVQ_age3_new, by = "mcsid") %>% 
  mutate(paternal_nvq_variable = case_when(!is.na(paternal_nvq) ~ paternal_nvq, 
                                           is.na(paternal_nvq) ~ paternal_education_age3_new)) %>% 
  select(mcsid, paternal_nvq_variable)

#both parents
parent_nvq = merge(all=TRUE, maternal_nvq, paternal_nvq, by="mcsid") %>% 
  mutate(highest_nvq =pmax(maternal_nvq_variable, paternal_nvq_variable, na.rm=TRUE)) %>% 
  select(mcsid, highest_nvq)


#WEALTH ####
wealth_variables <- c("mcsid", "eresp00", "epmopa00", "ephval00", "epinvt00", "epdeba00")
wealth_variables <- mcs5_parent[wealth_variables]
wealth_variables$eresp00 = as.character(wealth_variables$eresp00)
wealth1 <- wealth_variables[which(wealth_variables$eresp00 == "1"),]

#3 mcsids from respondent 4
wealth4 <- wealth_variables[which(wealth_variables$eresp00 == "4"),]
wealth4 = wealth4[!is.na(wealth4$epmopa00) | !is.na(wealth4$ephval00) | !is.na(wealth4$epinvt00) | !is.na(wealth4$epdeba00) ,]
#mortgage
mortgage1 <-c ("mcsid", "epmopa00")
mortgage1 <- wealth1[mortgage1]
mortgage1[mortgage1 ==-1:-9] <- NA
mortgage4 <-c ("mcsid", "epmopa00")
mortgage4 <- wealth4[mortgage4]
mortgage4[mortgage4 ==-1:-9] <- NA
mortgage <- merge(all=TRUE, mortgage1, mortgage4,by="mcsid")
mortgage$mortgage_combine<- ifelse(!is.na(mortgage$epmopa00.x), mortgage$epmopa00.x,mortgage$epmopa00.y)
mortgage_outstanding <- c("mcsid", "mortgage_combine")
mortgage_outstanding<-mortgage[mortgage_outstanding]

value1 <-c ("mcsid", "ephval00")
value1 <- wealth1[value1]
value1[value1 ==-1:-9] <- NA
value4 <-c ("mcsid", "ephval00")
value4 <- wealth4[value4]
value4[value4 ==-1:-9] <- NA
value <- merge(all=TRUE, value1, value4,by="mcsid")
value$value_combine<- ifelse(!is.na(value$ephval00.x), value$ephval00.x,value$ephval00.y)
house_value <- c("mcsid", "value_combine")
house_value<-value[house_value]


savings1 <-c ("mcsid", "epinvt00")
savings1 <- wealth1[savings1]
savings1[savings1 ==-1:-9] <- NA
savings4 <-c ("mcsid", "epinvt00")
savings4 <- wealth4[savings4]
savings4[savings4 ==-1:-9] <- NA
savings <- merge(all=TRUE, savings1, savings4,by="mcsid")
savings$savings_combine<- ifelse(!is.na(savings$epinvt00.x), savings$epinvt00.x,savings$epinvt00.y)
total_savings <- c("mcsid", "savings_combine")
total_savings<-savings[total_savings]


debt1 <-c ("mcsid", "epdeba00")
debt1 <- wealth1[debt1]
debt1[debt1 ==-1:-9] <- NA
debt4 <-c ("mcsid", "epdeba00")
debt4 <- wealth4[debt4]
debt4[debt4 ==-1:-9] <- NA
debt <- merge(all=TRUE, debt1, debt4,by="mcsid")
debt$debt_combine<- ifelse(!is.na(debt$epdeba00.x), debt$epdeba00.x,debt$epdeba00.y)
total_debt <- c("mcsid", "debt_combine")
total_debt<-debt[total_debt]

#INDICES OF MULTIPLE DEPRIVATION (DECILES) (AGE 3, 9 MONTHS IF MISSING)####
imd_variables_sweep2_1 <- c("mcsid",  "bimdscoe", "biwimdsc", "bisimdsc", "bimdscon")
imd_variables_sweep2_1 <- mcs2_geography[imd_variables_sweep2_1]
imd_s2_1<- imd_variables_sweep2_1[which(mcsid_number_age3$bhcnuma0=="1"),]
new_imd_s2_1 <- merge (all=TRUE, imd_s2_1, sweep_entry, by="mcsid")
imdsweep2_1<- new_imd_s2_1[which(new_imd_s2_1$sentry == "1"),]
imd_sweep2_1 <- transform(imdsweep2_1, imd2_1 = pmax(bimdscoe, biwimdsc, bisimdsc, bimdscon,  na.rm = TRUE))
IMD_sweep2_1<- c("mcsid",  "imd2_1")
IMD_sweep2_1 <- imd_sweep2_1[IMD_sweep2_1]
#second entry families sweep 2 IMD
imd_variables_sweep2_2 <- c("mcsid",  "bimdscoe", "biwimdsc", "bisimdsc", "bimdscon")
imd_variables_sweep2_2 <- mcs2_geography[imd_variables_sweep2_2]
imd_s2_2<- imd_variables_sweep2_2[which(mcsid_number_age3$bhcnuma0=="1"),]
new_imd_s2_2 <- merge (all=TRUE, imd_s2_2, sweep_entry, by="mcsid")
imdsweep2_2<- new_imd_s2_2[which(new_imd_s2_2$sentry == "2"),]
imd_sweep2_2 <- transform(imdsweep2_2, imd2_2 = pmax(bimdscoe, biwimdsc, bisimdsc, bimdscon,  na.rm = TRUE))
IMD_sweep2_2<- c("mcsid",  "imd2_2")
IMD_sweep2_2 <- imd_sweep2_2[IMD_sweep2_2]
#combine sweeps
imd_sweep2 <- merge(all=TRUE, IMD_sweep2_1, IMD_sweep2_2, by="mcsid")
imd_sweep2$sweep2_IMD <- ifelse(!is.na(imd_sweep2$imd2_1), imd_sweep2$imd2_1, imd_sweep2$imd2_2)
IMD_sweep2 <- c("mcsid","sweep2_IMD")
IMD_sweep2 <- imd_sweep2[IMD_sweep2]

#IMD at sweep 1
imd_variables_sweep1 <- c("mcsid",  "aimdscoe", "aiwimdsc", "aisimdsc", "aimdscon")
imd_variables_sweep1 <- mcs1_geography[imd_variables_sweep1]
imd_s1<- imd_variables_sweep1[which(mcsid_number_age9mo$ahcnuma0=="1"),]
new_imd_s1 <- merge (all=TRUE, imd_s1, sweep_entry, by="mcsid")
imdsweep1<- new_imd_s1[which(new_imd_s1$sentry == "1"),]
imd_sweep1 <- transform(imdsweep1, imd1 = pmax(aimdscoe, aiwimdsc, aisimdsc, aimdscon,  na.rm = TRUE))
IMD_sweep1<- c("mcsid",  "imd1")
IMD_sweep1 <- imd_sweep1[IMD_sweep1]

#IMD at age 3 and if NA, replace with 9 months.
imd_combined_sweeps <- merge(all=TRUE, IMD_sweep2, IMD_sweep1, by="mcsid")
imd_combined_sweeps$IMD <- ifelse(!is.na(imd_combined_sweeps$sweep2_IMD), imd_combined_sweeps$sweep2_IMD, imd_combined_sweeps$imd1)
IMD_variables <- c("mcsid",  "IMD")
IMD_variables <- imd_combined_sweeps[IMD_variables]

#creating potential confounders ####
#language spoken at home####
language_home1<- c("mcsid", "bhhlan00")
language_home1 <- mcs2_parent[language_home1]
language_home1[language_home1==-1]<-NA
language_home<- language_home1[which(mcsid_number_age3$bhcnuma0=="1"),]
language_home_1 <- merge (all=TRUE, language_home , sweep_entry, by="mcsid")
new_language_home<- language_home_1[which(language_home_1$sentry == "1"),]
EAL_sweep1 <- c("mcsid", "ahlang00")
EAL_sweep1 <- mcs1_parent[EAL_sweep1]
EAL_sweep1[EAL_sweep1==-9:-1] <- NA
EAL_home<- EAL_sweep1[which(mcsid_number_age9mo$ahcnuma0=="1"),]
EAL_sentry1 <- merge(all=TRUE, new_language_home, EAL_home, by="mcsid")
EAL_sentry1$EAL <- ifelse(!is.na(EAL_sentry1$bhhlan00), EAL_sentry1$bhhlan00 ,EAL_sentry1$ahlang00)

language_home2<- c("mcsid", "bhhlan00")
language_home2 <- mcs2_parent[language_home2]
language_home2[language_home2==-1]<-NA
language_home22<- language_home2[which(mcsid_number_age3$bhcnuma0=="1"),]
new_lang2 <- merge (all=TRUE, language_home22 , sweep_entry, by="mcsid")
lang2<- new_lang2[which(new_lang2$sentry == "2"),]
new_language <- merge(all=TRUE, EAL_sentry1, lang2,by="mcsid")

#merge together so that only one value 
new_language$new_langcombine<- ifelse(!is.na(new_language$EAL), new_language$EAL ,new_language$bhhlan00.y)
#create dataframe so also have mcsid 
#lang_home <- data.frame(language_combine, new_langcombine)
#subset data so just have 1 score and mcsid for the variable. may need recoding as all 0s seem to be 1 and all 1s coming up as 2?
language_used <- c("mcsid", "new_langcombine" )
language_used <- new_language[language_used]

#ethnicity####
#ethnicity single births mcs1
ethnicity1 <- c("mcsid", "adc06ea0")
ethnicity1 <- mcs1_derived[ethnicity1]
ethnicity1[ethnicity1==-1:-9] <- NA
ethnicity<- ethnicity1[which(mcsid_number_age9mo$ahcnuma0=="1"),]

#ethnicity sweep 2 new families
ethnicity2 <- c("mcsid", "bdc06ea0")
ethnicity2 <- mcs2_derived[ethnicity2]
ethnicity2[ethnicity2==-1:-9] <- NA
ethnicity_2<- ethnicity2[which(mcsid_number_age3$bhcnuma0=="1"),]
new_ethnicity2 <- merge (all=TRUE, ethnicity_2 , sweep_entry, by="mcsid")
ethnic2<- new_ethnicity2[which(new_ethnicity2$sentry == "2"),]
#combine
new_ethnicity <- merge(all=TRUE, ethnicity, ethnic2,by="mcsid")
#merge together so that only one value for standardised score
ethnic_combine<- ifelse(!is.na(new_ethnicity$adc06ea0), new_ethnicity$adc06ea0,new_ethnicity$bdc06ea0)
#create dataframe so also have mcsid 
new_ethnic<- data.frame(ethnic_combine, new_ethnicity)
#subset data so just have 1 score and mcsid for the variable
cm_ethnicity <- c("mcsid", "ethnic_combine" )
cm_ethnicity <- new_ethnic[cm_ethnicity]


#gender####
sex1 <- c("mcsid", "ahcsexa0")
sex1 <- mcs1_parent[sex1]
sex1[sex1==-1] <- NA
sex_1<- sex1[which(mcsid_number_age9mo$ahcnuma0=="1"),]
new_sex1 <- merge (all=TRUE, sex_1 , sweep_entry, by="mcsid")
sex_1<- new_sex1[which(new_sex1$sentry == "1"),]

sex2 <- c("mcsid", "bhcsexa0")
sex2 <- mcs2_parent[sex2]
sex2[sex2==-1] <- NA
sex_2<- sex2[which(mcsid_number_age9mo$ahcnuma0=="1"),]
new_sex2 <- merge (all=TRUE, sex_2 , sweep_entry, by="mcsid")
sex_2<- new_sex2[which(new_sex2$sentry == "2"),]
#combine
new_sex <- merge(all=TRUE, sex_1, sex_2,by="mcsid")
#merge together so that only one value for standardised score
sex_combine<- ifelse(!is.na(new_sex$ahcsexa0), new_sex$ahcsexa0,new_sex$bhcsexa0)
#create dataframe so also have mcsid 
new_sex1<- data.frame(sex_combine, new_sex)
#subset data so just have 1 score and mcsid for the variable
cm_sex<- c("mcsid", "sex_combine" )
cm_sex <- new_sex1[cm_sex]

#caregiver vocabulary ####
#main respondent word activity test
#get items for main respondent activity test to recode
main_vocabTest = mcs6_parent_assessment %>% select(mcsid, fresp00, fpmcog0a:fpmcog0t) %>% 
  filter(fresp00 == 1) %>% 
  select(!fresp00)

#recode so that correct answer = 1 and incorrect answer = 0 for each item (consult data dictionary for correct answers)
main_vocabTest = main_vocabTest %>%  mutate(item1 = case_when(is.na(fpmcog0a) ~ NA_real_, 
                                                              fpmcog0a == 1 ~ 1,
                                                              TRUE ~ 0)) %>% 
  mutate(item2 = case_when(is.na(fpmcog0b) ~ NA_real_,
                           fpmcog0b == 5 ~ 1, 
                           TRUE ~ 0)) %>% 
  mutate(item3 = case_when(is.na(fpmcog0c) ~ NA_real_,
                           fpmcog0c == 5 ~ 1, 
                           TRUE ~ 0)) %>% 
  mutate(item4 = case_when(is.na(fpmcog0d) ~ NA_real_,
                           fpmcog0d == 1 ~ 1, 
                           TRUE ~ 0)) %>% 
  mutate(item5 = case_when(is.na(fpmcog0e) ~ NA_real_,
                           fpmcog0e == 1 ~1, 
                           TRUE ~ 0 )) %>% 
  mutate(item6 = case_when(is.na(fpmcog0f) ~ NA_real_,
                           fpmcog0f == 4 ~1, 
                           TRUE ~ 0 )) %>% 
  mutate(item7 = case_when(is.na(fpmcog0g) ~ NA_real_,
                           fpmcog0g == 4 ~1, 
                           TRUE ~ 0 )) %>% 
  mutate(item8 = case_when(is.na(fpmcog0h) ~ NA_real_,
                           fpmcog0h == 3 ~1, 
                           TRUE ~ 0 )) %>% 
  mutate(item9 = case_when(is.na(fpmcog0i) ~ NA_real_,
                           fpmcog0i == 2 ~1, 
                           TRUE ~ 0 )) %>% 
  mutate(item10 = case_when(is.na(fpmcog0j) ~ NA_real_,
                            fpmcog0j == 1 ~1, 
                            TRUE ~ 0 )) %>% 
  mutate(item11 = case_when(is.na(fpmcog0k) ~ NA_real_,
                            fpmcog0k == 2 ~1, 
                            TRUE ~ 0 )) %>% 
  mutate(item12 = case_when(is.na(fpmcog0l) ~ NA_real_,
                            fpmcog0l == 1 ~1, 
                            TRUE ~ 0 )) %>% 
  mutate(item13 = case_when(is.na(fpmcog0m) ~ NA_real_,
                            fpmcog0m == 2 ~1, 
                            TRUE ~ 0 )) %>% 
  mutate(item14 = case_when(is.na(fpmcog0n) ~ NA_real_,
                            fpmcog0n == 4 ~1, 
                            TRUE ~ 0 )) %>% 
  mutate(item15 = case_when(is.na(fpmcog0o) ~ NA_real_,
                            fpmcog0o == 2 ~1, 
                            TRUE ~ 0 )) %>% 
  mutate(item16 = case_when(is.na(fpmcog0p) ~ NA_real_,
                            fpmcog0p == 4 ~1, 
                            TRUE ~ 0 )) %>% 
  mutate(item17 = case_when(is.na(fpmcog0q) ~ NA_real_,
                            fpmcog0q == 1 ~1, 
                            TRUE ~ 0 )) %>% 
  mutate(item18 = case_when(is.na(fpmcog0r) ~ NA_real_,
                            fpmcog0r == 3 ~1, 
                            TRUE ~ 0 )) %>% 
  mutate(item19 = case_when(is.na(fpmcog0s) ~ NA_real_,
                            fpmcog0s == 1 ~1, 
                            TRUE ~ 0 )) %>% 
  mutate(item20 = case_when(is.na(fpmcog0t) ~ NA_real_,
                            fpmcog0t == 2 ~1, 
                            TRUE ~ 0 )) %>% 
  rowwise(mcsid) %>% 
  mutate(main_totalScore = sum(c_across(item1:item20)))

#partner vocab test 
partner_vocabTest = mcs6_parent_assessment %>% select(mcsid, fresp00, fppcog0a:fppcog0t) %>% 
  filter(fresp00 == 2) %>% 
  select(!fresp00)
#recode so that correct answer = 1 and incorrect answer = 0 for each item (consult data dictionary for correct answers)
partner_vocabTest = partner_vocabTest %>%  mutate(item1 = case_when(is.na(fppcog0a) ~ NA_real_, 
                                                                    fppcog0a == 5 ~ 1,
                                                                    TRUE ~ 0)) %>% 
  mutate(item2 = case_when(is.na(fppcog0b) ~ NA_real_,
                           fppcog0b == 3 ~ 1, 
                           TRUE ~ 0)) %>% 
  mutate(item3 = case_when(is.na(fppcog0c) ~ NA_real_,
                           fppcog0c == 3 ~ 1, 
                           TRUE ~ 0)) %>% 
  mutate(item4 = case_when(is.na(fppcog0d) ~ NA_real_,
                           fppcog0d == 5 ~ 1, 
                           TRUE ~ 0)) %>% 
  mutate(item5 = case_when(is.na(fppcog0e) ~ NA_real_,
                           fppcog0e == 5 ~1, 
                           TRUE ~ 0 )) %>% 
  mutate(item6 = case_when(is.na(fppcog0f) ~ NA_real_,
                           fppcog0f == 4 ~1, 
                           TRUE ~ 0 )) %>% 
  mutate(item7 = case_when(is.na(fppcog0g) ~ NA_real_,
                           fppcog0g == 4 ~1, 
                           TRUE ~ 0 )) %>% 
  mutate(item8 = case_when(is.na(fppcog0h) ~ NA_real_,
                           fppcog0h == 5 ~1, 
                           TRUE ~ 0 )) %>% 
  mutate(item9 = case_when(is.na(fppcog0i) ~ NA_real_,
                           fppcog0i == 2 ~1, 
                           TRUE ~ 0 )) %>% 
  mutate(item10 = case_when(is.na(fppcog0j) ~ NA_real_,
                            fppcog0j == 3 ~1, 
                            TRUE ~ 0 )) %>% 
  mutate(item11 = case_when(is.na(fppcog0k) ~ NA_real_,
                            fppcog0k == 4 ~1, 
                            TRUE ~ 0 )) %>% 
  mutate(item12 = case_when(is.na(fppcog0l) ~ NA_real_,
                            fppcog0l == 2 ~1, 
                            TRUE ~ 0 )) %>% 
  mutate(item13 = case_when(is.na(fppcog0m) ~ NA_real_,
                            fppcog0m == 4 ~1, 
                            TRUE ~ 0 )) %>% 
  mutate(item14 = case_when(is.na(fppcog0n) ~ NA_real_,
                            fppcog0n == 3 ~1, 
                            TRUE ~ 0 )) %>% 
  mutate(item15 = case_when(is.na(fppcog0o) ~ NA_real_,
                            fppcog0o == 2 ~1, 
                            TRUE ~ 0 )) %>% 
  mutate(item16 = case_when(is.na(fppcog0p) ~ NA_real_,
                            fppcog0p == 4 ~1, 
                            TRUE ~ 0 )) %>% 
  mutate(item17 = case_when(is.na(fppcog0q) ~ NA_real_,
                            fppcog0q == 3 ~1, 
                            TRUE ~ 0 )) %>% 
  mutate(item18 = case_when(is.na(fppcog0r) ~ NA_real_,
                            fppcog0r == 4 ~1, 
                            TRUE ~ 0 )) %>% 
  mutate(item19 = case_when(is.na(fppcog0s) ~ NA_real_,
                            fppcog0s == 2 ~1, 
                            TRUE ~ 0 )) %>% 
  mutate(item20 = case_when(is.na(fppcog0t) ~ NA_real_,
                            fppcog0t == 4 ~1, 
                            TRUE ~ 0 )) %>% 
  rowwise(mcsid) %>% 
  mutate(partner_totalScore = sum(c_across(item1:item20)))

#combine together to get the mean 
main_vocabTotal = main_vocabTest %>% select(mcsid, main_totalScore)
partner_vocabTotal = partner_vocabTest %>% select(mcsid, partner_totalScore)
caregiver_vocabTotal = merge(all=TRUE, main_vocabTotal, partner_vocabTotal, by="mcsid")
#calculate mean across main and partner
caregiver_vocabTotal = caregiver_vocabTotal %>% mutate (caregiver_vocab = rowMeans(.[-1], na.rm = TRUE), .after = 1) 
#convert NaN to NA
caregiver_vocabTotal$caregiver_vocab[is.nan(caregiver_vocabTotal$caregiver_vocab)]<-NA

#auxiliary variables for imputation####
#mother's age at birth of CM####
#sweep 1 - creating mother respondent variables
mother_main_birth <- MAINrespondent[MAINrespondent$amdres00 == 1 | MAINrespondent$amdres00 == 11 | MAINrespondent$amdres00 == 15 ,]
mother_partner_birth <- PARTNERrespondent[PARTNERrespondent$apdres00 == 1 |PARTNERrespondent$apdres00 == 11| PARTNERrespondent$apdres00 == 15,]
mother_partner_birth <- mother_partner_birth[!is.na(mother_partner_birth$mcsid),] 
mother_main_sweep1_birth <- merge (all=TRUE, mother_main_birth, sweep_entry, by="mcsid")
mother_main_sweep1_1_birth<- mother_main_sweep1_birth[which(mother_main_sweep1_birth$sentry == "1"),]
sweep1_main_mother_birth <- c("mcsid", "amdres00")
sweep1_main_mother_birth <-mother_main_sweep1_1_birth[sweep1_main_mother_birth]
sweep1_main_mother_birth  <- sweep1_main_mother_birth[!is.na(sweep1_main_mother_birth$amdres00),] 
mother_partner_sweep1_birth <- merge (all=TRUE, mother_partner_birth, sweep_entry, by="mcsid")
mother_partner_sweep1_1_birth<- mother_partner_sweep1_birth[which(mother_partner_sweep1_birth$sentry == "1"),]
sweep1_partner_mother_birth <- c("mcsid", "apdres00")
sweep1_partner_mother_birth <-mother_partner_sweep1_1_birth[sweep1_partner_mother_birth]
sweep1_partner_mother_birth  <- sweep1_partner_mother_birth[!is.na(sweep1_partner_mother_birth$apdres00),] 

#mother sweep 2 - new families 
mother_main2_birth <- MAINrespondent2[MAINrespondent2$bmdres00 == 1 | MAINrespondent2$bmdres00 == 11 | MAINrespondent2$bmdres00 == 15 ,]
mother_partner2_birth <- PARTNERrespondent2[PARTNERrespondent2$bpdres00 == 1 | PARTNERrespondent2$bpdres00 == 11 | PARTNERrespondent2$bpdres00 == 15 ,]
mother_partner2_birth <- mother_partner2_birth[!is.na(mother_partner2_birth$mcsid),] 
mum_main_sweep2_1st_birth <- merge (all=TRUE, mother_main2_birth, sweep_entry, by="mcsid")
mum_main_sweep2_1_birth<- mum_main_sweep2_1st_birth[which(mum_main_sweep2_1st_birth$sentry == "2"),]
sweep2_main_mum1_birth <- c("mcsid", "bmdres00")
sweep2_main_mum1_birth <- mum_main_sweep2_1_birth[sweep2_main_mum1_birth]
sweep2_main_mum1_birth <- sweep2_main_mum1_birth[!is.na(sweep2_main_mum1_birth$bmdres00),] 
mum_partner_sweep2_1st_birth <- merge (all=TRUE, mother_partner2_birth, sweep_entry, by="mcsid")
mum_partner_sweep2_1_birth<- mum_partner_sweep2_1st_birth[which(mum_partner_sweep2_1st_birth$sentry == "2"),]
sweep2_partner_mum1_birth <- c("mcsid", "bpdres00")
sweep2_partner_mum1_birth <- mum_partner_sweep2_1_birth[sweep2_partner_mum1_birth]
sweep2_partner_mum1_birth <- sweep2_partner_mum1_birth[!is.na(sweep2_partner_mum1_birth$bpdres00),] 
#sweep 1
MAINbirth_age <- c("mcsid", "amdagb00")
MAINbirth_age <- mcs1_derived[MAINbirth_age]
MAINbirth_age[MAINbirth_age==-2:-1] <- NA

PARTNERbirth_age <- c("mcsid", "apdagb00")
PARTNERbirth_age <- mcs1_derived[PARTNERbirth_age]
PARTNERbirth_age[PARTNERbirth_age==-2:-1] <- NA
#PARTNERbirth_age1 <- PARTNERbirth_age[which(mcsid_number_age9mo$ahcnuma0=="1"),]

MAIN_mother_age<- MAINbirth_age[MAINbirth_age$mcsid %in% sweep1_main_mother_birth$mcsid,]
PARTNER_mother_age<- PARTNERbirth_age[PARTNERbirth_age$mcsid %in% sweep1_partner_mother_birth$mcsid,]

maternal_age<- merge(all=TRUE,MAIN_mother_age, PARTNER_mother_age, by="mcsid")
mum_age <- ifelse(!is.na(maternal_age$amdagb00), maternal_age$amdagb00, maternal_age$apdagb00)
mum_age1 <- data.frame(maternal_age, mum_age)
MATERNAL_AGEsweep1<- c("mcsid", "mum_age")
MATERNAL_AGEsweep1 <- mum_age1[MATERNAL_AGEsweep1]

#sweep 2 - original families
MAINbirth_age2 <- c("mcsid", "bmdagb00")
MAINbirth_age2 <- mcs2_derived[MAINbirth_age2]
MAINbirth_age2[MAINbirth_age2==-2:-1] <- NA
#MAINbirth_age_2<-MAINbirth_age2[which(mcsid_number_age3$bhcnuma0=="1"),]

PARTNERbirth_age2 <- c("mcsid", "bpdagb00")
PARTNERbirth_age2 <- mcs2_derived[PARTNERbirth_age2]
PARTNERbirth_age2[PARTNERbirth_age2==-2:-1] <- NA
#PARTNERbirth_age_2 <- PARTNERbirth_age2[which(mcsid_number_age3$bhcnuma0=="1"),]

MAIN_mother_age2<- MAINbirth_age2[MAINbirth_age2$mcsid %in% sweep2_main_mum1_birth$mcsid,]
PARTNER_mother_age2<- PARTNERbirth_age2[PARTNERbirth_age2$mcsid %in% sweep2_partner_mum1_birth$mcsid,]



maternal_age2<- merge(all=TRUE,MAIN_mother_age2, PARTNER_mother_age2, by="mcsid")
mum_age2 <- ifelse(!is.na(maternal_age2$bmdagb00), maternal_age2$bmdagb00, maternal_age2$bpdagb00)
mum_age_2 <- data.frame(maternal_age2, mum_age2)
MATERNAL_AGEsweep2<- c("mcsid", "mum_age2")
MATERNAL_AGEsweep2 <- mum_age_2[MATERNAL_AGEsweep2]

mother_age <- merge(all=TRUE, MATERNAL_AGEsweep1, MATERNAL_AGEsweep2, by="mcsid")
mother_age$mothers_age <- ifelse(!is.na(mother_age$mum_age), mother_age$mum_age, mother_age$mum_age2)
mother_birth_age <- c("mcsid", "mothers_age")
mother_birth_age<- mother_age[mother_birth_age]


#housing tenure at age 3####
housing_tenure <- c("mcsid", "bdroow00")
housing_tenure <- mcs2_derived[housing_tenure]
housing_tenure[housing_tenure==-9] <- NA
housing_tenure[housing_tenure==-8] <- NA
housing_tenure[housing_tenure==-1] <- NA


tenure<-housing_tenure[which(mcsid_number_age3$bhcnuma0=="1"),]
new_tenure1<- merge (all=TRUE, tenure, sweep_entry, by="mcsid")
tenure1<- new_tenure1[which(new_tenure1$sentry == "1"),]


#sweep 2 entry families
housing_tenure2 <- c("mcsid", "bdroow00")
housing_tenure2 <- mcs2_derived[housing_tenure2]
housing_tenure2[housing_tenure2==-9] <- NA
housing_tenure2[housing_tenure2==-8] <- NA
housing_tenure2[housing_tenure2==-1] <- NA
tenure2<-housing_tenure2[which(mcsid_number_age3$bhcnuma0=="1"),]
new_tenure2<- merge (all=TRUE, tenure2, sweep_entry, by="mcsid")
tenure_2<- new_tenure2[which(new_tenure2$sentry == "2"),]

house_tenure <- merge(all=TRUE, tenure1, tenure_2,by="mcsid")
house_tenure$tenure_combine <- ifelse(!is.na(house_tenure$bdroow00.x), house_tenure$bdroow00.x, house_tenure$bdroow00.y)

#subset data so just have 1 entry and mcsid for the variable
tenure_house <- c("mcsid", "tenure_combine")
tenure_house<- house_tenure[tenure_house]

#sweep 1 - replace remaining NA values. 
housing_tenure_sweep1 <- c("mcsid", "adroow00")
housing_tenure_sweep1 <- mcs1_derived[housing_tenure_sweep1]
housing_tenure_sweep1[housing_tenure_sweep1==-9] <- NA
housing_tenure_sweep1[housing_tenure_sweep1==-8] <- NA
housing_tenure_sweep1[housing_tenure_sweep1==-1] <- NA
tenure_sweep1<-housing_tenure_sweep1[which(mcsid_number_age9mo$ahcnuma0=="1"),]

tenure_1 <- merge(all=TRUE, tenure_house,tenure_sweep1, by="mcsid")
tenure_1$tenure_type_combined <- ifelse(!is.na(tenure_1$tenure_combine), tenure_1$tenure_combine, tenure_1$adroow00)
TENURE <- c("mcsid", "tenure_type_combined")
TENURE<- tenure_1[TENURE]

TENURE[TENURE==1] <-1
TENURE[TENURE==2] <-1
TENURE[TENURE==3] <-2
TENURE[TENURE==4] <-2
TENURE[TENURE==5] <-2
TENURE[TENURE==6] <-2
TENURE[TENURE==7] <-3
TENURE[TENURE==8] <-4
TENURE[TENURE==9] <-5
TENURE[TENURE==10] <-5

#accommodation type at age 3####
accommodation <-c ("mcsid", "bmmoty00")
accommodation <- mcs2_parent[accommodation]
accommodation[accommodation ==-1:-9] <- NA
accommodation[accommodation == 95] <- NA
accommodation <- accommodation[which(mcsid_number_age3$bhcnuma0=="1"),]
new_accommodation <- merge (all=TRUE,accommodation, sweep_entry, by="mcsid")
new_accommodation1<- new_accommodation[which(new_accommodation$sentry == "1"),]
#new families
accommodation2 <-c ("mcsid", "bmmoty00")
accommodation2 <- mcs2_parent[accommodation2]
accommodation2[accommodation2 ==-1:-9] <- NA
accommodation2[accommodation2 == 95] <- NA
accommodation2 <- accommodation2[which(mcsid_number_age3$bhcnuma0=="1"),] 
new_accommodation2 <- merge (all=TRUE,accommodation2, sweep_entry, by="mcsid")
new_accommodation2<- new_accommodation2[which(new_accommodation2$sentry == "2"),]
accomm <- merge(all=TRUE, new_accommodation1, new_accommodation2,by="mcsid")
accomm_combine <- ifelse(!is.na(accomm$bmmoty00.x), accomm$bmmoty00.x, accomm$bmmoty00.y)
#create dataframe so also have mcsid 
new_accommodation<- data.frame(accomm_combine, accomm)
#subset data so just have 1 standard score and mcsid for the variable
accommodation_type <- c("mcsid", "accomm_combine")
accommodation_type <- new_accommodation[accommodation_type ]

#accommodation type at sweep 1 to fill in missing values
accommodation_sweep1 <-c ("mcsid", "ammoty00")
accommodation_sweep1 <- mcs1_parent[accommodation_sweep1]
accommodation_sweep1[accommodation_sweep1 ==-1:-9] <- NA
accommodation_sweep1[accommodation_sweep1 == 98] <- NA
accommodation_sweep1[accommodation_sweep1 == 99] <- NA
accommodation_sweep1[accommodation_sweep1 == 95] <- NA
accommodation_sweep1[accommodation_sweep1 == 85] <- NA
accommodation_sweep1[accommodation_sweep1 == 86] <- NA
accommodation_sweep1 <- accommodation_sweep1[which(mcsid_number_age9mo$ahcnuma0=="1"),]

accomm_type <- merge(all=TRUE, accommodation_type, accommodation_sweep1, by="mcsid")
accomm_type$accommodation_type_combined <- ifelse(!is.na(accomm_type$accomm_combine), accomm_type$accomm_combine, accomm_type$ammoty00)
ACCOMMODATION <- c("mcsid", "accommodation_type_combined")
ACCOMMODATION<- accomm_type[ACCOMMODATION]

ACCOMMODATION[ACCOMMODATION==1] <-1
ACCOMMODATION[ACCOMMODATION==2] <-2
ACCOMMODATION[ACCOMMODATION==3] <-2
ACCOMMODATION[ACCOMMODATION==4] <-3

#whether CM breastfed####
breastfed <-c("mcsid", "ambfeva0")
breastfed <- mcs1_parent[breastfed]
breastfed[breastfed == -1:-9]<- NA
breastfed <- breastfed[which(mcsid_number_age9mo$ahcnuma0=="1"),]


#number of parents present in household at age 3####
household <-c ("mcsid", "bdhtys00")
household <- mcs2_derived[household]
household[household ==-1:-9] <- NA
household <-household[which(mcsid_number_age3$bhcnuma0=="1"),]
new_household <- merge (all=TRUE,household, sweep_entry, by="mcsid")
new_household1<- new_household[which(new_household$sentry == "1"),]

household2 <-c ("mcsid", "bdhtys00")
household2 <- mcs2_derived[household2]
household2[household2 ==-1:-9] <- NA
household2 <-household2[which(mcsid_number_age3$bhcnuma0=="1"),]
new_household2 <- merge (all=TRUE,household2, sweep_entry, by="mcsid")
new_household_2<- new_household2[which(new_household2$sentry == "2"),]


house <- merge(all=TRUE, new_household1, new_household_2,by="mcsid")
house_combine <- ifelse(!is.na(house$bdhtys00.x), house$bdhtys00.x, house$bdhtys00.y)
#create dataframe so also have mcsid 
new_household<- data.frame(house_combine,house)
#subset data so just have 1 standard score and mcsid for the variable
parents_in_hh <- c("mcsid", "house_combine")
parents_in_hh <- new_household[parents_in_hh ]

#replace NA with sweep1 responses
household_sweep1 <-c ("mcsid", "adhtys00")
household_sweep1 <- mcs1_derived[household_sweep1]
household_sweep1[household_sweep1 ==-1:-9] <- NA
household_sweep1<-household_sweep1[which(mcsid_number_age9mo$ahcnuma0=="1"),]
parents_home <- merge(all=TRUE, household_sweep1, parents_in_hh,by="mcsid")
parents_home$parents_in_household <- ifelse(!is.na(parents_home$house_combine), parents_home$house_combine, parents_home$adhtys00)
PARENTS_IN_HH <- c("mcsid", "parents_in_household")
PARENTS_IN_HH <- parents_home[PARENTS_IN_HH]

