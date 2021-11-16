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
#mcs2_child_assessment <- read_sav("mcs2_cm_cognitive_assessment.sav")
mcs1_hh <- read_sav("mcs1_hhgrid.sav")
mcs2_hh <- read_sav("mcs2_hhgrid.sav")
mcs_family <- read_sav("mcs_longitudinal_family_file.sav")
mcs1_parent <- read_sav("mcs1_parent_interview.sav")
mcs1_parent_12thEd <- read_sav("mcs1_parent_interview_12thEd.sav")
mcs2_parent <- read_sav("mcs2_parent_interview.sav")
mcs2_parent_9thEd <- read_sav("mcs2_parent_interview_9thEd.sav")
mcs2_derived <- read_sav("mcs2_parent_derived.sav")
mcs2_derived_family <- read_sav("mcs2_family_derived.sav")
mcs1_derived_family <- read_sav("mcs1_family_derived.sav")
mcs1_derived <- read_sav("mcs1_parent_derived.sav")
mcs5_parent<- read_sav("mcs5_parent_interview_4thEd.sav")
mcs5_family<- read_sav("mcs5_family_derived.sav") #2018 version
mcs2_geography <- read_sav("mcs2_geographically_linked_data.sav")
mcs1_geography <- read_sav("mcs1_geographically_linked_data.sav")
mcs6_parent_assessment <- read_sav("mcs6_parent_assessment.sav")
mcs3_cm_derived = read_sav("mcs3_cm_derived.sav")
mcs1_cm_derived = read_sav("mcs1_cm_derived.sav")
mcs1_cm_parent = read_sav("mcs1_parent_cm_interview.sav")
mcs2_cm_parent = read_sav("mcs2_parent_cm_interview.sav")
mcs2_cm_derived = read_sav("mcs2_cm_derived.sav")
mcs2_family_derived = read_sav("mcs2_family_derived.sav")
mcs1_family_derived = read_sav("mcs1_family_derived.sav")
mcs7_qualifications = read_sav("mcs7_cm_qualifications.sav")
mcs7_hh_grid = read_sav("mcs7_hhgrid.sav")
mcs6_hh_grid = read_sav("mcs6_hhgrid.sav")
mcs6_family_derived = read_sav("mcs6_family_derived.sav")
#convert all to lowercase####
names(mcs3_child_assessment) <- tolower(names(mcs3_child_assessment))
#names(mcs2_child_assessment) <- tolower(names(mcs2_child_assessment))
names(mcs_family) <- tolower(names(mcs_family))
names(mcs5_family) <- tolower(names(mcs5_family))
names(mcs5_parent) <- tolower(names(mcs5_parent))
names(mcs1_parent) <- tolower(names(mcs1_parent))
names(mcs1_parent_12thEd) <- tolower(names(mcs1_parent_12thEd))
names(mcs2_parent) <- tolower(names(mcs2_parent))
names(mcs2_parent_9thEd) <- tolower(names(mcs2_parent_9thEd))
names(mcs2_derived) <- tolower(names(mcs2_derived))
names(mcs2_derived_family) <- tolower(names(mcs2_derived_family))
names(mcs1_derived_family) <- tolower(names(mcs1_derived_family))
names(mcs1_derived) <- tolower(names(mcs1_derived))
names(mcs1_hh) <- tolower(names(mcs1_hh))
names(mcs2_hh) <- tolower(names(mcs2_hh))
names(mcs2_geography) <- tolower(names(mcs2_geography))
names(mcs1_geography) <- tolower(names(mcs1_geography))
names(mcs6_parent_assessment) <- tolower(names(mcs6_parent_assessment))
names(mcs3_cm_derived) <- tolower(names(mcs3_cm_derived))
names(mcs2_cm_derived) <- tolower(names(mcs2_cm_derived))
names(mcs2_family_derived) <- tolower(names(mcs2_family_derived))
names(mcs1_cm_derived) <- tolower(names(mcs1_cm_derived))
names(mcs1_cm_parent) <- tolower(names(mcs1_cm_parent))
names(mcs2_cm_parent) <- tolower(names(mcs2_cm_parent))
names(mcs1_family_derived) <- tolower(names(mcs1_family_derived))
names(mcs7_hh_grid) <- tolower(names(mcs7_hh_grid))
names(mcs6_hh_grid) <- tolower(names(mcs6_hh_grid))
names(mcs6_family_derived) <- tolower(names(mcs6_family_derived))
names(mcs7_qualifications) <- tolower(names(mcs7_qualifications))
#create sweep entry variable to identify second entry families later####
sweep_entry <- c("mcsid", "sentry")
sweep_entry <- mcs_family[sweep_entry]
sweep_entry$sentry = as.character(sweep_entry$sentry)


#create weight variable####
#attrition and sample weight age 5 sweep - as this is where exposure was measured. 
age5_weight = mcs_family %>% select(mcsid, covwt2)
age17_weight = mcs_family %>% select(mcsid, govwt2)
weight = merge(all=TRUE, age5_weight, age17_weight, by="mcsid") %>% 
  mutate(weight = case_when(!is.na(covwt2) ~ covwt2, 
                            is.na(covwt2) ~ govwt2)) %>% 
  select(mcsid, weight)


respondent_key = c(`1` = "main",
                   `2` = "partner", 
                   `3` = "partner_proxy",
                   `4` = "not_eligible")


#PREDICTOR VARIABLE _AGE 5 VOCAB ####

age5_vocab = mcs3_child_assessment %>% select(mcsid, ccnvtscore, ccnum00) %>% 
  filter(ccnum00 == 1) %>% 
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
  select(mcsid, highest_household_occupation_r) %>% 
  rename("occupational_status" = highest_household_occupation_r)

#income####
#INCOME AT AGE 3. OECD weighted quintiles
#oecd income at age 3
oecd_income = mcs2_derived_family %>% select(mcsid, boecduk0)
#oecd income at 9 months to replace NA
oecd_income_9months = mcs1_derived_family %>% select(mcsid, aoecduk0)
#combine together
income = merge(all=TRUE, oecd_income, oecd_income_9months, by="mcsid") %>% 
  mutate(oecd_income = case_when(!is.na(boecduk0) ~ boecduk0, 
                                 is.na(boecduk0) ~ aoecduk0)) %>% 
  select(mcsid, oecd_income)

#NVQ education variable####
#NVQ qualifications

#RESPONDENT VARIABLE
#parent education variable ####
#first need to identify mother and father figures from main and partner respondents
#do this separately for sweep 1 and 2 as can change between sweeps

#sweep 1 respondent identity
respondent_identity_sweep1 = mcs1_derived %>% select(mcsid, addres00, aelig00) 
mother_respondent_main = respondent_identity_sweep1 %>% filter(aelig00 == 1 & (addres00 == 1 | addres00 == 3 |addres00 ==5|
                                                                                 addres00==7 | addres00 == 9 | addres00 == 11 |
                                                                                 addres00 == 13 |addres00 == 15)) %>% 
  select(mcsid, addres00) %>% 
  rename(mother_mainRespondent = "addres00")

mother_respondent_partner = respondent_identity_sweep1 %>% filter((aelig00 == 2 | aelig00 == 3) & (addres00 == 1 | addres00 == 3 |addres00 ==5|
                                                                                                     addres00==7 | addres00 == 9 | addres00 == 11 |
                                                                                                     addres00 == 13 |addres00 == 15 | addres00 == 21)) %>% 
  select(mcsid, addres00) %>% 
  rename(mother_partnerRespondent = "addres00")

father_respondent_main = respondent_identity_sweep1 %>% filter(aelig00 == 1 & (addres00 == 2 | addres00 == 4 |addres00 ==6|
                                                                                 addres00==8 | addres00 == 10 | addres00 == 12 |
                                                                                 addres00 == 14 |addres00 == 16)) %>% 
  select(mcsid, addres00) %>% 
  rename(father_mainRespondent = "addres00")

father_respondent_partner = respondent_identity_sweep1 %>% filter((aelig00 == 2 | aelig00 == 3) & (addres00 == 2 | addres00 == 4 |addres00 ==6|
                                                                                                     addres00==8 | addres00 == 10 | addres00 == 12 |
                                                                                                     addres00 == 14 |addres00 == 16 | addres00 == 22 | addres00 == 24)) %>% 
  select(mcsid, addres00) %>% 
  rename(father_partnerRespondent = "addres00")

#sweep 2 respondent identity

respondent_identity_sweep2  = mcs2_derived %>% select(mcsid, bddres00, belig00)
#split sweep 2 into original families and families that joined in second sweep. 
respondent_sweep2_original = merge(all=TRUE, respondent_identity_sweep2, sweep_entry, by = "mcsid") %>% 
  filter(sentry == 1)
mother_respondent_main_sweep2_original = respondent_sweep2_original %>% filter(belig00 == 1 & (bddres00 == 1 | bddres00 == 3 |bddres00 ==5|
                                                                                                 bddres00==7 | bddres00 == 9 | bddres00 == 11 |
                                                                                                 bddres00 == 13 |bddres00 == 15)) %>% 
  select(mcsid, bddres00) %>% 
  rename(mother_mainRespondent_2 = "bddres00")

mother_respondent_partner_sweep2_original = respondent_sweep2_original %>% filter((belig00 == 2 | belig00 == 3) & (bddres00 == 1 | bddres00 == 3 |bddres00 ==5|
                                                                                                                     bddres00==7 | bddres00 == 9 | bddres00 == 11 |
                                                                                                                     bddres00 == 13 |bddres00 == 15 | bddres00 == 21)) %>% 
  select(mcsid, bddres00) %>% 
  rename(mother_partnerRespondent_2 = "bddres00")

father_respondent_main_sweep2_original = respondent_sweep2_original%>% filter(belig00 == 1  & (bddres00 == 2 | bddres00 == 4 |bddres00 ==6|
                                                                                                 bddres00==8 | bddres00 == 10 | bddres00 == 12 |
                                                                                                 bddres00 == 14 |bddres00 == 16)) %>% 
  select(mcsid, bddres00) %>% 
  rename(father_mainRespondent_2 = "bddres00")

father_respondent_partner_sweep2_original = respondent_sweep2_original %>% filter((belig00 == 2 | belig00 == 3) & bddres00 == 2 | bddres00 == 4 |bddres00 ==6|
                                                                                    bddres00==8 | bddres00 == 10 | bddres00 == 12 |
                                                                                    bddres00 == 14 |bddres00 == 16 |bddres00 == 18 | bddres00 == 22 | bddres00 == 24) %>% 
  select(mcsid, bddres00) %>% 
  rename(father_partnerRespondent_2 = "bddres00")

#families that joined in second sweep - new families
respondent_sweep2_new = merge(all=TRUE, respondent_identity_sweep2, sweep_entry, by = "mcsid") %>% 
  filter(sentry == 2)
mother_respondent_main_sweep2_new = respondent_sweep2_new %>% filter(belig00 == 1 & (bddres00 == 1 | bddres00 == 3 |bddres00 ==5|
                                                                                       bddres00==7 | bddres00 == 9 | bddres00 == 11 |
                                                                                       bddres00 == 13 |bddres00 == 15)) %>% 
  select(mcsid, bddres00) %>% 
  rename(mother_mainRespondent_2_new = "bddres00")

mother_respondent_partner_sweep2_new = respondent_sweep2_new %>% filter((belig00 == 2 | belig00 == 3) & (bddres00 == 1 | bddres00 == 3 |bddres00 ==5|
                                                                                                           bddres00==7 | bddres00 == 9 | bddres00 == 11 |
                                                                                                           bddres00 == 13 |bddres00 == 15 | bddres00 == 21 )) %>% 
  select(mcsid, bddres00) %>% 
  rename(mother_partnerRespondent_2_new = "bddres00")

father_respondent_main_sweep2_new = respondent_sweep2_new%>% filter(belig00 == 1  & (bddres00 == 2 | bddres00 == 4 |bddres00 ==6|
                                                                                       bddres00==8 | bddres00 == 10 | bddres00 == 12 |
                                                                                       bddres00 == 14 |bddres00 == 16)) %>% 
  select(mcsid, bddres00) %>% 
  rename(father_mainRespondent_2_new = "bddres00")

father_respondent_partner_sweep2_new = respondent_sweep2_new %>% filter((belig00 == 2 | belig00 == 3) & (bddres00 == 2 | bddres00 == 4 |bddres00 ==6|
                                                                                                           bddres00==8 | bddres00 == 10 | bddres00 == 12 |
                                                                                                           bddres00 == 14 |bddres00 == 16 | bddres00 == 18 |bddres00 == 22 | bddres00 == 24)) %>% 
  select(mcsid, bddres00) %>% 
  rename(father_partnerRespondent_2_new = "bddres00")


#parent NVQ at age 3 - for original families
#recode education variables
mcs2_derived$bddnvq00[mcs2_derived$bddnvq00==-1] <- NA
mcs2_derived$bddnvq00[mcs2_derived$bddnvq00==-1] <- NA
mcs2_derived$bddnvq00[mcs2_derived$bddnvq00==95] <- 0 #overseas qual only - convert into an NVQ level 0
mcs2_derived$bddnvq00[mcs2_derived$bddnvq00==96] <- 0

#mother
maternal_NVQ_age3_original_main = mcs2_derived %>% select(mcsid, bddnvq00, belig00) %>% 
  filter(belig00 == 1 & (mcsid %in% mother_respondent_main_sweep2_original$mcsid)) %>% 
  select(mcsid, bddnvq00) %>% 
  rename("main_education" = bddnvq00)
maternal_NVQ_age3_original_partner = mcs2_derived %>% select(mcsid, bddnvq00, belig00) %>% 
  filter((belig00 == 2 |belig00 == 3) & (mcsid %in% mother_respondent_partner_sweep2_original$mcsid)) %>% 
  select(mcsid, bddnvq00) %>% 
  rename("partner_education" = bddnvq00)
maternal_NVQ_age3_original = merge(all=TRUE, maternal_NVQ_age3_original_main, maternal_NVQ_age3_original_partner, by="mcsid") %>% 
  mutate(maternal_education_age3 = case_when(!is.na(main_education) ~ main_education, 
                                             is.na(main_education) ~ partner_education)) %>% 
  select(mcsid, maternal_education_age3)

#father
paternal_NVQ_age3_original_main = mcs2_derived %>%  select(mcsid, bddnvq00, belig00) %>% 
  filter(belig00 ==1 & (mcsid %in% father_respondent_main_sweep2_original$mcsid)) %>% 
  select(mcsid, bddnvq00) %>% 
  rename("main_education" = bddnvq00)
paternal_NVQ_age3_original_partner = mcs2_derived %>%  select(mcsid, bddnvq00, belig00) %>% 
  filter((belig00 == 2 |belig00 == 3) & (mcsid %in% father_respondent_partner_sweep2_original$mcsid)) %>% 
  select(mcsid, bddnvq00) %>% 
  rename("partner_education" = bddnvq00)
paternal_NVQ_age3_original = merge(all=TRUE, paternal_NVQ_age3_original_main,paternal_NVQ_age3_original_partner, by = "mcsid") %>% 
  mutate(paternal_education_age3 = case_when(!is.na(partner_education) ~ partner_education, 
                                             is.na(partner_education) ~ main_education)) %>% 
  select(mcsid, paternal_education_age3)

#original families - parent NVQ at 9 months 
mcs1_derived$addnvq00[mcs1_derived$addnvq00==-1] <- NA
mcs1_derived$addnvq00[mcs1_derived$addnvq00==-1] <- NA
mcs1_derived$addnvq00[mcs1_derived$addnvq00==95] <- 0 #overseas qual only - convert into an NVQ level 0
mcs1_derived$addnvq00[mcs1_derived$addnvq00==96] <- 0

#mother
maternal_NVQ_9months_main = mcs1_derived %>% select(mcsid, addnvq00, aelig00) %>% 
  filter(aelig00 == 1 &(mcsid %in% mother_respondent_main$mcsid)) %>% 
  select(mcsid, addnvq00) %>% 
  rename("main_education" = addnvq00)
maternal_NVQ_9months_partner = mcs1_derived %>% select(mcsid, addnvq00, aelig00) %>% 
  filter((aelig00 == 2 | aelig00 == 3) & (mcsid %in% mother_respondent_partner$mcsid)) %>%
  select(mcsid, addnvq00) %>% 
  rename("partner_education" = addnvq00)
maternal_NVQ_9months = merge(all=TRUE, maternal_NVQ_9months_main, maternal_NVQ_9months_partner, by="mcsid") %>% 
  mutate(maternal_education_9months = case_when(!is.na(main_education) ~ main_education, 
                                                is.na(main_education) ~ partner_education)) %>% 
  select(mcsid, maternal_education_9months)
#father
paternal_NVQ_9months_main = mcs1_derived %>% select(mcsid, addnvq00, aelig00) %>% 
  filter(aelig00 == 1 & (mcsid %in% father_respondent_main$mcsid)) %>% 
  select(mcsid, addnvq00) %>% 
  rename("main_education" = addnvq00)
paternal_NVQ_9months_partner = mcs1_derived %>% select(mcsid, addnvq00, aelig00) %>% 
  filter((aelig00 == 2 | aelig00 == 3) & (mcsid %in% father_respondent_partner$mcsid)) %>% 
  select(mcsid, addnvq00) %>% 
  rename("partner_education" = addnvq00)
paternal_NVQ_9months = merge(all=TRUE,paternal_NVQ_9months_main, paternal_NVQ_9months_partner, by="mcsid") %>% 
  mutate(paternal_education_9months = case_when(!is.na(partner_education) ~ partner_education, 
                                                is.na(partner_education) ~ main_education)) %>% 
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

maternal_NVQ_age3_new_main = mcs2_derived %>% select(mcsid, bddnvq00, belig00) %>% 
  filter(belig00 ==1 &(mcsid %in% mother_respondent_main_sweep2_new$mcsid)) %>% 
  select(mcsid, bddnvq00) %>% 
  rename("main_education" = bddnvq00)
maternal_NVQ_age3_partner = mcs2_derived %>% select(mcsid,  bddnvq00, belig00) %>% 
  filter((belig00 == 2 | belig00 == 3) & (mcsid %in% mother_respondent_partner_sweep2_new$mcsid)) %>% 
  select(mcsid, bddnvq00) %>% 
  rename("partner_education" = bddnvq00)
maternal_NVQ_age3_new = merge(all=TRUE, maternal_NVQ_age3_new_main, maternal_NVQ_age3_partner, by="mcsid") %>% 
  mutate(maternal_education_age3_new = case_when(!is.na(main_education) ~ main_education, 
                                                 is.na(main_education) ~ partner_education)) %>% 
  select(mcsid, maternal_education_age3_new)
#father
paternal_NVQ_age3_new_main = mcs2_derived %>%  select(mcsid, bddnvq00, belig00) %>% 
  filter(belig00 ==1 &(mcsid %in% father_respondent_main_sweep2_new$mcsid)) %>% 
  select(mcsid, bddnvq00) %>% 
  rename("main_education" = bddnvq00)
paternal_NVQ_age3_new_partner = mcs2_derived %>%  select(mcsid,bddnvq00, belig00) %>% 
  filter((belig00 == 2 | belig00 == 3) & (mcsid %in% father_respondent_partner_sweep2_new$mcsid))%>% 
  select(mcsid, bddnvq00) %>% 
  rename("partner_education" = bddnvq00)
paternal_NVQ_age3_new = merge(all=TRUE, paternal_NVQ_age3_new_main, paternal_NVQ_age3_new_partner, by="mcsid") %>% 
  mutate(paternal_education_age3_new = case_when(!is.na(partner_education) ~ partner_education, 
                                                 is.na(partner_education) ~ main_education)) %>% 
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
#debt seems to be missing from mcs5 new datasets? from 2017 release of the data.
#University of London. Institute of Education. Centre for Longitudinal Studies. (2017). Millennium Cohort Study: Fifth Survey, 2012. [data collection]. 4th Edition. UK Data Service. SN: 7464, http://doi.org/10.5255/UKDA-SN-7464-4

wealth_variables <- mcs5_parent%>%  select(mcsid, eresp00, epmopa00, ephval00, epinvt00, epdeba00, eelig00) %>% 
  filter(eelig00 == 1) %>% 
  select(mcsid, epmopa00, ephval00, epinvt00, epdeba00) %>% 
  rename("mortgage" = epmopa00,
         "houseValue" = ephval00, 
         "savings" = epinvt00, 
         "debt" = epdeba00)


#IMD at age 3, 9 months if missing
imd_sweep2 = mcs2_geography %>% select(mcsid, bimdscoe, biwimdsc, bisimdsc,bimdscon) %>% 
  mutate(imd_sweep2 =  pmax(bimdscoe, biwimdsc, bisimdsc, bimdscon,  na.rm = TRUE))

imd_sweep1 = mcs1_geography %>% select(mcsid, aimdscoe, aiwimdsc, aisimdsc, aimdscon) %>% 
  mutate(imd_sweep1 =  pmax(aimdscoe, aiwimdsc, aisimdsc, aimdscon,  na.rm = TRUE))

#IMD at age 3 and if NA, replace with 9 months.
imd = merge(all=TRUE, imd_sweep2, imd_sweep1, by="mcsid") %>% 
  select(mcsid, imd_sweep2, imd_sweep1) %>% 
  mutate(imd = case_when(!is.na(imd_sweep2) ~ imd_sweep2, 
                         is.na(imd_sweep2) ~imd_sweep1)) %>% 
  select(mcsid, imd)

#creating potential confounders ####
#language spoken at home#### 
#University of London. Institute of Education. Centre for Longitudinal Studies. (2017). Millennium Cohort Study: First Survey, 2001-2003. [data collection]. 12th Edition. UK Data Service. SN: 4683, http://doi.org/10.5255/UKDA-SN-4683-4
#University of London. Institute of Education. Centre for Longitudinal Studies. (2017). Millennium Cohort Study: Second Survey, 2003-2005. [data collection]. 9th Edition. UK Data Service. SN: 5350, http://doi.org/10.5255/UKDA-SN-5350-4
EAL_sweep1 = mcs1_parent_12thEd %>% select(mcsid, ahlang00) 

EAL_sweep2_original = mcs2_parent_9thEd %>% select(mcsid, bhhlan00) %>% 
  merge(all =TRUE, sweep_entry, by = "mcsid") %>% 
  filter(sentry == 1) %>% 
  merge(all=TRUE, EAL_sweep1, by ="mcsid") %>% 
  mutate(EAL_sentry1 = case_when(!is.na(bhhlan00) ~ bhhlan00, #language spoken at home at age 3, if missing, at 9 months. 
                                 is.na(bhhlan00) ~ ahlang00)) %>% 
  select(mcsid, EAL_sentry1)

EAL = mcs2_parent_9thEd %>% select(mcsid, bhhlan00) %>% 
  merge(all =TRUE, sweep_entry, by = "mcsid") %>% 
  filter(sentry == 2) %>% 
  merge(all=TRUE, EAL_sweep2_original, by="mcsid") %>% 
  mutate(EAL = case_when(!is.na(EAL_sentry1) ~ EAL_sentry1, 
                         is.na(EAL_sentry1) ~ bhhlan00)) %>% 
  select(mcsid, EAL) 

#ethnicity####
#ethnicity single births mcs1
ethnicity_sweep1 = mcs1_cm_derived %>% select(mcsid, adc06e00, acnum00) %>% 
  filter(acnum00==1) %>% 
  select(mcsid, adc06e00)
ethnicity_sweep2 = mcs2_cm_derived %>% select(mcsid, bdc06e00, bcnum00) %>% 
  filter(bcnum00==1) %>% 
  #merge(all=TRUE,  sweep_entry, by="mcsid") %>% 
  #filter(sentry == 2) %>% #check if want to do this or just replace sweep 1 NA with sweep 2 responses regardless of sweep entry
  select(mcsid, bdc06e00) %>% 
  merge(all= TRUE, ethnicity_sweep1, by = "mcsid") %>% 
  mutate(ethnicity = case_when(!is.na(adc06e00) ~ adc06e00, 
                               is.na(adc06e00) ~ bdc06e00))
ethnicity = ethnicity_sweep2 %>% select(mcsid, ethnicity)


#sex at birth####
sex_sweep1 = mcs1_hh %>% select(mcsid, ahcsex00,acnum00) %>% 
  filter(acnum00 ==1) %>% 
  select(mcsid, ahcsex00)

sex_sweep2 = mcs2_hh %>% select(mcsid, bhcsex00, bcnum00) %>% 
  filter(bcnum00 == 1) %>% 
  select(mcsid, bhcsex00) %>% 
  #merge(all=TRUE, sweep_entry, by="mcsid") %>% 
  #filter(sentry == 2) %>% 
  select(mcsid, bhcsex00) %>% 
  merge(all=TRUE, sex_sweep1, by = "mcsid") %>% 
  mutate(sex = case_when(!is.na(ahcsex00) ~ ahcsex00,
                         is.na(ahcsex00) ~ bhcsex00))

sex = sex_sweep2 %>% select(mcsid, sex)


#caregiver vocabulary ####
#main respondent word activity test
#get items for main respondent activity test to recode
main_vocabTest = mcs6_parent_assessment %>% select(mcsid, fresp00, fpmcog0a:fpmcog0t) %>% 
  filter(fresp00 == 1) %>% 
  select(!fresp00)

#recode so that correct answer = 1 and incorrect answer = 0 for each item (consult data dictionary for correct answers)
main_vocabTest = main_vocabTest %>%  mutate(item1 = case_when(is.na(fpmcog0a) ~ NA_real_, 
                                                              fpmcog0a == 1 ~ 1,
                                                              fpmcog0a == 6 ~ 0,
                                                              fpmcog0a == 7 ~ NA_real_,
                                                              TRUE ~ 0)) %>% 
  mutate(item2 = case_when(is.na(fpmcog0b) ~ NA_real_,
                           fpmcog0b == 5 ~ 1, 
                           fpmcog0b == 6 ~ 0,
                           fpmcog0b == 7 ~ NA_real_,
                           TRUE ~ 0)) %>% 
  mutate(item3 = case_when(is.na(fpmcog0c) ~ NA_real_,
                           fpmcog0c == 5 ~ 1,
                           fpmcog0c == 6 ~ 0,
                           fpmcog0c == 7 ~ NA_real_,
                           TRUE ~ 0)) %>% 
  mutate(item4 = case_when(is.na(fpmcog0d) ~ NA_real_,
                           fpmcog0d == 1 ~ 1, 
                           fpmcog0d == 6 ~ 0,
                           fpmcog0d == 7 ~ NA_real_,
                           TRUE ~ 0)) %>% 
  mutate(item5 = case_when(is.na(fpmcog0e) ~ NA_real_,
                           fpmcog0e == 1 ~1, 
                           fpmcog0e == 6 ~ 0,
                           fpmcog0e == 7 ~ NA_real_,
                           TRUE ~ 0 )) %>% 
  mutate(item6 = case_when(is.na(fpmcog0f) ~ NA_real_,
                           fpmcog0f == 4 ~1, 
                           fpmcog0f == 6 ~ 0,
                           fpmcog0f == 7 ~ NA_real_,
                           TRUE ~ 0 )) %>% 
  mutate(item7 = case_when(is.na(fpmcog0g) ~ NA_real_,
                           fpmcog0g == 4 ~1, 
                           fpmcog0g == 6 ~ 0,
                           fpmcog0g == 7 ~ NA_real_,
                           TRUE ~ 0 )) %>% 
  mutate(item8 = case_when(is.na(fpmcog0h) ~ NA_real_,
                           fpmcog0h == 3 ~1, 
                           fpmcog0h == 6 ~ 0,
                           fpmcog0h == 7 ~ NA_real_,
                           TRUE ~ 0 )) %>% 
  mutate(item9 = case_when(is.na(fpmcog0i) ~ NA_real_,
                           fpmcog0i == 2 ~1, 
                           fpmcog0i == 6 ~ 0,
                           fpmcog0i == 7 ~ NA_real_,
                           TRUE ~ 0 )) %>% 
  mutate(item10 = case_when(is.na(fpmcog0j) ~ NA_real_,
                            fpmcog0j == 1 ~1, 
                            fpmcog0j == 6 ~ 0,
                            fpmcog0j == 7 ~ NA_real_,
                            TRUE ~ 0 )) %>% 
  mutate(item11 = case_when(is.na(fpmcog0k) ~ NA_real_,
                            fpmcog0k == 2 ~1, 
                            fpmcog0k == 6 ~ 0,
                            fpmcog0k == 7 ~ NA_real_,
                            TRUE ~ 0 )) %>% 
  mutate(item12 = case_when(is.na(fpmcog0l) ~ NA_real_,
                            fpmcog0l == 1 ~1, 
                            fpmcog0l == 6 ~ 0,
                            fpmcog0l == 7 ~ NA_real_,
                            TRUE ~ 0 )) %>% 
  mutate(item13 = case_when(is.na(fpmcog0m) ~ NA_real_,
                            fpmcog0m == 2 ~1, 
                            fpmcog0m == 6 ~ 0,
                            fpmcog0m == 7 ~ NA_real_,
                            TRUE ~ 0 )) %>% 
  mutate(item14 = case_when(is.na(fpmcog0n) ~ NA_real_,
                            fpmcog0n == 4 ~1, 
                            fpmcog0n == 6 ~ 0,
                            fpmcog0n == 7 ~ NA_real_,
                            TRUE ~ 0 )) %>% 
  mutate(item15 = case_when(is.na(fpmcog0o) ~ NA_real_,
                            fpmcog0o == 2 ~1, 
                            fpmcog0o == 6 ~ 0,
                            fpmcog0o == 7 ~ NA_real_,
                            TRUE ~ 0 )) %>% 
  mutate(item16 = case_when(is.na(fpmcog0p) ~ NA_real_,
                            fpmcog0p == 4 ~1, 
                            fpmcog0p == 6 ~ 0,
                            fpmcog0p == 7 ~ NA_real_,
                            TRUE ~ 0 )) %>% 
  mutate(item17 = case_when(is.na(fpmcog0q) ~ NA_real_,
                            fpmcog0q == 1 ~1, 
                            fpmcog0q == 6 ~ 0,
                            fpmcog0q == 7 ~ NA_real_,
                            TRUE ~ 0 )) %>% 
  mutate(item18 = case_when(is.na(fpmcog0r) ~ NA_real_,
                            fpmcog0r == 3 ~1, 
                            fpmcog0r == 6 ~ 0,
                            fpmcog0r == 7 ~ NA_real_,
                            TRUE ~ 0 )) %>% 
  mutate(item19 = case_when(is.na(fpmcog0s) ~ NA_real_,
                            fpmcog0s == 1 ~1, 
                            fpmcog0s == 6 ~ 0,
                            fpmcog0s == 7 ~ NA_real_,
                            TRUE ~ 0 )) %>% 
  mutate(item20 = case_when(is.na(fpmcog0t) ~ NA_real_,
                            fpmcog0t == 2 ~1, 
                            fpmcog0t == 6 ~ 0,
                            fpmcog0t == 7 ~ NA_real_,
                            TRUE ~ 0 )) %>% 
  rowwise(mcsid) %>% 
  mutate(main_totalScore = sum(c_across(item1:item20)))
#mutate(main_totalScore = item1 + item2 + item3 + item4 + item5 +
# item6 + item7 + item8 + item9 + item10 +
#item11 +item12 +item13 +item14 +item15 +
# item16 +item17 + item18 + item19 + item20, .after = 1)


#partner vocab test 
partner_vocabTest = mcs6_parent_assessment %>% select(mcsid, fresp00, fppcog0a:fppcog0t) %>% 
  filter(fresp00 == 2) %>% 
  select(!fresp00)
#recode so that correct answer = 1 and incorrect answer = 0 for each item (consult data dictionary for correct answers)
partner_vocabTest = partner_vocabTest %>%  mutate(item1 = case_when(is.na(fppcog0a) ~ NA_real_, 
                                                                    fppcog0a == 5 ~ 1,
                                                                    fppcog0a == 6 ~ 0,
                                                                    fppcog0a == 7 ~ NA_real_,
                                                                    
                                                                    TRUE ~ 0)) %>% 
  mutate(item2 = case_when(is.na(fppcog0b) ~ NA_real_,
                           fppcog0b == 3 ~ 1, 
                           fppcog0b == 6 ~ 0,
                           fppcog0b == 7 ~ NA_real_,
                           TRUE ~ 0)) %>% 
  mutate(item3 = case_when(is.na(fppcog0c) ~ NA_real_,
                           fppcog0c == 3 ~ 1, 
                           fppcog0c == 6 ~ 0,
                           fppcog0c == 7 ~ NA_real_,
                           TRUE ~ 0)) %>% 
  mutate(item4 = case_when(is.na(fppcog0d) ~ NA_real_,
                           fppcog0d == 5 ~ 1, 
                           fppcog0d == 6 ~ 0,
                           fppcog0d == 7 ~ NA_real_,
                           TRUE ~ 0)) %>% 
  mutate(item5 = case_when(is.na(fppcog0e) ~ NA_real_,
                           fppcog0e == 5 ~1, 
                           fppcog0e == 6 ~ 0,
                           fppcog0e == 7 ~ NA_real_,
                           TRUE ~ 0 )) %>% 
  mutate(item6 = case_when(is.na(fppcog0f) ~ NA_real_,
                           fppcog0f == 4 ~1, 
                           fppcog0f == 6 ~ 0,
                           fppcog0f == 7 ~ NA_real_,
                           TRUE ~ 0 )) %>% 
  mutate(item7 = case_when(is.na(fppcog0g) ~ NA_real_,
                           fppcog0g == 4 ~1, 
                           fppcog0g == 6 ~ 0,
                           fppcog0g == 7 ~ NA_real_,
                           TRUE ~ 0 )) %>% 
  mutate(item8 = case_when(is.na(fppcog0h) ~ NA_real_,
                           fppcog0h == 5 ~1, 
                           fppcog0h == 6 ~ 0,
                           fppcog0h == 7 ~ NA_real_,
                           TRUE ~ 0 )) %>% 
  mutate(item9 = case_when(is.na(fppcog0i) ~ NA_real_,
                           fppcog0i == 2 ~1, 
                           fppcog0i == 6 ~ 0,
                           fppcog0i == 7 ~ NA_real_,
                           TRUE ~ 0 )) %>% 
  mutate(item10 = case_when(is.na(fppcog0j) ~ NA_real_,
                            fppcog0j == 3 ~1, 
                            fppcog0j == 6 ~ 0,
                            fppcog0j == 7 ~ NA_real_,
                            TRUE ~ 0 )) %>% 
  mutate(item11 = case_when(is.na(fppcog0k) ~ NA_real_,
                            fppcog0k == 4 ~1,
                            fppcog0k == 6 ~ 0,
                            fppcog0k == 7 ~ NA_real_,
                            TRUE ~ 0 )) %>% 
  mutate(item12 = case_when(is.na(fppcog0l) ~ NA_real_,
                            fppcog0l == 2 ~1, 
                            fppcog0l == 6 ~ 0,
                            fppcog0l == 7 ~ NA_real_,
                            TRUE ~ 0 )) %>% 
  mutate(item13 = case_when(is.na(fppcog0m) ~ NA_real_,
                            fppcog0m == 4 ~1, 
                            fppcog0m == 6 ~ 0,
                            fppcog0m == 7 ~ NA_real_,
                            TRUE ~ 0 )) %>% 
  mutate(item14 = case_when(is.na(fppcog0n) ~ NA_real_,
                            fppcog0n == 3 ~1,
                            fppcog0n == 6 ~ 0,
                            fppcog0n == 7 ~ NA_real_,
                            TRUE ~ 0 )) %>% 
  mutate(item15 = case_when(is.na(fppcog0o) ~ NA_real_,
                            fppcog0o == 2 ~1, 
                            fppcog0o == 6 ~ 0,
                            fppcog0o == 7 ~ NA_real_,
                            TRUE ~ 0 )) %>% 
  mutate(item16 = case_when(is.na(fppcog0p) ~ NA_real_,
                            fppcog0p == 4 ~1, 
                            fppcog0p == 6 ~ 0,
                            fppcog0p == 7 ~ NA_real_,
                            TRUE ~ 0 )) %>% 
  mutate(item17 = case_when(is.na(fppcog0q) ~ NA_real_,
                            fppcog0q == 3 ~1, 
                            fppcog0q == 6 ~ 0,
                            fppcog0q == 7 ~ NA_real_,
                            TRUE ~ 0 )) %>% 
  mutate(item18 = case_when(is.na(fppcog0r) ~ NA_real_,
                            fppcog0r == 4 ~1, 
                            fppcog0r == 6 ~ 0,
                            fppcog0r == 7 ~ NA_real_,
                            TRUE ~ 0 )) %>% 
  mutate(item19 = case_when(is.na(fppcog0s) ~ NA_real_,
                            fppcog0s == 2 ~1, 
                            fppcog0s == 6 ~ 0,
                            fppcog0s == 7 ~ NA_real_,
                            TRUE ~ 0 )) %>% 
  mutate(item20 = case_when(is.na(fppcog0t) ~ NA_real_,
                            fppcog0t == 4 ~1, 
                            fppcog0t == 6 ~ 0,
                            fppcog0t == 7 ~ NA_real_,
                            TRUE ~ 0 )) %>% 
  rowwise(mcsid) %>% 
  mutate(partner_totalScore = sum(c_across(item1:item20)))
#mutate(partner_totalScore = item1 + item2 + item3 + item4 + item5 +
#item6 + item7 + item8 + item9 + item10 +
#item11 +item12 +item13 +item14 +item15 +
#item16 +item17 + item18 + item19 + item20, .after = 1) 


#combine together to get the mean 
main_vocabTotal = main_vocabTest %>% select(mcsid, main_totalScore)
partner_vocabTotal = partner_vocabTest %>% select(mcsid, partner_totalScore)
#calculate mean across main and partner
caregiver_vocabTotal = merge(all=TRUE, main_vocabTotal, partner_vocabTotal, by="mcsid") %>% 
 mutate (caregiver_vocab = rowMeans(.[-1], na.rm = TRUE), .after = 1) %>% 
  select(mcsid, caregiver_vocab)
#convert NaN to NA
caregiver_vocabTotal$caregiver_vocab[is.nan(caregiver_vocabTotal$caregiver_vocab)]<-NA

#country (at time of GCSE/N5 exams - age 17 or age 14 if age 17 missing) ####
#sweep 7
sweep7_country = mcs7_hh_grid %>% select(mcsid, gactry00, gcnum00) %>% 
  filter(gcnum00 == 1) %>% 
  select(mcsid, gactry00)

sweep6_country = mcs6_family_derived %>% select(mcsid, factry00)

country_17 = merge(all=TRUE, sweep7_country, sweep6_country, by= "mcsid") %>% 
  mutate(country = case_when(!is.na(gactry00)~ gactry00, 
                             is.na(gactry00)~factry00)) %>% 
  select(mcsid, country)

#auxiliary variables for imputation####
#mother's age at birth of CM####
#sweep 1 - creating mother respondent variables
age_atBirth_sweep2_motherMain = mcs2_derived %>% select(mcsid, bddagb00, bddres00,belig00) %>% 
  filter(belig00 == 1 & (mcsid %in% mother_respondent_main_sweep2_original$mcsid)) %>% 
  select(mcsid, bddagb00) %>% 
  rename("main_birthAge" = bddagb00)


age_atBirth_sweep2_motherPartner = mcs2_derived %>% select(mcsid, bddagb00, bddres00,belig00) %>% 
  filter((belig00 == 2 |belig00 == 3) & (mcsid %in% mother_respondent_partner_sweep2_original$mcsid)) %>% 
  select(mcsid, bddagb00) %>% 
  rename("partner_birthAge" = bddagb00)

age_atBirth_sweep2_original = merge(all=TRUE, age_atBirth_sweep2_motherMain, age_atBirth_sweep2_motherPartner, by="mcsid") %>% 
  mutate(age_atBirth_sweep2 = case_when(!is.na(main_birthAge) ~main_birthAge, 
                                        is.na(main_birthAge) ~ partner_birthAge))

#new families
age_atBirth_sweep2_motherMain_new = mcs2_derived %>% select(mcsid, bddagb00, bddres00,belig00) %>% 
  filter(belig00 == 1 & (mcsid %in% mother_respondent_main_sweep2_new$mcsid)) %>% 
  select(mcsid, bddagb00) %>% 
  rename("main_birthAge_new" = bddagb00)


age_atBirth_sweep2_motherPartner_new = mcs2_derived %>% select(mcsid, bddagb00, bddres00,belig00) %>% 
  filter((belig00 == 2 |belig00 == 3) & (mcsid %in% mother_respondent_partner_sweep2_new$mcsid)) %>% 
  select(mcsid, bddagb00) %>% 
  rename("partner_birthAge_new" = bddagb00)

age_atBirth_sweep2_newFamilies = merge(all=TRUE, age_atBirth_sweep2_motherMain_new, age_atBirth_sweep2_motherPartner_new, by="mcsid") %>% 
  mutate(age_atBirth_sweep2_new = case_when(!is.na(main_birthAge_new) ~main_birthAge_new, 
                                            is.na(main_birthAge_new) ~ partner_birthAge_new))

#combine new entry families with original families
age_atBirth_sweep2 = merge(all=TRUE, age_atBirth_sweep2_original, age_atBirth_sweep2_newFamilies, by="mcsid") %>% 
  mutate(sweep2_birthAge = case_when(!is.na(age_atBirth_sweep2) ~ age_atBirth_sweep2, 
                                     is.na(age_atBirth_sweep2) ~age_atBirth_sweep2_new)) %>% 
  select(mcsid, sweep2_birthAge)



#sweep 1
#main
age_atBirth_main_sweep1 = mcs1_derived %>% select(mcsid, addagb00, addres00, aelig00) %>% 
  filter(aelig00 == 1 & (mcsid %in% mother_respondent_main$mcsid)) %>% 
  select(mcsid, addagb00) %>% 
  rename("main_birthAge" = addagb00)

#partner
age_atBirth_partner_sweep1 = mcs1_derived %>% select(mcsid, addagb00, addres00, aelig00) %>% 
  filter((aelig00 == 2 | aelig00 == 3) & (mcsid %in% mother_respondent_partner$mcsid)) %>% 
  select(mcsid, addagb00) %>% 
  rename("partner_birthAge" = addagb00)
#merge
age_atBirth_sweep1 = merge(all=TRUE, age_atBirth_main_sweep1, age_atBirth_partner_sweep1, by="mcsid") %>% 
  mutate(age_atBirth_sweep1 = case_when(!is.na(main_birthAge) ~ main_birthAge,
                                        is.na(main_birthAge) ~partner_birthAge)) %>% 
  select(mcsid,age_atBirth_sweep1 )

#combine sweeps
age_atBirth = merge(all=TRUE, age_atBirth_sweep1, age_atBirth_sweep2, by="mcsid") %>% 
  mutate(age_atBirth = case_when(!is.na(age_atBirth_sweep1) ~ age_atBirth_sweep1,
                                 is.na(age_atBirth_sweep1) ~ sweep2_birthAge, 
                                 is.na(sweep2_birthAge) ~ age_atBirth_sweep1, 
                                 TRUE ~ NA_real_)) %>% 
  select(mcsid, age_atBirth)



#housing tenure at age 3####
#housing tenure at age 3, replace with 9 months if missing
tenure_sweep1 = mcs1_derived_family %>% select(mcsid, adroow00) 
tenure_sweep2 = mcs2_derived_family %>% select(mcsid, bdroow00) %>% 
  merge(all=TRUE, tenure_sweep1, by="mcsid") %>% 
  mutate(housing_tenure = case_when(!is.na(bdroow00) ~ bdroow00,
                                    is.na(bdroow00) ~ adroow00)) %>% 
  merge(all=TRUE, sweep_entry,by="mcsid") 

tenure = tenure_sweep2 %>% select(mcsid, housing_tenure)

tenure[tenure==1] <-1
tenure[tenure==2] <-1
tenure[tenure==3] <-2
tenure[tenure==4] <-2
tenure[tenure==5] <-2
tenure[tenure==6] <-2
tenure[tenure==7] <-3
tenure[tenure==8] <-4
tenure[tenure==9] <-5
tenure[tenure==10] <-5

#accommodation type at age 3, replace with sweep 1 if NA####
accommodation_sweep2 = mcs2_parent %>% select(mcsid, bpmotm00, belig00) %>% 
  filter (belig00 == 1)

accommodation_sweep1 = mcs1_parent %>% select(mcsid, apmoty00, aelig00) %>% 
  filter (aelig00 == 1) %>% 
  merge(all=TRUE, accommodation_sweep2, by="mcsid") %>% 
  merge(all=TRUE, sweep_entry, by="mcsid") 
accommodation_sweep1$accommodation_type = 
  ifelse(!is.na(accommodation_sweep1$apmoty00),accommodation_sweep1$apmoty00, accommodation_sweep1$bpmotm00)  

accommodation = accommodation_sweep1  %>% select(mcsid, accommodation_type)
#recode values 
accommodation[accommodation == 85] <- NA
accommodation[accommodation == 86] <- NA  
accommodation[accommodation ==-1:-9] <- NA
accommodation[accommodation == 98] <- NA
accommodation[accommodation == 99] <- NA
accommodation[accommodation == 95] <- NA

accommodation[accommodation==1] <-1
accommodation[accommodation==2] <-2
accommodation[accommodation==3] <-2
accommodation[accommodation==4] <-3

#whether CM breastfed####
breastfed_sweep1 = mcs1_cm_parent %>% select(mcsid, acbfev00, acbfem00,aelig00, acnum00) %>% 
  filter(acnum00 == 1) %>% 
  filter(aelig00 == 1) %>% 
  merge(all=TRUE, sweep_entry, by="mcsid") %>% 
  filter(sentry ==1) %>% 
  select(mcsid, acbfev00, acbfem00)

breastfed = mcs2_cm_parent %>% select(mcsid, bpbfmt00, belig00, bcnum00) %>% 
  filter(bcnum00 ==1) %>% 
  filter(belig00 ==1) %>% 
  merge(all=TRUE, sweep_entry, by="mcsid") %>% 
  filter(sentry ==2) %>% 
  select(mcsid, bpbfmt00) %>% 
  merge(all=TRUE, breastfed_sweep1, by="mcsid") %>% 
  mutate(cm_breastfed = case_when(acbfev00 == 1 ~ 1, 
                                  acbfev00 == 2 ~ 2, 
                                  bpbfmt00 == 0 ~ 2, 
                                  bpbfmt00 > 0 ~ 1,
                                  is.na(acbfev00)  ~ NA_real_,
                                  is.na(bpbfmt00) ~ NA_real_)) %>% 
  select(mcsid, cm_breastfed)
#ACBFEV00 ==1, breastfed = 1 (yes)
#ACBFEV00 ==2, breastfed = 2 (no)
#bpbfmt00 ==0, breastfed = 2 (no) as month last breastfed = 0
#bpbfmt00 = any other number, breastfed = 1
#bpbfmt00 = NA, breastfed = NA


#number of parents present in household at age 3; replace with 9 months if NA####
carers_in_hh_s1 = mcs1_family_derived %>% select(mcsid,adhtys00)
carers_in_hh = mcs2_family_derived %>% select(mcsid,bdhtys00) %>% 
  merge(all=TRUE, carers_in_hh_s1, by="mcsid") %>% 
  mutate(carers_in_hh = case_when(!is.na(bdhtys00)~bdhtys00,
                                  is.na(bdhtys00)~adhtys00)) %>% 
  select(mcsid, carers_in_hh)

#COHORT MEMBER EDUCATION AT SWEEP 7 (AGE 16 - GCSE LEVEL QUALIFICATIONS) ####
country = mcs7_hh_grid %>% filter(gcnum00 == 1)
country = country %>%  select(mcsid, gactry00)
names(country) <- c("mcsid", "country")

#single cm per family 
qualifications1 = mcs7_qualifications %>% filter(gcnum00 == 1)

#convert gcse subject names into text names - key for this which will do later. 
gcse_subject_key <-c(`2`	= "Additional Applied Science",
                     `3`	 =" Additional Mathematics",
                     `4`	 = "Additional Science",
                     `261`	 = "Recoded due to low counts - check SA",
                     `7`	 = "Anthropology",
                     `9`	 = "Applied Art and Design",
                     `10`	 = "Applied Business",
                     `15`	 = "Applied ICT ",
                     `18`	 = "Applied Physical Education",
                     `19`	 = "Applied Science",
                     #`-8`	 = "Do not know",
                     `24` = "Art and Design (Art, Craft and Design / Fine Art / Graphic Communication / Photography / Textile Design / Three-Dimensio",
                     `25`	 = "Art and Design (short course)",
                     `30`	 = "Language: Biblical Hebrew",
                     `31`	 = "Biology",
                     `32`	 = "Biology (Human)",
                     `36`	 = "Business",
                     `37`	 = "Business and Communication Systems",
                     `39`	 = "Business Studies",
                     `42`	 = "Catering",
                     `43`	 = "Chemistry",
                     `47`	 = "Children s Learning and Development",
                     `49`	 = "Language: Chinese: Spoken Language ",
                     `51`	 = "Citizenship Studies",
                     `53`	 = "Civilisation",
                     `54`	 = "Classical Civilisation",
                     `57`	 = "Combined Science ",
                     `62`	 = "Communication and Culture",
                     `63`	 = "Computer Science",
                     `64`	 = "Computing",
                     `65`	 = "Construction and the Built Environment",
                     `68`	 = "Language: Cymraeg / Welsh",
                     `69`	 = "Language: Cymraeg Ail Iaith / Welsh Second Language",
                     `70`	 = "Language: Cymraeg Ail Iaith (Short Course) / Welsh Second Language",
                     `73`	 = "Dance",
                     `74`	 = "Design and Technology",
                     `76`	 = "Design and Technology: Graphic Products",
                     `78`	 = "Design and Technology: Textiles Technology",
                     `81`	 = "Drama",
                     `83`	 = "Language: Dutch: spoken language" ,
                     `84`	 = "Language: Dutch: written language ",
                     `85`	 = "Economics",
                     `88`	 = "Engineering",
                     `89`	 = "Language: English",
                     `92`	 = "Language: English - Spoken English Studies (Short Course) ",
                     `95`	 = "Language: English Language",
                     `96`	 = "Language: English Literature",
                     `101`	 = "Film Studies",
                     `102`	 = "Financial Services",
                     `259` = "Other",
                     `106`	 = "Language: French",
                     `110`	 = "Further Additional Science",
                     `111`	 = "Further Mathematics",
                     `113`	 = "Language: Gaidhlig (Scottish Gaelic)",
                     `115`	 = "Geography",
                     `122`	 = "Language: German",
                     `125`	 = "Language: Government and Politics",
                     `128`	 = "Language: Greek: Written Language ",
                     `130`	 = "Language: Gujarati: spoken language" ,
                     `131`	 = "Language: Gujarati: written language ",
                     `132`	 = "Health and Social Care",
                     `133`	 = "Language: Hebrew (Modern)" ,
                     `134`	 = "History",
                     `136`	 = "Home Economics",
                     `137` =" Home Economics Child Development",
                     `139`	 = "Home Economics: Textiles",
                     `141`	 =" Hospitality and Catering",
                     `146`	 = "Information and Communication Technology (ICT, IT)",
                     `150`	 = "Language: Irish",
                     `151`	 = "Language: Irish (short course)",
                     `152`	 = "Language: Irish; spoken language (Short Course)",
                     `155`	 = "Language: Italian: Spoken Language ",
                     `156	` = "Language: Italian: Written Language" ,
                     `158`	 = "Language: Japanese: Spoken Language ",
                     `159`	 =" Language: Japanese: Written Language ",
                     `163`	 = "Language: Latin",
                     `165`	 = "Learning for Life and Work",
                     `170`	 = "Mathematics",
                     `171` = "Mathematics - Linear",
                     `172`	 = "Mathematics - Numeracy",
                     `174`	 = "Media Studies",
                     `176`	 = "Language: Modern Greek",
                     `178`	 = "Language: Modern Hebrew: Spoken Language" ,
                     `179` = "Language: Modern Hebrew: Written Language " ,
                     `182`	 = "Music",
                     `185`	 = "Language: Panjabi: Spoken Language  ",
                     `186`	 = "Language: Panjabi: Written Language"  ,
                     `187`	 = "Performing Arts",
                     `190`	 = "Language: Persian: spoken language ",
                     `191	` = "Language: Persian: written language ",
                     `194`	 = "Philosophy",
                     `195`	 = "Physical Education (Games)",
                     `197`	 = "Physical Education (PE)",
                     `199`	 = "Physics",
                     #`-1`	 = "Not applicable",
                     `204	` = "Language: Polish: Spoken Language" , 
                     `205`	 = "Language: Polish: Written Language ",
                     `206`	 = "Politics",
                     `208`	 = "Language: Portuguese: spoken language" ,
                     `209`	 = "Language: Portuguese: written language ",
                     `211`	 = "Psychology",
                     `214`	 = "Religious Studies",
                     `215`	 = "Religious Studies (Short Course) ",
                     `216`	 = "Rural and Agricultural Science",
                     `219`	 = "Language: Russian: Written Language ",
                     `220`	 = "Science",
                     `222`	 = "Science (Modular)",
                     `224`	 = "Sociology",
                     `226`	 = "Language: Spanish",
                     `229`	 = "Statistics",
                     `233`	 = "Language: Turkish: Spoken Language" ,
                     `234	` = "Language: Turkish: Written Language ",
                     `235`	 = "Language: Urdu",
                     `239`	 = "Language: Welsh First Language",
                     `240`	 = "Welsh Literature",
                     `241`	 = "Work and Life Skills",
                     `245` = "Design and Technology: Product Design",
                     #`-9`	 = "Refusal",
                     `248`	 = "Design and Technology (Resistant Materials Technology)",
                     `249`	 = "Additional Science (Modular)",
                     `252`	 = "Art and Design",
                     `254`	 = "Design and Technology: Food technology")

#those who said yes to having gcse qualification
gcse = qualifications1 %>% filter(gc_s_qual_gcse == 1) # be careful here as for rest of rows per cm, this will be NA.
gcse = qualifications1[qualifications1$mcsid %in% gcse$mcsid,] #those who have yes to gcse, get rest of long data 
gcse = gcse %>% select(mcsid, gcnum00, gc_rowid, gc_s_qual_gcsn_r20, 
                       gc_l_gcsb_name_r40, gc_l_gcdb, gc_l_gcsb_code_r40, gc_l_gcgd) #get relevant variables 

names(gcse) <- c("mcsid", "cm_number", "row_id", "total_quals", "subject_name","double award", "subject_code", "subject_grade") #rename variables
gcse$subject_name = as.factor(gcse$subject_name)#convert subject_name into names
gcse$subject_name =  recode(gcse$subject_name, !!!gcse_subject_key)#convert subject_name into names
gcse = gcse %>%  filter(!is.na(subject_name)) #remove the NA subject name as due to long format, each cm has 20 rows and alot will be NA
gcse$subject_grade =  as.numeric(gcse$subject_grade) #recode grade so all on a 1-9 scale
gcse$subject_grade  = recode(gcse$subject_grade, `10` = 0, `11` = 8.5, `12` = 7, `13` =5.5, 
                             `14` = 4, `15` = 3, `16` = 2, `17` = 1.5, `18` = 1)

#iGCSEs #### 
igcse_subject_key <- c(`1`	 = "Accounting",
                       `2`	 = "Language: Arabic as first language",
                       `4`	 = "Language: Bangla",
                       `5`	 = "Bangladesh Studies",
                       `6`	 = "Language: Bengali",
                       `7`	 = "Biology",
                       `9`	 = "Chemistry",
                       `11`	 = "Language: Classical Arabic",
                       `12`	 = "Commerce",
                       `15`	 = "Language: English - Second language",
                       `16`	 = "Language: English - First Language",
                       `18`	 = "Language: English Language B",
                       `19`	 = "Language: English Literature",
                       `20`	 = "Language: French",
                       `22`	 = "Geography",
                       `23`	 = "Language: German",
                       `25`	 = "Language: Greek as first language",
                       `26`	 = "Language: Gujarati",
                       `27`	 = "Language: Hindi",
                       `28`	 = "History",
                       `31`	 = "Islamic Studies",
                       `32`	 = "Islamiyat",
                       `33`	 = "Mathematics",
                       `37`	 = "Pakistan Studies",
                       `38`	 = "Physics",
                       `39`	 = "Religious Studies",
                       `40`	 = "Science", 
                       `41`	 = "Language: Sinhala",
                       `42`	 = "Language: Spanish",
                       `43`	 = "Language: Swahili",
                       `44`	 = "Language: Tamil",
                       `46`	 = "Language: Urdu",
                       `47`	 = "Language: Afrikaans First Language",
                       `48`	 = "Language: Afrikaans Second Language",
                       `49`	 = "Agriculture",
                       `50`	 = "Language: Arabic - Foreign Language",
                       `51`	 = "Language: Bahasa Indonesia",
                       `52`	 = "Child Development",
                       `53`	 = "Language: Czech",
                       `55`	 = "Development Studies",
                       `57`	 = "Language: Dutch - First Language",
                       `58`	 = "Language: Dutch - Second Language",
                       `59`	 = "Enterprise",
                       `64`	 = "India Studies",
                       `65`	 = "Language: Indonesian - Foreign Language",
                       `66`	 = "Language: IsiZulu as a Second Language",
                       `71`	 = "Language: Kazakh",
                       `72`	 = "Language: Korean",
                       `74`	 = "Language: Malay - Foreign Language",
                       `76`	 = "Mathematics - International",
                       `80`	 = "Language: Portuguese - First Language",
                       `81`	 = "Language: Portuguese - Second Language",
                       `83`	 = "Language: Sanskrit",
                       `87`	 = "Language: Spanish - Literature",
                       `89`	 = "Language: Thai - First Language",
                       `90`	 = "Travel and Tourism",
                       `91`	 = "World Literature",
                       `94`	 = "Recoded due to low counts - check SA")
# `-9`	 = "Refusal
# `-8`	 = "Do not know
# `-1`	 = "Not applicable
#those who said yes to having iGCSE qualifications
Igcse = qualifications1 %>% filter(gc_s_qual_igcs == 1) # be careful here as for rest of rows per cm, this will be NA.
Igcse = qualifications1[qualifications1$mcsid %in% Igcse$mcsid,] 
Igcse = Igcse %>% select(mcsid, gcnum00, gc_rowid, gc_s_qual_igsn_rec,
                         gc_l_igsb_name_r30, gc_l_iggd)


names(Igcse) <- c("mcsid", "cm_number", "row_id", "total_quals", "subject_name", "subject_grade")
Igcse$subject_name = as.factor(Igcse$subject_name)
Igcse$subject_name =  recode(Igcse$subject_name, !!!igcse_subject_key)
Igcse = Igcse %>%  filter(!is.na(subject_name)) #remove the NA subject name as due to long format, each cm has 20 rows and alot will be NA

#these are different to GCSE grades!! so the gcse grade recoding will be different here - updated. 
# recode so that 1= 1, 2=2, 3=3, 4=4, 5=5, 6=6, 7=7, 8=8, 9=9. at the moment in the data, 1= grade 9 etc. 
#so when recode, 1=9, 2=8 etc 
Igcse$subject_grade =  as.numeric(Igcse$subject_grade)
Igcse$subject_grade  = recode(Igcse$subject_grade, `1` = 9, `2` = 8, `3` = 7, `4` = 6, `6`= 4, `7` = 3, `8` = 2, `9` = 1,
                              `10` = 8.5, `11` = 7, `12` = 5.5, `14` =4, 
                              `14` = 4, `15` = 3, `16` = 2, `17` = 1.5, `19` = 0)

#cm have grade 4 or above on core subjects ####


#binary variable of who got above benchmark in core subjects at gcse
#core subjects = maths, english (lang and/or lit), science
#for science, any form of science as due to self reports these may be reported differently 
#either GCSE or iGCSE
#could put into wide format where the column names are subject name and column values are subject grade? 
gcse_grades <- gcse %>% select(mcsid, row_id,  subject_name, subject_grade)
gcse_grades$subject_name = as.character(gcse_grades$subject_name)
gcse_grades$subject_grade = as.numeric(gcse_grades$subject_grade)
#wide_gcse_grades = pivot_wider(gcse_grades, id_cols=c("mcsid"), names_from = subject_name,
#  values_from = subject_grade, values_fill = 0)


#identify those who have duplicate subjects based on wide data
#duplicated_wide = wide_grades$mcsid[duplicated(wide_grades$mcsid)]
#remove duplicate subjects and keep the subject with the higher grade
gcse_grades1 <- gcse_grades %>% group_by(mcsid) %>% 
  arrange(desc(subject_grade), .by_group = TRUE) %>%  #arrange subject_grade by descending order, so distinct can select the higher entry 
  distinct(subject_name, .keep_all = TRUE) #this will give only one of the duplicated values - because have sorted subject grade in descending order, this will keep the higher grade for the duplicated subject


#convert to wide dataset where each column name is a subject with the grade as the entry per CM
wide_grades = gcse_grades1 %>% select(!row_id)
wide_grades = wide_grades %>%
  group_by(mcsid, subject_name) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = subject_name, values_from = subject_grade) %>% 
  select(-row)


#wide_grades = wide_grades %>% rename(english_lit = "Language: English Literature", 
#                                    english = "Language: English", 
#                                   english_lang = "Language: English Language", 
#                                  maths = "Mathematics", 
#                                    maths_linear = "Mathematics - Linear", 
#                                   maths_numeracy = "Mathematics - Numeracy", 
#                                  further_maths = "Further Mathematics", 
#                                 additional_maths =  " Additional Mathematics", 
#                                biology = "Biology", 
#                               chemistry = "Chemistry", 
#                              physics = "Physics",
#                             additional_science = "Additional Science", 
#                            science = "Science", 
#                           applied_science = "Applied Science", 
#                          combined_science = `Combined Science `,
#                         modular_science = "Science (Modular)",
#                        further_additional_science = `Further Additional Science`,
#                       computer_science = `Computer Science`,
#                      additional_applied_science = `Additional Applied Science`,
#                     additional_science_modular = `Additional Science (Modular)`,
#                    human_biology = `Biology (Human)`)


#select core subjects from this wide dataset

core_subjects_grades <- wide_grades %>% select(mcsid, c("Language: English", "Language: English Language", "Language: English Literature",
                                                        "Mathematics", "Mathematics - Linear", "Mathematics - Numeracy", contains("math"), "Biology", "Chemistry", "Physics"), #include additional mathematics?
                                               contains("science"), contains("biology"))

names(core_subjects_grades) <- c("mcsid", "english", "english_lang", "english_lit",
                                 "maths", "maths_linear", "maths_numeracy", "further_maths", "additional_maths", "biology", "chemistry", "physics", 
                                 "additional_science", "science", "applied_science", "combined_science", "modular_science", "further_additional_science", "computer_science", 
                                 "additional_applied_science", "additional_science_modular", "human_biology")

#core_grades$english[core_grades$english == 'NULL'] <- NA
#core_grades$english_lang[core_grades$english_lang == 'NULL'] <- NA
#core_grades$english_lit[core_grades$english_lit == 'NULL'] <- NA
#core_grades$maths[core_grades$maths == 'NULL'] <- NA
#core_grades$maths_linear[core_grades$maths_linear == 'NULL'] <- NA
#core_grades$maths_numeracy[core_grades$maths_numeracy == 'NULL'] <- NA

#create core grades binary variable - those with 4 and above in english, maths and science (in one of each of these subjects), then binary variable = 1
#make sure this also includes iGCSE grades

#add in Igcse to core_subjects data - merge? 

#could put into wide format where the column names are subject name and column values are subject grade? 
Igcse_grades <- Igcse %>% select(mcsid, row_id,  subject_name, subject_grade)
Igcse_grades$subject_name = as.character(Igcse_grades$subject_name)
Igcse_grades$subject_grade = as.numeric(Igcse_grades$subject_grade)

#identify those who have duplicate subjects based on wide data
#duplicated_wide = wide_grades$mcsid[duplicated(wide_grades$mcsid)]
#remove duplicate subjects and keep the subject with the higher grade
Igcse_grades1 <- Igcse_grades %>% group_by(mcsid) %>% 
  arrange(desc(subject_grade), .by_group = TRUE) %>%  #arrange subject_grade by descending order, so distinct can select the higher entry 
  distinct(subject_name, .keep_all = TRUE) #this will give only one of the duplicated values - because have sorted subject grade in descending order, this will keep the higher grade for the duplicated subject



#convert to wide dataset where each column name is a subject with the grade as the entry per CM
wide_IGCSE_grades = Igcse_grades1 %>% select(!row_id)
wide_IGCSE_grades = wide_IGCSE_grades %>%
  group_by(mcsid, subject_name) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = subject_name, values_from = subject_grade) %>% 
  select(-row)

core_iGCSE_subjects_grades <- wide_IGCSE_grades %>% select(mcsid, c("Biology", "Chemistry", "Physics", "Science"), #include additional mathematics?
                                                           contains("english"), contains("math"))

names(core_iGCSE_subjects_grades) <- c("mcsid", "biology_I", "chemistry_I", "physics_I", "science_I", "english_first_lang_I", "english_lit_I", "maths_I")

#add in BTEC science at level 2 (equiv to a pass at GCSE)

btec_subject_key <- c(`1`	= "3D Design",
                      `2`	= "Achieving Excellence in Skills Performance",
                      `3`	= "Administration",
                      `4`	= "Adult Care",
                      `6`	= "Advertisement Production for the Electronic and the Printed Page",
                      `7`	= "Aeronautical Engineering",
                      `8`	= "Aerospace and Aviation Engineering",
                      `9`	= "Aerospace and Aviation Engineering",
                      `13`	= "Airline and Airport Operations",
                      `14`	= "Ambulance Care Assistance",
                      `19`	= "Anti-Armour Weapon Systems",
                      `20`	= "Apparel, Footwear, Leather or Textile Production",
                      `23`	= "Applied Science",
                      `24`	= "Aromatherapy Massage",
                      `28`	= "Assisting and Moving Individuals for Social Care Settings",
                      `29`	= "Auto Electrical and Mobile Electrical Principles",
                      `32`	= "Awareness of Dementia",
                      `33`	= "Awareness of End of Life Care",
                      `34`	= "Barista Skills",
                      `37`	= "Beauty Therapy Sciences",
                      `39`	= "Body Building Principles",
                      `40`	= "Body Massage",
                      `42`	= "Business",
                      `49`	= "Call Handling Operations",
                      `50`	= "Caring",
                      `53`	= "Carry and Deliver Goods",
                      `54`	= "CCTV Operations (Public Space Surveillance)",
                      `60`	= "Cleaning Principles",
                      `61`	= "Cleaning Principles (Confined Spaces)",
                      `62`	= "Cleaning Principles (Deep Cleaning of Internal Equipment Surfaces and Areas)",
                      `63`	= "Cleaning Principles (External Surfaces and Areas)",
                      `64`	= "Cleaning Principles (Food Areas)",
                      `65`	= "Cleaning Principles (Glazed Surfaces and Facades)",
                      `66`	= "Cleaning Principles (High Risk Areas; Controlled Environments)",
                      `67`	= "Cleaning Principles (Interiors and Washrooms)",
                      `68`	= "Cleaning Principles (Maintenance and Minor Repairs of Property)",
                      `69`	= "Cleaning Principles (Manual Street Cleaning)",
                      `70`	= "Cleaning Principles (Mechanical Street Cleaning)",
                      `71`	= "Cleaning Principles (Passenger Transport Interiors)",
                      `72`	= "Cleaning Principles (Periodic Cleaning of Hard and Semi-hard Floors)",
                      `73`	= "Cleaning Principles (Periodic Cleaning of Soft Floors and Furnishings)",
                      `74`	= "Cleaning Principles (Specialist Electronic Equipment)",
                      `75`	= "Cleaning Principles (Water-Fed Pole Systems)",
                      `76`	= "Cleaning Principles (Working Safely at Heights)",
                      `77`	= "Close Protection Operations",
                      `78`	= "Coastal Zone Management",
                      `80`	= "Communications Electronic Engineering",
                      `81`	= "Communications Technology",
                      `82`	= "Community Safety for Accredited Persons",
                      `88`	= "Computing for Creative Industries",
                      `89`	= "Conflict Management",
                      `91`	= "Construction",
                      `94`	= "Construction Plant Operations",
                      `96`	= "Contact Centre Operations",
                      `97`	= "Contact Centre Skills",
                      `98`	= "Control and Administration of Medicines",
                      `99`	= "Counselling Skills",
                      `100`	= "Countryside Access and Recreation",
                      `102`	= "Countryside Management",
                      `103`	= "Countryside Studies",
                      `104`	= "Craft Cuisine Skills",
                      `109`	= "Customer Contact",
                      `110`	= "Customer Service",
                      `111`	= "Customer Service Operations",
                      `112`	= "Dementia Care",
                      `113`	= "Dental Technology",
                      `117`	= "Developing An Entrepreneurial Approach",
                      `118`	= "Diagnosing Faults in Land-based Machines",
                      `119`	= "Digital Audio Editing",
                      `121`	= "Digital Content Production",
                      `122`	= "Digital Film and Video Production",
                      `123`	= "Digital Games Design and Development",
                      `124`	= "Digital Games Production",
                      `125`	= "Digital Literacy Skills (Entry 1)",
                      `128`	= "Digital Publishing",
                      `130`	= "Digital Tools and Technologies",
                      `131`	= "Digital Video Editing",
                      `132`	= "Disengagement and Non-restrictive Physical Intervention Skills",
                      `133`	= "Disengagement and Physical Intervention Skills",
                      `134`	= "Door Supervision",
                      `135`	= "Drugs Awareness in the Licensed Retail Sector",
                      `136`	= "Early Years",
                      `139`	= "e-business",
                      `140`	= "Ecological Surveys and Techniques",
                      `145`	= "Employment Awareness in Active Leisure and Learning",
                      `146`	= "Employment Responsibilities and Rights in Health, Social Care and Children and Young People s Settings",
                      `147`	= "Engineering",
                      `152`	= "Engineering Electronics and Computer Control Technologies",
                      `155`	= "Enterprise and Entrepreneurship",
                      `158`	= "Environmental Sustainability",
                      `159`	= "Equine Management",
                      `160`	= "Equine Management (Equitation)",
                      `161`	= "Equine Management (Yard Management)",
                      `163`	= "e-Responsibility for Education",
                      `166`	= "Essential Digital Literacy Skills",
                      `167`	= "Essential Employability Skills",
                      `169`	= "Event Support",
                      `171`	= "Explore Complementary Therapies",
                      `172`	= "Exploring the Caring Sectors",
                      `173`	= "Exploring the Construction and Engineering Sectors",
                      `175`	= "Facilities Services Principles",
                      `178`	= "Film and Television Production",
                      `179`	= "Film and Television Visual Effects",
                      `180`	= "Fine Art",
                      `182`	= "Fish Husbandry",
                      `184`	= "Fitness Instructing",
                      `185`	= "Fitness Services",
                      `186`	= "Floristry",
                      `187`	= "Food and Beverage Service",
                      `189`	= "Food Safety for Logistics",
                      `190`	= "Food Safety for Retail",
                      `194`	= "Forensic and Criminal Investigation",
                      `195`	= "Forensic Investigation",
                      `196`	= "Forestry and Arboriculture",
                      `197`	= "Foster Care",
                      `199`	= "Front Line Services",
                      `200`	= "Front of House Operations",
                      `201`	= "General Beverage Service Skills",
                      `204`	= "General Front Office Operations",
                      `205`	= "General Housekeeping Operations",
                      `208`	= "HACCP Based Food Safety Systems in Manufacturing",
                      `210`	= "Hairdressing Services",
                      `212`	= "Health and Safety in a Construction Environment",
                      `214`	= "Health and Social Care",
                      `217`	= "Healthcare Science",
                      `219`	= "Heavy Machine Gun Operations",
                      `220`	= "Heavy Vehicle Maintenance and Repair Principles",
                      `222`	= "Horse Care",
                      `223`	= "Horse Management",
                      `226`	= "Hospitality and Catering Principles (Beverage Service)",
                      `228`	= "Hospitality and Catering Principles (Food Production and Cooking)",
                      `232`	= "Hospitality and Catering Principles (Housekeeping)",
                      `233`	= "Hospitality and Catering Principles (Kitchen Services)",
                      `234`	= "Hospitality and Catering Principles (Professional Cookery)",
                      `235`	= "Hospitality and Tourism",
                      `236`	= "Hospitality Skills",
                      `237`	= "Hospitality Supervision",
                      `239`	= "Housing Practice",
                      `240`	= "Human Resource Practices in Hospitality",
                      `243`	= "ICT Systems and Principles",
                      `245`	= "Improving Performance for Manufacturing Engineering Operations",
                      `246`	= "Indian Head Massage",
                      `247`	= "Induction into Adult Social Care in Northern Ireland",
                      `248`	= "Induction to Supporting People who have Learning Disabilities",
                      `249`	= "Infection Control",
                      `250`	= "Infection Prevention and Control",
                      `251`	= "Information and Creative Technology",
                      `252`	= "Information Technology",
                      `253`	= "Information Technology (Specialist)",
                      `254`	= "Instructional Techniques",
                      `256`	= "Interactive Use of Media",
                      `257`	= "Intermediate Overseas Resort Operations",
                      `258`	= "Introducing Team Leading",
                      `259`	= "Introduction to Administering Examinations",
                      `260`	= "Introduction to Cabin Crew",
                      `261`	= "Introduction to Childcare",
                      `262`	= "Introduction to Contact Centres",
                      `263`	= "Introduction to Counselling Skills",
                      `264`	= "Introduction to Culinary Skills",
                      `266`	= "Introduction to Healthcare Science",
                      `267`	= "Introduction to Life Coaching Skills",
                      `268`	= "Introduction to Professional Cookery",
                      `270`	= "Introduction to Professional Food and Beverage Service Skills",
                      `271`	= "Introduction to the Hospitality Industry",
                      `272`	= "Introduction to the Role of the Professional Taxi and Private Hire Driver",
                      `274`	= "Investigating the Hospitality Industry",
                      `275`	= "IT",
                      `276`	= "IT at Work",
                      `277`	= "IT Practitioners (Software Development)",
                      `278`	= "IT Support",
                      `280`	= "Knowledge for a Professional Bus or Coach Driver",
                      `281`	= "Knowledge of Court/Tribunal Administration",
                      `282`	= "Knowledge of Custodial Care",
                      `283`	= "Knowledge of Providing Security Services",
                      `284`	= "Laboratory Science",
                      `285`	= "Land and Environment",
                      `286`	= "Land-based Studies",
                      `288`	= "Laundry and Dry Cleaning Technology",
                      `289`	= "Law and Legal Work",
                      `290`	= "Leadership Skills",
                      `292`	= "Lean Organisation Management Techniques",
                      `294`	= "Legal Secretaries",
                      `295`	= "Leisure Operations",
                      `297`	= "Lift Truck Maintenance and Repair Principles",
                      `299`	= "Lingerie fitting",
                      `300`	= "Lloyds and London Market Insurance",
                      `301`	= "Logistics",
                      `302`	= "Management (Supervision and Leadership)",
                      `303`	= "Managing Business Performance in Hospitality",
                      `304`	= "Managing Examinations",
                      `306`	= "Marketing for Hospitality",
                      `311`	= "Medical Administration",
                      `312`	= "Meeting the Requirements of Customers with Specific Needs in Hospitality, Leisure, Travel and Tourism",
                      `313`	= "Military Engineering",
                      `317`	= "Music",
                      `322`	= "Nutrition Awareness",
                      `324`	= "Operations and Maintenance Engineering",
                      `325`	= "Paediatric First Aid",
                      `328`	= "Performing Arts",
                      `330`	= "Performing Arts (Dance)",
                      `332`	= "Personal Safety Awareness",
                      `333`	= "Pharmaceutical Science",
                      `334`	= "Pharmacy Services",
                      `337`	= "Plant Maintenance",
                      `338`	= "Playwork",
                      `339`	= "Playwork for Early Years and Child Care Workers",
                      `340`	= "Polymer Processing and Materials Technology",
                      `342`	= "Preparation and Operation of a Tractor",
                      `343`	= "Preparation for Air Cabin Crew Service",
                      `344`	= "Preparation for Tourist Guiding",
                      `345`	= "Preparing for a Career in the Hospitality Industry",
                      `347`	= "Preparing to Work in Adult Social Care",
                      `348`	= "Prevention and Control of Infection",
                      `349`	= "Principles and Practice of Care",
                      `350`	= "Principles for Carrying and Delivering Goods by Road",
                      `353`	= "Principles of Bus and Coach Engineering and Maintenance (Electrical)",
                      `354`	= "Principles of Bus and Coach Engineering and Maintenance (Mechanical)",
                      `355`	= "Principles of Business Administration",
                      `357`	= "Principles of Customer Service",
                      `358`	= "Principles of Customer Service in Hospitality, Leisure, Travel and Tourism",
                      `359`	= "Principles of Funeral Operations and Services",
                      `360`	= "Principles of Holland & Barrett Vitamins, Minerals, Supplements and Health Products",
                      `361`	= "Principles of Marketing",
                      `362`	= "Principles of Providing Security Services",
                      `363`	= "Principles of Sales",
                      `364`	= "Principles of Team Leading",
                      `365`	= "Principles of the Creative and Cultural Sector",
                      `366`	= "Principles of the Cultural Heritage Sector",
                      `367`	= "Principles of Trade Business Services",
                      `368`	= "Principles of Working in Business Administration",
                      `369`	= "Principles of Working in Cleaning",
                      `370`	= "Principles of Working in Customer Service",
                      `371`	= "Principles of Working in Facilities Services",
                      `373`	= "Principles of Working in Hospitality and Catering (Hospitality Services)",
                      `374`	= "Principles of Working in Trade Business Services",
                      `375`	= "Principles of Working in Warehousing and Storage",
                      `380`	= "Professional Cookery for Professional Chefs (Kitchen and Larder)",
                      `381`	= "Professional Cookery for Professional Chefs (Patisserie and Confectionery)",
                      `382`	= "Professional Development for Working in Childrens Services",
                      `383`	= "Professional Food and Beverage Service",
                      `384`	= "Professional Patisserie and Confectionery",
                      `387`	= "Providing Financial Advice",
                      `388`	= "Public Sector Practice",
                      `390`	= "Recreation Assistants",
                      `391`	= "Recruitment Resourcing",
                      `392`	= "Retail",
                      `393`	= "Retail Beauty Consultancy",
                      `394`	= "Retail Knowledge",
                      `395`	= "Retail Knowledge (Beauty)",
                      `396`	= "Retail Operations",
                      `397`	= "Retailing",
                      `400`	= "Sailing and Watersports",
                      `401`	= "Sales",
                      `402`	= "Sales and Marketing",
                      `404`	= "Security Guarding",
                      `405`	= "Security Keyholding and Alarm Response",
                      `406`	= "Security Operations",
                      `407`	= "Setting up a Business Online",
                      `408`	= "Signing with Babies and Young Children",
                      `411`	= "Social Media",
                      `412`	= "Social Networking for Business",
                      `414`	= "Sound Engineering",
                      `415`	= "Sound Production",
                      `416`	= "Specialised Play for Sick Children and Young People",
                      `417`	= "Sport",
                      `424`	= "Staff Training and Development for Hospitality",
                      `425`	= "STEM",
                      `427`	= "Subsidiary Design",
                      `428`	= "Subsidiary Sailing and Watersports",
                      `429`	= "Supply Chain Management Principles",
                      `430`	= "Support for Deaf Learners",
                      `431`	= "Support Work in Social Care",
                      `432`	= "Supporting Activities and Events for Sport and Active Leisure",
                      `433`	= "Supporting Activity Provision in Social Care",
                      `434`	= "Supporting Employability and Personal Effectiveness",
                      `435`	= "Supporting Individuals with Learning Disabilities",
                      `436`	= "Supporting Learning in the Workplace",
                      `437`	= "Supporting Teaching and Learning in Schools",
                      `438`	= "Sustainability Skills",
                      `439`	= "Teaching Assistants",
                      `440`	= "Teaching Employability Skills",
                      `441`	= "Teaching Employability Skills and Vocational Learning",
                      `447`	= "Telecommunications",
                      `449`	= "Thermal Insulation",
                      `450`	= "Tourism Events Assistant",
                      `451`	= "Trade Business Services",
                      `453`	= "Transporting Passengers by Bus and Coach",
                      `454`	= "Transporting Passengers by Taxi and Private Hire",
                      `455`	= "Travel and Tourism",
                      `457`	= "Travel Operations",
                      `458`	= "Travel Services",
                      `459`	= "Underage Sales Prevention",
                      `460`	= "Understanding An Entrepreneurial Approach",
                      `462`	= "Understanding Enterprise and Entrepreneurship",
                      `463`	= "Understanding Stewarding at Spectator Events",
                      `464`	= "Understanding the Laws of Sport",
                      `465`	= "Understanding the Safe Use of Medicines",
                      `467`	= "Using and Promoting Multi-Channel Retailing in Store",
                      `468`	= "Vehicle Accident Repair Body Principles",
                      `470`	= "Vehicle Accident Repair Paint Principles",
                      `471`	= "Vehicle Fitting Principles",
                      `472`	= "Vehicle Immobilisation",
                      `473`	= "Vehicle Repair and Technology",
                      `474`	= "Vehicle Sales Principles",
                      `475`	= "Vehicle Service and Technology",
                      `477`	= "Video Journalism",
                      `480`	= "Warehouse Operatives",
                      `481`	= "Warehousing and Storage Principles",
                      `483`	= "Welcoming Tourists and Visitors to their Destination in Hospitality, Leisure, Travel and Tourism",
                      `484`	= "Work Experience for Enterprise",
                      `486`	= "Working in the Health Sector",
                      `489`	= "Animal Care and Veterinary Science",
                      `490`	= "Art, Design and Media",
                      `493`	= "Drama, Theatre Studies and Performing Arts",
                      `494`	= "Economics",
                      `495`	= "Education and Training",
                      `496`	= "Energy and Utilities",
                      `498`	= "Environmental Conservation",
                      `501`	= "Funeral Services",
                      `503`	= "Geography",
                      `504`	= "Government and Politics",
                      `507`	= "Horticulture and Forestry",
                      `510`	= "Manufacturing Technologies",
                      `512`	= "Medicine",
                      `513`	= "Dentistry",
                      `514`	= "Physical Education and Sport",
                      `516`	= "Property and Housing",
                      `518`	= "Retail and Wholesaling",
                      `519`	= "Science ",
                      `520`	= "Social Services",
                      `521`	= "Textiles and Apparel Manufacturing",
                      `522`	= "Transportation, Operations and Maintenance",
                      `523`	= "Warehousing and Distribution",
                      `524`	= "Other",
                      `526`	= "Recoded due to low counts - check SA")

btec = qualifications1 %>%  filter(gc_s_qual_btec == 1) #those who said yes to having BTECs
btec = qualifications1[qualifications1$mcsid %in% btec$mcsid,]
btec_subjects = btec %>%  select(mcsid, gcnum00, gc_rowid, gc_s_qual_btcn_rec,
                                 gc_l_btec_name_r30,gc_l_btlv, gc_l_btgd)
names(btec_subjects)<- c("mcsid", "cm_number", "row_number", "btec_number","subject_name", "level", "grade")
btec_subjects$subject_name = as.factor(btec_subjects$subject_name)
btec_subjects$subject_name =  recode(btec_subjects$subject_name, !!!btec_subject_key)
btec_subjects = btec_subjects %>%  filter(!is.na(subject_name)) #remove the NA subject name as due to long format, each cm has 20 rows and alot will be NA

#want Science and Applied science, where level = 3 in data (this is level 2)

#select science subjects
btec_science = btec_subjects %>% filter(subject_name == "Science " | subject_name ==  "Applied Science")
#select level == 3 (level 2 btec)
btec_science = btec_science %>% filter(level == 3)
btec_science = btec_science %>% select(mcsid, subject_name, level)

#make wide so will fit with other variables when creating binary variable.
wide_btec_science = btec_science %>%
  group_by(mcsid, subject_name) %>%
  tidyr::pivot_wider(names_from = subject_name, values_from = level) 
names(wide_btec_science) <- c("mcsid", "science_btec", "applied_science_btec")


#add in those who say have none of these subjects - for those who live in england, wales or NI (will do scotland separately)
#country = 1, 2 or 4. country !=3 (ie not scotland)
no_quals = qualifications1 %>% filter(gc_s_qual_none== 1) # be careful here as for rest of rows per cm, this will be NA.
no_quals = no_quals %>% select(mcsid, gcnum00, gc_rowid, gc_s_qual_none) #get relevant variables 
#add country 
no_quals = merge(no_quals, country, by = "mcsid")
no_quals = no_quals %>% filter(country !=3)
no_quals = no_quals %>% select(mcsid, gc_s_qual_none)
names(no_quals) = c("mcsid", "no_quals")

#btec only. this will = 1 only if all other quals reported are answeered no (2)
btec_only = qualifications1 %>% filter(gc_s_qual_btec== 1)
btec_only = btec_only %>% select(mcsid, gc_s_qual_btec, gc_s_qual_five, gc_s_qual_gcse, gc_s_qual_igcs)
btec_only = merge(btec_only, country, by = "mcsid")
btec_only = btec_only %>%  filter(country !=3)
btec_only = btec_only %>% mutate(btec_only1 = case_when((gc_s_qual_btec == 1) & (gc_s_qual_five == 1) | (gc_s_qual_gcse == 1)  |(gc_s_qual_igcs == 1) ~  2, 
                                                        (gc_s_qual_btec == 1) |(gc_s_qual_five == 2) | (gc_s_qual_gcse == 2) |(gc_s_qual_igcs == 2) ~ 1))
btec_only = btec_only %>% filter(btec_only1 == 1)
btec_only = btec_only %>% select(mcsid,btec_only1)


#combine core grades together into one dataframe (for GCSE and iGCSE)
combined_core_grades = merge(all=TRUE, core_subjects_grades, core_iGCSE_subjects_grades, by = "mcsid")
combined_core_grades = merge(all=TRUE, combined_core_grades, no_quals, by="mcsid")
combined_core_grades = merge(all=TRUE, combined_core_grades, btec_only, by="mcsid")
combined_core_grades = merge(all=TRUE, combined_core_grades, wide_btec_science, by = "mcsid")


#create binary variable. those who score >=4 on at least an english subject, at least one maths subject and at least 1 science subject = 1. else = 0. 
core_grades <- combined_core_grades %>%  mutate(benchmark = case_when(
  (english >= 4  | english_lang >= 4  | english_lit >= 4 | english_first_lang_I >= 4 |english_lit_I >= 4 ) & 
    (maths >= 4 | maths_linear >= 4 | maths_numeracy >= 4 | further_maths >= 4 | additional_maths >= 4 | maths_I >= 4) &
    (biology >= 4 | chemistry >= 4 | physics >= 4 | additional_science >= 4 | science >= 4 | applied_science >= 4 |
       combined_science >= 4 | modular_science >= 4 | further_additional_science >= 4 |computer_science >=4 |
       additional_applied_science >= 4 | additional_science_modular >= 4 |human_biology >= 4 | 
       biology_I >= 4 | chemistry_I >= 4 | physics_I >= 4 | science_I >= 4 | science_btec == 3 | applied_science_btec == 3) ~ 1,
  TRUE ~ 0))

#get counts
benchmark = table(core_grades$benchmark)
prop.table(benchmark)*100

#NATIONAL 5 - SCOTALND. GRADE C OR ABOVE. 

n5 = qualifications1 %>% filter(gc_s_qual_five == 1)
n5 = qualifications1[qualifications1$mcsid %in% n5$mcsid,] 



n5 = n5 %>% select(mcsid, gcnum00, gc_rowid,gc_s_qual_nfir_r20, gc_l_fvsb_name_r30, gc_l_fvgd) 
names(n5) <- c("mcsid", "cm_number", "row_id", "total_quals", "subject_name", "subject_grade")

n5_subject_key = c(`3`	 = "Design and Manufacture",
                   `4`	 = "Drama",
                   `7`	 = "Energy (Skills for Work)",
                   `8`	 = "Engineering Science",
                   `10`	 = "Language: English",
                   `12`	 = "ESOL (English for Speakers of Other Languages)",
                   `14`	 = "Financial Services (Skills for Work)",
                   `15`	 = "Food and Drink Manufcturing Industry (Skills for Work)",
                   `16`	 = "Language: French",
                   `19`	 = "Geography",
                   `20`	 = "Language: German",
                   `21`	 = "Graphic Communication",
                   `25`	 = "History",
                   `28`	 = "Hospitality:Practical Cookery",
                   `34`	 = "Martime Skills (Skills for Work)",
                   `35`	 = "Mathematics",
                   `37`	 = "Modern Studies",
                   `38`	 = "Music",
                   `41`	 = "Physical Education",
                   `42`	 = "Physics",
                   `45`	 = "Practical Woodworking",
                   `47`	 = "Religious, Moral and Philosophical Studies",
                   `50`	 = "Language: Spanish",
                   `52`	 = "Textiles Industry  (Skills for Work)",
                   `56`	 = "Art and Design",
                   `57`	 = "Biology",
                   `58`	 = "Business Management",
                   `59`	 = "Language: Cantonese",
                   `60`	 = "Care",
                   `61`	 = "Chemistry",
                   `63`	 = "Computing Science",
                   `64`	 = "Other",
                   `65`	 = "Recoded due to low counts - check SA")
#-9	 = Refusal
#-8	 = Do not know
#-1	 = Not applicable
n5$subject_name = as.factor(n5$subject_name)
n5$subject_name =  recode(n5$subject_name, !!!n5_subject_key)
n5 = n5 %>%  filter(!is.na(subject_name))
#grades
#Value = 1.0	Label = A
#Value = 2.0	Label = B
#Value = 3.0	Label = C
#Value = 4.0	Label = D
#Value = 5.0	Label = No award (fail)
#Value = -9.0	Label = Refusal
#Value = -8.0	Label = Do not know
#Value = -1.0	Label = Not applicable
#recode so that A is a higher number. 
#check what want scale of this to be  when meet!!

n5$subject_grade =  as.numeric(n5$subject_grade)
n5$subject_grade  = recode(n5$subject_grade, `1` = 5, `2` = 4, `3` = 3, `4` =2, `5` = 0)

#convert to wide format
#could put into wide format where the column names are subject name and column values are subject grade? 
n5_grades <- n5 %>% select(mcsid, row_id,  subject_name, subject_grade)
n5_grades$subject_name = as.character(n5_grades$subject_name)
n5_grades$subject_grade = as.numeric(n5_grades$subject_grade)
#wide_n5_grades = pivot_wider(n5_grades, id_cols=c("mcsid"), names_from = subject_name,
#  values_from = subject_grade, values_fill = 0)


#identify those who have duplicate subjects based on wide data
#duplicated_wide = wide_grades$mcsid[duplicated(wide_grades$mcsid)]
#remove duplicate subjects and keep the subject with the higher grade
n5_grades1 <- n5_grades %>% group_by(mcsid) %>% 
  arrange(desc(subject_grade), .by_group = TRUE) %>%  #arrange subject_grade by descending order, so distinct can select the higher entry 
  distinct(subject_name, .keep_all = TRUE) #this will give only one of the duplicated values - because have sorted subject grade in descending order, this will keep the higher grade for the duplicated subject


#convert to wide dataset where each column name is a subject with the grade as the entry per CM
wide_n5_grades = n5_grades1 %>% select(!row_id)
wide_n5_grades = wide_n5_grades %>%
  group_by(mcsid, subject_name) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = subject_name, values_from = subject_grade) %>% 
  select(-row)

core_n5_grades <- wide_n5_grades %>% select(mcsid, c("Language: English",  contains("math"), "Computing Science", "Biology", "Chemistry", "Physics"))
names(core_n5_grades) <- c("mcsid", "english", "maths","computer_science", "biology", "chemistry", "physics")

#those who have only n4 quals = will be 0 in binary variable (dont have core subjects grade 4 and above)
n4_only = qualifications1 %>% filter(gc_s_qual_four == 1 & gc_s_qual_five == 2) #n4 = yes, n5 = no
#n4_only = n4_only %>% select(mcsid, gc_s_qual_four, gc_s_qual_five)
#n4_only = n4_only %>% filter(gc_s_qual_four == 1)
#n4_only = qualifications1[qualifications1$mcsid %in% n4_only$mcsid,] 
#n4_only = n4_only %>% mutate(national_4_only = case_when(gc_s_qual_four == 1 & 
#                gc_s_qual_five == 2 ~ 1, 
#        TRUE ~ 0 ))

n4_only = n4_only %>% select(mcsid, gc_s_qual_four)


#add in those who say have none of these subjects - for those who live in Scotland)
#country = 3 
no_quals_scotland = qualifications1 %>% filter(gc_s_qual_none== 1) # be careful here as for rest of rows per cm, this will be NA.
no_quals_scotland = no_quals_scotland %>% select(mcsid, gcnum00, gc_rowid, gc_s_qual_none) #get relevant variables 
#add country 
no_quals_scotland = merge(no_quals_scotland, country, by = "mcsid")
no_quals_scotland = no_quals_scotland %>% filter(country ==3)
no_quals_scotland = no_quals_scotland %>% select(mcsid, gc_s_qual_none)
names(no_quals_scotland) = c("mcsid", "no_quals_scotland")

#btec only scotland. this will = 1 only if all other quals reported are answeered no (2)
btec_only_scotland = qualifications1 %>% filter(gc_s_qual_btec== 1)
btec_only_scotland  = btec_only_scotland  %>% select(mcsid, gc_s_qual_btec, gc_s_qual_five, gc_s_qual_gcse, gc_s_qual_igcs)
btec_only_scotland  = merge(btec_only_scotland , country, by = "mcsid")
btec_only_scotland  = btec_only_scotland  %>%  filter(country ==3)
btec_only_scotland  = btec_only_scotland  %>% mutate(btec_only1_scotland = case_when((gc_s_qual_btec == 1) & (gc_s_qual_five == 1) | (gc_s_qual_gcse == 1)  |(gc_s_qual_igcs == 1) ~  2, (gc_s_qual_btec == 1) |(gc_s_qual_five == 2) | (gc_s_qual_gcse == 2) |(gc_s_qual_igcs == 2) ~ 1))
btec_only_scotland  = btec_only_scotland  %>% filter(btec_only1_scotland == 1)
btec_only_scotland  = btec_only_scotland  %>% select(mcsid,btec_only1_scotland)

core_grades_scotland = merge(all = TRUE, core_n5_grades, n4_only, by = "mcsid")
core_grades_scotland = merge(all = TRUE, core_grades_scotland, no_quals_scotland, by = "mcsid")
core_grades_scotland = merge(all = TRUE, core_grades_scotland, btec_only_scotland, by = "mcsid")

#grade c or above (coded as 3 in N5 data)
core_grades_n5 <-  core_grades_scotland %>%  mutate(benchmarkN5 = case_when((english >= 3 )  & 
                                                                              (maths >= 3 )  &
                                                                              (computer_science >=3 |
                                                                                 biology >= 3 | chemistry >= 3 | physics >= 3 ) ~ 1,
                                                                            TRUE ~ 0))

#combine N5 and GCSE core grades into one variable
#first combine the dataframes
#replace is/na with the other one 
core_grades_binary= merge (all=TRUE, core_grades, core_grades_n5, by="mcsid")
core_grades_binary = core_grades_binary %>% select(mcsid, benchmark, benchmarkN5)



core_grades_binary= core_grades_binary %>% mutate(benchmark_binary = 
                                                    case_when(benchmark == 0 & benchmarkN5 == 1 ~  1, 
                                                              benchmark == 1 & benchmarkN5 == 1 ~ 1,
                                                              benchmark == 0 & benchmarkN5 == 0 ~ 0,
                                                              benchmark == 1 & benchmarkN5 == 0 ~ 1,
                                                              is.na(benchmarkN5) ~ benchmark, 
                                                              is.na(benchmark) ~ benchmarkN5, 
                                                              TRUE ~ 0)) %>% 
  select(mcsid, benchmark_binary)


benchmark_binary = table(core_grades_binary$benchmark_binary)
prop.table(benchmark_binary)*100

#CONTINOUS OUTCOME VARIABLE. ####
#merge together core subejcts for GCSE and iGCSE
core_subjects_continuous = merge(all=TRUE, core_subjects_grades, core_iGCSE_subjects_grades, by = "mcsid")
#create english data, maths data, science data. 
#english 
english_subjects_gcse = core_subjects_continuous %>% select(mcsid, contains("english")) %>% 
  mutate (english_score =
            rowMeans(.[-1], na.rm = TRUE), .after = 1) #calculate means across rows excluding row 1 (mcsid)
#convert NaN to NA
english_subjects_gcse$english_score[is.nan(english_subjects_gcse$english_score)]<-NA
#maths                                                                              
maths_subjects_gcse = core_subjects_continuous %>% select(mcsid, contains("math")) %>% 
  mutate (maths_score = 
            rowMeans(.[-1], na.rm = TRUE), .after = 1) #calculate means across rows excluding row 1 (mcsid)
#convert NaN to NA
maths_subjects_gcse$maths_score[is.nan(maths_subjects_gcse$maths_score)]<-NA

#science
science_subjects_gcse = core_subjects_continuous %>% select(mcsid, contains("biology"), contains("chemistry"), 
                                                            contains("physics"), contains("science")) %>% 
  mutate (science_score = 
            rowMeans(.[-1], na.rm = TRUE), .after = 1) #calculate means across rows excluding row 1 (mcsid)
#convert NaN to NA
science_subjects_gcse$science_score[is.nan(science_subjects_gcse$science_score)]<-NA

#pull out english score, maths score and science score, and mcsid

english_score = english_subjects_gcse %>% select(mcsid, english_score)
maths_score = maths_subjects_gcse %>% select(mcsid, maths_score)
science_score = science_subjects_gcse %>% select(mcsid, science_score)

#combine english, maths and science score together
core_subjects_score = merge(all=TRUE, english_score, maths_score, by = "mcsid")
core_subjects_score = merge(all=TRUE, core_subjects_score, science_score, by = "mcsid")

#get mean of these scores. need to have a response for english, maths and science score
core_subjects_score = core_subjects_score %>% mutate(average_grade = rowMeans(.[-1]), 
                                                     .after = 1) %>% 
  select(mcsid, average_grade)
core_subjects_score$average_grade = round(core_subjects_score$average_grade, 2)

#N5 CONTINOUS VARIABLE
n5_english_score = as.data.frame(core_n5_grades %>% select(mcsid, english))
n5_maths_score = as.data.frame(core_n5_grades %>% select(mcsid, maths))
n5_science_subjects = as.data.frame(core_n5_grades %>% select(mcsid, computer_science, biology, chemistry, physics)) %>% 
  mutate (science_score = rowMeans(.[-1], na.rm = TRUE), .after = 1)
#convert NaN to NA
n5_science_subjects$science_score[is.nan(n5_science_subjects$science_score)]<-NA
n5_science_subjects$science_score = round(n5_science_subjects$science_score, 2)
n5_science_score = n5_science_subjects %>% select(mcsid, science_score)

n5_core_subjects_score = merge(all=TRUE, n5_english_score, n5_maths_score, by="mcsid")
n5_core_subjects_score = merge(all= TRUE, n5_core_subjects_score, n5_science_score, by="mcsid")

#get mean of these scores. need to have a response for english, maths and science score
n5_core_subjects_score = n5_core_subjects_score %>% mutate(average_grade_n5 = rowMeans(.[-1]), 
                                                           .after = 1) %>% 
  select(mcsid, average_grade_n5)
n5_core_subjects_score$average_grade_n5 = round(n5_core_subjects_score$average_grade_n5, 2)

education_main_outcomes = merge(all=TRUE, core_grades_binary, core_subjects_score, by = "mcsid")
education_main_outcomes = merge(all=TRUE, education_main_outcomes, n5_core_subjects_score, by="mcsid")

#convert gcse and n5 score into z scores and then combine into one variable
education_main_outcomes = education_main_outcomes %>% 
  mutate(standardised_gcse = scale(average_grade, 
                                   center = TRUE, scale = TRUE)) %>% #standardised within the relevant population
  mutate(standardised_n5 =  scale(average_grade_n5, 
                                  center = TRUE, scale = TRUE)) %>% 
  mutate(standardised_core_subjects = case_when(!is.na(standardised_gcse)~standardised_gcse, 
                                                is.na(standardised_gcse) ~ standardised_n5, 
                                                !is.na(standardised_n5) ~standardised_n5,
                                                is.na(standardised_n5) ~ standardised_gcse)) %>% 
  select(mcsid, benchmark_binary, standardised_core_subjects)

#continous score version 2 - average of highest grade for each subject ####

#english_subjects_highest = english_subjects_gcse %>% select(!english_score) %>% 
  #mutate(highest_english = pmax(english,english_lang,english_lit,
                                #english_first_lang_I,english_lit_I, na.rm= TRUE), .after = 1) #get this to give the highest per row without having to name each column??

english_cols = english_subjects_gcse %>% select(english:english_lit_I) %>% names()

english_subjects_highest = english_subjects_gcse %>% select(!english_score) %>% 
  mutate(highest_english = pmax(!!!rlang::syms(english_cols), na.rm= TRUE), .after = 1) %>% 
  select(mcsid, highest_english)

maths_cols = maths_subjects_gcse %>% select(maths:maths_I) %>% names()
maths_subjects_highest = maths_subjects_gcse %>% select(!maths_score) %>% 
  mutate(highest_maths = pmax(!!!rlang::syms(maths_cols), na.rm= TRUE), .after = 1) %>% 
  select(mcsid, highest_maths)

science_cols = science_subjects_gcse %>% select(biology:science_I) %>% names()
science_subjects_highest = science_subjects_gcse %>% select(!science_score) %>% 
  mutate(highest_science = pmax(!!!rlang::syms(science_cols), na.rm= TRUE), .after = 1) %>% 
  select(mcsid, highest_science)

highest_average_grade = merge(all=TRUE, english_subjects_highest, maths_subjects_highest, by="mcsid")
highest_average_grade = merge(all=TRUE, highest_average_grade, science_subjects_highest, by="mcsid") %>% 
  mutate(highest_grade = rowMeans(.[-1]), 
         .after = 1)

#VARIABLES FOR IMPUTATION/ANALYSIS - COMBINE INTO ONE DATAFRAME.####
#change order of these so auxiliary and SES variables first - for order of imputation
#also add wealth and EAL when resolved these issues.
analysis_data = merge(all=TRUE, weight, sex, by = "mcsid")
analysis_data = merge(all=TRUE, analysis_data, ethnicity, by = "mcsid")
analysis_data = merge(all=TRUE, analysis_data, EAL, by = "mcsid")
analysis_data = merge(all=TRUE, analysis_data, age_atBirth, by = "mcsid")
analysis_data = merge(all=TRUE, analysis_data, tenure, by = "mcsid")
analysis_data = merge(all=TRUE, analysis_data, accommodation, by = "mcsid")
analysis_data = merge(all=TRUE, analysis_data, parent_nvq, by = "mcsid")
analysis_data = merge(all=TRUE, analysis_data, breastfed, by = "mcsid")
analysis_data = merge(all=TRUE, analysis_data, carers_in_hh, by = "mcsid")
analysis_data = merge(all=TRUE, analysis_data, income, by = "mcsid")
analysis_data = merge(all=TRUE, analysis_data, imd, by = "mcsid")
analysis_data = merge(all=TRUE, analysis_data, occupational_status, by = "mcsid")
analysis_data = merge(all=TRUE, analysis_data, wealth_variables, by = "mcsid")
analysis_data = merge(all=TRUE, analysis_data, country_17, by = "mcsid")
analysis_data = merge(all=TRUE, analysis_data, caregiver_vocabTotal, by = "mcsid")
analysis_data = merge(all=TRUE, analysis_data, age5_vocab, by = "mcsid")
analysis_data = merge(all=TRUE, analysis_data, education_main_outcomes, by = "mcsid")

#select sample - those with a response on age 5 vocabulary test OR an educaion outcome
mcs_analysis = analysis_data %>% filter((!is.na(age5_vocab)) | (!is.na(benchmark_binary)))

#save analysis data as a csv file ####
write.csv(mcs_analysis, file = "education_data.csv")

#mcs_analysis = analysis_data[!is.na(analysis_data$age5_vocab),]


# outcome variable sensitivity check - welsh included as a core variable for those in Wales. ####

#add Welsh as a core subject for GCSE

core_subjects_grades_WELSH <- wide_grades %>% select(mcsid, c("Language: English", "Language: English Language", "Language: English Literature",
                                                              "Mathematics", "Mathematics - Linear", "Mathematics - Numeracy", contains("math"), "Biology", "Chemistry", "Physics"), #include additional mathematics?
                                                     contains("science"), contains("biology"), contains("welsh")) %>% 
  merge(all=TRUE, country, by = "mcsid") 
# filter(country == 2) %>% 
#select(!country)

names(core_subjects_grades_WELSH) <- c("mcsid", "english", "english_lang", "english_lit",
                                       "maths", "maths_linear", "maths_numeracy", "further_maths", "additional_maths", "biology", "chemistry", "physics", 
                                       "additional_science", "science", "applied_science", "combined_science", "modular_science", "further_additional_science", "computer_science", 
                                       "additional_applied_science", "additional_science_modular", "human_biology", 
                                       "welsh_literature", "welsh_first_language", "welsh_second_language", "welsh", 
                                       "welsh_second_language_short", "country")


#combine core grades together into one dataframe (for GCSE and iGCSE)
combined_core_grades_WELSH = merge(all=TRUE, core_subjects_grades_WELSH, core_iGCSE_subjects_grades, by = "mcsid")
combined_core_grades_WELSH = merge(all=TRUE, combined_core_grades_WELSH, no_quals, by="mcsid")
combined_core_grades_WELSH = merge(all=TRUE, combined_core_grades_WELSH, btec_only, by="mcsid")
combined_core_grades_WELSH = merge(all=TRUE, combined_core_grades_WELSH, wide_btec_science, by = "mcsid")


#create binary variable. those who score >=4 on at least an english subject, at least one maths subject and at least 1 science subject = 1. else = 0. 
#where country == 2 (i.e. welsh, also need >=4 on at least one welsh subject. )
core_grades_WELSH <- combined_core_grades_WELSH %>% mutate(benchmark_WELSH = case_when((country == 2) & #wales
                                                                                         (english >= 4  | english_lang >= 4  | english_lit >= 4 | english_first_lang_I >= 4 |english_lit_I >= 4 ) & 
                                                                                         (maths >= 4 | maths_linear >= 4 | maths_numeracy >= 4 | further_maths >= 4 | additional_maths >= 4 | maths_I >= 4) &
                                                                                         (biology >= 4 | chemistry >= 4 | physics >= 4 | additional_science >= 4 | science >= 4 | applied_science >= 4 |
                                                                                            combined_science >= 4 | modular_science >= 4 | further_additional_science >= 4 |computer_science >=4 |
                                                                                            additional_applied_science >= 4 | additional_science_modular >= 4 |human_biology >= 4 | 
                                                                                            biology_I >= 4 | chemistry_I >= 4 | physics_I >= 4 | science_I >= 4 | science_btec == 3 | applied_science_btec == 3) &
                                                                                         (welsh_literature >=4 | welsh_first_language >= 4 | welsh_second_language >= 4 | welsh >= 4 | welsh_second_language_short >=4) ~1, 
                                                                                       (country != 2) & (english >= 4  | english_lang >= 4  | english_lit >= 4 | english_first_lang_I >= 4 |english_lit_I >= 4 ) & #not wales
                                                                                         (maths >= 4 | maths_linear >= 4 | maths_numeracy >= 4 | further_maths >= 4 | additional_maths >= 4 | maths_I >= 4) &
                                                                                         (biology >= 4 | chemistry >= 4 | physics >= 4 | additional_science >= 4 | science >= 4 | applied_science >= 4 |
                                                                                            combined_science >= 4 | modular_science >= 4 | further_additional_science >= 4 |computer_science >=4 |
                                                                                            additional_applied_science >= 4 | additional_science_modular >= 4 |human_biology >= 4 | 
                                                                                            biology_I >= 4 | chemistry_I >= 4 | physics_I >= 4 | science_I >= 4 | science_btec == 3 | applied_science_btec == 3)  ~ 1,
                                                                                       TRUE ~ 0)) 

#combine N5 and GCSE core grades into one variable
#first combine the dataframes
#replace is/na with the other one 
core_grades_binary_WELSH= merge (all=TRUE, core_grades_WELSH, core_grades_n5, by="mcsid")
core_grades_binary_WELSH = core_grades_binary_WELSH %>% select(mcsid, benchmark_WELSH, benchmarkN5) %>%  
  mutate(benchmark_binary_welsh = 
           case_when(benchmark_WELSH == 0 & benchmarkN5 == 1 ~  1, 
                     benchmark_WELSH == 1 & benchmarkN5 == 1 ~ 1,
                     benchmark_WELSH == 0 & benchmarkN5 == 0 ~ 0,
                     benchmark_WELSH == 1 & benchmarkN5 == 0 ~ 1,
                     is.na(benchmarkN5) ~ benchmark_WELSH, 
                     is.na(benchmark_WELSH) ~ benchmarkN5, 
                     TRUE ~ 0)) %>% 
  select(mcsid, benchmark_binary_welsh)

#continuous variable sensitivity check - including score for welsh grades ####


#merge together core subejcts for GCSE and iGCSE
core_subjects_continuous_WELSH = merge(all=TRUE, core_subjects_grades_WELSH, core_iGCSE_subjects_grades, by = "mcsid")
#create welsh data. 
#welsh
welsh_subjects_gcse = core_subjects_continuous_WELSH %>% select(mcsid, contains("welsh")) %>% 
  mutate (welsh_score =
            rowMeans(.[-1], na.rm = TRUE), .after = 1) #calculate means across rows excluding row 1 (mcsid)
#convert NaN to NA
welsh_subjects_gcse$welsh_score[is.nan(welsh_subjects_gcse$welsh_score)]<-NA


#pull out welsh score
welsh_score = welsh_subjects_gcse %>% select(mcsid, welsh_score)

#combine english, maths and science score together
core_subjects_score_welsh = merge(all=TRUE, english_score, maths_score, by = "mcsid")
core_subjects_score_welsh = merge(all=TRUE, core_subjects_score_welsh, science_score, by = "mcsid")
core_subjects_score_welsh = merge(all=TRUE, core_subjects_score_welsh, welsh_score, by = "mcsid")
core_subjects_score_welsh = merge(all=TRUE, core_subjects_score_welsh, country, by = "mcsid")

#get mean of these scores. need to have a response for english, maths and science score (and welsh score if welsh)

wales_core_subjects = core_subjects_score_welsh %>% filter(country==2) %>% 
  select(!country) %>% 
  mutate(average_grade_welsh = rowMeans(.[-1]), 
         .after = 1) %>% 
  select(mcsid, average_grade_welsh)

notWales_core_subjects = core_subjects_score_welsh %>% filter(country!=2) %>% 
  select(mcsid, english_score, maths_score, science_score) %>% 
  mutate(average_grade = rowMeans(.[-1]), 
         .after = 1) %>% 
  select(mcsid, average_grade)

#combine welsh and not welsh 
welsh_core_subjectsContinuous = merge(all=TRUE, wales_core_subjects, notWales_core_subjects, by = "mcsid")
welsh_core_subjectsContinuous = merge(all=TRUE, welsh_core_subjectsContinuous, country, by = "mcsid")

welsh_core_subjectsContinuous = welsh_core_subjectsContinuous %>% 
  mutate(welsh_averageScore = case_when(!is.na(average_grade) ~ average_grade, 
                                        is.na(average_grade) ~ average_grade_welsh)) %>% 
  select(mcsid, welsh_averageScore)

welsh_core_subjectsContinuous = merge(all=TRUE, welsh_core_subjectsContinuous, n5_core_subjects_score, by = "mcsid")


welsh_core_subjectsContinuous = welsh_core_subjectsContinuous  %>% 
  mutate(standardised_gcseWelsh = scale(welsh_averageScore, 
                                        center = TRUE, scale = TRUE)) %>% #standardised within the relevant population
  mutate(standardised_n5 =  scale(average_grade_n5, 
                                  center = TRUE, scale = TRUE)) %>% 
  mutate(standardised_core_subjectsWelsh = case_when(!is.na(standardised_gcseWelsh)~standardised_gcseWelsh, 
                                                     is.na(standardised_gcseWelsh) ~ standardised_n5, 
                                                     !is.na(standardised_n5) ~standardised_n5,
                                                     is.na(standardised_n5) ~ standardised_gcseWelsh)) %>% 
  select(mcsid, standardised_core_subjectsWelsh)

education_outcomesWelsh = merge(all=TRUE, core_grades_binary_WELSH, welsh_core_subjectsContinuous, by = "mcsid")

#VARIABLES FOR IMPUTATION/ANALYSIS - COMBINE INTO ONE DATAFRAME - WELSH INCLUDED SENSITIVITY CHECK.####
#change order of these so auxiliary and SES variables first - for order of imputation
#also add wealth and EAL when resolved these issues.
analysis_dataWelsh = merge(all=TRUE, weight, sex, by = "mcsid")
analysis_dataWelsh = merge(all=TRUE, analysis_dataWelsh, ethnicity, by = "mcsid")
analysis_dataWelsh = merge(all=TRUE, analysis_dataWelsh, EAL, by = "mcsid")
analysis_dataWelsh = merge(all=TRUE, analysis_dataWelsh, age_atBirth, by = "mcsid")
analysis_dataWelsh = merge(all=TRUE, analysis_dataWelsh, tenure, by = "mcsid")
analysis_dataWelsh = merge(all=TRUE, analysis_dataWelsh, accommodation, by = "mcsid")
analysis_dataWelsh = merge(all=TRUE, analysis_dataWelsh, parent_nvq, by = "mcsid")
analysis_dataWelsh = merge(all=TRUE, analysis_dataWelsh, breastfed, by = "mcsid")
analysis_dataWelsh = merge(all=TRUE, analysis_dataWelsh, carers_in_hh, by = "mcsid")
analysis_dataWelsh = merge(all=TRUE, analysis_dataWelsh, income, by = "mcsid")
analysis_dataWelsh = merge(all=TRUE, analysis_dataWelsh, imd, by = "mcsid")
analysis_dataWelsh = merge(all=TRUE, analysis_dataWelsh, occupational_status, by = "mcsid")
analysis_dataWelsh = merge(all=TRUE, analysis_dataWelsh, wealth_variables, by = "mcsid")
analysis_dataWelsh = merge(all=TRUE, analysis_dataWelsh, country_17, by = "mcsid")
analysis_dataWelsh = merge(all=TRUE, analysis_dataWelsh, caregiver_vocabTotal, by = "mcsid")
analysis_dataWelsh = merge(all=TRUE, analysis_dataWelsh, age5_vocab, by = "mcsid")
analysis_dataWelsh = merge(all=TRUE, analysis_dataWelsh, education_outcomesWelsh, by = "mcsid")

#select sample - those with a response on age 5 vocabulary test OR an educaion outcome
mcs_analysisWelsh = analysis_dataWelsh %>% filter((!is.na(age5_vocab)) | (!is.na(benchmark_binary_welsh)))

#save analysis data as a csv file ####
write.csv(mcs_analysisWelsh, file = "education_dataWelsh.csv")
