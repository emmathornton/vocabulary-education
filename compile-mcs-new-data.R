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
mcs2_parent <- read_sav("mcs2_parent_interview.sav")
mcs2_derived <- read_sav("mcs2_parent_derived.sav")
mcs2_derived_family <- read_sav("mcs2_family_derived.sav")
mcs1_derived_family <- read_sav("mcs1_family_derived.sav")
mcs1_derived <- read_sav("mcs1_parent_derived.sav")
mcs5_parent<- read_sav("mcs5_parent_interview.sav")
mcs5_family<- read_sav("mcs5_family_derived.sav")
mcs2_geography <- read_sav("mcs2_geographically_linked_data.sav")
mcs1_geography <- read_sav("mcs1_geographically_linked_data.sav")
mcs6_parent_assessment <- read_sav("mcs6_parent_assessment.sav")
mcs3_cm_derived = read_sav("mcs3_cm_derived.sav")
mcs1_cm_derived = read_sav("mcs1_cm_derived.sav")
mcs2_cm_derived = read_sav("mcs2_cm_derived.sav")
#convert all to lowercase####
names(mcs3_child_assessment) <- tolower(names(mcs3_child_assessment))
#names(mcs2_child_assessment) <- tolower(names(mcs2_child_assessment))
names(mcs_family) <- tolower(names(mcs_family))
names(mcs5_family) <- tolower(names(mcs5_family))
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
names(mcs3_cm_derived) <- tolower(names(mcs3_cm_derived))
names(mcs2_cm_derived) <- tolower(names(mcs2_cm_derived))
names(mcs1_cm_derived) <- tolower(names(mcs1_cm_derived))
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




#WEALTH #### - debt seems to be missing from mcs5 new datasets? 
wealth_variables <- mcs5_parent%>%  select(mcsid, eresp00, epmopa00, ephval00, epinvt00, epdeba00)
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
#language spoken at home#### - can't find this variable at all in new dataset for sweep1. 
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
ethnicity_sweep1 = mcs1_cm_derived %>% select(mcsid, adc06e00, acnum00) %>% 
  filter(acnum00==1) %>% 
  select(mcsid, adc06e00)
ethnicity_sweep2 = mcs2_cm_derived %>% select(mcsid, bdc06e00, bcnum00) %>% 
  filter(bcnum00==1) %>% 
  merge(all=TRUE,  sweep_entry, by="mcsid") %>% 
  filter(sentry == 2) %>% 
  select(mcsid, bdc06e00) %>% 
  merge(all= TRUE, ethnicity, by = "mcsid") %>% 
  mutate(ethnicity = case_when(!is.na(adc06e00) ~ adc06e00, 
                               is.na(adc06e00) ~ bdc06e00))
ethnicity = ethnicity_sweep2 %>% select(mcsid, ethnicity)


#gender####
sex_sweep1 = mcs1_hh %>% select(mcsid, ahcsex00,acnum00) %>% 
  filter(acnum00 ==1) %>% 
  select(mcsid, ahcsex00)

sex_sweep2 = mcs2_hh %>% select(mcsid, bhcsex00, bcnum00) %>% 
  filter(bcnum00 == 1) %>% 
  select(mcsid, bhcsex00) %>% 
  merge(all=TRUE, sweep_entry, by="mcsid") %>% 
  filter(sentry == 2) %>% 
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
 mutate (caregiver_vocab = rowMeans(.[-1], na.rm = TRUE), .after = 1) 
#convert NaN to NA
caregiver_vocabTotal$caregiver_vocab[is.nan(caregiver_vocabTotal$caregiver_vocab)]<-NA

#auxiliary variables for imputation####
#mother's age at birth of CM####
#sweep 1 - creating mother respondent variables
age_atBirth_sweep2 = mcs2_derived %>% select(mcsid, bddagb00, bddres00) %>% 
  filter(bddres00 == 1 | bddres00 == 3 |bddres00 ==5|
           bddres00==7 | bddres00 == 9 | bddres00 == 11 |
           bddres00 == 13 |bddres00 == 15) 

age_atBirth = mcs1_derived %>% select(mcsid, addagb00, addres00) %>% 
  filter(addres00 == 1 | addres00 == 3 |addres00 ==5|
           addres00==7 | addres00 == 9 | addres00 == 11 |
           addres00 == 13 |addres00 == 15) %>% 
  merge(all=TRUE, age_atBirth_sweep2, by="mcsid") %>% 
  merge(all=TRUE, sweep_entry, by="mcsid") %>% 
  mutate(mum_ageAtBirth = case_when(!is.na(bddagb00) ~ bddagb00,
                                    is.na(bddagb00) ~ addagb00)) %>% 
  select(mcsid, mum_ageAtBirth )

#housing tenure at age 3####
BDROOW00
#housing tenure at age 3, replace with 9 months if missing
tenure_sweep1 = mcs1_derived_family %>% select(mcsid, adroow00) 
tenure_sweep2 = mcs2_derived_family %>% select(mcsid, bdroow00) %>% 
  merge(all=TRUE, tenure_sweep1, by="mcsid") %>% 
  mutate(housing_tenure = case_when(!is.na(bdroow00) ~ bdroow00,
                                    is.na(bdroow00) ~ adroow00)) %>% 
  merge(all=TRUE, sweep_entry,by="mcsid") %>% 
  
#tenure key
  
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

