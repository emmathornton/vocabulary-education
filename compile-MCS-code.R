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
mcs3_child_assessment <- read_sav("mcs3_child_assessment_data.sav")
mcs1_hh <- read_sav("mcs1_hhgrid.sav")
mcs2_hh <- read_sav("mcs2_hhgrid.sav")
mcs_family <- read_sav("mcs_longitudinal_family_file.sav")
mcs1_parent <- read_sav("mcs1_parent_interview.sav")
mcs2_parent <- read_sav("mcs2_parent_interview.sav")
mcs2_derived <- read_sav("mcs2_derived_variables.sav")
mcs1_derived <- read_sav("mcs1_derived_variables.sav")
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
#create mcsid number so can pull out first CM later ####
mcsid_number_age9mo <- c("mcsid", "ahcnuma0")
mcsid_number_age9mo <- mcs1_parent[mcsid_number_age9mo]
mcsid_number_age9mo$ahcnuma0 = as.character(mcsid_number_age9mo$ahcnuma0)

mcsid_number_age3 <- c("mcsid", "bhcnuma0")
mcsid_number_age3 <- mcs2_parent[mcsid_number_age3]
mcsid_number_age3$bhcnuma0 = as.character (mcsid_number_age3$bhcnuma0)

#create respondent variable so can pull out correct respondent later####
respondent1 <- c("mcsid", "amdres00", "apdres00")
respondent1<-mcs1_derived[respondent1]
respondent2 <- c("mcsid", "bmdres00", "bpdres00")
respondent2<-mcs2_derived[respondent2]

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

#PREDICTOR VARIABLE _AGE 5 VOCAB ####
#AGE 5 naming vocabulary t score for single births in MCS1
mcsid_number_age5 <- c("mcsid", "chcnum00")
mcsid_number_age5 <- mcs3_child_assessment[mcsid_number_age5]
mcsid_number_age5$chcnum00= as.character(mcsid_number_age5$chcnum00)
NVability1_new <- c("mcsid",  "cdnvtscr")
NVability1_new <-mcs3_child_assessment[NVability1_new]
NVability1_new[ NVability1_new== -1:-9] <-NA
#NVability1_new$cdnvtscr=as.character(NVability1_new$cdnvtscr)
naming_vocab_age5<- NVability1_new[which(mcsid_number_age5$chcnum00=="1"),]
new_naming_vocab5 <- merge (all=TRUE, naming_vocab_age5, sweep_entry, by="mcsid")
naming_vocab5_sweep1<- new_naming_vocab5[which(new_naming_vocab5$sentry == "1"),]
#BAS standardised score for single births who were new families in sweep 2
NVability2_new <- c("mcsid",  "cdnvtscr")
NVability2_new <-mcs3_child_assessment[NVability2_new]
NVability2_new[ NVability2_new== -1:-9] <-NA
#NVability2_new$cdnvtscr=as.character(NVability2_new$cdnvtscr)
naming_vocab_age5_2<- NVability2_new[which(mcsid_number_age5$chcnum00=="1"),]
new_NVability2 <- merge (all=TRUE, naming_vocab_age5_2, sweep_entry, by="mcsid")
NVability_second2<- new_NVability2[which(new_NVability2$sentry == "2"),]
#combine single births MCS1 and single births MCS2 new entry
new_NVability2 <- merge(all=TRUE, naming_vocab5_sweep1, NVability_second2, by="mcsid")
#merge together so that only one value for standardised score
new_NVabilitycombine <- ifelse(!is.na(new_NVability2$cdnvtscr.x), new_NVability2$cdnvtscr.x, new_NVability2$cdnvtscr.y)
#create dataframe so also have mcsid 
NVability_score <- data.frame(new_NVabilitycombine, new_NVability2)
#subset data so just have 1 standard score and mcsid for the variable
final_NVability <- c("mcsid", "new_NVabilitycombine")
age5_language<- NVability_score[final_NVability]
#SES VARIABLES#### 

#occupational status####
#OCCUPATION 4 CLASSES - AGE 3.

#create NS-SEC 3 categories. 
nssec3_2_1<- c("mcsid", "bmd05s00", "bpd05s00")
nssec3_2_1 <-mcs2_derived[nssec3_2_1]
nssec3_2_1[nssec3_2_1==-9] <- NA
nssec3_2_1[nssec3_2_1==-1] <- NA
nssec3_2_1[nssec3_2_1==-8] <- NA
nssec31<- nssec3_2_1[which(mcsid_number_age3$bhcnuma0=="1"),]
new_nssec3_2_1 <- merge (all=TRUE, nssec31, sweep_entry, by="mcsid")
new_sec3_2_1<- new_nssec3_2_1[which(new_nssec3_2_1$sentry == "1"),]
nssec3_2_2<- c("mcsid", "bmd05s00", "bpd05s00")
nssec3_2_2 <-mcs2_derived[nssec3_2_2]
nssec32<- nssec3_2_2[which(mcsid_number_age3$bhcnuma0=="1"),]
new_nssec3_2_2 <- merge (all=TRUE, nssec32, sweep_entry, by="mcsid")
new_sec3_2_2<- new_nssec3_2_2[which(new_nssec3_2_2$sentry == "2"),]
new_sec3_2_2[new_sec3_2_2==-9] <- NA
new_sec3_2_2[new_sec3_2_2==-1] <- NA
new_sec3_2_2[new_sec3_2_2==-8] <- NA
nssec3_sweep2 <- merge(all=TRUE, new_sec3_2_1, new_sec3_2_2, by="mcsid")
#main respondent
bmsec3 <- ifelse(!is.na(nssec3_sweep2$bmd05s00.x), nssec3_sweep2$bmd05s00.x, nssec3_sweep2$bmd05s00.y)
#partner respondent
bpsec3 <- ifelse(!is.na(nssec3_sweep2$bpd05s00.x), nssec3_sweep2$bpd05s00.x, nssec3_sweep2$bpd05s00.y)
new_sec32<- data.frame(nssec3_sweep2, bmsec3 , bpsec3 )
bsec3 <- c("mcsid", "bmsec3", "bpsec3")
bsec3 <- new_sec32[bsec3]
bsec3[is.na(bsec3)] <- 9
bsec3[bsec3==1] <- 1
bsec3[bsec3==2] <- 2
bsec3[bsec3==3] <- 2
bsec3[bsec3==4] <- 3
bsec3[bsec3==5] <- 3
#get highest value for asec/bsec/csec (minimum value is the highest)
bhighest <- transform(bsec3, bhighest = pmin(bmsec3, bpsec3, na.rm=TRUE))
sec3 <- merge(all=TRUE, bsec3, bhighest, by="mcsid")
highsec3<- c("mcsid",  "bhighest")
highsec3<- sec3[highsec3]
highsec3[highsec3==9] <- NA
#adding in unemployment as a 4th category
labour1<- c("mcsid", "bdcwrk00")
labour1<- mcs2_derived[labour1]
labour1[labour1==-9] <- NA
labour1[labour1==-8] <- NA
labour1[labour1==-1] <- NA
labour1_2<- labour1[which(mcsid_number_age3$bhcnuma0=="1"),]
new_labour1 <- merge (all=TRUE, labour1_2, sweep_entry, by="mcsid")
lab1<- new_labour1[which(new_labour1$sentry == "1"),]

labour2<- c("mcsid", "bdcwrk00")
labour2<- mcs2_derived[labour2]
labour2[labour2==-9] <- NA
labour2[labour2==-8] <- NA
labour2[labour2==-1] <- NA
labour2_2<- labour2[which(mcsid_number_age3$bhcnuma0=="1"),]
new_labour2 <- merge (all=TRUE, labour2_2, sweep_entry, by="mcsid")
lab2<- new_labour2[which(new_labour2$sentry == "2"),]

labour_market <- merge(all=TRUE, lab1, lab2, by="mcsid")
labour2_combine <- ifelse(!is.na(labour_market$bdcwrk00.x), labour_market$bdcwrk00.x, labour_market$bdcwrk00.y)
labour_market1 <- data.frame(labour_market, labour2_combine)
cat4sec <- merge(all=TRUE, labour_market1, highsec3, by="mcsid")
cat4sec[is.na(cat4sec$bhighest) & cat4sec$labour2_combine %in% c(4,6,10),]$bhighest = 4
#variable with just mcsid and 4 category occupation 
nssec4 <- c("mcsid", "bhighest")
nssec4 <- cat4sec[nssec4]
#nssec4_number <- merge(all=TRUE, nssec4, mcsid_number_age3, by="mcsid")
#nssec41<- nssec4_number[which(mcsid_number_age3$bhcnum00=="1"),]
#occupation_4class <- c("mcsid", "bhighest")
#occupation_4class <- nssec41[occupation_4class]

#replace NA with 9 months occupation.
#create NS-SEC 3 categories
nssec3_1 <- c("mcsid", "amd05s00", "apd05s00")
nssec3_1 <-mcs1_derived[nssec3_1]
asec3<-nssec3_1[which(mcsid_number_age9mo$ahcnuma0=="1"),]
asec3$amd05s00[asec3$amd05s00==-9] <- NA
asec3$amd05s00[asec3$amd05s00==-1] <- NA
asec3$amd05s00[asec3$amd05s00==-8] <- NA
asec3$amd05s00[asec3$amd05s00==1] <- 1
asec3$amd05s00[asec3$amd05s00==2] <- 2
asec3$amd05s00[asec3$amd05s00==3] <- 2
asec3$amd05s00[asec3$amd05s00==4] <- 3
asec3$amd05s00[asec3$amd05s00==5] <- 3

asec3$apd05s00[asec3$apd05s00==-9] <- NA
asec3$apd05s00[asec3$apd05s00==-1] <- NA
asec3$apd05s00[asec3$apd05s00==-8] <- NA
asec3$apd05s00[asec3$apd05s00==1] <- 1
asec3$apd05s00[asec3$apd05s00==2] <- 2
asec3$apd05s00[asec3$apd05s00==3] <- 2
asec3$apd05s00[asec3$apd05s00==4] <- 3
asec3$apd05s00[asec3$apd05s00==5] <- 3
#get highest household value from 9 months sweep
ahighest <- transform(asec3, ahighest = pmin(amd05s00, apd05s00, na.rm=TRUE))

#sweep1 labour market status 
labour_sweep1<- c("mcsid", "adcwrk00")
labour_sweep1<- mcs1_derived[labour_sweep1]
labour_sweep1<-labour_sweep1[which(mcsid_number_age9mo$ahcnuma0=="1"),]
labour_sweep1[labour_sweep1==-9] <- NA
labour_sweep1[labour_sweep1==-8] <- NA
labour_sweep1[labour_sweep1==-1] <- NA

cat4sec_sweep1 <- merge(all=TRUE, labour_sweep1, ahighest, by="mcsid")
cat4sec_sweep1[is.na(cat4sec_sweep1$ahighest) & cat4sec_sweep1$adcwrk00 %in% c(4,6,10),]$ahighest = 4
#variable with just mcsid and 4 category occupation 
nssec4_sweep1 <- c("mcsid", "ahighest")
nssec4_sweep1 <- cat4sec_sweep1[nssec4_sweep1]

occupation <- merge(all=TRUE, nssec4, nssec4_sweep1, by="mcsid")
occupational_status <- ifelse(!is.na(occupation$bhighest), occupation$bhighest, occupation$ahighest)
highest_oc <- data.frame(occupation, occupational_status)
highest_hh_occupation <- c("mcsid", "occupational_status")
highest_hh_occupation <- highest_oc[highest_hh_occupation]

highest_hh_occupation$highest_occupation1 <- rec(highest_hh_occupation$occupational_status,  rec = "1=4; 2=3; 3=2; 4=1", as.num = TRUE, var.label = NULL, val.labels = NULL, append = FALSE, suffix = "_r")
highest_occupation<- c("mcsid", "highest_occupation1")
highest_occupation <- highest_hh_occupation[highest_occupation]

#income####
#INCOME AT AGE 3. OECD weighted quintiles
oecdincome<- c("mcsid", "boecduk0")
oecdincome<- mcs2_derived[oecdincome]
oecdincome[oecdincome==-1]<- NA
income_oecd1<- oecdincome[which(mcsid_number_age3$bhcnuma0=="1"),]
oecdincome <- merge (all=TRUE, income_oecd1, sweep_entry, by="mcsid")
oecd_income <- oecdincome[which(oecdincome$sentry == "1"),]
#new families
oecdincome2<- c("mcsid", "boecduk0")
oecdincome2<- mcs2_derived[oecdincome2]
oecdincome2[oecdincome2==-1]<- NA
income_oecd2<- oecdincome2[which(mcsid_number_age3$bhcnuma0=="1"),]
oecdincome2 <- merge (all=TRUE, income_oecd2, sweep_entry, by="mcsid")
oecd_income2 <- oecdincome2[which(oecdincome2$sentry == "2"),]
oincome <- merge(all=TRUE, oecd_income, oecd_income2,by="mcsid")
oecd_combine <- ifelse(!is.na(oincome$boecduk0.x), oincome$boecduk0.x, oincome$boecduk0.y)
#create dataframe so also have mcsid 
new_oincome<- data.frame(oecd_combine, oincome)
#subset data so just have 1 standard score and mcsid for the variable
income_oecd <- c("mcsid", "oecd_combine")
income_oecd<- new_oincome[income_oecd]

#OECD weighted quintiles at 9 months to replace NA
oecdincome_sweep1<- c("mcsid", "aoecduk0")
oecdincome_sweep1<- mcs1_derived[oecdincome_sweep1]
oecdincome_sweep1[oecdincome_sweep1==-1]<- NA
income_oecd_sweep1<- oecdincome_sweep1[which(mcsid_number_age9mo$ahcnuma0=="1"),]

combine_income_sweeps <- merge(all=TRUE,income_oecd, income_oecd_sweep1,by="mcsid")
oecd_combined_sweeps <- ifelse(!is.na(combine_income_sweeps$oecd_combine), combine_income_sweeps$oecd_combine, combine_income_sweeps$aoecduk0)
oecd_income_bothsweeps <- data.frame(combine_income_sweeps, oecd_combined_sweeps)
oecd_income_quintiles <- c("mcsid", "oecd_combined_sweeps")
oecd_income_quintiles <- oecd_income_bothsweeps[oecd_income_quintiles]
#NVQ education variable####
#NVQ qualifications

#RESPONDENT VARIABLE
#sweep 1 entry families at 9 months
MAINrespondent <- c("mcsid", "amdres00")
MAINrespondent<-mcs1_derived[MAINrespondent]
PARTNERrespondent <- c("mcsid", "apdres00")
PARTNERrespondent<-mcs1_derived[PARTNERrespondent]
#mother 
#identify mother from respondent variable - main and partner in sweep 1 
mother_main <- MAINrespondent[MAINrespondent$amdres00 == 1 | MAINrespondent$amdres00== 3 | MAINrespondent$amdres00 == 5 | MAINrespondent$amdres00 == 7 |MAINrespondent$amdres00 == 9 |MAINrespondent$amdres00 == 11 | MAINrespondent$amdres00 == 13|MAINrespondent$amdres00 == 15 ,]
mother_partner <- PARTNERrespondent[PARTNERrespondent$apdres00 == 1 | PARTNERrespondent$apdres00== 3 | PARTNERrespondent$apdres00 == 5 | PARTNERrespondent$apdres00 == 7 |PARTNERrespondent$apdres00 == 11 | PARTNERrespondent$apdres00 == 13| PARTNERrespondent$apdres00 == 15| PARTNERrespondent$apdres00 == 21,]
mother_partner <- mother_partner[!is.na(mother_partner$mcsid),] 
#for original families - mother as main respondent
mother_main_sweep1 <- merge (all=TRUE, mother_main, sweep_entry, by="mcsid")
mother_main_sweep1_1<- mother_main_sweep1[which(mother_main_sweep1$sentry == "1"),]
sweep1_main_mother <- c("mcsid", "amdres00")
sweep1_main_mother <-mother_main_sweep1_1[sweep1_main_mother]
sweep1_main_mother  <- sweep1_main_mother[!is.na(sweep1_main_mother$amdres00),] 
#for original families - mother as partner respondent. sweep 1
mother_partner_sweep1 <- merge (all=TRUE, mother_partner, sweep_entry, by="mcsid")
mother_partner_sweep1_1<- mother_partner_sweep1[which(mother_partner_sweep1$sentry == "1"),]
sweep1_partner_mother <- c("mcsid", "apdres00")
sweep1_partner_mother <-mother_partner_sweep1_1[sweep1_partner_mother]
sweep1_partner_mother  <- sweep1_partner_mother[!is.na(sweep1_partner_mother$apdres00),] 
#identify father from respondent variable - main and partner. sweep 1
father_main <- MAINrespondent[MAINrespondent$amdres00 == 2 | MAINrespondent$amdres00== 4 | MAINrespondent$amdres00 == 6 | MAINrespondent$amdres00 == 8 |MAINrespondent$amdres00 == 12 | MAINrespondent$amdres00 == 14 ,]
father_partner <- PARTNERrespondent[PARTNERrespondent$apdres00 == 2 | PARTNERrespondent$apdres00== 4 | PARTNERrespondent$apdres00 == 6 | PARTNERrespondent$apdres00 == 8 |PARTNERrespondent$apdres00 == 10 |PARTNERrespondent$apdres00 == 12 | PARTNERrespondent$apdres00 == 14 | PARTNERrespondent$apdres00 == 16 | PARTNERrespondent$apdres00 == 22 | PARTNERrespondent$apdres00 == 24 ,]
father_partner <- father_partner[!is.na(father_partner$mcsid),] 
#for original families - father as main respondent. sweep 1
father_main_sweep1 <- merge (all=TRUE, father_main, sweep_entry, by="mcsid")
father_main_sweep1_1<- father_main_sweep1[which(father_main_sweep1$sentry == "1"),]
sweep1_main_father <- c("mcsid", "amdres00")
sweep1_main_father <-father_main_sweep1_1[sweep1_main_father]
sweep1_main_father  <- sweep1_main_father[!is.na(sweep1_main_father$amdres00),] 
#for original families - father as partner respondent. sweep 1
father_partner_sweep1 <- merge (all=TRUE, father_partner, sweep_entry, by="mcsid")
father_partner_sweep1_1<- father_partner_sweep1[which(father_partner_sweep1$sentry == "1"),]
sweep1_partner_father <- c("mcsid", "apdres00")
sweep1_partner_father <-father_partner_sweep1_1[sweep1_partner_father]
sweep1_partner_father  <- sweep1_partner_father[!is.na(sweep1_partner_father$apdres00),] 

#sweep 2: respondent variables
MAINrespondent2 <- c("mcsid", "bmdres00")
MAINrespondent2<-mcs2_derived[MAINrespondent2]
PARTNERrespondent2 <- c("mcsid", "bpdres00")
PARTNERrespondent2<-mcs2_derived[PARTNERrespondent2]
#identify mother in sweep 2 from main and partner respondent 
mother_main2 <- MAINrespondent2[MAINrespondent2$bmdres00 == 1 | MAINrespondent2$bmdres00== 3 | MAINrespondent2$bmdres00 == 5 | MAINrespondent2$bmdres00 == 7 |MAINrespondent2$bmdres00 == 9 |MAINrespondent2$bmdres00 == 11 | MAINrespondent2$bmdres00 == 13| MAINrespondent2$bmdres00 == 15 ,]
mother_partner2 <- PARTNERrespondent2[PARTNERrespondent2$bpdres00 == 1 | PARTNERrespondent2$bpdres00== 3 | PARTNERrespondent2$bpdres00 == 5 | PARTNERrespondent2$bpdres00 == 7 |PARTNERrespondent2$bpdres00 == 11 | PARTNERrespondent2$bpdres00 == 13| PARTNERrespondent2$bpdres00 == 15 | PARTNERrespondent2$bpdres00 == 21,]
mother_partner2 <- mother_partner2[!is.na(mother_partner2$mcsid),] 
#original families mother from main respondent at sweep 2
mum_main_sweep2_1st <- merge (all=TRUE, mother_main2, sweep_entry, by="mcsid")
mum_main_sweep2_1<- mum_main_sweep2_1st[which(mum_main_sweep2_1st$sentry == "1"),]
sweep2_main_mum1 <- c("mcsid", "bmdres00")
sweep2_main_mum1 <- mum_main_sweep2_1[sweep2_main_mum1]
sweep2_main_mum1 <- sweep2_main_mum1[!is.na(sweep2_main_mum1$bmdres00),] 
#original families mother from partner respondent at sweep 2
mum_partner_sweep2_1st <- merge (all=TRUE, mother_partner2, sweep_entry, by="mcsid")
mum_partner_sweep2_1<- mum_partner_sweep2_1st[which(mum_partner_sweep2_1st$sentry == "1"),]
sweep2_partner_mum1 <- c("mcsid", "bpdres00")
sweep2_partner_mum1 <- mum_partner_sweep2_1[sweep2_partner_mum1]
sweep2_partner_mum1 <- sweep2_partner_mum1[!is.na(sweep2_partner_mum1$bpdres00),] 


#identify father in sweep 2 from main and partner respondent 
father_main2 <- MAINrespondent2[MAINrespondent2$bmdres00 == 2 | MAINrespondent2$bmdres00== 4 | MAINrespondent2$bmdres00 == 6 | MAINrespondent2$bmdres00 == 8 |MAINrespondent2$bmdres00 == 12 | MAINrespondent2$bmdres00 == 14 | MAINrespondent2$bmdres00 == 16 ,]
father_partner2 <- PARTNERrespondent2[PARTNERrespondent2$bpdres00 == 2 | PARTNERrespondent2$bpdres00== 4 | PARTNERrespondent2$bpdres00 == 6 | PARTNERrespondent2$bpdres00 == 8 |PARTNERrespondent2$bpdres00 == 10 |PARTNERrespondent2$bpdres00 == 12 | PARTNERrespondent2$bpdres00 == 14 | PARTNERrespondent2$bpdres00 == 16| PARTNERrespondent2$bpdres00 == 18| PARTNERrespondent2$bpdres00 == 22,]
father_partner2 <- father_partner2[!is.na(father_partner2$mcsid),]
#original families father from main respondent at sweep 2
dad_main_sweep2_1st <- merge (all=TRUE,father_main2, sweep_entry, by="mcsid")
dad_main_sweep2_1<- dad_main_sweep2_1st[which(dad_main_sweep2_1st$sentry == "1"),]
sweep2_main_dad1 <- c("mcsid", "bmdres00")
sweep2_main_dad1 <- dad_main_sweep2_1[sweep2_main_dad1]
sweep2_main_dad1 <- sweep2_main_dad1[!is.na(sweep2_main_dad1$bmdres00),] 
#original families father from partner respondent at sweep 2
dad_partner_sweep2_1st <- merge (all=TRUE, father_partner2, sweep_entry, by="mcsid")
dad_partner_sweep2_1<- dad_partner_sweep2_1st[which(dad_partner_sweep2_1st$sentry == "1"),]
sweep2_partner_dad1 <- c("mcsid", "bpdres00")
sweep2_partner_dad1 <- dad_partner_sweep2_1[sweep2_partner_dad1]
sweep2_partner_dad1 <- sweep2_partner_dad1[!is.na(sweep2_partner_dad1$bpdres00),] 

#"new families" ie sentry==2 families respondent - mother from main respondent 
mum_main_sweep2 <- merge (all=TRUE, mother_main2, sweep_entry, by="mcsid")
mum_main_sweep2<- mum_main_sweep2[which(mum_main_sweep2$sentry == "2"),]
sweep2_main_mum<- c("mcsid", "bmdres00")
sweep2_main_mum <- mum_main_sweep2[sweep2_main_mum]
sweep2_main_mum <- sweep2_main_mum[!is.na(sweep2_main_mum$bmdres00),] 
#"new families" ie sentry==2 families respondent - mother from partner respondent 
mum_partner_sweep2 <- merge (all=TRUE, mother_partner2, sweep_entry, by="mcsid")
mum_partner_sweep2<- mum_partner_sweep2[which(mum_partner_sweep2$sentry == "2"),]
sweep2_partner_mum <- c("mcsid", "bpdres00")
sweep2_partner_mum <- mum_partner_sweep2[sweep2_partner_mum]
sweep2_partner_mum<- sweep2_partner_mum[!is.na(sweep2_partner_mum$bpdres00),] 
#"new families" ie sentry==2 families respondent - father from main respondent 
dad_main_sweep2 <- merge (all=TRUE,father_main2, sweep_entry, by="mcsid")
dad_main_sweep2<- dad_main_sweep2[which(dad_main_sweep2$sentry == "2"),]
sweep2_main_dad <- c("mcsid", "bmdres00")
sweep2_main_dad <- dad_main_sweep2[sweep2_main_dad]
sweep2_main_dad <- sweep2_main_dad[!is.na(sweep2_main_dad$bmdres00),] 
#"new families" ie sentry==2 families respondent - father from partner respondent 
dad_partner_sweep2 <- merge (all=TRUE, father_partner2, sweep_entry, by="mcsid")
dad_partner_sweep2<- dad_partner_sweep2[which(dad_partner_sweep2$sentry == "2"),]
sweep2_partner_dad <- c("mcsid", "bpdres00")
sweep2_partner_dad <- dad_partner_sweep2[sweep2_partner_dad]
sweep2_partner_dad <- sweep2_partner_dad[!is.na(sweep2_partner_dad$bpdres00),] 

#sweep 1 entry families NVQ at age 3
#main respondent
main_NVQ1 <- c("mcsid", "bmdnvq00")
main_NVQ1 <- mcs2_derived[main_NVQ1]
main_NVQ1[main_NVQ1==-1] <- NA
main_NVQ1[main_NVQ1==-1] <- NA
main_NVQ1$bmdnvq00[main_NVQ1$bmdnvq00==95] <- 0 #overseas qual only - convert into an NVQ level 0
main_NVQ1$bmdnvq00[main_NVQ1$bmdnvq00==96] <- 0 #none of these - convert into an NVQ level 0
#partner respondent
partner_NVQ1 <- c("mcsid", "bpdnvq00")
partner_NVQ1 <- mcs2_derived[partner_NVQ1]
partner_NVQ1[partner_NVQ1==-1] <- NA
partner_NVQ1[partner_NVQ1==-1] <- NA
partner_NVQ1$bpdnvq00[partner_NVQ1$bpdnvq00==95] <- 0 #overseas qual only - convert into an NVQ level 0
partner_NVQ1$bpdnvq00[partner_NVQ1$bpdnvq00==96] <- 0 #none of these- convert into an NVQ level 0
#first entry families mother's NVQ sweep 2
MAIN_motherNVQ1<- main_NVQ1[main_NVQ1$mcsid %in% sweep2_main_mum1$mcsid,]
PARTNER_motherNVQ1<- partner_NVQ1[partner_NVQ1$mcsid %in% sweep2_partner_mum1$mcsid,]
maternal_NVQ1 <- merge(all=TRUE,MAIN_motherNVQ1, PARTNER_motherNVQ1, by="mcsid")
maternal_NVQ1$mum_NVQ1 <- ifelse(!is.na(maternal_NVQ1$bmdnvq00), maternal_NVQ1$bmdnvq00, maternal_NVQ1$bpdnvq00)
MATERNAL_NVQsweep1<- c("mcsid", "mum_NVQ1")
MATERNAL_NVQsweep1 <- maternal_NVQ1[MATERNAL_NVQsweep1]
#first entry families father's NVQ sweep 2
MAIN_fatherNVQ1<- main_NVQ1[main_NVQ1$mcsid %in% sweep2_main_dad1$mcsid,]
PARTNER_fatherNVQ1<- partner_NVQ1[partner_NVQ1$mcsid %in% sweep2_partner_dad1$mcsid,]
paternal_NVQ1 <- merge(all=TRUE,MAIN_fatherNVQ1, PARTNER_fatherNVQ1, by="mcsid")
paternal_NVQ1$dad_NVQ1 <- ifelse(!is.na(paternal_NVQ1$bpdnvq00), paternal_NVQ1$bpdnvq00, paternal_NVQ1$bmdnvq00)
PATERNAL_NVQsweep1<- c("mcsid", "dad_NVQ1")
PATERNAL_NVQsweep1 <- paternal_NVQ1[PATERNAL_NVQsweep1]


#sweep 1 entry families NVQ at age 9 months
main_NVQ_sweep1 <- c("mcsid", "amdnvq00")
main_NVQ_sweep1 <- mcs1_derived[main_NVQ_sweep1]
main_NVQ_sweep1[main_NVQ_sweep1==-1] <- NA
main_NVQ_sweep1[main_NVQ_sweep1==-1] <- NA
main_NVQ_sweep1$amdnvq00[main_NVQ_sweep1$amdnvq00==95] <- 0 #overseas qual only - convert into an NVQ level 0
main_NVQ_sweep1$amdnvq00[main_NVQ_sweep1$amdnvq00==96] <- 0 #none of these - convert into an NVQ level 0

partner_NVQ_sweep1 <- c("mcsid", "apdnvq00")
partner_NVQ_sweep1 <- mcs1_derived[partner_NVQ_sweep1]
partner_NVQ_sweep1[partner_NVQ_sweep1==-1] <- NA
partner_NVQ_sweep1[partner_NVQ_sweep1==-1] <- NA
partner_NVQ_sweep1$apdnvq00[partner_NVQ_sweep1$apdnvq00==95] <- 0 #overseas qual only - convert into an NVQ level 0
partner_NVQ_sweep1$apdnvq00[partner_NVQ_sweep1$apdnvq00==96] <- 0 #none of these- convert into an NVQ level 0

#mother 
MAIN_motherNVQ_sweep1<- main_NVQ_sweep1[main_NVQ_sweep1$mcsid %in% sweep1_main_mother$mcsid,]
PARTNER_motherNVQ_sweep1<- partner_NVQ_sweep1[partner_NVQ_sweep1$mcsid %in% sweep1_partner_mother$mcsid,]
maternal_NVQ_sweep1 <- merge(all=TRUE,MAIN_motherNVQ_sweep1, PARTNER_motherNVQ_sweep1, by="mcsid")
maternal_NVQ_sweep1$mum_NVQ_sweep1<- ifelse(!is.na(maternal_NVQ_sweep1$amdnvq00), maternal_NVQ_sweep1$amdnvq00, maternal_NVQ_sweep1$apdnvq00)
MATERNAL_NVQ_sweep1<- c("mcsid", "mum_NVQ_sweep1")
MATERNAL_NVQ_sweep1 <- maternal_NVQ_sweep1[MATERNAL_NVQ_sweep1]

#father 
MAIN_fatherNVQ_sweep1<- main_NVQ_sweep1[main_NVQ_sweep1$mcsid %in% sweep1_main_father$mcsid,]
PARTNER_fatherNVQ_sweep1<- partner_NVQ_sweep1[partner_NVQ_sweep1$mcsid %in% sweep1_partner_father$mcsid,]
paternal_NVQ_sweep1 <- merge(all=TRUE,MAIN_fatherNVQ_sweep1, PARTNER_fatherNVQ_sweep1, by="mcsid")
paternal_NVQ_sweep1$dad_NVQ_sweep1<- ifelse(!is.na(paternal_NVQ_sweep1$apdnvq00), paternal_NVQ_sweep1$apdnvq00, paternal_NVQ_sweep1$amdnvq00)
PATERNAL_NVQ_sweep1<- c("mcsid", "dad_NVQ_sweep1")
PATERNAL_NVQ_sweep1 <- paternal_NVQ_sweep1[PATERNAL_NVQ_sweep1]

#merge and replace missing age 3 with 9 months
#mother
mother_sweep1_nvq <- merge(all=TRUE, MATERNAL_NVQ_sweep1, MATERNAL_NVQsweep1, by="mcsid")
mother_sweep1_nvq$mum_nvq1 <- ifelse(!is.na(mother_sweep1_nvq$mum_NVQ1), mother_sweep1_nvq$mum_NVQ1, mother_sweep1_nvq$mum_NVQ_sweep1)
mother_nvq_first_entry <- c("mcsid", "mum_nvq1")
mother_nvq_first_entry <- mother_sweep1_nvq[mother_nvq_first_entry]
#father
father_sweep1_nvq <- merge(all=TRUE, PATERNAL_NVQ_sweep1, PATERNAL_NVQsweep1, by="mcsid")
father_sweep1_nvq$dad_nvq1 <- ifelse(!is.na(father_sweep1_nvq$dad_NVQ1), father_sweep1_nvq$dad_NVQ1, father_sweep1_nvq$dad_NVQ_sweep1)
father_nvq_first_entry <- c("mcsid", "dad_nvq1")
father_nvq_first_entry <- father_sweep1_nvq[father_nvq_first_entry]


#second entry families age age 3 NVQ
main_NVQ2 <- c("mcsid", "bmdnvq00")
main_NVQ2 <- mcs2_derived[main_NVQ2]
main_NVQ2[main_NVQ2==-1] <- NA
main_NVQ2[main_NVQ2==-1] <- NA
main_NVQ2$bmdnvq00[main_NVQ2$bmdnvq00==95] <- 0 #overseas qual only - convert into an NVQ level 0
main_NVQ2$bmdnvq00[main_NVQ2$bmdnvq00==96] <- 0 #none of these- convert into an NVQ level 0

partner_NVQ2 <- c("mcsid", "bpdnvq00")
partner_NVQ2 <- mcs2_derived[partner_NVQ2]
partner_NVQ2[partner_NVQ2==-1] <- NA
partner_NVQ2[partner_NVQ2==-1] <- NA
partner_NVQ2$bpdnvq00[partner_NVQ2$bpdnvq00==95] <- 0 #overseas qual only - convert into an NVQ level 0
partner_NVQ2$bpdnvq00[partner_NVQ2$bpdnvq00==96] <- 0
#second entry families mother's NVQ sweep 2
MAIN_motherNVQ2<- main_NVQ2[main_NVQ2$mcsid %in% sweep2_main_mum$mcsid,]
PARTNER_motherNVQ2<- partner_NVQ2[partner_NVQ2$mcsid %in% sweep2_partner_mum$mcsid,]
maternal_NVQ2 <- merge(all=TRUE,MAIN_motherNVQ2, PARTNER_motherNVQ2, by="mcsid")
maternal_NVQ2$mum_NVQ2 <- ifelse(!is.na(maternal_NVQ2$bmdnvq00), maternal_NVQ2$bmdnvq00, maternal_NVQ2$bpdnvq00)
MATERNAL_NVQsweep2<- c("mcsid", "mum_NVQ2")
MATERNAL_NVQsweep2 <- maternal_NVQ2[MATERNAL_NVQsweep2]
#second entry families father's NVQ sweep 2
MAIN_fatherNVQ2<- main_NVQ2[main_NVQ2$mcsid %in% sweep2_main_dad$mcsid,]
PARTNER_fatherNVQ2<- partner_NVQ2[partner_NVQ2$mcsid %in% sweep2_partner_dad$mcsid,]
paternal_NVQ2 <- merge(all=TRUE,MAIN_fatherNVQ2, PARTNER_fatherNVQ2, by="mcsid")
paternal_NVQ2$dad_NVQ2 <- ifelse(!is.na(paternal_NVQ2$bpdnvq00), paternal_NVQ2$bpdnvq00, paternal_NVQ2$bmdnvq00)
PATERNAL_NVQsweep2<- c("mcsid", "dad_NVQ2")
PATERNAL_NVQsweep2 <- paternal_NVQ2[PATERNAL_NVQsweep2]

#combining sweep 1 and sweep 2 families - parent education variable
#mother
maternal_NVQ_combine <- merge (all=TRUE, mother_nvq_first_entry, MATERNAL_NVQsweep2,by="mcsid")
maternal_NVQ_combine$mother_NVQ_combine <- ifelse(!is.na(maternal_NVQ_combine$mum_nvq1), maternal_NVQ_combine$mum_nvq1, maternal_NVQ_combine$mum_NVQ2)
#mum_nvqcombine <- data.frame(maternal_NVQ_combine, mother_NVQ_combine)
mother_NVQ <- c("mcsid", "mother_NVQ_combine")
mother_NVQ <- maternal_NVQ_combine[mother_NVQ]

#father
paternal_NVQ_combine <- merge (all=TRUE, father_nvq_first_entry, PATERNAL_NVQsweep2,by="mcsid")
paternal_NVQ_combine$father_NVQ_combine <- ifelse(!is.na(paternal_NVQ_combine$dad_nvq1), paternal_NVQ_combine$dad_nvq1, paternal_NVQ_combine$dad_NVQ2)
#dad_nvqcombine <- data.frame(paternal_NVQ_combine, father_NVQ_combine)
father_NVQ <- c("mcsid", "father_NVQ_combine")
father_NVQ <- paternal_NVQ_combine[father_NVQ]

#both_parents
parent_NVQ <- merge(all=TRUE,mother_NVQ, father_NVQ,by="mcsid")
#parentsNVQ<- parent_NVQ[which(mcsid_number_age3$bhcnuma0=="1"),]

#to get highest household level 
highest_NVQ <- transform(parent_NVQ, highestNVQ = pmax(mother_NVQ_combine, father_NVQ_combine, na.rm=TRUE))
#highest_NVQ1 <- data.frame(parent_NVQ, highest_NVQ)
highest_parent_NVQ <- c("mcsid", "highestNVQ")
highest_parent_NVQ<- highest_NVQ[highest_parent_NVQ]




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

