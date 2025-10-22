###################################
### Creating data for Mplus SEM ### 
###################################

#load libraries
library(dplyr)
library(mice)
library(MplusAutomation)
library(gtools)

#Load imputed dataset

load("2023-01-20_vocabulary_education_imputedMAIN.Rdata")
imputed = mi.res
#create long format imputed data, so that we can average the wealth and average grade
#variables across imputed datasets. we will then add these two variables to the original
#data, which will then be fiml'd in mplus. 

long_imputed<- mice::complete(imputed, "long", include = FALSE)
original_data = mice::complete(imputed, 0)

# Step 1: compute average wealth and average grade across imputations per person
mean_wealth <- long_imputed %>%
  group_by(mcsid) %>%
  summarise(wealth = mean(standardised_net_wealth, na.rm = TRUE))

mean_grade = long_imputed %>%
  group_by(mcsid) %>%
  summarise(avg_grade = mean(standardised_core_subjects, na.rm = TRUE))

# Step 2: merge back into the original data
original_data_1 <- original_data %>%
  left_join(mean_wealth, by = "mcsid") %>% 
  left_join(mean_grade, by = "mcsid")


#convert average wealth into quintiles
original_data_1$wealth_quintiles = quantcut(original_data_1$wealth,5)
levels(original_data_1$wealth_quintiles)[1] = "1"
levels(original_data_1$wealth_quintiles)[2] = "2"
levels(original_data_1$wealth_quintiles)[3] = "3"
levels(original_data_1$wealth_quintiles)[4] = "4"
levels(original_data_1$wealth_quintiles)[5] = "5"
original_data_1$wealth_quintiles <- as.factor(original_data_1$wealth_quintiles)

#select relevant variables for mplus

data = original_data_1 %>% 
  select(mcsid, age5_vocab, occupational_status, highest_nvq, oecd_income, imd, 
         ethnicity, sex, caregiver_vocab, wealth_quintiles, avg_grade, benchmark_binary, EAL, country)


#data = prepareMplusData(data,
                       # filename = "vocab_education_sem.dat", 
                       # inpfile = TRUE)

#dummy code x variables (Sex, ethnicity, eal, country)
data = data %>% 
  rename("eth" = ethnicity, 
         "reg" = country) %>% 
  dummy_cols(select_columns = c("sex", "eth", "EAL", "reg"))




#Mplus automation SEM
#rename variables so short enough for mplus
dat = data %>% 
  select(-sex, -eth, -EAL, -reg,
         -eth, -sex_1, -EAL_1,-reg_1, 
         -eth_NA, -sex_NA, -EAL_NA) %>% 
  rename("id" = mcsid,
         "bin_y" =  benchmark_binary,
         "cont_y" = avg_grade, 
        # "x1" = sex,
        # "x2" = ethnicity, 
        # "x3" = EAL,
        #"x4" = country,
         "x5" = caregiver_vocab,
         "vocab" = age5_vocab,
         "f1" = highest_nvq, 
         "f2" = oecd_income,
         "f3" = occupational_status, 
         "f4" = wealth_quintiles, 
         "f5" = imd) 





#Create Mplus input file for SEM

sem_binary =  mplusObject(
  TITLE = "SEC*vocab moderation predicting binary education - SEM;",
  VARIABLE=
    " usevariables = 
    bin_y !outcome 
    sex_2 !female
    eth_2, !mixed
    eth_3, !indian
    eth_4, !pakistani/bangladeshi
    eth_5, !Black/ black British
    eth_6, !other incl Chinese
    EAL_2, !english and another language
    EAL_3, !only another language
    reg_2, !Wales
    reg_3, !Scotland
    reg_4, !NI
    x5 !caregiver vocab
    vocab !age 5 vocab
    f1 !parent education
    f2 !income
    f3 !occupational status
    f4 !wealth
    f5; !imd
  categorical = bin_y f1 f2 f3 f4 f5;",
          
  DEFINE = "
  standardize x5 vocab; !zscore
  ", 
  
  ANALYSIS = "
  TYPE = RANDOM;!needed for the interaction XWITH
  ALGORITHM =INTEGRATION;
  INTEGRATION=MONTECARLO(500);
  PROCESSORS = 8;",
  
  MODEL = "
    sec BY f1-f5;
    fxo | sec XWITH vocab; !identify interaction between latent and observed
    bin_y ON 
    sex_2 
    eth_2 
    eth_3 
    eth_4 
    eth_5 
    eth_6 
    EAL_2 
    EAL_3 
    reg_2 
    reg_3 
    reg_4 
    sec 
    vocab
    fxo; !interaction between SEC and vocab
    
    sex_2;
    eth_2;
    eth_3;
    eth_4;
    eth_5;
    eth_6;
    EAL_2;
    EAL_3;
    reg_2;
    reg_3;
    reg_4;
    vocab; !list these vraiables to use fiml on x variables
    ",
  
  
  OUTPUT= "stand TECH1 TECH8 cinterval;
 ",
  SAVEDATA = "
FILE = vocab_sec_sem.dat;
MISSFLAG = .;",
  usevariables = colnames(dat),
  rdata = dat)



binary_sem <- mplusModeler(sem_binary,
                                        dataout=paste0("binary_sem_final", Sys.Date(),".dat"),
                                        modelout= paste0("binary_sem_final", Sys.Date(),".inp"),
                                        check=TRUE, run = TRUE, hashfilename = FALSE)




#Continuous outcome SEM

sem_cont =  mplusObject(
  TITLE = "SEC*vocab moderation predicting average education - SEM;",
  VARIABLE=
    " usevariables = 
    cont_y !outcome 
    sex_2 !female
    eth_2, !mixed
    eth_3, !indian
    eth_4, !pakistani/bangladeshi
    eth_5, !Black/ black British
    eth_6, !other incl Chinese
    EAL_2, !english and another language
    EAL_3, !only another language
    reg_2, !Wales
    reg_3, !Scotland
    reg_4, !NI
    x5 !caregiver vocab
    vocab !age 5 vocab
    f1 !parent education
    f2 !income
    f3 !occupational status
    f4 !wealth
    f5; !imd
  categorical = f1 f2 f3 f4 f5;",
  
  DEFINE = "
  standardize x5 vocab; !zscore
  ", 
  
  ANALYSIS = "
  TYPE = RANDOM;!needed for the interaction XWITH
  ALGORITHM =INTEGRATION;
  INTEGRATION=MONTECARLO(500);
  PROCESSORS = 8;",
  
  MODEL = "
    sec BY f1-f5;
    fxo | sec XWITH vocab; !identify interaction between latent and observed
    cont_y ON 
    sex_2 
    eth_2 
    eth_3 
    eth_4 
    eth_5 
    eth_6 
    EAL_2 
    EAL_3 
    reg_2 
    reg_3 
    reg_4 
    sec 
    vocab
    fxo; !interaction between SEC and vocab
    
    sex_2;
    eth_2;
    eth_3;
    eth_4;
    eth_5;
    eth_6;
    EAL_2;
    EAL_3;
    reg_2;
    reg_3;
    reg_4;
    vocab; !list these vraiables to use fiml on x variables
    ",
  
  
  OUTPUT= "stand TECH1 TECH8 cinterval;
 ",
  SAVEDATA = "
FILE = vocab_sec_sem_cont.dat;
MISSFLAG = .;",
  usevariables = colnames(dat),
  rdata = dat)



cont_sem <- mplusModeler(sem_cont,
                           dataout=paste0("cont_sem_final", Sys.Date(),".dat"),
                           modelout= paste0("cont_sem_final", Sys.Date(),".inp"),
                           check=TRUE, run = TRUE, hashfilename = FALSE)


