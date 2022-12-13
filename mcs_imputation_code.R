#load in required packages####
library(haven)
require(swfscMisc)
require(sjmisc)
require(Hmisc)
require(psych)
library(mice)
library(miceadds)
library(gtools)
library(glue)
library(lubridate)
library(tidyverse)

#open mcs data####
vocabulary_education<-read.csv("education_data1.csv")
vocabulary_education[,1]<- NULL
#Rename wealth variables and income
vocabulary_education = vocabulary_education %>%
  rename(mortgage = "new_mortgage", 
         houseValue = "new_value", 
         savings = "new_savings", 
         debt = "new_debt", 
         oecd_income = "income_quintiles")

#make dummy variables for imputation - n - 1 levels as one will be the reference category. by default in regressions this is the lowest number so  take out first level 
#nvq - without nvq = 0 
vocabulary_education$nvq_int1 = ifelse(vocabulary_education$highest_nvq == 1, 1, 0)
vocabulary_education$nvq_int2 = ifelse(vocabulary_education$highest_nvq == 2, 1, 0)
vocabulary_education$nvq_int3 = ifelse(vocabulary_education$highest_nvq == 3, 1, 0)
vocabulary_education$nvq_int4 = ifelse(vocabulary_education$highest_nvq == 4, 1, 0)
vocabulary_education$nvq_int5 = ifelse(vocabulary_education$highest_nvq == 5, 1, 0)

#income - without income = 1
vocabulary_education$income_int2 = ifelse(vocabulary_education$oecd_income == 2, 1, 0)
vocabulary_education$income_int3 = ifelse(vocabulary_education$oecd_income == 3, 1, 0)
vocabulary_education$income_int4 = ifelse(vocabulary_education$oecd_income == 4, 1, 0)
vocabulary_education$income_int5 = ifelse(vocabulary_education$oecd_income == 5, 1, 0)

#occupation - without occupational status = 1

vocabulary_education$occupation_int2 = ifelse(vocabulary_education$occupational_status == 2, 1, 0)
vocabulary_education$occupation_int3 = ifelse(vocabulary_education$occupational_status == 3, 1, 0)
vocabulary_education$occupation_int4 = ifelse(vocabulary_education$occupational_status == 4, 1, 0)

#imd - without imd = 1
vocabulary_education$imd_int2 = ifelse(vocabulary_education$imd == 2, 1, 0)
vocabulary_education$imd_int3 = ifelse(vocabulary_education$imd == 3, 1, 0)
vocabulary_education$imd_int4 = ifelse(vocabulary_education$imd == 4, 1, 0)
vocabulary_education$imd_int5 = ifelse(vocabulary_education$imd == 5, 1, 0)
vocabulary_education$imd_int6 = ifelse(vocabulary_education$imd == 6, 1, 0)
vocabulary_education$imd_int7 = ifelse(vocabulary_education$imd == 7, 1, 0)
vocabulary_education$imd_int8 = ifelse(vocabulary_education$imd == 8, 1, 0)
vocabulary_education$imd_int9 = ifelse(vocabulary_education$imd == 9, 1, 0)
vocabulary_education$imd_int10 = ifelse(vocabulary_education$imd == 10, 1, 0)

#create interaction term variables - each dummy multiplied by vocab
#nvq
vocabulary_education$vocab.nvq1 = vocabulary_education$nvq_int1*vocabulary_education$age5_vocab
vocabulary_education$vocab.nvq2=  vocabulary_education$nvq_int2*vocabulary_education$age5_vocab
vocabulary_education$vocab.nvq3 = vocabulary_education$nvq_int3*vocabulary_education$age5_vocab
vocabulary_education$vocab.nvq4 = vocabulary_education$nvq_int4*vocabulary_education$age5_vocab
vocabulary_education$vocab.nvq5 = vocabulary_education$nvq_int5*vocabulary_education$age5_vocab

#income
vocabulary_education$vocab.income2 = vocabulary_education$income_int2*vocabulary_education$age5_vocab
vocabulary_education$vocab.income3 = vocabulary_education$income_int3*vocabulary_education$age5_vocab
vocabulary_education$vocab.income4 = vocabulary_education$income_int4*vocabulary_education$age5_vocab
vocabulary_education$vocab.income5 = vocabulary_education$income_int5*vocabulary_education$age5_vocab

#occupation
vocabulary_education$vocab.occupation2  = vocabulary_education$occupation_int2*vocabulary_education$age5_vocab
vocabulary_education$vocab.occupation3  = vocabulary_education$occupation_int3*vocabulary_education$age5_vocab
vocabulary_education$vocab.occupation4  = vocabulary_education$occupation_int4*vocabulary_education$age5_vocab

#imd
vocabulary_education$vocab.imd2 = vocabulary_education$imd_int2*vocabulary_education$age5_vocab
vocabulary_education$vocab.imd3 = vocabulary_education$imd_int3*vocabulary_education$age5_vocab
vocabulary_education$vocab.imd4 = vocabulary_education$imd_int4*vocabulary_education$age5_vocab
vocabulary_education$vocab.imd5 = vocabulary_education$imd_int5*vocabulary_education$age5_vocab
vocabulary_education$vocab.imd6 = vocabulary_education$imd_int6*vocabulary_education$age5_vocab
vocabulary_education$vocab.imd7 = vocabulary_education$imd_int7*vocabulary_education$age5_vocab
vocabulary_education$vocab.imd8 = vocabulary_education$imd_int8*vocabulary_education$age5_vocab
vocabulary_education$vocab.imd9 = vocabulary_education$imd_int9*vocabulary_education$age5_vocab
vocabulary_education$vocab.imd10= vocabulary_education$imd_int10*vocabulary_education$age5_vocab

#remove dummy variables
vocabulary_education = vocabulary_education %>% select(!nvq_int1:imd_int10)

#make categorical predictors factor/binary variables. 
vocabulary_education$sex=as.factor(vocabulary_education$sex)
vocabulary_education$ethnicity=as.factor(vocabulary_education$ethnicity)
vocabulary_education$EAL=as.factor(vocabulary_education$EAL)
vocabulary_education$oecd_income=as.factor(vocabulary_education$oecd_income)
vocabulary_education$imd=as.factor(vocabulary_education$imd)
vocabulary_education$occupational_status=as.factor(vocabulary_education$occupational_status)
vocabulary_education$highest_nvq=as.factor(vocabulary_education$highest_nvq)
vocabulary_education$carers_in_hh=as.factor(vocabulary_education$carers_in_hh)
vocabulary_education$cm_breastfed=as.factor(vocabulary_education$cm_breastfed)
vocabulary_education$accommodation_type=as.factor(vocabulary_education$accommodation_type)
vocabulary_education$housing_tenure=as.factor(vocabulary_education$housing_tenure)
vocabulary_education$benchmark_binary=as.factor(vocabulary_education$benchmark_binary)
vocabulary_education$benchmark_binary_welsh=as.factor(vocabulary_education$benchmark_binary_welsh)
vocabulary_education$country=as.factor(vocabulary_education$country)

#multiple imputation####
methods(mice)
init = mice(vocabulary_education, maxit=0) 
meth = init$method
predM = init$predictorMatrix

#To impute the missing values, mice package use an algorithm in a such a way that use information 
#from other variables in dataset to predict and impute the missing values. 
#Therefore, you may not want to use certain variable as predictors. 
#For example the ID variable does not have any predictive value.

#If you want to skip a variable from imputation use the code below. 
#Keep in mind that this variable will be used for prediction.

meth[c("mcsid")]=""
meth[c("weight")]=""
meth[c("countryWeight")]=""
#Now let specify the methods for imputing the missing values. 
#There are specific methods for continuous, binary and ordinal variables. 
#I set different methods for each variable. You can add more than one variable in each methods.

methods(mice)

# define methods for imputation
#logistic regression for binary variables
#polynomial regression for categorical variables 
#cart regression trees for continuous variables

meth[c("sex")]="logreg"
meth[c("ethnicity")]="polyreg"
meth[c("EAL")]="polyreg"
meth[c("age_atBirth")]="cart" 
meth[c("highest_nvq")]="polyreg"
meth[c("oecd_income")]="polyreg"
meth[c("imd")]="polyreg"
meth[c("occupational_status")]="polyreg"
meth[c("housing_tenure")]="polyreg"
meth[c("accommodation_type")]="polyreg"
meth[c("cm_breastfed")]="logreg"
meth[c("carers_in_hh")]="logreg"
meth[c("mortgage")]="cart"
meth[c("houseValue")]="cart"
meth[c("savings")]="cart"
meth[c("debt")]="cart"
meth[c("country")]="polyreg"
meth[c("caregiver_vocab")]="cart"
meth[c("age5_vocab")]="cart" 
meth[c("benchmark_binary", "benchmark_binary_welsh")]="logreg"
meth[c("standardised_core_subjects", "standardised_core_subjectsWelsh", 
       "standardised_english", "standardised_maths", "standardised_science")]="cart" 
meth[c("vocab.nvq1", "vocab.nvq2", "vocab.nvq3", "vocab.nvq4", "vocab.nvq5")] = "cart"
meth[c("vocab.income2", "vocab.income3", "vocab.income4", "vocab.income4", "vocab.income5")] = "cart"
meth[c("vocab.occupation2","vocab.occupation3", "vocab.occupation4" )] = "cart"
meth[c("vocab.imd2", "vocab.imd3", "vocab.imd4", "vocab.imd4", "vocab.imd5", 
       "vocab.imd6", "vocab.imd7", "vocab.imd8", "vocab.imd9", "vocab.imd10")] = "cart"


blocksvec=names(meth)
#set predictor matrix so that interaction terms and main effects arent predicting themselves
#if dont want a variable as a predictor, set it to 0 in the predictor matrix 
predM = predM[blocksvec,]
predM[,c("mcsid", "countryWeight", "benchmark_binary_welsh", "standardised_core_subjectsWelsh",
         "standardised_english", "standardised_maths", "standardised_science")]=0
predM[c("vocab.nvq1", "vocab.nvq2", "vocab.nvq3", "vocab.nvq4", "vocab.nvq5"), 
      c("highest_nvq", "age5_vocab")] = 0
predM[c("vocab.nvq1", "vocab.nvq2", "vocab.nvq3", "vocab.nvq4", "vocab.nvq5"), 
      c("vocab.nvq1", "vocab.nvq2", "vocab.nvq3", "vocab.nvq4", "vocab.nvq5")] = 0

predM[c("vocab.income2", "vocab.income3", "vocab.income4", "vocab.income5"), 
      c("oecd_income", "age5_vocab")] = 0
predM[c("vocab.income2", "vocab.income3", "vocab.income4", "vocab.income5"), 
      c("vocab.income2", "vocab.income3", "vocab.income4", "vocab.income5")] = 0

predM[c("vocab.occupation2", "vocab.occupation3", "vocab.occupation4"), 
      c("occupational_status", "age5_vocab")] = 0
predM[c("vocab.occupation2", "vocab.occupation3", "vocab.occupation4"), 
      c("vocab.occupation2", "vocab.occupation3", "vocab.occupation4")] = 0

predM[c("vocab.imd2", "vocab.imd3", "vocab.imd4", "vocab.nvq5", 
        "vocab.imd6", "vocab.imd7", "vocab.imd8", "vocab.imd9", "vocab.imd10"), 
      c("imd", "age5_vocab")] = 0
predM[c("vocab.imd2", "vocab.imd3", "vocab.imd4", "vocab.imd5", 
        "vocab.imd6", "vocab.imd7", "vocab.imd8", "vocab.imd9", "vocab.imd10"), 
      c("vocab.imd2", "vocab.imd3", "vocab.imd4", "vocab.imd5", 
        "vocab.imd6", "vocab.imd7", "vocab.imd8", "vocab.imd9", "vocab.imd10")] = 0
predM[c("benchmark_binary_welsh", "standardised_core_subjectsWelsh",
        "standardised_english", "standardised_maths", "standardised_science"), 
      c("standardised_core_subjects", "benchmark_binary")] = 0

vis = c("mcsid", "weight", "countryWeight", "sex","ethnicity","EAL","age_atBirth","housing_tenure","accommodation_type" , 
        "highest_nvq", "vocab.nvq1", "vocab.nvq2","vocab.nvq3","vocab.nvq4", "vocab.nvq5", "cm_breastfed","carers_in_hh",
        "oecd_income" , "vocab.income2","vocab.income3","vocab.income4","vocab.income5" ,
        "imd", "vocab.imd2","vocab.imd3","vocab.imd4","vocab.imd5","vocab.imd6" ,"vocab.imd7","vocab.imd8" ,"vocab.imd9",   
        "vocab.imd10","occupational_status", "vocab.occupation2","vocab.occupation3","vocab.occupation4",
        "mortgage","houseValue", "savings", "debt","country","caregiver_vocab",
        "age5_vocab","benchmark_binary", "standardised_core_subjects","benchmark_binary_welsh", "standardised_core_subjectsWelsh",
        "standardised_english", "standardised_maths", "standardised_science")
#now lets run the imputation (m=25) imputations

imputed_mcs2 = mice(vocabulary_education, 
                    blocks=blocksvec, method=meth, 
                    visitSequence = vis, seed = 1895, 
                    predictorMatrix=predM, m=25)  

#deriving post imputation variables####
long_format_mcs <- mice::complete(imputed_mcs2, "long", include=TRUE)

long_format_mcs$age5_standardised <- with(long_format_mcs, scale(age5_vocab, center=TRUE, scale=TRUE))
long_format_mcs$age5_standardised <- as.numeric(long_format_mcs$age5_standardised)

long_format_mcs$caregiver_vocabStandardised <- with(long_format_mcs, scale(caregiver_vocab, center=TRUE, scale=TRUE))
long_format_mcs$caregiver_vocabStandardised <- as.numeric(long_format_mcs$caregiver_vocabStandardised)

#add in standardised caregiver vocab

#deriving  wealth variable
long_format_mcs$housing_wealth <- with(long_format_mcs, houseValue - mortgage)
long_format_mcs$financial_wealth <- with(long_format_mcs, savings - debt)
long_format_mcs$net_wealth <- with(long_format_mcs, housing_wealth + financial_wealth)
long_format_mcs$standardised_wealth <- with(long_format_mcs, scale(net_wealth, center=TRUE, scale=TRUE))
long_format_mcs$standardised_wealth<- as.numeric(long_format_mcs$standardised_wealth)


long_format_mcs$highest_nvq <- with(long_format_mcs, relevel(highest_nvq, ref = "1"))
long_format_mcs$highest_nvq <- as.factor(long_format_mcs$highest_nvq)
long_format_mcs$occupational_status <- with(long_format_mcs, relevel(occupational_status, ref = "2"))
long_format_mcs$occupational_status <- as.factor(long_format_mcs$occupational_status)
long_format_mcs$success <- as.factor(long_format_mcs$benchmark_binary)
long_format_mcs$wealth_quintiles <- with(long_format_mcs, quantcut(standardised_wealth,5))
levels(long_format_mcs$wealth_quintiles)[1] = "1"
levels(long_format_mcs$wealth_quintiles)[2] = "2"
levels(long_format_mcs$wealth_quintiles)[3] = "3"
levels(long_format_mcs$wealth_quintiles)[4] = "4"
levels(long_format_mcs$wealth_quintiles)[5] = "5"
long_format_mcs$wealth_quintiles <- as.factor(long_format_mcs$wealth_quintiles)

#convert back to mids object.
imputed_mcs2<-as.mids(long_format_mcs)

#save mids object to working directory####

write.mice.imputation(mi.res=imputed_mcs2, name = glue("{today()}_vocabulary_education_imputedMAIN"),
                      include.varnames = TRUE, long=TRUE,dattype = "csv", mids2spss = FALSE)

#get each individual imputed dataset#####
imputed_mcs2_0 <- complete(imputed_mcs2)
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