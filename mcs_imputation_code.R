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

#open mcs data####
vocabulary_education<-read.csv("education_data.csv")
vocabulary_education[,1]<- NULL

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


#Now let specify the methods for imputing the missing values. 
#There are specific methods for continuous, binary and ordinal variables. 
#I set different methods for each variable. You can add more than one variable in each methods.

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
vocabulary_education$country=as.factor(vocabulary_education$country)
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
meth[c("benchmark_binary")]="logreg"
meth[c("standardised_core_subjects")]="cart" 

#now lets run the imputation (m=20) imputations

blocksvec=names(meth)

# predM=0 --> variable not used to form imputation 
predM = predM[blocksvec,]
predM[,c("mcsid")]=0


imputed_mcs2 = mice(vocabulary_education, blocks=blocksvec, method=meth, seed = 1895, predictorMatrix=predM, m=25) #can change this to a smaller numebr so runs quicker when figuring out. 

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

write.mice.imputation(mi.res=imputed_mcs2, name = glue("{today()}_vocabulary_education_imputedRQ1"), long=TRUE,dattype = "csv")

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