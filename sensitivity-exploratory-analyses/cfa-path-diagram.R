# Libraries 
library(mice)
library(semTools)
library(lavaan)
#library(semPlot)
library(lavaanPlot)
library(ggplot2)
library(lavaan.mi)

#factor analysis model on imputed dataset
#load imputed dataset
load("~/Documents/updated MCS datasets/education-datasetsDec22/2023-01-20_vocabulary_education_imputedMAIN/2023-01-20_vocabulary_education_imputedMAIN.Rdata")
imputed_mcs2 = mi.res
#specify model 
SEP_model <- 'SEP =~ highest_nvq + oecd_income + wealth_quintiles + occupational_status + imd'

sepCFA.mi = cfa.mi(
  SEP_model, 
  data = imputed_mcs2,
  ordered = c("highest_nvq", "oecd_income","wealth_quintiles", "occupational_status", "imd"), 
  std.lv=TRUE, 
  estimator="WLSMV")



#model fit statistics 
#normed chi square
#SRMR
#CFI
#TLI
#RMSEA

summary(sepCFA.mi, fit.measures = TRUE)

#modification indices
modificationindices.mi(sepCFA.mi, sort = TRUE)

sep_revised = 'SEP =~ highest_nvq + oecd_income + wealth_quintiles + occupational_status + imd
highest_nvq ~~ occupational_status'

sepCFA.mi_2 = cfa.mi(
  sep_revised, 
  data = imputed_mcs2,
  ordered = c("highest_nvq", "oecd_income","wealth_quintiles", "occupational_status", "imd"), 
  std.lv=TRUE, 
  estimator="WLSMV")

summary(sepCFA.mi_2, fit.measures = TRUE)


#SEM path diagram. 
#substitute parameter estimates from MI model into a semPlotModel object, created from a lavaan object


# Create a 'dummy' object, with the same model structure as we'll use in the MI method 
#create single imputed dataset for this
imputed_mcs2_1 <- complete(imputed_mcs2,1)
SEP1 <- cfa(SEP_model, imputed_mcs2_1, 
            ordered = c("highest_nvq", "oecd_income","wealth_quintiles", "occupational_status", "imd"), 
            std.lv=TRUE, 
            estimator="WLSMV")

# Extract the direct results from the SEM with MI data 
desired_output <- data.frame(standardizedsolution(sepCFA.mi_2))



#dummy lavaanPlot
# Subsitute the desired parameter estimates for the desired ones, in the dummy data 
SEP1@ParTable$est <- desired_output$est.std

#lavaan plot
labels <- list(SEP = "Socioeconomic \n Circumstances", highest_nvq = "Parent \n Education", oecd_income = "Income",
               wealth_quintiles = "Wealth", occupational_status = "Occupational \n Status", 
               imd = "Neighbourhood \n Deprivation")
lavaanPlot(model = SEP1, labels=labels, 
           graph_options= list(overlap=F, fontsize="12"),
           node_options = list(shape = "box", fontname = "Times"), 
           edge_options = list(color = "grey"), coefs = T, stars="latent")





