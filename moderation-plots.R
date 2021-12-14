#log odds
composite_logOdds <- summary(pool(as.mira(compositeModerator)),conf.int = TRUE, conf.level = 0.95) 
#plogis converts log odds to probabilities 
predicted_values =plogis(predict(compositeModerator[[1]], type = "terms"))


probabilities = exp(compositeModerator[[1]]$coefficients)/ (1+exp(compositeModerator[[1]]$coefficients))

ggpredict(compositeModerator[[1]], terms = c("age5_standardised", "ses_latent"))


predicted = lapply(compositeModerator, ggpredict, terms = c("age5_standardised", "ses_latent"))

composite_plot = plot(pool_predictions(predicted)) +
  scale_colour_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                      name="SEC Composite",
                      breaks=c("1", "0", "-1"),
                      labels=c("+1 SD", "Mean", "-1 SD"))+
  scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"), 
                      name="SEC Composite",
                      breaks=c("1", "0", "-1"),
                      labels=c("+1 SD", "Mean", "-1 SD")) +
  labs(x = "Age 5 Vocabulary Score \n (Standardised)", 
       y = "Achieving ≥ Grade 4 on Core Subjects ", 
       title = "Predicted probabilities of Successfully Passing Benchmark Threshold") +
  labs(colour = "SEC Composite") +
  theme_classic()
  

#nvq
nvq1 <- function(df) {
  fit <- glm(success ~ sex + ethnicity + EAL + country +
               + caregiver_vocabStandardised + highest_nvq*age5_standardised, 
             family = binomial, weights = weight, data=df)
  return(fit)
}
nvq_model <- lapply(list(imputed_data1, imputed_data2, imputed_data3, imputed_data4, imputed_data5,
                   imputed_data6, imputed_data7, imputed_data8, imputed_data9, imputed_data10,
                   imputed_data11, imputed_data12, imputed_data13, imputed_data14, imputed_data15,
                   imputed_data16, imputed_data17, imputed_data18, imputed_data19, imputed_data20,
                   imputed_data21, imputed_data22, imputed_data23, imputed_data24, imputed_data25),
              nvq1)
predicted_nvq = lapply(nvq_model, ggpredict, terms = c("age5_standardised", "highest_nvq"))

pool_predictions(predicted_nvq)


nvq_plot = plot(pool_predictions(predicted_nvq)) +
  scale_colour_manual(values=c( "#b8396b", "#ffd1d7", "#fff5cc", "#76bae0", "#b28f81", "#54483e"), 
                      name="Parent Education",
                      breaks=c("1", "2", "3", "4", "5", "0"),
                      labels=c("NVQ1 (ref)", "NVQ2", "NVQ3", "NVQ4", "NVQ5", "NVQ0"))+
  scale_fill_manual(values=c( "#b8396b", "#ffd1d7", "#fff5cc", "#76bae0", "#b28f81", "#54483e"), 
                    name="Parent Education",
                    breaks=c("1", "2", "3", "4", "5", "0"),
                    labels=c("NVQ1 (ref)", "NVQ2", "NVQ3", "NVQ4", "NVQ5", "NVQ0")) +
  labs(x = "Age 5 Vocabulary Score \n (Standardised)", 
       y = "Probability of Successfully Passing Benchmark Threshold", 
       title = "Predicted probabilities of Successfully Passing Benchmark Threshold") 



#income 

income1 <- function(df) {
  fit <- glm(success ~ sex + ethnicity + EAL + country +
               + caregiver_vocabStandardised + oecd_income*age5_standardised, 
             family = binomial, weights = weight, data=df)
  return(fit)
}
income_model <- lapply(list(imputed_data1, imputed_data2, imputed_data3, imputed_data4, imputed_data5,
                         imputed_data6, imputed_data7, imputed_data8, imputed_data9, imputed_data10,
                         imputed_data11, imputed_data12, imputed_data13, imputed_data14, imputed_data15,
                         imputed_data16, imputed_data17, imputed_data18, imputed_data19, imputed_data20,
                         imputed_data21, imputed_data22, imputed_data23, imputed_data24, imputed_data25),
                    income1)
predicted_income = lapply(income_model, ggpredict, terms = c("age5_standardised", "oecd_income"))

pool_predictions(predicted_income)


income_plot = plot(pool_predictions(predicted_income)) +
  scale_colour_manual(values=c( "#b8396b", "#ffd1d7", "#fff5cc", "#76bae0", "#b28f81"), 
                      name="OECD Income",
                      breaks=c("1", "2", "3", "4", "5"),
                      labels=c("Income Quintile 1 (ref)", "Income Quintile 2", "Income Quintile 3", "Income Quintile 4", "Income Quintile 5"))+
  scale_fill_manual(values=c( "#b8396b", "#ffd1d7", "#fff5cc", "#76bae0", "#b28f81"), 
                    name="OECD Income",
                    breaks=c("1", "2", "3", "4", "5"),
                    labels=c("Income Quintile 1 (ref)", "Income Quintile 2", "Income Quintile 3", "Income Quintile 4", "Income Quintile 5")) +
  labs(x = "Age 5 Vocabulary Score \n (Standardised)", 
       y = "Probability of Successfully Passing Benchmark Threshold", 
       title = "Predicted probabilities of Successfully Passing Benchmark Threshold") 

#wealth
wealth1 <- function(df) {
  fit <- glm(success ~ sex + ethnicity + EAL + country +
               + caregiver_vocabStandardised + wealth_quintiles*age5_standardised, 
             family = binomial, weights = weight, data=df)
  return(fit)
}
wealth_model <- lapply(list(imputed_data1, imputed_data2, imputed_data3, imputed_data4, imputed_data5,
                            imputed_data6, imputed_data7, imputed_data8, imputed_data9, imputed_data10,
                            imputed_data11, imputed_data12, imputed_data13, imputed_data14, imputed_data15,
                            imputed_data16, imputed_data17, imputed_data18, imputed_data19, imputed_data20,
                            imputed_data21, imputed_data22, imputed_data23, imputed_data24, imputed_data25),
                       wealth1)
predicted_wealth = lapply(wealth_model, ggpredict, terms = c("age5_standardised", "wealth_quintiles"))

pool_predictions(predicted_wealth)


wealth_plot = plot(pool_predictions(predicted_wealth)) +
  scale_colour_manual(values=c( "#b8396b", "#ffd1d7", "#fff5cc", "#76bae0", "#b28f81"), 
                      name="Wealth Quintiles",
                      breaks=c("1", "2", "3", "4", "5"),
                      labels=c("Wealth Quintile 1 (ref)", "Wealth Quintile 2", "Wealth Quintile 3", "Wealth Quintile 4", "Wealth Quintile 5"))+
  scale_fill_manual(values=c( "#b8396b", "#ffd1d7", "#fff5cc", "#76bae0", "#b28f81"), 
                    name="Wealth Quintiles",
                    breaks=c("1", "2", "3", "4", "5"),
                    labels=c("Wealth Quintile 1 (ref)", "Wealth Quintile 2", "Wealth Quintile 3", "Wealth Quintile 4", "Wealth Quintile 5")) +
  labs(x = "Age 5 Vocabulary Score \n (Standardised)", 
       y = "Probability of Successfully Passing Benchmark Threshold", 
       title = "Predicted probabilities of Successfully Passing Benchmark Threshold") 

#occupational status

occupation1 <- function(df) {
  fit <- glm(success ~ sex + ethnicity + EAL + country +
               + caregiver_vocabStandardised + occupational_status*age5_standardised, 
             family = binomial, weights = weight, data=df)
  return(fit)
}
occupation_model <- lapply(list(imputed_data1, imputed_data2, imputed_data3, imputed_data4, imputed_data5,
                            imputed_data6, imputed_data7, imputed_data8, imputed_data9, imputed_data10,
                            imputed_data11, imputed_data12, imputed_data13, imputed_data14, imputed_data15,
                            imputed_data16, imputed_data17, imputed_data18, imputed_data19, imputed_data20,
                            imputed_data21, imputed_data22, imputed_data23, imputed_data24, imputed_data25),
                       occupation1)
predicted_occupation = lapply(occupation_model, ggpredict, terms = c("age5_standardised", "occupational_status"))

pool_predictions(predicted_occupation)


occupation_plot = plot(pool_predictions(predicted_occupation)) +
  scale_colour_manual(values=c( "#b8396b", "#ffd1d7", "#fff5cc", "#76bae0"), 
                      name="Occupational Status",
                      breaks=c("2", "3", "4", "1"),
                      labels=c("Routine (ref)", "Intermediate", "Higher Managerial", "Unemployed"))+
  scale_fill_manual(values=c( "#b8396b", "#ffd1d7", "#fff5cc", "#76bae0"), 
                    name="Occupational Status",
                    breaks=c("2", "3", "4", "1"),
                    labels=c("Routine (ref)", "Intermediate", "Higher Managerial", "Unemployed")) +
  labs(x = "Age 5 Vocabulary Score \n (Standardised)", 
       y = "Probability of Successfully Passing Benchmark Threshold", 
       title = "Predicted probabilities of Successfully Passing Benchmark Threshold") 



#IMD 

#occupational status

imd1 <- function(df) {
  fit <- glm(success ~ sex + ethnicity + EAL + country +
               + caregiver_vocabStandardised + imd*age5_standardised, 
             family = binomial, weights = weight, data=df)
  return(fit)
}
imd_model <- lapply(list(imputed_data1, imputed_data2, imputed_data3, imputed_data4, imputed_data5,
                                imputed_data6, imputed_data7, imputed_data8, imputed_data9, imputed_data10,
                                imputed_data11, imputed_data12, imputed_data13, imputed_data14, imputed_data15,
                                imputed_data16, imputed_data17, imputed_data18, imputed_data19, imputed_data20,
                                imputed_data21, imputed_data22, imputed_data23, imputed_data24, imputed_data25),
                           imd1)
predicted_imd= lapply(imd_model, ggpredict, terms = c("age5_standardised", "imd"))

pool_predictions(predicted_imd)


imd_plot = plot(pool_predictions(predicted_imd)) +
  scale_colour_manual(values=c( "#9E0142", "#D53E4F", "#F46D43" ,"#FDAE61", "#FEE08B", 
                                "#E6F598", "#ABDDA4" ,"#66C2A5", "#3288BD", "#5E4FA2"), 
                      name="Relative Neighbourhood Deprivation",
                      labels=c("most deprived decile (ref)", "10 - <20%", "20 - <30%", "30 - <40%", "40 - <50%", 
                               "50 - <60%", "60 - <70%", "70 - <80%", "80 - <90%", "least deprived decile"))+
  scale_fill_manual(values=c( "#9E0142", "#D53E4F", "#F46D43" ,"#FDAE61", "#FEE08B", 
                              "#E6F598", "#ABDDA4" ,"#66C2A5", "#3288BD", "#5E4FA2"), 
                    name="Relative Neighbourhood Deprivation",
                    labels=c("most deprived decile (ref)", "10 - <20%", "20 - <30%", "30 - <40%", "40 - <50%", 
                             "50 - <60%", "60 - <70%", "70 - <80%", "80 - <90%", "least deprived decile")) +
  labs(x = "Age 5 Vocabulary Score \n (Standardised)", 
       y = "Probability of Successfully Passing Benchmark Threshold", 
       title = "Predicted probabilities of Successfully Passing Benchmark Threshold") 


#put plots all together 
plots = ggarrange(composite_plot, nvq_plot, income_plot, occupation_plot, wealth_plot, imd_plot, 
                  ncol =2, nrow=3,  align = c( "hv")
