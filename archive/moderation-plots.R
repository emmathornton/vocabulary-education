library(ggeffects)
library(mice)

#composite plot 

#add composite quintiles to data
#data 1
imputed_data1$composite_quintiles = quantcut(imputed_data1$ses_latent,5)
levels(imputed_data1$composite_quintiles)[1] = "1"
levels(imputed_data1$composite_quintiles)[2] = "2"
levels(imputed_data1$composite_quintiles)[3] = "3"
levels(imputed_data1$composite_quintiles)[4] = "4"
levels(imputed_data1$composite_quintiles)[5] = "5"
#data2
imputed_data2$composite_quintiles = quantcut(imputed_data2$ses_latent,5)
levels(imputed_data2$composite_quintiles)[1] = "1"
levels(imputed_data2$composite_quintiles)[2] = "2"
levels(imputed_data2$composite_quintiles)[3] = "3"
levels(imputed_data2$composite_quintiles)[4] = "4"
levels(imputed_data2$composite_quintiles)[5] = "5"
#data3
imputed_data3$composite_quintiles = quantcut(imputed_data3$ses_latent,5)
levels(imputed_data3$composite_quintiles)[1] = "1"
levels(imputed_data3$composite_quintiles)[2] = "2"
levels(imputed_data3$composite_quintiles)[3] = "3"
levels(imputed_data3$composite_quintiles)[4] = "4"
levels(imputed_data3$composite_quintiles)[5] = "5"
#dat4
imputed_data4$composite_quintiles = quantcut(imputed_data4$ses_latent,5)
levels(imputed_data4$composite_quintiles)[1] = "1"
levels(imputed_data4$composite_quintiles)[2] = "2"
levels(imputed_data4$composite_quintiles)[3] = "3"
levels(imputed_data4$composite_quintiles)[4] = "4"
levels(imputed_data4$composite_quintiles)[5] = "5"
#DATA5
imputed_data5$composite_quintiles = quantcut(imputed_data5$ses_latent,5)
levels(imputed_data5$composite_quintiles)[1] = "1"
levels(imputed_data5$composite_quintiles)[2] = "2"
levels(imputed_data5$composite_quintiles)[3] = "3"
levels(imputed_data5$composite_quintiles)[4] = "4"
levels(imputed_data5$composite_quintiles)[5] = "5"
#data6
imputed_data6$composite_quintiles = quantcut(imputed_data6$ses_latent,5)
levels(imputed_data6$composite_quintiles)[1] = "1"
levels(imputed_data6$composite_quintiles)[2] = "2"
levels(imputed_data6$composite_quintiles)[3] = "3"
levels(imputed_data6$composite_quintiles)[4] = "4"
levels(imputed_data6$composite_quintiles)[5] = "5"
#data7
imputed_data7$composite_quintiles = quantcut(imputed_data7$ses_latent,5)
levels(imputed_data7$composite_quintiles)[1] = "1"
levels(imputed_data7$composite_quintiles)[2] = "2"
levels(imputed_data7$composite_quintiles)[3] = "3"
levels(imputed_data7$composite_quintiles)[4] = "4"
levels(imputed_data7$composite_quintiles)[5] = "5"
#data 8
imputed_data8$composite_quintiles = quantcut(imputed_data8$ses_latent,5)
levels(imputed_data8$composite_quintiles)[1] = "1"
levels(imputed_data8$composite_quintiles)[2] = "2"
levels(imputed_data8$composite_quintiles)[3] = "3"
levels(imputed_data8$composite_quintiles)[4] = "4"
levels(imputed_data8$composite_quintiles)[5] = "5"
#data 9 
imputed_data9$composite_quintiles = quantcut(imputed_data9$ses_latent,5)
levels(imputed_data9$composite_quintiles)[1] = "1"
levels(imputed_data9$composite_quintiles)[2] = "2"
levels(imputed_data9$composite_quintiles)[3] = "3"
levels(imputed_data9$composite_quintiles)[4] = "4"
levels(imputed_data9$composite_quintiles)[5] = "5"
#data10
imputed_data10$composite_quintiles = quantcut(imputed_data10$ses_latent,5)
levels(imputed_data10$composite_quintiles)[1] = "1"
levels(imputed_data10$composite_quintiles)[2] = "2"
levels(imputed_data10$composite_quintiles)[3] = "3"
levels(imputed_data10$composite_quintiles)[4] = "4"
levels(imputed_data10$composite_quintiles)[5] = "5"
#data11
imputed_data11$composite_quintiles = quantcut(imputed_data11$ses_latent,5)
levels(imputed_data11$composite_quintiles)[1] = "1"
levels(imputed_data11$composite_quintiles)[2] = "2"
levels(imputed_data11$composite_quintiles)[3] = "3"
levels(imputed_data11$composite_quintiles)[4] = "4"
levels(imputed_data11$composite_quintiles)[5] = "5"
#data12
imputed_data12$composite_quintiles = quantcut(imputed_data12$ses_latent,5)
levels(imputed_data12$composite_quintiles)[1] = "1"
levels(imputed_data12$composite_quintiles)[2] = "2"
levels(imputed_data12$composite_quintiles)[3] = "3"
levels(imputed_data12$composite_quintiles)[4] = "4"
levels(imputed_data12$composite_quintiles)[5] = "5"
#data13
imputed_data13$composite_quintiles = quantcut(imputed_data13$ses_latent,5)
levels(imputed_data13$composite_quintiles)[1] = "1"
levels(imputed_data13$composite_quintiles)[2] = "2"
levels(imputed_data13$composite_quintiles)[3] = "3"
levels(imputed_data13$composite_quintiles)[4] = "4"
levels(imputed_data13$composite_quintiles)[5] = "5"
#data14
imputed_data14$composite_quintiles = quantcut(imputed_data14$ses_latent,5)
levels(imputed_data14$composite_quintiles)[1] = "1"
levels(imputed_data14$composite_quintiles)[2] = "2"
levels(imputed_data14$composite_quintiles)[3] = "3"
levels(imputed_data14$composite_quintiles)[4] = "4"
levels(imputed_data14$composite_quintiles)[5] = "5"
#data15
imputed_data15$composite_quintiles = quantcut(imputed_data15$ses_latent,5)
levels(imputed_data15$composite_quintiles)[1] = "1"
levels(imputed_data15$composite_quintiles)[2] = "2"
levels(imputed_data15$composite_quintiles)[3] = "3"
levels(imputed_data15$composite_quintiles)[4] = "4"
levels(imputed_data15$composite_quintiles)[5] = "5"
#data16
imputed_data16$composite_quintiles = quantcut(imputed_data16$ses_latent,5)
levels(imputed_data16$composite_quintiles)[1] = "1"
levels(imputed_data16$composite_quintiles)[2] = "2"
levels(imputed_data16$composite_quintiles)[3] = "3"
levels(imputed_data16$composite_quintiles)[4] = "4"
levels(imputed_data16$composite_quintiles)[5] = "5"
#data 17
imputed_data17$composite_quintiles = quantcut(imputed_data17$ses_latent,5)
levels(imputed_data17$composite_quintiles)[1] = "1"
levels(imputed_data17$composite_quintiles)[2] = "2"
levels(imputed_data17$composite_quintiles)[3] = "3"
levels(imputed_data17$composite_quintiles)[4] = "4"
levels(imputed_data17$composite_quintiles)[5] = "5"
#data18
imputed_data18$composite_quintiles = quantcut(imputed_data18$ses_latent,5)
levels(imputed_data18$composite_quintiles)[1] = "1"
levels(imputed_data18$composite_quintiles)[2] = "2"
levels(imputed_data18$composite_quintiles)[3] = "3"
levels(imputed_data18$composite_quintiles)[4] = "4"
levels(imputed_data18$composite_quintiles)[5] = "5"
#data 19
imputed_data19$composite_quintiles = quantcut(imputed_data19$ses_latent,5)
levels(imputed_data19$composite_quintiles)[1] = "1"
levels(imputed_data19$composite_quintiles)[2] = "2"
levels(imputed_data19$composite_quintiles)[3] = "3"
levels(imputed_data19$composite_quintiles)[4] = "4"
levels(imputed_data19$composite_quintiles)[5] = "5"
#data 20
imputed_data20$composite_quintiles = quantcut(imputed_data20$ses_latent,5)
levels(imputed_data20$composite_quintiles)[1] = "1"
levels(imputed_data20$composite_quintiles)[2] = "2"
levels(imputed_data20$composite_quintiles)[3] = "3"
levels(imputed_data20$composite_quintiles)[4] = "4"
levels(imputed_data20$composite_quintiles)[5] = "5"
#data 21
imputed_data21$composite_quintiles = quantcut(imputed_data21$ses_latent,5)
levels(imputed_data21$composite_quintiles)[1] = "1"
levels(imputed_data21$composite_quintiles)[2] = "2"
levels(imputed_data21$composite_quintiles)[3] = "3"
levels(imputed_data21$composite_quintiles)[4] = "4"
levels(imputed_data21$composite_quintiles)[5] = "5"
#data22
imputed_data22$composite_quintiles = quantcut(imputed_data22$ses_latent,5)
levels(imputed_data22$composite_quintiles)[1] = "1"
levels(imputed_data22$composite_quintiles)[2] = "2"
levels(imputed_data22$composite_quintiles)[3] = "3"
levels(imputed_data22$composite_quintiles)[4] = "4"
levels(imputed_data22$composite_quintiles)[5] = "5"
#data 23
imputed_data23$composite_quintiles = quantcut(imputed_data23$ses_latent,5)
levels(imputed_data23$composite_quintiles)[1] = "1"
levels(imputed_data23$composite_quintiles)[2] = "2"
levels(imputed_data23$composite_quintiles)[3] = "3"
levels(imputed_data23$composite_quintiles)[4] = "4"
levels(imputed_data23$composite_quintiles)[5] = "5"
#data 24
imputed_data24$composite_quintiles = quantcut(imputed_data24$ses_latent,5)
levels(imputed_data24$composite_quintiles)[1] = "1"
levels(imputed_data24$composite_quintiles)[2] = "2"
levels(imputed_data24$composite_quintiles)[3] = "3"
levels(imputed_data24$composite_quintiles)[4] = "4"
levels(imputed_data24$composite_quintiles)[5] = "5"
#data 25
imputed_data25$composite_quintiles = quantcut(imputed_data25$ses_latent,5)
levels(imputed_data25$composite_quintiles)[1] = "1"
levels(imputed_data25$composite_quintiles)[2] = "2"
levels(imputed_data25$composite_quintiles)[3] = "3"
levels(imputed_data25$composite_quintiles)[4] = "4"
levels(imputed_data25$composite_quintiles)[5] = "5"

#regression model
compositeQuintiles_model <- function(df) {
  fit <- glm(success ~ sex + ethnicity + EAL + country +
               + caregiver_vocabStandardised + composite_quintiles*age5_standardised, 
             family = binomial, weights = weight, data=df)
  return(fit)
}


compositeQuintiles<- lapply(list(imputed_data1, imputed_data2, imputed_data3, imputed_data4, imputed_data5,
                                 imputed_data6, imputed_data7, imputed_data8, imputed_data9, imputed_data10,
                                 imputed_data11, imputed_data12, imputed_data13, imputed_data14, imputed_data15,
                                 imputed_data16, imputed_data17, imputed_data18, imputed_data19, imputed_data20,
                                 imputed_data21, imputed_data22, imputed_data23, imputed_data24, imputed_data25),
                            compositeQuintiles_model)

#get probabilities
composite_quintiles = lapply(compositeQuintiles, ggpredict, terms = c("age5_standardised[all]", "composite_quintiles"))
#plot 
 
compositeQuintiles_plot = plot(pool_predictions(composite_quintiles), ci = FALSE,
                               limits = c(0, 1), 
                               breaks = seq(0, 1, by = 0.1)) +
  scale_colour_manual(values=c("#054a91", "#6596c1","#9dbcd7" , "#f6a55c", "#f17300"), 
                      name="SEC Composite", 
                      breaks=c("1", "2", "3", "4", "5"),
                      labels=c("Lowest Quintile", "Quintile 2", "Quintile 3", "Quintile 4", "Highest Quintile"))+
  scale_fill_manual(values=c("#054a91", "#6596c1","#9dbcd7" , "#f6a55c", "#f17300"), 
                    name="SEC Composite",
                    breaks=c("1", "2", "3", "4", "5"),
                    labels=c("Lowest Quintile", "Quintile 2", "Quintile 3", "Quintile 4", "Highest Quintile")) +
  labs(x = "Age 5 Vocabulary Score \n (Standardised)", 
       y = "Achieving ≥ Grade 4 on Core Subjects ", 
       title = "SEC Composite ") +
  labs(colour = "SEC Composite") +
  theme_classic() +
  scale_x_continuous(breaks=seq(-3, 4, .5)) +
  theme(panel.grid.major.y = element_line(colour = "gainsboro", size = 0.2),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        plot.title = element_text(size = 10, face = "italic"))


  

#nvq
nvq1 <- function(df) {
  fit <- glm(success ~ sex + ethnicity + EAL + country +
               oecd_income + occupational_status + wealth_quintiles + imd + 
               caregiver_vocabStandardised + highest_nvq*age5_standardised, 
             family = binomial, weights = weight, data=df)
  return(fit)
}
nvq_model <- lapply(list(imputed_data1, imputed_data2, imputed_data3, imputed_data4, imputed_data5,
                   imputed_data6, imputed_data7, imputed_data8, imputed_data9, imputed_data10,
                   imputed_data11, imputed_data12, imputed_data13, imputed_data14, imputed_data15,
                   imputed_data16, imputed_data17, imputed_data18, imputed_data19, imputed_data20,
                   imputed_data21, imputed_data22, imputed_data23, imputed_data24, imputed_data25),
              nvq1)
predicted_nvq = lapply(nvq_model, ggpredict, terms = c("age5_standardised[all]", "highest_nvq"))

pool_predictions(predicted_nvq)


nvq_plot = plot(pool_predictions(predicted_nvq), ci = FALSE,
                limits = c(0, 1), 
                breaks = seq(0, 1, by = 0.1)) +
  scale_colour_manual(values=c( "#054a91", "#6596c1", "#9dbcd7", "#f8b77d", "#f48f33", "#f17300"), 
                      name="Parent Education",
                      breaks=c("1", "0", "2", "3", "4", "5"),
                      labels=c("NVQ1 (lowest qualifications, ref)", "No/ overseas qualifications", "NVQ2", "NVQ3", "NVQ4", "NVQ5"))+
  scale_fill_manual(values=c( "#054a91", "#6596c1", "#9dbcd7", "#f8b77d", "#f48f33", "#f17300"), 
                    name="Parent Education",
                    breaks=c("1", "0", "2", "3", "4", "5"),
                    labels=c("NVQ1 (lowest qualifications,ref)","No/ overseas qualifications", "NVQ2", "NVQ3", "NVQ4", "NVQ5")) +
  labs(x = "Age 5 Vocabulary Score \n (Standardised)", 
       y = "Achieving ≥ Grade 4 on Core Subjects ", 
       title = "Parent Education") +
  theme_classic() +
  scale_x_continuous(breaks=seq(-3, 4, .5)) +
  theme(panel.grid.major.y = element_line(colour = "gainsboro", size = 0.2),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        plot.title = element_text(size = 10, face = "italic"))



#income 

income1 <- function(df) {
  fit <- glm(success ~ sex + ethnicity + EAL + country +
               highest_nvq+ occupational_status  + wealth_quintiles + imd + 
               caregiver_vocabStandardised + oecd_income*age5_standardised, 
             family = binomial, weights = weight, data=df)
  return(fit)
}
income_model <- lapply(list(imputed_data1, imputed_data2, imputed_data3, imputed_data4, imputed_data5,
                         imputed_data6, imputed_data7, imputed_data8, imputed_data9, imputed_data10,
                         imputed_data11, imputed_data12, imputed_data13, imputed_data14, imputed_data15,
                         imputed_data16, imputed_data17, imputed_data18, imputed_data19, imputed_data20,
                         imputed_data21, imputed_data22, imputed_data23, imputed_data24, imputed_data25),
                    income1)
predicted_income = lapply(income_model, ggpredict, terms = c("age5_standardised[all]", "oecd_income"))

pool_predictions(predicted_income)


income_plot = plot(pool_predictions(predicted_income), ci = FALSE,
                   limits = c(0, 1), 
                   breaks = seq(0, 1, by = 0.1)) +
  scale_colour_manual(values=c("#054a91", "#6596c1","#9dbcd7" , "#f6a55c", "#f17300"), 
                      name="OECD Income",
                      breaks=c("1", "2", "3", "4", "5"),
                      labels=c("Lowest Quintile (ref)", "Quintile 2", "Quintile 3", "Quintile 4", "Highest Quintile"))+
  scale_fill_manual(values=c( "#054a91", "#6596c1","#9dbcd7" , "#f6a55c", "#f17300"), 
                    name="OECD Income",
                    breaks=c("1", "2", "3", "4", "5"),
                    labels=c("Lowest Quintile (ref)", "Quintile 2", "Quintile 3", "Quintile 4", "Highest Quintile")) +
  labs(x = "Age 5 Vocabulary Score \n (Standardised)", 
       y = "Achieving ≥ Grade 4 on Core Subjects ", 
       title = "Household Income  ") +
  theme_classic() +
  scale_x_continuous(breaks=seq(-3, 4, .5)) +
  theme(panel.grid.major.y = element_line(colour = "gainsboro", size = 0.2),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        plot.title = element_text(size = 10, face = "italic"))

#wealth
wealth1 <- function(df) {
  fit <- glm(success ~ sex + ethnicity + EAL + country +
               highest_nvq+ oecd_income + occupational_status   + imd + 
               caregiver_vocabStandardised + wealth_quintiles*age5_standardised, 
             family = binomial, weights = weight, data=df)
  return(fit)
}
wealth_model <- lapply(list(imputed_data1, imputed_data2, imputed_data3, imputed_data4, imputed_data5,
                            imputed_data6, imputed_data7, imputed_data8, imputed_data9, imputed_data10,
                            imputed_data11, imputed_data12, imputed_data13, imputed_data14, imputed_data15,
                            imputed_data16, imputed_data17, imputed_data18, imputed_data19, imputed_data20,
                            imputed_data21, imputed_data22, imputed_data23, imputed_data24, imputed_data25),
                       wealth1)
predicted_wealth = lapply(wealth_model, ggpredict, terms = c("age5_standardised[all]", "wealth_quintiles"))

pool_predictions(predicted_wealth)


wealth_plot = plot(pool_predictions(predicted_wealth), ci = FALSE,
                   limits = c(0, 1), 
                   breaks = seq(0, 1, by = 0.1)) +
  scale_colour_manual(values=c( "#054a91", "#6596c1","#9dbcd7" , "#f6a55c", "#f17300"), 
                      name="Wealth Quintiles",
                      breaks=c("1", "2", "3", "4", "5"),
                      labels=c("Lowest Quintile (ref)", "Quintile 2", "Quintile 3", "Quintile 4", "Highest Quintile"))+
  scale_fill_manual(values=c( "#054a91", "#6596c1","#9dbcd7" , "#f6a55c", "#f17300"), 
                    name="Wealth Quintiles",
                    breaks=c("1", "2", "3", "4", "5"),
                    labels=c("Lowest Quintile (ref)", "Quintile 2", "Quintile 3", "Quintile 4", "Highest Quintile")) +
  labs(x = "Age 5 Vocabulary Score \n (Standardised)", 
       y = "Achieving ≥ Grade 4 on Core Subjects ", 
       title = "Household Wealth ") +
  theme_classic() +
  scale_x_continuous(breaks=seq(-3, 4, .5)) +
  theme(panel.grid.major.y = element_line(colour = "gainsboro", size = 0.2),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        plot.title = element_text(size = 10, face = "italic"))

#occupational status

occupation1 <- function(df) {
  fit <- glm(success ~ sex + ethnicity + EAL + country +
               highest_nvq + oecd_income  + wealth_quintiles + imd + 
               caregiver_vocabStandardised + occupational_status + age5_standardised, 
             family = binomial, weights = weight, data=df)
  return(fit)
}
occupation_model <- lapply(list(imputed_data1, imputed_data2, imputed_data3, imputed_data4, imputed_data5,
                            imputed_data6, imputed_data7, imputed_data8, imputed_data9, imputed_data10,
                            imputed_data11, imputed_data12, imputed_data13, imputed_data14, imputed_data15,
                            imputed_data16, imputed_data17, imputed_data18, imputed_data19, imputed_data20,
                            imputed_data21, imputed_data22, imputed_data23, imputed_data24, imputed_data25),
                       occupation1)
predicted_occupation = lapply(occupation_model, ggpredict, terms = c("age5_standardised[all]", "occupational_status"))

pool_predictions(predicted_occupation)


occupation_plot = plot(pool_predictions(predicted_occupation), ci = FALSE,
                       limits = c(0, 1), 
                       breaks = seq(0, 1, by = 0.1)) +
  scale_colour_manual(values=c( "#054a91", "#6596c1", "#f6a55c", "#f17300"), 
                      name="Occupational Status",
                      breaks=c("2", "1", "3", "4"),
                      labels=c("Routine (most deprived, ref)", "Unemployed", "Intermediate", "Higher Managerial (Least Deprived)"))+
  scale_fill_manual(values=c( "#054a91", "#6596c1", "#f6a55c", "#f17300"), 
                    name="Occupational Status",
                    breaks=c("2", "1", "4", "3"),
                    labels=c("Routine (most deprived, ref)", "Unemployed", "Intermediate", "Higher Managerial (Least Deprived)")) +
  labs(x = "Age 5 Vocabulary Score \n (Standardised)", 
       y = "Achieving ≥ Grade 4 on Core Subjects ", 
       title = "Parent Occupational Status") +
  theme_classic() +
  scale_x_continuous(breaks=seq(-3, 4, .5)) +
  theme(panel.grid.major.y = element_line(colour = "gainsboro", size = 0.2),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        plot.title = element_text(size = 10, face = "italic"))



#IMD 


imd1 <- function(df) {
  fit <- glm(success ~ sex + ethnicity + EAL + country +
               highest_nvq + oecd_income + occupational_status  + wealth_quintiles + 
               caregiver_vocabStandardised + imd + age5_standardised, 
             family = binomial, weights = weight, data=df)
  return(fit)
}
imd_model <- lapply(list(imputed_data1, imputed_data2, imputed_data3, imputed_data4, imputed_data5,
                                imputed_data6, imputed_data7, imputed_data8, imputed_data9, imputed_data10,
                                imputed_data11, imputed_data12, imputed_data13, imputed_data14, imputed_data15,
                                imputed_data16, imputed_data17, imputed_data18, imputed_data19, imputed_data20,
                                imputed_data21, imputed_data22, imputed_data23, imputed_data24, imputed_data25),
                           imd1)
predicted_imd= lapply(imd_model, ggpredict, terms = c("age5_standardised[all]", "imd"))

pool_predictions(predicted_imd)


imd_plot = plot(pool_predictions(predicted_imd), ci = FALSE, 
                limits = c(0, 1), 
                breaks = seq(0, 1, by = 0.1)) +
  scale_colour_manual(values=c( "#054a91", "#3e7cb1", "#6596c1" ,"#84abcd", "#9dbcd7", 
                                "#f9c597", "#f8b77d" ,"#f6a55c", "#f48f33", "#f17300"), 
                      name="Relative Neighbourhood Deprivation",
                      labels=c("Most Deprived (ref)", "10 - <20%", "20 - <30%", "30 - <40%", "40 - <50%", 
                               "50 - <60%", "60 - <70%", "70 - <80%", "80 - <90%", "Least Deprived"))+
  scale_fill_manual(values=c(  "#054a91", "#3e7cb1", "#6596c1" ,"#84abcd", "#9dbcd7", 
                               "#f9c597", "#f8b77d" ,"#f6a55c", "#f48f33", "#f17300"), 
                    name="Relative Neighbourhood Deprivation",
                    labels=c("Most Deprived (ref)", "10 - <20%", "20 - <30%", "30 - <40%", "40 - <50%", 
                             "50 - <60%", "60 - <70%", "70 - <80%", "80 - <90%", "Least Deprived")) +
  labs(x = "Age 5 Vocabulary Score \n (Standardised)", 
       y = "Achieving ≥ Grade 4 on Core Subjects ", 
       title = "Relative Neighbourhood Deprivation") +
  theme_classic() +
  scale_x_continuous(breaks=seq(-3, 4, .5)) +
  theme(panel.grid.major.y = element_line(colour = "gainsboro", size = 0.2),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(), 
        plot.title = element_text(size = 10, face = "italic"))


#put plots all together 
plots = ggarrange(compositeQuintiles_plot, nvq_plot, income_plot, occupation_plot, wealth_plot, imd_plot, 
                  ncol =2, nrow=3,  align = c( "hv"))

plots = annotate_figure(plots, top = text_grob("Predicted Probabilities of Achieving the Benchmark Threshold for Vocabulary Moderated by each SEC Indicator", 
                                                      color = "black", face = "italic", size = 14))
