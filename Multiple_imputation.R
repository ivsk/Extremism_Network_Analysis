#Syntax for the imputation process. Multiple imputation with chained equations (MICE) was used with m = 30 due to high rates of missing values.

library('dplyr')
library('mice')



setwd("~/Extremism_network_analysis")
dataPrDir <- "~/Extremism_network_analysis/Data_processed/"


pirus_for_imputation <- readRDS(file = paste0(dataPrDir, "pirus_df_for_imputation2.RDS"))

#Differentiating categorical and ordinal variables
catcols <- c('Violent', 'Broad_Ethnicity' ,
             'Gender', 'Abused',
             "Education",
             "Work_History",
             "Military",
             "Trauma",
             "Standing",'Angry_US',
             "Relationship_Troubles",'Group_Membership','Group_Grievance',
             "Platonic_Troubles",'Role_Group','Psychological','Marital_Status',
             "Kicked_Out", 'Employment_Status','Immigrant_Generation','Social_Media')
ordcols <- c("Social_Stratum_Adulthood","Previous_Criminal_Activity",
             "Radical_Friend",
             "Radical_Family",'Radical_Behaviors',
             "Radical_Signif_Other","Internet_Radicalization")

pirus_for_imputation[catcols] <- lapply(pirus_for_imputation[catcols], factor)
pirus_for_imputation[ordcols] <- lapply(pirus_for_imputation[ordcols], ordered)

#Initializing the MICE algorithm and defining the predictor matrix.
init = mice(pirus_for_imputation, maxit=0, nnet.MaxNWts = 6000) 
meth = init$method
predM = init$predictorMatrix

meth[c('Violent', 
       'Gender',
       'Abused')] = ""
meth[c("Education",'Role_Group','Marital_Status',
       "Work_History", 'Broad_Ethnicity', 'Social_Media')] = "polyreg"
meth[c("Social_Stratum_Adulthood",
       "Radical_Friend","Previous_Criminal_Activity","Internet_Radicalization",
       "Radical_Family",
       "Radical_Signif_Other",'Radical_Behaviors')] = "polr"
meth[c("Military","Immigrant_Generation","Employment_Status","Relationship_Troubles",
       "Platonic_Troubles", 'Psychological','Group_Grievance','Angry_US',
       "Kicked_Out","Standing", "Trauma")] = "logreg"
meth[c("Age",
       "Length_Group")] = "pmm"
predM <- quickpred(pirus_for_imputation)
table(rowSums(predM))

#
imputed <- pirus_for_imputation %>%
  mice(seed = 123, m = 30, maxit = 20, method=meth, predictorMatrix=predM, nnet.MaxNWts = 6000)

#Selecting one imputed dataset randomly for the main analysis.
pirus_df_imputed <- complete(imputed, sample(1:30,1))

#Saving the imputed dataset
saveRDS(pirus_df_imputed, file = paste0(dataPrDir, "pirus_df_imputed.RDS"))


