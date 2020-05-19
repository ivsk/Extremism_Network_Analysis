library('ggplot2')
library('readxl')
library('dplyr')
library('magrittr')
library('purrr')
library('tidyr')
library('moments')


setwd("~/Extremism_network_analysis")
dataDir <- "C:/Extremism_network_analysis/Data_raw/"
dataPrDir <- "C:/Extremism_network_analysis/Data_processed/"

#Loading the data
pirus <- list.files(dataDir);pirus
pirus_df <- lapply(paste0(dataDir, pirus), read_excel, na = "-99")
pirus_df <- bind_cols(pirus_df)

#Selecting variables of interest
variables = c('Violent', 'Employment_Status','Broad_Ethnicity', 'Work_History','Aspirations','Social_Stratum_Adulthood',
             'Relationship_Troubles','Platonic_Troubles', 'Standing','Kicked_Out','Trauma','Radical_Friend', 
             'Radical_Family', 'Radical_Signif_Other', 'Age', 'Gender', 'Education','Immigrant_Generation','Military',
             'Previous_Criminal_Activity', 'Abuse_Child', 'Abuse_Adult', 'Role_Group','Length_Group','Psychological','Marital_Status','Radical_Behaviors',
             'Group_Membership','Social_Media', 'Group_Grievance','Angry_US', 'Internet_Radicalization')

pirus_df <- subset(pirus_df, select = variables)

#Recoding variables
pirus_df$Abuse_Adult <- recode(pirus_df$Abuse_Adult, "0" = 0, "1" = 1, "2" = 1, "3" = 1, "-88" = 0)
pirus_df$Abuse_Child <- recode(pirus_df$Abuse_Child, "0" = 0, "1" = 1, "2" = 1, "3" = 1)
pirus_df$Broad_Ethnicity <- recode(pirus_df$Broad_Ethnicity, "3" = 0, "1" = 1, "2" = 1, "4" = 1, "5"= 1, "6" = 1, "7" = 1)
pirus_df$Education <- recode(pirus_df$Education, "1"=0, "2"=1, "3"=1, "4"=2,"5"=2,"6"=2, "7"=2, "8"= 2, "9" = 2, "10" = 2, "11" = 2)
pirus_df$Employment_Status <- recode(pirus_df$Employment_Status, "1"=0, "2"=0, "3"=1, "4"=1,"5"=0,"6"=0, "-88" = NA_real_) #recode employment status
pirus_df$Gender <- pirus_df$Gender -1 
pirus_df$Group_Grievance <- recode(pirus_df$Group_Grievance,  "0" = 0, "1" = 1, "2" =1, "3"=1)
pirus_df$Immigrant_Generation <- recode(pirus_df$Immigrant_Generation, "2" = 1)
pirus_df$Internet_Radicalization <- recode(pirus_df$Internet_Radicalization, "-88"= 0)
pirus_df$Length_Group <- recode(pirus_df$Length_Group, "-88" = 0)
pirus_df$Marital_Status <- recode(pirus_df$Marital_Status , "1" = 0, "2"=1, "3" = 2, "4" =2)
pirus_df$Military <- recode(pirus_df$Military, "0" = 0, "1" =1, "2" = 1, "3" = 1, "4" = 1, "5" = 1, "6" =1)
pirus_df$Psychological <- recode(pirus_df$Psychological, "0" = 0, "1" = 1, "2" = 1)
pirus_df$Radical_Signif_Other <- recode(pirus_df$Radical_Signif_Other, "-88" = 0)
pirus_df$Role_Group <- recode(pirus_df$Role_Group, "-88" = 0)
pirus_df$Social_Media <- recode(pirus_df$Social_Media, "-88" = 0)
pirus_df$Social_Stratum_Adulthood <- recode(pirus_df$Social_Stratum_Adulthood, "-88" = NA_real_)
pirus_df$Standing <- recode(pirus_df$Standing, "2" = 1, "3" = 1)
pirus_df$Trauma <- recode(pirus_df$Trauma, "2" = 1, "3" = 1)
pirus_df$Work_History <- recode(pirus_df$Work_History, "-88" = NA_real_)

pirus_df$Abused <- ifelse(pirus_df$Abuse_Adult == 1 | pirus_df$Abuse_Child == 1, 1, 0)
pirus_df <- select(pirus_df, -c('Abuse_Child', 'Abuse_Adult'))

#Saving
saveRDS(pirus_df, file = paste0(dataPrDir, "pirus_df_for_imputation.RDS"))
