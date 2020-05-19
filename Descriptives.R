library('dplyr')
library('ggplot2')
library('purrr')
library('tidyr')
library('huge')
library('foreign')
library('markdown')
library('summarytools')

setwd("C:/Users/kr/Google Drive/Pirus/Pirus_network_analysis")
dataDir <- "C:/Users/kr/Google Drive/Pirus/Pirus_network_analysis/Data_raw/"
dataPrDir <- "C:/Users/kr/Google Drive/Pirus/Pirus_network_analysis/Data_processed/"


imputed_pirus_df <- readRDS(file = paste0(dataPrDir, "pirus_df_imputed.RDS"))

#visualizing the distribution of continuous variables
imputed_pirus_df %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + geom_histogram() + scale_x_log10()

#Visualizing categorical variables
imputed_pirus_df %>%
  keep(is.factor) %>%
  gather() %>%
  ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + geom_bar()

#applying nonparanormal transformation to skewed continuous variables
imputed_pirus_df[, c("Age","Length_Group")]<-huge.npn(imputed_pirus_df[, c("Age","Length_Group")])

#splitting the data into violent and nonviolent cases
pirus_df_violent <- imputed_pirus_df[imputed_pirus_df$Violent == 1, ]
pirus_df_nonviolent <- imputed_pirus_df[imputed_pirus_df$Violent == 0, ]
imputed_pirus_df <- select(imputed_pirus_df, -c('Violent'))


#saving both files
saveRDS(pirus_df_nonviolent, file = paste0(dataPrDir, "nonviolent.RDS"))
saveRDS(pirus_df_violent, file = paste0(dataPrDir, "violent.RDS"))

