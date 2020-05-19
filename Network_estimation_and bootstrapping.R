library('mgm')
library('qgraph')
library('bootnet')
library('NetworkComparisonTest')
library('OperMx')


#Loading the files
setwd("~/Extremism_network_analysis")
dataPrDir <- "~/Extremism_network_analysis/Data_processed/"

violent <- readRDS(file = paste0(dataPrDir, "violent.RDS"))
nonviolent <- readRDS(file = paste0(dataPrDir, "nonviolent.RDS"))

violent <- select(violent, -c('Radical_Behaviors', 'Education', 'Abused', 'Social_Media','Group_Membership', 'Broad_Ethnicity'))
nonviolent <- select(nonviolent, -c('Radical_Behaviors', 'Education', 'Abused', 'Social_Media','Group_Membership', 'Broad_Ethnicity'))

#Initializing the network models. I used Mixed Graphical Model for estimating the network since the dataset contains both categorical and continuous variables.
violent_dat <- list("data" = NULL, 
                    "type" = NULL, 
                    "names" = NULL,
                    "labels" = NULL,
                    "grouplabels"= NULL)

violent_dat$data <- as.matrix(sapply(violent, as.numeric))
violent_dat$type <-  c(rep("c", 11), "g", rep("c", 5), "g", rep("c",5))
violent_dat$labels <- list("Demographic and socioeconomic" = c(1:3,12:16,20), "Personal" = c(4:11,19,21:22), "Group" = c(17:18), "Radicalization" = c(23))
violent_dat$names <- colnames(violent)

violent_model <- mgm(data = violent_dat$data, 
                     type = violent_dat$type, 
                     lambdaSel = "EBIC", 
                     lambdaGam = 0)


nonviolent_dat <- list("data" = NULL, 
                    "type" = NULL, 
                    "names" = NULL,
                    "labels" = NULL,
                    "grouplabels"= NULL)

nonviolent_dat$data <- as.matrix(sapply(nonviolent, as.numeric))
nonviolent_dat$type <-  c(rep("c", 11), "g", rep("c", 5), "g", rep("c",5))
nonviolent_dat$labels <- list(c(1:3,12:16,20), c(4:11,19,21:22), c(17:18) , c(23))
nonviolent_dat$names <- colnames(nonviolent)



nonviolent_model <- mgm(data = nonviolent_dat$data, 
                     type = nonviolent_dat$type, 
                     lambdaSel = "EBIC", 
                     lambdaGam = 0)

#Measuring the predictability of the nodes in both models.
Pred_violent <- predict(violent_model, violent_dat$data)
Pred_violent$errors
pie1aa<- as.numeric(as.character(Pred_violent$errors[1:11, 4]))
pie1aaa<- as.numeric(as.character(Pred_violent$errors[12, 3]))
pie1aaaa<- as.numeric(as.character(Pred_violent$errors[13:17, 4]))
pie1aaaaa<- as.numeric(as.character(Pred_violent$errors[18, 3]))
pie1aaaaaa<- as.numeric(as.character(Pred_violent$errors[19:23, 4]))
pie1a <- c(pie1aa,pie1aaa,pie1aaaa,pie1aaaaa,pie1aaaaaa)
mean(pie1a)

Pred_nonviolent <- predict(nonviolent_model, nonviolent_dat$data)
Pred_nonviolent$errors
pie2aa<- as.numeric(as.character(Pred_nonviolent$errors[1:11, 4]))
pie2aaa<- as.numeric(as.character(Pred_nonviolent$errors[12, 3]))
pie2aaaa<- as.numeric(as.character(Pred_nonviolent$errors[13:17, 4]))
pie2aaaaa<- as.numeric(as.character(Pred_nonviolent$errors[18, 3]))
pie2aaaaaa<- as.numeric(as.character(Pred_nonviolent$errors[19:23, 4]))
pie2a <- c(pie2aa,pie2aaa,pie2aaaa,pie2aaaaa,pie2aaaaaa)
mean(pie2a)


#Visualizing the networks
layout(matrix(c(
  1,2),1,2,byrow=TRUE),widths = c(2.3,3.7))

nonviolent_graph <- qgraph(nonviolent_model$pairwise$wadj, layout = "spring", groups = nonviolent_dat$labels, vsize = 7, pie = pie2a, pieBorder = 0.25, title = "Nonviolent network")
violent_graph <- qgraph(violent_model$pairwise$wadj, layout = nonviolent_graph$layout, groups = violent_dat$labels, nodeNames = violent_dat$names, vsize = 5.5,legend.cex = 0.4, pie = pie1a, pieBorder = 0.25, title = "Violent network")

centralityPlot(list(violent = violent_graph, nonviolent =  nonviolent_graph), include = 'all')


#re-estimating the models using bootnet
violent_bootstrap <- estimateNetwork(data = violent_dat$data,   
                                   type = violent_dat$type, 
                                   default="mgm",
                                   criterion = "EBIC", 
                                   tuning = 0)


nonviolent_bootstrap <- estimateNetwork(data = nonviolent_dat$data,   
                                      type = nonviolent_dat$type, 
                                      default="mgm",
                                      criterion = "EBIC", 
                                      tuning = 0)

#check if the models match
cor(vech(violent_model$pairwise$wadj), vech(abs(violent_for_nct$graph)))
cor(vech(nonviolent_model$pairwise$wadj), vech(abs(nonviolent_for_nct$graph)))
# both equal to 1


#Bootstrapping both networks to check if the networks are stable
boot_violent <- bootnet(violent_bootstrap, type = "nonparametric", nBoots = 1000, nCores = 6)
boot_nonviolent <- bootnet(nonviolent_bootstrap, type = "nonparametric", nBoots = 1000, nCores = 6)
nct_results <- NCT(violent_bootstrap, nonviolent_bootstrap, it = 100, test.centrality = TRUE, centrality = c("closeness", "strength"))

#plotting the bootstrap results
plot(boot_violent, order = "sample", labels = F)
plot(boot_nonviolent, order = "sample", labels = F)

pdf(paste0(dataPrDir, "violent_bootstrap_1000.pdf"), height=ncol(violent_dat$data)*2)
plot(boot_violent, order="sample", plot="area", prop0=TRUE)
dev.off()

pdf(paste0(dataPrDir, "nonviolent_bootstrap_1000.pdf"), height=ncol(nonviolent_dat$data)*2)
plot(boot_nonviolent, order="sample", plot="area", prop0=TRUE)
dev.off()




