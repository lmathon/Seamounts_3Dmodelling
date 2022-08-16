# INIT : source du 00_Initialisation ----
rm(list=ls(all=TRUE))


#load libraries
library(readxl)
library(writexl)
library(ggplot2)
library(jtools)
library(ggstance)
library(rms)
library(fitdistrplus)
library(huxtable)
library(parallel)
library(foreach)
library(doParallel)
library(dplyr)
library(here)
library(raster)


# creer un repertoire de sortie

dir.exists("04_Modelling/02_pelagic/02_eDNA/BRT_Output_edna")
dir.create("04_Modelling/02_pelagic/02_eDNA/BRT_Output_edna")

# charger les fonctions BRT necessaires
source("04_Modelling/02_pelagic/02_eDNA/00_Functions_BRT.R")


# definir le jeu de donnee, les variables "reponse" (Y) si on voulait analyser plus qu'une variable reponse, et les variables predicteur (X)
load("02_formating_data/02_pelagic/Rdata/edna_richness_pelagic.rdata")
load("00_metadata/edna_explanatory_variables_pelagic.rdata")
edna_var$log_richness<- log(edna_richness_pelagic$richness_tot+1)
edna_var$BottomDepth <- as.numeric(edna_var$BottomDepth)
edna_var$richness <- edna_richness_pelagic$richness_tot

myData <- edna_var

myData$Habitat <- as.factor(myData$Habitat)
myData$Sampling_Depth <- as.numeric(myData$Sampling_Depth)

myResponse=c("log_richness")

myPredictor=c("SummitDepth","SummitAreaKm2", "SummitRugosity","BottomDepth", "TravelTime", "Sampling_Depth",
              "SSTmean", "SSTmax", "EastwardVelocity", "NorthwardVelocity", "Chla", "ReefMinDist",
              "Salinity", "seafloorTemp", "SuspendedParticulateMatter", "LandMinDist", "Habitat")

myPredictorNumeric=c("SummitDepth","SummitAreaKm2", "SummitRugosity","BottomDepth", "TravelTime", "Sampling_Depth",
                     "SSTmean", "SSTmax", "EastwardVelocity", "NorthwardVelocity", "Chla", "ReefMinDist",
                     "Salinity", "seafloorTemp", "SuspendedParticulateMatter", "LandMinDist")


# verifier les correlations entre predicteurs numeriques
cort = cor(na.omit(myData[,myPredictorNumeric]))
cort


# calcul parallele

cores = detectCores()
cores    # ma machine a 8 coeurs


# Setting the different parameters to combine
tree.complexity = c(1:5)
learning.rate = c(0.01, 0.005, 0.001)
bag.fraction = c(0.5,0.75)


#Make cluster for parallel processing
cores = detectCores()
cl = makeCluster(cores-1)
registerDoParallel(cl)


responseName=myResponse # in case there is only one response variable
print(paste("----------------------", responseName))


### Optimisation

# define empty output matrix
output <- matrix(ncol=9)

# Run the function through the for loop to test all parameter combinations

par_output =  foreach(i = tree.complexity, .packages=c("foreach")) %dopar% {
  foreach(j = learning.rate, .packages=c("foreach")) %dopar% {
    foreach(k = bag.fraction, .packages=c("foreach")) %dopar% {
      #need to load package within foreach loop
      source("04_Modelling/02_pelagic/02_eDNA/00_Functions_BRT.R")
      # model name
      nam = paste0("Model_",responseName,"_tc_", i, "_lr_", j, "_bf_", k)
      # model optimization
      t = optimize_gaussian_brts(tree.com = i,learn = j, bag.f = k, myData, responseName, myPredictor)
      # assign results to output matrix
      if(!is.null(t$interaction.depth)){ output = rbind(output, c(nam, unlist(t))) }
      output
    }
  }
}




# extract best brts parameters
best_parameters = extract_best_parameters_par(par_output, responseName, "gaussian")
best_parameters


### Fit best gaussian BRT with fixed nb of trees

# Fit best model
mod_best_fixed = fit_best_gaussian_brt_fixed(myData, responseName, best_parameters, myPredictor)
mod_best_fixed
summary(mod_best_fixed)
names(mod_best_fixed)
mod_best_fixed$contributions

# Make plot of variable contributions best fixed model
make_contribution_reduced_plot(mod_best_fixed, responseName, "gaussian")


# Get variables with contributions > 5%
var_sup5_best_fixed = get_variables_contrib_sup5(mod_best_fixed)


### Fit best gaussian BRT REDUCED (same as brt)

# Refit after dropping predictors with contributions < 5%
mod_best_fixed_reduced = fit_best_reduced_gaussian_brt_fixed(myData, responseName, best_parameters,
                                                             preds = var_sup5_best_fixed)
mod_best_fixed_reduced
summary(mod_best_fixed_reduced)
names(mod_best_fixed_reduced)
mod_best_fixed_reduced$contributions
mod_best_fixed_reduced$var.names

# Make plot of variable contributions reduced model
make_contribution_reduced_plot(mod_best_fixed_reduced, responseName, "gaussian")

# Partial dependance plots reduced model
partial_dependance_plots3(mod_best_fixed_reduced, responseName, "gaussian")

# Refit a gbmStep after dropping predictors with contributions < 5%
mod_best_gbmStep_reduced = fit_best_reduced_gaussian_brt_gbmStep(myData, responseName, best_parameters,
                                                                 preds = var_sup5_best_fixed)

names(mod_best_gbmStep_reduced)
mod_best_gbmStep_reduced$cv.statistics
mod_best_gbmStep_reduced$shrinkage
mod_best_gbmStep_reduced$n.trees
mod_best_gbmStep_reduced$contributions
mod_best_fixed_reduced$contributions


#explore interactions for best reduced model (must be a gbmStep - dont work with gbmfixed)
find.int <- dismo::gbm.interactions(mod_best_gbmStep_reduced)
find.int$interactions
find.int$rank.list

#dev.new()

png(paste0("04_Modelling/02_pelagic/02_eDNA/BRT_Output_edna/", "InteractionPlotsBestModel.png"), width = 1200, height = 600)

par(mfrow=c(1,2))
dismo::gbm.perspec(mod_best_gbmStep_reduced, 1, 3, z.range=c(0,2))
dismo::gbm.perspec(mod_best_gbmStep_reduced, 4, 2, z.range=c(1,3.5))

dev.off()

par(mfrow=c(1,1))

#Stop cluster
stopCluster(cl)


png(paste0("04_Modelling/02_pelagic/02_eDNA/BRT_Output_edna/", "InteractionPlotsBestModelDismo.png"), width = 1200, height = 600)

dismo::gbm.plot(mod_best_gbmStep_reduced, n.plots=5, plot.layout=c(4, 2), write.title = FALSE)

dev.off()



gbm::plot.gbm(mod_best_gbmStep_reduced, i.var=c(1))
gbm::plot.gbm(mod_best_gbmStep_reduced, i.var=c(1,2),level.plot=FALSE)




### Predict REDUCED BRT on study area
load("02_formating_data/00_Prediction_raster/Raster_df_predictions/df_pelagic.rdata")

df_pelagic <- df_pelagic %>%
  filter(!is.na(Sampling_Depth))

list_df <- split(df_pelagic, df_pelagic$Sampling_Depth)
depth <- unique(df_pelagic$Sampling_Depth)

pred_fish <- vector("list", 30)
pred_df <- vector("list", 30)
 
pelagic_motu_predict <- list_df[[1]][,1:3]
pelagic_motu_predict$x <- as.factor(pelagic_motu_predict$x)
pelagic_motu_predict$y <- as.factor(pelagic_motu_predict$y)


for (i in 1:length(list_df)) {
  df <- list_df[[i]]
  coordinates(df) <- ~x+y
  gridded(df) <- TRUE
  rast <- stack(df)
  
  # Predict based on reduced model
  pred_fish[[i]] = predict_brt(mod_best_gbmStep_reduced, "gaussian", responseName,
                          preds = var_sup5_best_fixed, rast)
 
  
  
  pred_df[[i]] <- as.data.frame(pred_fish[[i]], xy=TRUE) 
  pred_df[[i]] <- pred_df[[i]] %>% filter(!is.na(layer))
  pred_df[[i]]$layer <- exp(pred_df[[i]]$layer)-1
  names(pred_df[[i]]) <- c("x", "y", paste("depth_", depth[[i]], sep=""))
  
  pred_df[[i]]$x <- as.factor(pred_df[[i]]$x)
  pred_df[[i]]$y <- as.factor(pred_df[[i]]$y)
  
  pelagic_motu_predict <- left_join(pelagic_motu_predict, pred_df[[i]], by=c("x","y"))
}

save(pelagic_motu_predict, file="04_Modelling/02_pelagic/02_eDNA/BRT_Output_edna/pelagic_motu_predict.rdata")


#Stop cluster
stopCluster(cl)
