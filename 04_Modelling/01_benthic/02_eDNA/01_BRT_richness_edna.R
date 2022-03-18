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
devtools::load_all() 


# creer un repertoire de sortie

dir.exists("04_Modelling/01_benthic/02_eDNA/BRT_Output_edna")
dir.create("04_Modelling/01_benthic/02_eDNA/BRT_Output_edna")

# charger les fonctions BRT necessaires
source("04_Modelling/01_benthic/02_eDNA/00_Functions_BRT.R")


# definir le jeu de donnee, les variables "reponse" (Y) si on voulait analyser plus qu'une variable reponse, et les variables predicteur (X)
load("02_formating_data/01_Benthic/Rdata/edna_richness_benthic.rdata")
load("00_metadata/edna_explanatory_variables_benthic.rdata")
edna_var$log_richness<- log(edna_richness_benthic$richness_tot+1)
edna_var$BottomDepth <- as.numeric(edna_var$BottomDepth)

myData <- edna_var

myData$Habitat <- as.factor(myData$Habitat)


myResponse=c("log_richness")

myPredictor=c("BottomDepth", "TravelTime",
              "SSTmean", "EastwardVelocity", "NorthwardVelocity", "Chla",
              "Salinity", "seafloorTemp")

myPredictorNumeric=c("SummitAreaKm2", "SummitRugosity","BottomDepth", "TravelTime",
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
      source("04_Modelling/01_benthic/02_eDNA/00_Functions_BRT.R")
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

png(paste0("04_Modelling/01_benthic/02_eDNA/BRT_Output_edna/", "InteractionPlotsBestModel.png"), width = 1200, height = 600)

par(mfrow=c(1,2))
dismo::gbm.perspec(mod_best_gbmStep_reduced, 1, 2, z.range=c(0,2))
dismo::gbm.perspec(mod_best_gbmStep_reduced, 4, 2, z.range=c(1,3.5))

dev.off()

par(mfrow=c(1,1))

#Stop cluster
stopCluster(cl)


png(paste0("04_Modelling/01_benthic/02_eDNA/BRT_Output_edna/", "InteractionPlotsBestModelDismo.png"), width = 1200, height = 600)

dismo::gbm.plot(mod_best_gbmStep_reduced, n.plots=5, plot.layout=c(3, 2), write.title = FALSE)

dev.off()



gbm::plot.gbm(mod_best_gbmStep_reduced, i.var=c(1))
gbm::plot.gbm(mod_best_gbmStep_reduced, i.var=c(1,2),level.plot=FALSE)




### Predict REDUCED BRT on study area
load("02_formating_data/00_Prediction_raster/Rdata/df_seamount_islands.rdata")
df_seamount_islands$Latitude <- df_seamount_islands$y
df_seamount_islands$Longitude <- df_seamount_islands$x
df <- df_seamount_islands
coordinates(df) <- ~x+y
gridded(df) <- TRUE

rast <- stack(df)


# Predict based on reduced model
pred_fish = predict_brt(mod_best_gbmStep_reduced, "gaussian", responseName,
                        preds = var_sup5_best_fixed, rast)

plot(pred_fish)

# Map prediction
map_brt_prediction(pred_fish, responseName, "gaussian")

map_brt_prediction_exp_transf(pred_fish, responseName, "gaussian")

map_brt_prediction_quantile_cols(pred_fish, responseName, "gaussian")

# } ### BOUCLE de FOR


#Stop cluster
stopCluster(cl)