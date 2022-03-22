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

dir.exists("04_Modelling/01_benthic/03_acoustic/BRT_Output_acoustic")
dir.create("04_Modelling/01_benthic/03_acoustic/BRT_Output_acoustic")

# charger les fonctions BRT necessaires
source("04_Modelling/01_benthic/03_acoustic/00_Functions_BRT.R")


# definir le jeu de donnee, les variables "reponse" (Y) si on voulait analyser plus qu'une variable reponse, et les variables predicteur (X)
load("02_formating_data/01_Benthic/Rdata/acoustic_benthic.rdata")
load("00_metadata/acoustic_explanatory_variables_benthic.rdata")
acoustic_var$logAcousticFond <- log(acoustic_fond$AcousticFond+1)

myData <- acoustic_var

myData$Habitat <- as.factor(myData$Habitat)


myResponse=c("logAcousticFond")

myPredictor=c("BottomDepth","SSTmean", "EastwardVelocity", "NorthwardVelocity",
              "Chla", "Day", "LandMinDist")

myPredictorNumeric=c("SummitDepth", "SummitRugosity","BottomDepth",
                     "SSTmean", "EastwardVelocity", "NorthwardVelocity",
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
        source("04_Modelling/01_benthic/03_acoustic/00_Functions_BRT.R")
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
  
  png(paste0("04_Modelling/01_benthic/03_acoustic/BRT_Output_acoustic/", "InteractionPlotsBestModel.png"), width = 1200, height = 600)
  
  par(mfrow=c(1,2))
  dismo::gbm.perspec(mod_best_gbmStep_reduced, 1, 2, z.range=c(0,2))
  dismo::gbm.perspec(mod_best_gbmStep_reduced, 3, 3, z.range=c(1,3.5))
  
  dev.off()
  
  par(mfrow=c(1,1))
  
  #Stop cluster
  stopCluster(cl)
  
  
  png(paste0("04_Modelling/01_benthic/03_acoustic/BRT_Output_acoustic/", "InteractionPlotsBestModelDismo.png"), width = 1200, height = 600)
  
  dismo::gbm.plot(mod_best_gbmStep_reduced, n.plots=5, plot.layout=c(3, 2), write.title = FALSE)
  
  dev.off()
  
 
  
  gbm::plot.gbm(mod_best_gbmStep_reduced, i.var=c(1))
  gbm::plot.gbm(mod_best_gbmStep_reduced, i.var=c(3,2),level.plot=FALSE)
  
  
  
  
  ### Predict REDUCED BRT on study area
load("02_formating_data/00_Prediction_raster/Raster_df_predictions/df_benthic.rdata")
df_benthic <- na.omit(df_benthic)
  
df <- df_benthic
coordinates(df) <- ~x+y
gridded(df) <- TRUE
rast <- stack(df)
  
rast2 <- brick("02_formating_data/00_Prediction_raster/Raster_df_predictions/raster_benthic.tif")
  
  
  # Predict based on reduced model
pred_fish = predict_brt(mod_best_gbmStep_reduced, "gaussian", responseName,
                          preds = var_sup5_best_fixed, rast)
  
plot(pred_fish)
  
benthic_acoustic_predict <- as.data.frame(pred_fish, xy=TRUE)
  
benthic_acoustic_predict <- benthic_acoustic_predict %>% filter(!is.na(layer))

names(benthic_acoustic_predict) <- c("x", "y", "acoustic_predict")

benthic_acoustic_predict$acoustic_predict <- exp(benthic_acoustic_predict$acoustic_predict)-1 

benthic_acoustic_predict <- cbind(benthic_acoustic_predict, df_benthic[,-c(1,2)])

save(benthic_acoustic_predict, file="04_Modelling/01_benthic/03_acoustic/BRT_Output_acoustic/benthic_acoustic_predict.rdata")   


df <- benthic_acoustic_predict
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_benthic_acoustic_predict <- stack(df)
plot(raster_benthic_acoustic_predict)

writeRaster(raster_benthic_acoustic_predict, filename = "04_Modelling/01_benthic/03_acoustic/BRT_Output_acoustic/raster_benthic_acoustic_predict.tif")
