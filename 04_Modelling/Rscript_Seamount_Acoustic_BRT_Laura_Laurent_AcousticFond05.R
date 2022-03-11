# INIT : source du 00_Initialisation ----
rm(list=ls(all=TRUE))


#load libraries
if (!require("readxl")) install.packages("readxl")
if (!require("writexl")) install.packages("writexl")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("jtools")) install.packages("jtools")
if (!require("ggstance")) install.packages("ggstance")

if (!require("rms")) install.packages("rms")
if (!require("fitdistrplus")) install.packages("fitdistrplus")
if (!require("huxtable")) install.packages("huxtable")
if (!require("parallel")) install.packages("parallel")
if (!require("foreach")) install.packages("foreach")
if (!require("doParallel")) install.packages("doParallel")
if (!require("dplyr")) install.packages("dplyr")
if (!require("here")) install.packages("here")

# creer un repertoire de sortie

dir.exists("03_BRT/03_Acoustic/BRT_Output")
dir.create("03_BRT/03_Acoustic/BRT_Output")

# charger les fonctions BRT necessaires
source("03_BRT/ScriptR_Functions_BRT_Laura_Laurent.R")


# definir le jeu de donnee, les variables "reponse" (Y) si on voulait analyser plus qu'une variable reponse, et les variables predicteur (X)
load("01_formating_data/03_Acoustic/Rdata/acoustic.rdata")

myData=Acoustic
myResponse=c("LogAcousticFond")

myPredictor=c("Longitude","Latitude", "Day", "Habitat", "SummitDepth", "Height", "SummitAreasKm2", "SummitRugosity","BottomDepth",
              "SSTmean1k", "SSTmin1k", "SSTmax1k", "SSTsd1k", "ChlorA", "EastwardVelocity", "NorthwardVelocity",
              "Salinity", "SeaFloorPotentialTemperature", "SuspendedParticulateMatter", "ShortestDistanceReef", "ShortestDistanceLand",
              "TravelTimeHour")

myPredictorNumeric=c("Longitude","Latitude","SummitDepth", "Height", "SummitAreasKm2", "SummitRugosity","BottomDepth",
              "SSTmean1k", "SSTmin1k", "SSTmax1k", "SSTsd1k", "ChlorA", "EastwardVelocity", "NorthwardVelocity",
              "Salinity", "SeaFloorPotentialTemperature", "SuspendedParticulateMatter", "ShortestDistanceReef", "ShortestDistanceLand",
              "TravelTimeHour")


# verifier les corelelations entre predicteurs numeriques
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


#loop on responses for optimisation

#for (responseName in myResponse[1:length(myResponse)]){ # in case there are several response variables
  
responseName=myResponse # in case there is only one response variable
  print(paste("----------------------", responseName))


  ### Optimisation
  
  # define empty output matrix
  output <- matrix(ncol=9)
  
  # Run the function through the for loop to test all parameter combinations
  
  #source("R_BRT_Fonctions/R_Function_optimize_gaussian_brts_Laura_Laurent.R")
  #source("ScriptR_Functions_BRT_Laura_Laurent.R")
  
  par_output =  foreach(i = tree.complexity, .packages=c("foreach")) %dopar% {
    foreach(j = learning.rate, .packages=c("foreach")) %dopar% {
      foreach(k = bag.fraction, .packages=c("foreach")) %dopar% {
        #need to load package within foreach loop
        #devtools::load_all("./R_BRT_Fonctions/") ##### fonction qui charge toutes les fonctions d'un dossier R
        #source("ScriptR_Functions_BRT_Laura_Laurent.R")
        #source("R_BRT_Fonctions/R_Function_optimize_gaussian_brts_Laura_Laurent.R")
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
  
  # the greatest interaction (7.90) is between var 9 (SSTsd1k) and var 1 (BottomDepth)
  # the second greatest 6.92 is between var 6 (SSTsd1k) and var 1 (BottomDepth)
  # etc
  
  #dev.new()
  
  png(paste0("03_BRT/03_Acoustic/BRT_Output/", "InteractionPlotsBestModel.png"), width = 1200, height = 600)
  
  par(mfrow=c(1,2))
  dismo::gbm.perspec(mod_best_gbmStep_reduced, 5, 2, z.range=c(0,2))
  dismo::gbm.perspec(mod_best_gbmStep_reduced, 1, 3, z.range=c(1,3.5))
  
  dev.off()
  
  par(mfrow=c(1,1))
  
  #Stop cluster
  stopCluster(cl)
  
  # attention, il y a 2 packages, gbm et dismo, et avec les memes fonctions. utiliser gbm:: et dismo:: pour choisir
  # exemple
  
  ?dismo::gbm.plot
  
  png(paste0("03_BRT/03_Acoustic/BRT_Output/", "InteractionPlotsBestModelDismo.png"), width = 1200, height = 600)
  
  dismo::gbm.plot(mod_best_gbmStep_reduced, n.plots=5, plot.layout=c(3, 2), write.title = FALSE)
  
  dev.off()
  
  ?gbm::plot.gbm
  
  gbm::plot.gbm(mod_best_gbmStep_reduced, i.var=c(1))
  gbm::plot.gbm(mod_best_gbmStep_reduced, i.var=c(1,6),level.plot=FALSE)
  
  
  
  #########
  ######### LA SUIT EST SI ON VEUT PREDIRE SUR DES CARTES, PAS ENCORE LE CAS
  ######## JE DEPLACE DONC LA FIN DU CALCUL PARALLE ICI
  ######### IDEM DEPLACEMENT DE LA BOUCLE EVENTUELLE
  
  
  ### Predict REDUCED BRT on study area
  
  # Predict based on reduced model
  pred_fish = predict_brt(mod_fish_reduced, "uvcs", "gaussian", sp,
                          preds = var_sup5_fish, rast)
  
  # Map prediction
  map_brt_prediction(pred_fish, sp, "gaussian")
  
  map_brt_prediction_exp_transf(pred_fish, sp, "gaussian")
  
  map_brt_prediction_quantile_cols(pred_fish, sp, "gaussian")
  
 # } ### BOUCLE de FOR


#Stop cluster
stopCluster(cl)