# NB: BRT tutorial available at https://rspatial.org/raster/sdm/9_sdm_brt.html
 
# Load all functions
devtools::load_all()

#---------------------------------------------------------------------------------------------------
# Load uvcs and bruvs grids
load("3-extract_predictors_at_stations_and_join_to_grid.RData")


#---------------------------------------------------------------------------------------------------
# BRTs

# Prepare bruvs and uvcs data for BRTs
bruvspoly = prep_data_for_brts(bruvspoly)
uvcspoly = prep_data_for_brts(uvcspoly)

# Check correlations for numeric covariates
corbruvs = check_correlations(bruvspoly)
corbruvs
coruvcs = check_correlations(uvcspoly)
coruvcs

#predictors are sst_mean travel_time dist_reef pop_dens coral_cover coral_hab_type

# Setting the different parameters to combine
tree.complexity = c(1:5)
learning.rate = c(0.01, 0.005, 0.001)
bag.fraction = c(0.5,0.75)

# Make study area raster ***important***
rast = make_study_area_raster()







########################################## BRTs bruvs shark species with > 60 non zero cells ##########################################

# get and write list of species with > 60 non zero cells
shark_species_sup60 = get_list_shark_species_sup_60_nonzero_cells(bruvspoly)

# get and write list of species with <= 60 non zero cells (ie rare species)
shark_species_inf_equal60 = get_list_shark_species_inf_equal_60_nonzero_cells(bruvspoly)

# add rare shark species category by summing species with <= 60 non zero cells
bruvspoly = add_rare_shark_species_category(bruvspoly, shark_species_inf_equal60)




library(foreach)
library(doParallel)

#Make cluster for parallel processing
cores = detectCores()
cl = makeCluster(cores-1)
registerDoParallel(cl)


#loop on species for optimisation

#for (sp in c(shark_species_sup60, "rare_shark")){

sp="Carcharhinus_amblyrhynchos"
  print(paste("----------------------", sp))


  ### Optimisation

  # define empty output matrix
  output <- matrix(ncol=9)

  # Run the function through the for loop to test all parameter combinations

  library(foreach)
  par_output =  foreach(i = tree.complexity, .packages=c("foreach")) %dopar% {
    foreach(j = learning.rate, .packages=c("foreach")) %dopar% {
      foreach(k = bag.fraction, .packages=c("foreach")) %dopar% {
        #need to load package within foreach loop
        source("R/brts.R")
        # model name
        nam = paste0("all_shark_gbm_tc", i, "lr", j, "bf", k)
        # model optimization
        t = optimize_poisson_brts(tree.com = i,learn = j, bag.f = k, bruvspoly, sp)
        # assign results to output matrix
        if(!is.null(t$interaction.depth)){ output = rbind(output, c(nam, unlist(t))) }
        output
      }
    }
  }

  # extract best brts parameters
  best_parameters = extract_best_parameters_par(par_output, sp, "poisson")


  ### Fit best poisson BRT

  # Fit
  mod_shark = fit_best_poisson_brt(bruvspoly, sp, best_parameters)

  # Get variables with contributions > 5%
  var_sup5_shark = get_variables_contrib_sup5(mod_shark)


  ### Fit best poisson BRT REDUCED (same as brt)

  # Refit after dropping predictors with contributions < 5%
  mod_shark_reduced = fit_best_reduced_poisson_brt(bruvspoly, sp, best_parameters,
                                                   preds = var_sup5_shark)

  # Make plot of variable contributions reduced model
  make_contribution_reduced_plot(mod_shark_reduced, sp, "poisson")

  # Partial dependance plots reduced model
  partial_dependance_plots3(mod_shark_reduced, sp, "poisson")


  ### Predict REDUCED BRT on study area

  # Predict based on reduced model
  pred_shark = predict_brt(mod_shark_reduced, "bruvs", "poisson", sp,
                           preds = var_sup5_shark, rast)

  # Map prediction
  map_brt_prediction(pred_shark, sp, "poisson")

  map_brt_prediction_exp_transf(pred_shark, sp, "poisson")

  map_brt_prediction_quantile_cols(pred_shark, sp, "poisson")

}


#Stop cluster
stopCluster(cl)



########################################## BRTs bruvs shark species summary (without rare sharks) ##########################################


### map summed shark species predictions

map_summed_shark_species_pred()




### summarize variable contributions across shark species

# load contributions
all_contrib = load_contributions_all_shark_species(shark_species_sup60)

# plot frequency of variable selection
plot_frequency_var_all_shark_species(all_contrib)

# boxplot of contributions
boxplot_contributions_all_shark_species(all_contrib)




### Summary of brts results

summarize_brt_results_shark(shark_species_sup60)






########################################## BRTs uvcs fish species with > 60 non zero cells ##########################################

# get and write list of species with > 60 non zero cells
fish_species_sup60 = get_list_fish_species_sup_60_nonzero_cells(uvcspoly)

# get list of species that are present in chesterfield ONLY / in grande terre only / in both
list_modeled_species_in_chester_and_grande_terre(fish_species_sup60)

# get and write list of species with <= 60 non zero cells (ie rare species)
fish_species_inf_equal60 = get_list_fish_species_inf_equal_60_nonzero_cells(uvcspoly)

# add rare fish species category by summing species with <= 60 non zero cells
uvcspoly = add_rare_fish_species_category(uvcspoly, fish_species_inf_equal60)



library(foreach)
library(doParallel)

#Make cluster for parallel processing
cores = detectCores()
cl = makeCluster(cores-1)
registerDoParallel(cl)


#loop on species for optimisation

#NB: no valid brt optimization solution for 3 species so removed from fish_species_sup60 vector
fish_species_sup60_valid = fish_species_sup60[!fish_species_sup60 %in% c("Plectorhinchus_chaetodonoides", "Epinephelus_polyphekadion", "Epinephelus_howlandi")]
write.table(fish_species_sup60_valid[order(fish_species_sup60_valid)], here::here("/outputs/brts/gaussian/list_fish_species_sup60_valid.txt"), quote = FALSE, row.names = FALSE, col.names = FALSE)


for (sp in c(fish_species_sup60_valid, "rare_fish")){ #takes about 16h

  print(paste("----------------------", sp))


  ### Optimisation

  # define empty output matrix
  output <- matrix(ncol=9)

  # Run the function through the for loop to test all parameter combinations

   par_output =  foreach(i = tree.complexity, .packages=c("foreach")) %dopar% {
                  foreach(j = learning.rate, .packages=c("foreach")) %dopar% {
                    foreach(k = bag.fraction, .packages=c("foreach")) %dopar% {
                      #need to load package within foreach loop
                      devtools::load_all()
                      # model name
                      nam = paste0("all_fish_gbm_tc", i, "lr", j, "bf", k)
                      # model optimization
                      t = optimize_gaussian_brts(tree.com = i,learn = j, bag.f = k, uvcspoly, sp)
                      # assign results to output matrix
                      if(!is.null(t$interaction.depth)){ output = rbind(output, c(nam, unlist(t))) }
                      output
                    }
                  }
                }

    # extract best brts parameters
    best_parameters = extract_best_parameters_par(par_output, sp, "gaussian")


  ### Fit best gaussian BRT

  # Fit
  mod_fish = fit_best_gaussian_brt(uvcspoly, sp, best_parameters)

  # Get variables with contributions > 5%
  var_sup5_fish = get_variables_contrib_sup5(mod_fish)


  ### Fit best gaussian BRT REDUCED (same as brt)

  # Refit after dropping predictors with contributions < 5%
  mod_fish_reduced = fit_best_reduced_gaussian_brt(uvcspoly, sp, best_parameters,
                                                   preds = var_sup5_fish)

  # Make plot of variable contributions reduced model
  make_contribution_reduced_plot(mod_fish_reduced, sp, "gaussian")

  # Partial dependance plots reduced model
  partial_dependance_plots3(mod_fish_reduced, sp, "gaussian")


  ### Predict REDUCED BRT on study area

  # Predict based on reduced model
  pred_fish = predict_brt(mod_fish_reduced, "uvcs", "gaussian", sp,
                          preds = var_sup5_fish, rast)

  # Map prediction
  map_brt_prediction(pred_fish, sp, "gaussian")

  map_brt_prediction_exp_transf(pred_fish, sp, "gaussian")

  map_brt_prediction_quantile_cols(pred_fish, sp, "gaussian")

}


#Stop cluster
stopCluster(cl)




########################################## BRTs uvcs fish species summary (without rare fish) ##########################################



### map summed fish species predictions (always run this function before mask function below)

map_summed_fish_species_pred()




# mask fish species predictions with shark predictions

for (sp in fish_species_sup60_valid){

  mask_fish_species_predictions_with_sharks(sp)

}


# mask summed fish species predictions with shark predictions

mask_summed_fish_species_predictions_with_sharks()



### summarize variable contributions across fish species

# load contributions
all_contrib = load_contributions_all_fish_species(fish_species_sup60_valid)

# plot frequency of variable selection
plot_frequency_var_all_fish_species(all_contrib)

# boxplot of contributions
boxplot_contributions_all_fish_species(all_contrib)




### Summary of brts results

summarize_brt_results_fish(fish_species_sup60_valid)








################################################ Map co occurrence of fish and sharks based on 50% quantile (ie median)

map_cooccurrence_fish_shark()



###### Summary of predictor values

summarize_predictors(rast)

