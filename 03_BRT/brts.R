

#' Prepare bruvs and uvcs data for brts
#'
#' @param data
#'
#' @return
#' @export
#'

prep_data_for_brts <- function(data){

  # Use NA (instead of NaN) for missing values in sst_mean
  data$sst_mean[is.nan(data$sst_mean)] = NA

  # print correspondence coral levels character - numerical
  print("-------------------- coralpres levels --------------------")
  print(levels(as.factor(as.numeric(as.factor(data$coralpres)))))
  print(levels(as.factor(data$coralpres)))

  print("-------------------- coral_l2 levels --------------------")
  print(levels(as.factor(as.numeric(as.factor(data$coral_l2)))))
  print(levels(as.factor(data$coral_l2)))

  print("-------------------- coral_l3 levels --------------------")
  print(levels(as.factor(as.numeric(as.factor(data$coral_l3)))))
  print(levels(as.factor(data$coral_l3)))

  print("-------------------- mpa type levels --------------------")
  print(levels(as.factor(as.numeric(as.factor(data$mpa_type)))))
  print(levels(as.factor(data$mpa_type)))

  # Convert to numerical factor levels and rename coral_l3 to coral_hab_type
  data$coralpres = as.factor(as.numeric(as.factor(data$coralpres)))
  data$coral_l2 = as.factor(as.numeric(as.factor(data$coral_l2)))
  data$coral_hab_type = as.factor(as.numeric(as.factor(data$coral_l3)))
  data$mpa_type = as.factor(as.numeric(as.factor(data$mpa_type)))

  # convert to dataframe
  data = as.data.frame(data)

  return(data)
}


#' Check correlations for numeric covariates in bruvs and uvcs data before modeling
#'
#' @param data
#'
#' @return
#' @export
#'

check_correlations <- function(data){

cort = cor(na.omit(data[,c("sst_mean", "coralcover", "travel_time", "dist_reef", "pop_dens")]))

return(cort)
}





#' Get and write list of fish species with > 60 non zero cells
#'
#' @param data
#'
#' @return
#' @export
#'

get_list_fish_species_sup_60_nonzero_cells <- function(data){

  # get list of species with > 60 non zero cells
  l = lapply(data[4:385], function(x) length(x[x>0]))
  cond = lapply(l, function(x) x > 60)
  l60 = l[unlist(cond)]
  cat("number of species with > 60 non zero cells \n", length(l60), sep = "\n")

  #convert list to table with nb of observations and write to xlsx file
  df <- data.frame(matrix(unlist(l60)))
  row.names(df) = names(l60)
  colnames(df) = "presence_cells"
  xlsx::write.xlsx(df, here::here("/outputs/brts/poisson/list_fish_species_sup_60.xlsx"))

  # calculate proportion of biomass represented by these species
  species_list = names(l60)
  biom_allspecies = sum(data$all_fish, na.rm=T)
  data60 = data[,species_list]
  biom_species_list = sum(rowSums(data60))
  prop = biom_species_list / biom_allspecies
  cat("proportion of biomass represented by these species \n", prop, sep = "\n")

  return(species_list)
}




#' Get and write list of fish species with <= 60 non zero cells
#'
#' @param data
#'
#' @return
#' @export
#'

get_list_fish_species_inf_equal_60_nonzero_cells <- function(data){

  # get list of species with <= 60 non zero cells
  l = lapply(data[4:385], function(x) length(x[x>0]))
  cond = lapply(l, function(x) x <= 60)
  l60 = l[unlist(cond)]
  cat("number of species with <= 60 non zero cells \n", length(l60), sep = "\n")

  #convert list to table with nb of observations and write to txt file
  df <- data.frame(matrix(unlist(l60)))
  row.names(df) = names(l60)
  colnames(df) = "presence_cells"
  xlsx::write.xlsx(df, here::here("/outputs/brts/gaussian/list_fish_species_inf_equal_60.xlsx"))

  # calculate proportion of biomass represented by these species
  species_list = names(l60)
  biom_allspecies = sum(data$all_fish, na.rm=T)
  data60 = data[,species_list]
  biom_species_list = sum(rowSums(data60), na.rm=T)
  prop = biom_species_list / biom_allspecies
  cat("proportion of biomass represented by these species \n", prop, sep = "\n")

  return(species_list)
}



#' Add rare fish species category by summing species with <= 60 non zero cells
#'
#' @param data
#'
#' @return
#' @export
#'

add_rare_fish_species_category <- function(data, rare_species_list){

  data %>%
    dplyr::mutate(rare_fish = rowSums(dplyr::across(rare_species_list))) -> data

  return(data)

}




#' get list of modeled species that are present in chesterfield ONLY / in grande terre only / in both
#'
#' @param species_50
#'
#' @return
#' @export
#'

list_modeled_species_in_chester_and_grande_terre <- function (species_50){

  species_gde_terre_only = read.table(here::here("/outputs/raw_data/uvcs_list_species_gde_terre_only.txt"))
  species_chester_and_gde_terre = read.table(here::here("/outputs/raw_data/uvcs_list_species_chester_and_gde_terre.txt"))
  species_chester_only = read.table(here::here("/outputs/raw_data/uvcs_list_species_chester_only.txt"))

  species_50_gde_terre_only = species_50[species_50 %in% species_gde_terre_only[,1]]
  species_50_chester_and_gde_terre = species_50[species_50 %in% species_chester_and_gde_terre[,1]]
  species_50_chester_only = species_50[species_50 %in% species_chester_only[,1]]

  cat("proportion of modeled species that are present in Grande Terre only: ", length(species_50_gde_terre_only) / length(species_50), sep = "\n")
  cat("proportion of modeled species that are present in Chesterfield only: ", length(species_50_chester_only) / length(species_50), sep = "\n")
  cat("proportion of modeled species that are present in Chesterfield and Grande Terre: ", length(species_50_chester_and_gde_terre) / length(species_50), sep = "\n")

  #write results
  write.table(species_50_gde_terre_only, here::here("/outputs/raw_data/list_species_valid_gde_terre_only.txt"), row.names = FALSE, col.names = FALSE, quote = FALSE)
  write.table(species_50_chester_and_gde_terre, here::here("/outputs/raw_data/list_species_valid_chester_and_gde_terre.txt"), row.names = FALSE, col.names = FALSE, quote = FALSE)
  write.table(species_50_chester_only, here::here("/outputs/raw_data/list_species_valid_chester_only.txt"), row.names = FALSE, col.names = FALSE, quote = FALSE)

}





#' Get and write list of shark species with > 60 non zero cells
#'
#' @param data
#'
#' @return
#' @export
#'

get_list_shark_species_sup_60_nonzero_cells <- function(data){

  # get list of species with > 60 non zero cells
  l = lapply(data[4:17], function(x) length(x[x>0]))
  cond = lapply(l, function(x) x > 60)
  l60 = l[unlist(cond)]
  cat("number of species with > 60 non zero cells \n", length(l60), sep = "\n")

  #convert list to table with nb of observations and write to file
  df <- data.frame(matrix(unlist(l60)))
  row.names(df) = names(l60)
  colnames(df) = "presence_cells"
  #xlsx::write.xlsx(df, here::here("/outputs/brts/poisson/list_shark_species_sup_60.xls"))

  # calculate proportion of abundance represented by these species
  species_list = names(l60)
  abund_allspecies = sum(data$all_shark, na.rm=T)
  data60 = data[,species_list]
  abund_species_list = sum(rowSums(data60))
  prop = abund_species_list / abund_allspecies
  cat("proportion of abundance represented by these species \n", prop, sep = "\n")

  return(species_list)
}





#' Get and write list of shark species with <= 60 non zero cells
#'
#' @param data
#'
#' @return
#' @export
#'

get_list_shark_species_inf_equal_60_nonzero_cells <- function(data){

  # get list of species with <= 60 non zero cells
  l = lapply(data[4:17], function(x) length(x[x>0]))
  cond = lapply(l, function(x) x <= 60)
  l60 = l[unlist(cond)]
  cat("number of species with <= 60 non zero cells \n", length(l60), sep = "\n")

  #convert list to table with nb of observations and write to file
  df <- data.frame(matrix(unlist(l60)))
  row.names(df) = names(l60)
  colnames(df) = "presence_cells"
  xlsx::write.xlsx(df, here::here("/outputs/brts/poisson/list_shark_species_inf_equal_60.xlsx"))

  # calculate proportion of biomass represented by these species
  species_list = names(l60)
  abund_allspecies = sum(data$all_shark, na.rm=T)
  data60 = data[,species_list]
  abund_species_list = sum(rowSums(data60))
  prop = abund_species_list / abund_allspecies
  cat("proportion of abundance represented by these species \n", prop, sep = "\n")

  return(species_list)
}





#' Add rare shark species category by summing species with <= 60 non zero cells
#'
#' @param data
#'
#' @return
#' @export
#'

add_rare_shark_species_category <- function(data, rare_species_list){

  data %>%
    dplyr::mutate(rare_shark = rowSums(dplyr::across(rare_species_list))) -> data

  return(data)

}




#' Optimize BRT parameters by running through multiple iterations of BRT, changing tree.complexity,
#' learning rate and bag.fraction
#' for Poisson BRTs
#' @param tree.com
#' @param learn
#' @param bag.f
#' @param data_brts
#' @param species
#'
#' @return
#' @export
#'

optimize_poisson_brts <- function(tree.com, learn, bag.f, data_brts, species){

  # set seed for reproducibility
  set.seed(12345)

  # get column response number
  col_response = which(colnames(data_brts)==species)

  # assess optimal number of trees using k-fold cross validation
  # from help: The data is divided into 10 subsets, with stratification by prevalence if required for presence/absence data.
  # The function then fits a gbm model of increasing complexity along the sequence from n.trees to n.trees + (n.steps * step.size),
  # calculating the residual deviance at each step along the way. After each fold processed, the function calculates the average
  # holdout residual deviance and its standard error and then identifies the optimal number of trees as that at which the holdout
  # deviance is minimised. It fits a model with this number of trees, returning it as a gbm model along with additional information
  # from the cross-validation selection process.

  k1 = dismo::gbm.step(data = data_brts,
                       gbm.x = c("travel_time", "sst_mean",  "coral_hab_type",  "dist_reef", "pop_dens", "coralcover"),
                       gbm.y = col_response,
                       family = "poisson",
                       #offset= data_brts$id, replace with nb transect in 1 x 1 km cell
                       tree.complexity = tree.com,
                       learning.rate = learn,
                       bag.fraction = bag.f,
                       prev.stratify = FALSE,
                       n.folds = 10,
                       n.trees = 700,
                       step.size = 25,
                       silent = TRUE,
                       plot.main = FALSE,
                       n.cores = cores)

  k.out = list(interaction.depth = k1$interaction.depth,
               shrinkage = k1$shrinkage,
               n.trees = k1$n.trees,
               deviance = k1$self.statistics$mean.resid,
               cv.deviance = k1$cv.statistics$deviance.mean,
               cvse = k1$cv.statistics$deviance.se,
               cvcorr = k1$cv.statistics$correlation.mean,
               cvcorrse = k1$cv.statistics$correlation.se)

  return(k.out)
}






#' Optimize BRT parameters by running through multiple iterations of BRT, changing tree.complexity,
#' learning rate and bag.fraction
#' for Gaussian BRTs
#' @param tree.com
#' @param learn
#' @param bag.f
#' @param data_brts
#' @param species
#'
#' @return
#' @export
#'

optimize_gaussian_brts <- function(tree.com, learn, bag.f, data_brts, species){

  # set seed for reproducibility
  set.seed(12345)

  # create new column log-transformed
  data_brts2 = subset(data_brts, !is.na(data_brts[,species]))
  data_brts2$log_species = log(data_brts2[,species]+1)

  # problem with the below code log (species+1) not calculated correctly
  # data_brts2 %>%
  #   dplyr::filter(!is.na(get(species))) %>%
  #   dplyr::mutate(log_species = ifelse(species=="all_fish", log(get(species)),(log(get(species)+1))) -> data_brts2

  # get column response number
  col_response = which(colnames(data_brts2) == "log_species")

  # assess optimal number of trees using k-fold cross validation
  # from help: The data is divided into 10 subsets, with stratification by prevalence if required for presence/absence data.
  # The function then fits a gbm model of increasing complexity along the sequence from n.trees to n.trees + (n.steps * step.size),
  # calculating the residual deviance at each step along the way. After each fold processed, the function calculates the average
  # holdout residual deviance and its standard error and then identifies the optimal number of trees as that at which the holdout
  # deviance is minimised. It fits a model with this number of trees, returning it as a gbm model along with additional information
  # from the cross-validation selection process.

  k1 = dismo::gbm.step(data = data_brts2,
                       gbm.x = c("travel_time",  "sst_mean",  "coral_hab_type",  "dist_reef", "pop_dens", "coralcover"),
                       gbm.y = col_response,
                       family = "gaussian",
                       tree.complexity = tree.com,
                       learning.rate = learn,
                       bag.fraction = bag.f,
                       prev.stratify = FALSE,
                       n.folds = 10,
                       n.trees = 700,
                       step.size = 25,
                       silent = TRUE,
                       plot.main = FALSE,
                       n.cores = cores)

  k.out = list(interaction.depth = k1$interaction.depth,
               shrinkage = k1$shrinkage,
               n.trees = k1$n.trees,
               deviance = k1$self.statistics$mean.resid,
               cv.deviance = k1$cv.statistics$deviance.mean,
               cvse = k1$cv.statistics$deviance.se,
               cvcorr = k1$cv.statistics$correlation.mean,
               cvcorrse = k1$cv.statistics$correlation.se)

  return(k.out)
}



#' Extract best brts parameters
#'
#' @param output
#' @param species
#' @param distrib
#'
#' @return
#' @export
#'

extract_best_parameters <- function(output, species, distrib){

  # convert matrix to dataframe
  df <- na.omit(data.frame(matrix(unlist(output), ncol = 9, byrow = F)))

  # Change column names
  names(df) = c("model", "TC", "LR", "n.trees", "dev", "cv.dev", "cv.se", "corr", "corr.se")

  # change column class
  df[,2:9] <- lapply(df[,2:9], as.numeric)

  # Round columns
  df[,5:9]  =  round(df[,5:9],digits=3)

  # Sort by cv.dev and cv.se (used to assess the model, the lowest the better)
  #nb low cv.dev corresponds to high corr
  df  =  df[order(df$cv.dev, df$cv.se),]

  # add bag fraction
  df %>%
    dplyr::mutate(bag.fract = as.numeric(substr(df$model, nchar(df$model)-2, nchar(df$model)))) -> df

  #write results
  write.table(df, here::here(paste0("/outputs/brts/", distrib, "/train_results_", species, "_", distrib, ".txt")), row.names = FALSE)

  # extract best parameters
  best_parameters = df[1,]

  return(best_parameters)

}


#' Extract best brts parameters for parallel processing output
#'
#' @param output
#' @param species
#' @param distrib
#'
#' @return
#' @export
#'

extract_best_parameters_par <- function(output, species, distrib){

  # convert matrix to dataframe
  df <- na.omit(rbind(data.frame(matrix(unlist(output[[1]][[1]][[2]]), ncol = 9, byrow = F)),
                      data.frame(matrix(unlist(output[[1]][[2]][[2]]), ncol = 9, byrow = F)),
                      data.frame(matrix(unlist(output[[1]][[3]][[2]]), ncol = 9, byrow = F)),
                      data.frame(matrix(unlist(output[[2]][[1]][[2]]), ncol = 9, byrow = F)),
                      data.frame(matrix(unlist(output[[2]][[2]][[2]]), ncol = 9, byrow = F)),
                      data.frame(matrix(unlist(output[[2]][[3]][[2]]), ncol = 9, byrow = F)),
                      data.frame(matrix(unlist(output[[3]][[1]][[2]]), ncol = 9, byrow = F)),
                      data.frame(matrix(unlist(output[[3]][[2]][[2]]), ncol = 9, byrow = F)),
                      data.frame(matrix(unlist(output[[3]][[3]][[2]]), ncol = 9, byrow = F)),
                      data.frame(matrix(unlist(output[[4]][[1]][[2]]), ncol = 9, byrow = F)),
                      data.frame(matrix(unlist(output[[4]][[2]][[2]]), ncol = 9, byrow = F)),
                      data.frame(matrix(unlist(output[[4]][[3]][[2]]), ncol = 9, byrow = F)),
                      data.frame(matrix(unlist(output[[5]][[1]][[2]]), ncol = 9, byrow = F)),
                      data.frame(matrix(unlist(output[[5]][[2]][[2]]), ncol = 9, byrow = F)),
                      data.frame(matrix(unlist(output[[5]][[3]][[2]]), ncol = 9, byrow = F))
  ))

  # Change column names
  names(df) = c("model", "TC", "LR", "n.trees", "dev", "cv.dev", "cv.se", "corr", "corr.se")

  # change column class
  df[,2:9] <- lapply(df[,2:9], as.numeric)

  # Round columns
  df[,5:9]  =  round(df[,5:9],digits=3)

  # Sort by cv.dev and cv.se (used to assess the model, the lowest the better)
  #nb low cv.dev corresponds to high corr
  df  =  df[order(df$cv.dev, df$cv.se),]

  # add bag fraction
  df %>%
    dplyr::mutate(bag.fract = as.numeric(substr(df$model, nchar(df$model)-2, nchar(df$model)))) -> df

  #write results
  #write.table(df, here::here(paste0("/outputs/brts/", distrib, "/train_results_", species, "_", distrib, ".txt")), row.names = FALSE)

  # extract best parameters
  best_parameters = df[1,]

  return(best_parameters)

}



#' Fits optimal Poisson BRT
#'
#' @param data_brts
#' @param species
#' @param best_params
#'
#' @return
#' @export
#'

fit_best_poisson_brt <- function(data_brts, species, best_params){

  # get column response number
  col_response = which(colnames(data_brts) == species)

  #fit model with fixed number of trees
  tbmod = dismo::gbm.fixed(data = data_brts, gbm.x = c("travel_time",  "sst_mean",  "coral_hab_type",  "dist_reef", "pop_dens", "coralcover"),
                           gbm.y = col_response,
                           family = "poisson",
                           tree.complexity = best_params[,2],
                           learning.rate = best_params[,3],
                           bag.fraction = best_params[,10],
                           n.trees = best_params[,4])

  return(tbmod)
}


#' Fits optimal Gaussian BRT
#'
#' @param data_brts
#' @param species
#' @param best_params
#'
#' @return
#' @export
#'

fit_best_gaussian_brt <- function(data_brts, species, best_params){

  # create new column log-transformed
  data_brts2 = subset(data_brts, !is.na(data_brts[,species]))
  data_brts2$log_species = log(data_brts2[,species]+1)

  # get column response number
  col_response = which(colnames(data_brts2) == "log_species")

  #fit model with fixed number of trees
  tbmod = dismo::gbm.fixed(data = data_brts2, gbm.x = c("travel_time",  "sst_mean",  "coral_hab_type",  "dist_reef", "pop_dens", "coralcover"),
                           gbm.y = col_response,
                           family = "gaussian",
                           tree.complexity = best_params[,2],
                           learning.rate = best_params[,3],
                           bag.fraction = best_params[,10],
                           n.trees = best_params[,4])

  return(tbmod)
}



#' Fits reduced optimal Gaussian BRT after dropping predictors with contributions < 5%
#'
#' @param data_brts
#' @param species
#' @param best_params
#'
#' @return
#' @export
#'

fit_best_reduced_gaussian_brt <- function(data_brts, species,  best_params, preds){

  # create new column log-transformed
  data_brts2 = subset(data_brts, !is.na(data_brts[,species]))
  if(species=="all_fish"){
    data_brts2$log_species = log(data_brts2[,species])
  }else{
    data_brts2$log_species = log(data_brts2[,species]+1)
  }

  # get column response number
  col_response = which(colnames(data_brts2) == "log_species")

  #fit model with fixed number of trees
  tbmod = dismo::gbm.fixed(data = data_brts2, gbm.x = preds,
                           gbm.y = col_response,
                           family = "gaussian",
                           tree.complexity = best_params[,2],
                           learning.rate = best_params[,3],
                           bag.fraction = best_params[,10],
                           n.trees = best_params[,4])

  save(tbmod, file = here::here(paste0("outputs/brts/gaussian/", "mod_", species, ".RData")))

  return(tbmod)
}




#' Fits reduced optimal Poisson BRT after dropping predictors with contributions < 5%
#'
#' @param data_brts
#' @param species
#' @param best_params
#'
#' @return
#' @export
#'

fit_best_reduced_poisson_brt <- function(data_brts, species, best_params, preds){

  # get column response number
  col_response = which(colnames(data_brts) == species)

  #fit model with fixed number of trees
  tbmod = dismo::gbm.fixed(data = data_brts, gbm.x = preds,
                           gbm.y = col_response,
                           family = "poisson",
                           tree.complexity = best_params[,2],
                           learning.rate = best_params[,3],
                           bag.fraction = best_params[,10],
                           n.trees = best_params[,4])

  save(tbmod, file = here::here(paste0("outputs/brts/poisson/", "mod_", species, ".RData")))

  return(tbmod)
}



#' Make plot of variable contributions
#'
#' @param model
#' @param species
#' @param distrib
#'
#' @return
#' @export
#'

make_contribution_plot <- function(model, species, distrib){

  contrib = summary(model)

  plot = ggplot(data = contrib, aes(x = reorder(var, rel.inf), y = rel.inf))+
            geom_bar(stat="identity") +
            ylab("Relative contribution (%)") +
            xlab("") +
            coord_flip()+
            theme(axis.text=element_text(size=14))


  ggplot2::ggsave(here::here(paste0("/outputs/brts/", distrib, "/contributions_", species,"_", distrib , ".png")), plot, width = 6, height = 7)

}



#' Get variables with model contribution > 5%
#'
#' @param model
#'
#' @return
#' @export
#'

get_variables_contrib_sup5 <- function(model){

  contrib = summary(model)

  contrib %>% dplyr::filter(rel.inf > 5) -> contrib_sup5

  variables = contrib_sup5$var

  return(variables)

}


#' Make plot of variable contributions for reduced model
#'
#' @param model
#' @param species
#' @param distrib
#'
#' @return
#' @export
#'

make_contribution_reduced_plot <- function(model, species, distrib){

  contrib = summary(model)

  plot = ggplot(data = contrib, aes(x = reorder(var, rel.inf), y = rel.inf))+
    geom_bar(stat="identity") +
    ylab("Relative contribution (%)") +
    xlab("") +
    coord_flip()+
    theme(axis.text=element_text(size=14))

  ggplot2::ggsave(here::here(paste0("/outputs/brts/", distrib, "/contributions_reduced_", species,"_", distrib , ".png")), plot, width = 6, height = 7)

}


#' Partial dependance plots version 1
#'
#' @param model
#' @param species
#' @param distrib
#'
#' @return
#' @export
#'

partial_dependance_plots1 <- function(model, species, distrib){

  png(here::here(paste0("/outputs/brts/", distrib, "/partial_plots1_", species,"_", distrib, ".png")), width = 960, height = 960)
  dismo::gbm.plot(model, smooth = T, rug = T, write.title = F, show.contrib = T, plot.layout = c(3,3),
                  cex.lab = 2, cex.axis = 1.2, common.scale = FALSE)
  dev.off()

}


#' Partial dependance plots version 2
#'
#' @param model
#' @param species
#' @param distrib
#'
#' @return
#' @export
#'

partial_dependance_plots2 <- function(model, species, distrib){

  # draw plots
  plot1 <- plot(model, i.var = "travel_time")
  plot2 <- plot(model, i.var = "sst_mean")
  plot3 <- plot(model, i.var = "dist_reef")
  plot4 <- plot(model, i.var = "pop_dens")
  plot5 <- plot(model, i.var = "coral_hab_type")

  #save
  g <- gridExtra::arrangeGrob(plot1, plot2, plot3, plot4, plot5, nrow=2, ncol = 3)
  ggsave(file=here::here(paste0("/outputs/brts/", distrib, "/partial_plots2_", species, "_", distrib, ".png")), g, width = 8, height = 8)

}


#' Partial dependance plots version 3
#'
#' @param model
#' @param species
#' @param distrib
#'
#' @return
#' @export
#'

partial_dependance_plots3 <- function(model, species, distrib){

  predictors = model$var.names

  if (length(predictors)>=7){
    width = 800
    height = 800
  }

  if (length(predictors)==5 | length(predictors)==6){
    width = 500
    height = 600
  }

  if (length(predictors)==3 | length(predictors)==4){
    width = 600
    height = 600
  }

  if (length(predictors)==1 | length(predictors)==2){
    width = 600
    height = 300
  }

  png(here::here(paste0("/outputs/brts/", distrib, "/partial_plots3_", species, "_", distrib, ".png")), width = width, height = height)


  if (length(predictors)>=7){
    par(mfrow=c(3,3))
  }

  if (length(predictors)==5 | length(predictors)==6){
    par(mfrow=c(3,2))
  }

  if (length(predictors)==3 | length(predictors)==4){
    par(mfrow=c(2,2))
  }

  if (length(predictors)==2){
    par(mfrow=c(1,2))
  }

  for (j in 1:length(predictors)){

    #extract fitted value for predictor
    a=gbm::plot.gbm(model, i.var=j, return.grid=T)

    #plot
    if (species %in% c("Carcharhinus_amblyrhynchos", "Triaenodon_obesus")){
      plot(a, ylab="Abundance", pch = 16, cex = 0.7)
    }else{
      plot(a, ylab="Log biomass", pch = 16, cex = 0.7)
    }

    if (!predictors[j] %in%  c("coral_hab_type", "mpa_type")){
      #Fit a Smoothing Spline
      smoothingSpline = smooth.spline(a[,1], a[,2], spar=0.55)
      #add to plot
      lines(smoothingSpline, col= "blue", lwd=3)
    }

  }

  dev.off()

}


#' Make study area raster
#'
#'
#' @return
#' @export
#'

make_study_area_raster <- function(){

  #read NC land and reproject
  shp = rgdal::readOGR(here::here("data/nc_land/NOUVELLE_CALEDONIE.shp"))
  raster::crs(shp) <- sp::CRS('+init=EPSG:3163') ## lambert new caledonia
  shp2 = sp::spTransform(shp, "+init=epsg:4326")

  #polygon of main land
  shp3 = shp2[shp2$SHAPE_Area == max(shp2$SHAPE_Area),]

  #load sst raster for template
  r = raster::stack(here::here("/predictors", "sst_raster_uvcs.grd"))

  #rasterize
  shp_rast = raster::rasterize(shp3,  r[[1]])

  return(shp_rast)

}

#' Predict brt on study area
#'
#' @param model
#' @param data_type
#' @param distrib
#' @param species
#' @param preds
#' @param shp_rast
#'
#' @return
#' @export
#'

predict_brt <- function(model, data_type, distrib, species, preds, shp_rast){

  # Load rasters
  # load sst raster stack for bruvs
  sstbruvs = mspproject::load_sstbruvs_raster()

  # load sst raster stack for uvcs
  sstuvcs = mspproject::load_sstuvcs_raster()

  # load coral l3 raster (rename as predictor)
  coral_hab_type = mspproject::load_coral_l3_raster()

  # load travel time raster
  travel_time = mspproject::load_travel_time_raster()

  # load dist to reef raster and resample
  dist_reef = mspproject::load_dist_reef_raster()

  # load population density rasters and resample
  pop_dens_bruvs = mspproject::load_pop_dens_bruvs_raster()
  pop_dens_bruvs = raster::resample(pop_dens_bruvs, travel_time)
  pop_dens_uvcs = mspproject::load_pop_dens_uvcs_raster()
  pop_dens_uvcs = raster::resample(pop_dens_uvcs, travel_time)

  # load coral cover raster and resample
  coralcover = mspproject::load_coral_cover_raster()
  coralcover = raster::resample(coralcover, travel_time)

  # stack rasters (selecting sst raster depending on data type)
  if(data_type == "bruvs") {
    stackpreds = raster::stack(sstbruvs[["sst_mean"]],  coral_hab_type, travel_time, dist_reef, pop_dens_bruvs, coralcover)
  }else{
    stackpreds = raster::stack(sstuvcs[["sst_mean"]], coral_hab_type, travel_time, dist_reef, pop_dens_uvcs, coralcover)
  }

  names(stackpreds) = c("sst_mean",  "coral_hab_type", "travel_time", "dist_reef", "pop_dens", "coralcover")

  # restrict continuous predictors to range of values in data

  if(data_type == "bruvs") {
    data = bruvspoly
  }else{
    data = uvcspoly
  }

  sel = stackpreds[["sst_mean"]] <= max(data$sst_mean, na.rm=T) & stackpreds[["sst_mean"]] >= min(data$sst_mean, na.rm=T)
  cat("min sst_mean in data: ", min(data$sst_mean, na.rm=T), "\n")
  cat("max sst_mean in data: ", max(data$sst_mean, na.rm=T), "\n")
  cat("min sst_mean in predictor: ", min(raster::values(stackpreds[["sst_mean"]]), na.rm=T), "\n")
  cat("max sst_mean in predictor: ", max(raster::values(stackpreds[["sst_mean"]]), na.rm=T), "\n")
  sel = stackpreds[["travel_time"]] <= max(data$travel_time, na.rm=T) & stackpreds[["travel_time"]] >= min(data$travel_time, na.rm=T)
  cat("min travel_time in data: ", min(data$travel_time, na.rm=T), "\n")
  cat("max travel_time in data: ", max(data$travel_time, na.rm=T), "\n")
  cat("min travel_time in predictor: ", min(raster::values(stackpreds[["travel_time"]]), na.rm=T), "\n")
  cat("max travel_time in predictor: ", max(raster::values(stackpreds[["travel_time"]]), na.rm=T), "\n")
  is.na(stackpreds[["travel_time"]]) <- ! sel
  sel = stackpreds[["dist_reef"]] <= max(data$dist_reef, na.rm=T) & stackpreds[["dist_reef"]] >= min(data$dist_reef, na.rm=T)
  cat("min dist_reef in data: ", min(data$dist_reef, na.rm=T), "\n")
  cat("max dist_reef in data: ", max(data$dist_reef, na.rm=T), "\n")
  cat("min dist_reef in predictor: ", min(raster::values(stackpreds[["dist_reef"]]), na.rm=T), "\n")
  cat("max dist_reef in predictor: ", max(raster::values(stackpreds[["dist_reef"]]), na.rm=T), "\n")
  #is.na(stackpreds[["dist_reef"]]) <- ! sel #allow extrapolation for dist_reef (predictions will be constrained to corals habitats because coral_hab_type in the model and land mask)
  sel = stackpreds[["pop_dens"]] <= max(data$pop_dens, na.rm=T) & stackpreds[["pop_dens"]] >= min(data$pop_dens, na.rm=T)
  cat("min pop_dens in data: ", min(data$pop_dens, na.rm=T), "\n")
  cat("max pop_dens in data: ", max(data$pop_dens, na.rm=T), "\n")
  cat("min pop_dens in predictor: ", min(raster::values(stackpreds[["pop_dens"]]), na.rm=T), "\n")
  cat("max pop_dens in predictor: ", max(raster::values(stackpreds[["pop_dens"]]), na.rm=T), "\n")
  is.na(stackpreds[["pop_dens"]]) <- ! sel
  sel = stackpreds[["coralcover"]] <= max(data$coralcover, na.rm=T) & stackpreds[["coralcover"]] >= min(data$coralcover, na.rm=T)
  cat("min coralcover in data: ", min(data$coralcover, na.rm=T), "\n")
  cat("max coralcover in data: ", max(data$coralcover, na.rm=T), "\n")
  cat("min coralcover in predictor: ", min(raster::values(stackpreds[["coralcover"]]), na.rm=T), "\n")
  cat("max coralcover in predictor: ", max(raster::values(stackpreds[["coralcover"]]), na.rm=T), "\n")
  is.na(stackpreds[["coralcover"]]) <- ! sel


  # predict with selected predictors (preds)
  p = dismo::predict(stackpreds[[preds]], model, n.trees = model$gbm.call$best.trees, type = "response")
  #unit of predictions = nb of fish in 1 x 1 km cell

  #mask with study area raster (to avoid predictions on land0)
  p = raster::mask(p, shp_rast, inverse=T)

  # Write raster
  if (distrib == "poisson"){
    raster::writeRaster(p, here::here(paste0("outputs/brts/", distrib, "/predictions_", species, ".grd")), overwrite=TRUE)
  } else { #exp transformation of predictions
    raster::writeRaster(exp(p), here::here(paste0("outputs/brts/", distrib, "/predictions_", species, ".grd")), overwrite=TRUE)
  }

  return(p)

}




#' Map BRT prediction
#'
#' @param prediction
#' @param species
#' @param distrib
#'
#' @return
#' @export
#'

map_brt_prediction <- function(prediction, species, distrib){

  #read NC land and reproject
  shp = rgdal::readOGR(here::here("data/nc_land/NOUVELLE_CALEDONIE.shp"))
  raster::crs(shp) <- sp::CRS('+init=EPSG:3163') ## lambert new caledonia
  shp2 = sp::spTransform(shp, "+init=epsg:4326")

  #crop to extent
  prediction = raster::crop(prediction, raster::extent(-158, 168.5, -23.2, -17.8))

  png(here::here(paste0("/outputs/brts/", distrib, "/map_brt_prediction_", species, "_", distrib,".png")), width = 960, height = 480)
  if (distrib == "gaussian"){
    raster::plot(prediction, main=paste0('BRT prediction ', species, ' (LOG biomass (kg))'), col = viridisLite::viridis(10))
    sp::plot(shp2, add=T)
  }else{
    raster::plot(prediction, main=paste0('BRT prediction ', species, ' (nb individuals)'), col = viridisLite::viridis(10))
    sp::plot(shp2, add=T)
  }
  dev.off()

}



#' Map BRT prediction after exp transformation for uvcs
#'
#' @param prediction
#' @param species
#' @param distrib
#'
#' @return
#' @export
#'

map_brt_prediction_exp_transf <- function(prediction, species, distrib){

  #read NC land and reproject
  shp = rgdal::readOGR(here::here("data/nc_land/NOUVELLE_CALEDONIE.shp"))
  raster::crs(shp) <- sp::CRS('+init=EPSG:3163') ## lambert new caledonia
  shp2 = sp::spTransform(shp, "+init=epsg:4326")

  #crop to extent
  prediction = raster::crop(prediction, raster::extent(-158, 168.5, -23.2, -17.8))

  png(here::here(paste0("/outputs/brts/", distrib, "/map_brt_prediction_exp_transf_", species, "_", distrib,".png")), width = 960, height = 480)
  raster::plot(exp(prediction), main=paste0('BRT prediction ', species, ' biomass (kg)'), col = viridisLite::viridis(10))
  sp::plot(shp2, add=T)
  dev.off()
}


#' Map BRT prediction irregular color scale
#'
#' @param prediction
#' @param species
#' @param distrib
#' @param fivebreaks
#'
#' @return
#' @export
#'

map_brt_prediction_irreg_cols <- function(prediction, species, distrib, fivebreaks){

  #read NC land and reproject
  shp = rgdal::readOGR(here::here("data/nc_land/NOUVELLE_CALEDONIE.shp"))
  raster::crs(shp) <- sp::CRS('+init=EPSG:3163') ## lambert new caledonia
  shp2 = sp::spTransform(shp, "+init=epsg:4326")

  #crop to extent
  prediction = raster::crop(prediction, raster::extent(-158, 168.5, -23.2, -17.8))

  png(here::here(paste0("/outputs/brts/", distrib, "/map_brt_prediction_irreg_cols_", species, "_", distrib,".png")), width = 960, height = 480)
  if (distrib == "gaussian"){
    raster::plot(exp(prediction), breaks = fivebreaks, main=paste0('BRT prediction ', species, ' biomass (kg)'), col = c(viridisLite::viridis(5)), cex = 10)
    sp::plot(shp2, add=T)
  }else{
    raster::plot(prediction, breaks = fivebreaks, main=paste0('BRT prediction ', species, ' (nb individuals)'), col = c(viridisLite::viridis(5)))
    sp::plot(shp2, add=T)
  }
  dev.off()

}



#' Map BRT prediction quantile color scale
#'
#' @param prediction
#' @param species
#' @param distrib
#' @param fivebreaks
#'
#' @return
#' @export
#'

map_brt_prediction_quantile_cols <- function(prediction, species, distrib){

  #read NC land and reproject
  shp = rgdal::readOGR(here::here("data/nc_land/NOUVELLE_CALEDONIE.shp"))
  raster::crs(shp) <- sp::CRS('+init=EPSG:3163') ## lambert new caledonia
  shp2 = sp::spTransform(shp, "+init=epsg:4326")

  #crop to extent
  prediction = raster::crop(prediction, raster::extent(-158, 168.5, -23.2, -17.8))

  png(here::here(paste0("/outputs/brts/", distrib, "/map_brt_prediction_quantiles_cols_", species, "_", distrib,".png")), width = 960, height = 480)
  if (distrib == "gaussian"){
    prediction = exp(prediction)
    q1 = round(quantile(raster::values(prediction), 0.20, na.rm=T),3)
    q2 = round(quantile(raster::values(prediction), 0.40, na.rm=T),3)
    q3 = round(quantile(raster::values(prediction), 0.60, na.rm=T),3)
    q4 = round(quantile(raster::values(prediction), 0.80, na.rm=T),3)
    q5 = round(quantile(raster::values(prediction), 1, na.rm=T),3)
    #handle case when quantiles are equal
    if (q1==q2){
      q2 = q1+0.01
    }
    if (q2==q3){
      q3 = q2+0.01
    }
    if (q3==q4){
      q4 = q3+0.01
    }
    if (q4==q5){
      q5 = q4+0.01
    }
    raster::plot(prediction, breaks = c(0, q1, q2, q3, q4, q5), main=paste0('BRT prediction ', species, ' biomass (kg)'), col = c(viridisLite::viridis(5)),
                 legend.args = list(text='kg', side=4, line=2.5),
                 axis.args =  list(cex.axis=0.8))
    sp::plot(shp2, add=T)
  }else{
    q1 = round(quantile(raster::values(prediction), 0.20, na.rm=T),1)
    q2 = round(quantile(raster::values(prediction), 0.40, na.rm=T),1)
    q3 = round(quantile(raster::values(prediction), 0.60, na.rm=T),1)
    q4 = round(quantile(raster::values(prediction), 0.80, na.rm=T),1)
    q5 = round(quantile(raster::values(prediction), 1, na.rm=T),1)
    #handle case when quantiles are equal
    if (q1==q2){
      q2 = q1+0.01
    }
    if (q2==q3){
      q3 = q2+0.01
    }
    if (q3==q4){
      q4 = q3+0.01
    }
    if (q4==q5){
      q5 = q4+0.01
    }
    raster::plot(prediction, breaks = c(0, q1, q2, q3, q4, q5), main=paste0('BRT prediction ', species, ' (nb individuals)'), col = c(viridisLite::viridis(5)),
                 legend = F)
    raster::plot(prediction, legend.only= T, breaks = c(0, 10, 20, 30, 40, 50), col = c(viridisLite::viridis(5)),
                 lab.breaks = c(0, q1, q2, q3, q4, q5),
                 legend.args = list(text='Individuals', side=4, line=2.5), legend.shrink=1, zlim=c(0, 50),
                 axis.args =  list(cex.axis=0.8))
    sp::plot(shp2, add=T)
  }
  dev.off()

}



#'  Mask fish prediction with shark prediction so that predictions are on the same cells
#'
#' @param fish_species
#'
#' @return
#' @export
#'

mask_fish_species_predictions_with_sharks <- function(fish_species){

  fish_prediction = raster::raster(here::here(paste0("outputs/brts/gaussian/predictions_", fish_species, ".grd")))

  sharks_prediction = raster::raster(here::here("outputs/brts/poisson/predictions_summed_species.grd"))

  r = raster::mask(fish_prediction, sharks_prediction)

  raster::writeRaster(r, here::here(paste0("outputs/brts/gaussian/predictions_masked_", fish_species, ".grd")), overwrite=TRUE)

  return(r)

}




#'  Mask summed fish species prediction with shark prediction so that predictions are on the same cells
#'
#' @param fish_species
#'
#' @return
#' @export
#'

mask_summed_fish_species_predictions_with_sharks <- function(){

  summed_prediction = raster::raster(here::here("outputs/brts/gaussian/predictions_summed_species.grd"))

  sharks_prediction = raster::raster(here::here("outputs/brts/poisson/predictions_summed_species.grd"))

  r = raster::mask(summed_prediction, sharks_prediction)

  raster::writeRaster(r, here::here(paste0("outputs/brts/gaussian/predictions_masked_summed_species.grd")), overwrite=TRUE)

  return(r)

}


#' Map BRT prediction hotspots
#'
#' @param prediction
#' @param species
#' @param distrib
#'
#' @return
#' @export
#'

map_brt_prediction_hotspots <- function(prediction, species, distrib, percent){

  #read NC land and reproject
  shp = rgdal::readOGR(here::here("data/nc_land/NOUVELLE_CALEDONIE.shp"))
  raster::crs(shp) <- sp::CRS('+init=EPSG:3163') ## lambert new caledonia
  shp2 = sp::spTransform(shp, "+init=epsg:4326")

  #crop to extent
  prediction2 = raster::crop(prediction, raster::extent(-158, 168.5, -23.2, -17.8))

  # hotspots
  prediction_hotspots = prediction2 > quantile(raster::values(prediction2), percent, na.rm=T)

  png(here::here(paste0("/outputs/brts/", distrib, "/map_brt_prediction_top_", percent, "_", species,".png")), width = 960, height = 480)
  if (distrib == "gaussian"){
    raster::plot(prediction_hotspots, main=paste0('BRT prediction fish top ', 100-percent*100, ' % biomass'), legend=FALSE)
    sp::plot(shp2, add=T)
  }else{
    raster::plot(prediction_hotspots, main=paste0('BRT prediction shark top ', 100-percent*100, ' % abundance'), legend=FALSE)
    sp::plot(shp2, add=T)
  }
  dev.off()

}


#' Map co occurrence of fish and sharks based on 50% quantile (ie median)
#'
#' @param pred_fish
#' @param pred_sharks
#'
#' @return
#' @export
#'

map_cooccurrence_fish_shark <- function(){

  #load sharks and summed fish raster
  pred_fish = raster::raster(here::here("outputs/brts/gaussian/predictions_summed_species.grd"))
  pred_sharks = raster::raster(here::here("outputs/brts/poisson/predictions_summed_species.grd"))

  #crop to extent
  pred_fish2 = raster::crop(pred_fish, raster::extent(-158, 168.5, -23.2, -17.8))

  # fish quantiles (1: high fish - 2: low fish)
  quant_sup_fish = pred_fish2 > quantile(raster::values(pred_fish2), 0.5, na.rm=T)
  quant_inf_fish = pred_fish2 < quantile(raster::values(pred_fish2), 0.5, na.rm=T)
  quant_inf_fish[quant_inf_fish == 1] = 2
  quant_inf_fish[quant_inf_fish == 0] = NA
  quant_sup_fish[quant_sup_fish == 0] = NA

  #crop to extent
  pred_sharks2 = raster::crop(pred_sharks, raster::extent(-158, 168.5, -23.2, -17.8))

  # shark quantiles (1: high sharks - 2: low sharks)
  quant_sup_sharks = pred_sharks2 > quantile(raster::values(pred_sharks2), 0.5, na.rm=T)
  quant_inf_sharks = pred_sharks2 < quantile(raster::values(pred_sharks2), 0.5, na.rm=T)
  quant_inf_sharks[quant_inf_sharks == 1] = 2
  quant_inf_sharks[quant_inf_sharks == 0] = NA
  quant_sup_sharks[quant_sup_sharks == 0] = NA

  #stack fish and shark rasters
  quant_fish = raster::stack(quant_sup_fish, quant_inf_fish)
  quant_fish2 = raster::calc(quant_fish, fun = sum, na.rm=T)
  #plot(quant_fish2) #1: high fish - 2: low fish

  quant_sharks = raster::stack(quant_sup_sharks, quant_inf_sharks)
  quant_sharks2 = raster::calc(quant_sharks, fun = sum, na.rm=T)
  #plot(quant_sharks2) #1: high sharks - 2: low sharks

  quant = raster::stack(quant_fish2, quant_sharks2)
  names(quant) = c("quant_fish", "quant_sharks")

  #create new co-occurence raster

  #1 high fish high shark
  #2 high fish low shark
  #3 low fish high shark
  #4 low fish low shark

  fun = function(x) { ifelse(x[["quant_fish"]] == 1,
                             # if yes
                             ifelse(x[["quant_sharks"]] == 1,
                                    1,
                                    2),
                             # if no
                             ifelse(x[["quant_sharks"]] == 1,
                                    3,
                                    4))
  }


  x = raster::calc(quant, fun)

  # mask
  x2 = raster::mask(x, pred_fish2)

  png(here::here("/outputs/brts/map_cooccurrence_fish_shark.png"), width = 960, height = 480)

  #layout
  layout.matrix <- matrix(c(1, 2), ncol = 2)
  layout(mat = layout.matrix, heights = c(1,1), widths =c(5,1))

  #map
  arg = list(at=c(0.5, 1.5, 2.5, 3.5))

  par(mar = c(2, 0.5, 1, 1))
  raster::plot(x2, breaks = c(0,1,2,3,4), axis.args = arg, col = c("red", "orange", "light blue", "dark blue"), legend= FALSE, box = FALSE)

  # barplot
  ncell1 = raster::ncell(x2[x2==1])
  ncell2 = raster::ncell(x2[x2==2])
  ncell3 = raster::ncell(x2[x2==3])
  ncell4 = raster::ncell(x2[x2==4])

  ncells = c(ncell1, ncell2, ncell3, ncell4)

  par(mar=c(5, 23, 4, 2))
  barplot(ncells, names.arg = c("High fish / \n High shark", "High fish / \n Low shark", "Low fish / \n High shark", "Low fish / \n Low shark"),
          col = c("red", "orange", "light blue", "dark blue"), cex.names = 1,
          horiz=T, las=2, axes = FALSE, ann = FALSE)
  axis(1, cex.axis=1, las=2)
  title(xlab = "Nb cells", cex.lab = 1, line = 4)

  dev.off()



}



#' Map summed species predictions for fish species (without rare fish)
#'
#' @return
#' @export
#'

map_summed_fish_species_pred <- function(){

  #load and stack prediction rasters for all fish species (biomass in kg)
  raster_data = list.files(path=here::here("outputs/brts/gaussian"), pattern="predictions_.*.grd$", full.names=TRUE)

  #remove all_fish raster, summed species raster and rare fish ratesr
  raster_data = raster_data[raster_data != here::here("outputs/brts/gaussian/predictions_all_fish.grd")]
  raster_data = raster_data[raster_data != here::here("outputs/brts/gaussian/predictions_summed_species.grd")]
  raster_data = raster_data[raster_data != here::here("outputs/brts/gaussian/predictions_rare_fish.grd")]

  # read files as rasters
  s = raster::stack(raster_data)

  #calc summed predictions
  ss = raster::calc(s,fun=sum)

  #### map

  #read NC land and reproject
  shp = rgdal::readOGR(here::here("data/nc_land/NOUVELLE_CALEDONIE.shp"))
  raster::crs(shp) <- sp::CRS('+init=EPSG:3163') ## lambert new caledonia
  shp2 = sp::spTransform(shp, "+init=epsg:4326")

  #crop to extent
  prediction = raster::crop(ss, raster::extent(-158, 168.5, -23.2, -17.8))

  png(here::here("/outputs/brts/gaussian/map_brt_prediction_summed_species.png"), width = 960, height = 480)
  raster::plot(prediction, main='BRT prediction all species (biomass (kg)', col = viridisLite::viridis(10))
  sp::plot(shp2, add=T)
  dev.off()

  #map quantiles scale
  q1 = round(quantile(raster::values(prediction), 0.20, na.rm=T))
  q2 = round(quantile(raster::values(prediction), 0.40, na.rm=T))
  q3 = round(quantile(raster::values(prediction), 0.60, na.rm=T))
  q4 = round(quantile(raster::values(prediction), 0.80, na.rm=T))
  q5 = round(quantile(raster::values(prediction), 1, na.rm=T))

  png(here::here("/outputs/brts/gaussian/map_brt_prediction_summed_species_quantiles_cols.png"), width = 960, height = 480)
  raster::plot(prediction, breaks = c(0, q1, q2, q3, q4, q5), main='BRT prediction all species (biomass (kg)', col = c(viridisLite::viridis(5)),
               legend = F)
  raster::plot(prediction, legend.only= T, breaks = c(0, 10, 20, 30, 40, 50), col = c(viridisLite::viridis(5)),
               lab.breaks = c(0, q1, q2, q3, q4, q5),
               legend.args = list(text='kg', side=4, line=2.5), legend.shrink=1, zlim=c(0, 50),
               axis.args =  list(cex.axis=0.8))
  sp::plot(shp2, add=T)
  dev.off()

  #map irreg cols
  png(here::here("/outputs/brts/gaussian/map_brt_prediction_summed_species_irreg_cols.png"), width = 960, height = 480)
  raster::plot(prediction, breaks = c(0, 100, 200, 400, 800, 1600), main='BRT prediction all species (biomass (kg)', col = c(viridisLite::viridis(5)),
               legend.args = list(text='kg', side=4, line=2.5),
               axis.args =  list(cex.axis=0.8))
  sp::plot(shp2, add=T)
  dev.off()

  #write raster
  raster::writeRaster(ss, here::here("outputs/brts/gaussian/predictions_summed_species.grd"), overwrite=TRUE)

  return(s)


}




#' Map summed species predictions for shark species (without rare shark)
#'
#' @return
#' @export
#'

map_summed_shark_species_pred <- function(){

  #load and stack prediction rasters for all shark species (biomass in kg)
  raster_data = list.files(path=here::here("outputs/brts/poisson"), pattern="predictions_.*.grd$", full.names=TRUE)

  #remove all_shark raster, summed species raster and rare shark raster
  raster_data = raster_data[raster_data != here::here("outputs/brts/poisson/predictions_all_sharks.grd")]
  raster_data = raster_data[raster_data != here::here("outputs/brts/poisson/predictions_summed_species.grd")]
  raster_data = raster_data[raster_data != here::here("outputs/brts/poisson/predictions_rare_shark.grd")]

  # read files as rasters
  s = raster::stack(raster_data)

  #calc summed predictions
  ss = raster::calc(s,fun=sum)

  #### map

  #read NC land and reproject
  shp = rgdal::readOGR(here::here("data/nc_land/NOUVELLE_CALEDONIE.shp"))
  raster::crs(shp) <- sp::CRS('+init=EPSG:3163') ## lambert new caledonia
  shp2 = sp::spTransform(shp, "+init=epsg:4326")

  #crop to extent
  prediction = raster::crop(ss, raster::extent(-158, 168.5, -23.2, -17.8))

  png(here::here("/outputs/brts/poisson/map_brt_prediction_summed_species.png"), width = 960, height = 480)
  raster::plot(prediction, main='BRT prediction all species (abundance (ind))', col = viridisLite::viridis(10))
  sp::plot(shp2, add=T)
  dev.off()

  #map quantiles scale
  q1 = round(quantile(raster::values(prediction), 0.20, na.rm=T))
  q2 = round(quantile(raster::values(prediction), 0.40, na.rm=T))
  q3 = round(quantile(raster::values(prediction), 0.60, na.rm=T))
  q4 = round(quantile(raster::values(prediction), 0.80, na.rm=T))
  q5 = round(quantile(raster::values(prediction), 1, na.rm=T))

  png(here::here("/outputs/brts/poisson/map_brt_prediction_summed_species_quantiles_cols.png"), width = 960, height = 480)
  raster::plot(prediction, breaks = c(0, q1, q2, q3, q4, q5), main='BRT prediction all species (abundance (ind))', col = c(viridisLite::viridis(5)),
               legend = F)
  raster::plot(prediction, legend.only= T, breaks = c(0, 10, 20, 30, 40, 50), col = c(viridisLite::viridis(5)),
               lab.breaks = c(0, q1, q2, q3, q4, q5),
               legend.args = list(text='individuals', side=4, line=2.5), legend.shrink=1, zlim=c(0, 50),
               axis.args =  list(cex.axis=0.8))
  sp::plot(shp2, add=T)
  dev.off()

  #map irreg cols
  png(here::here("/outputs/brts/poisson/map_brt_prediction_summed_species_irreg_cols.png"), width = 960, height = 480)
  raster::plot(prediction, breaks = c(0, 2, 4, 6, 12, 16), main='BRT prediction all species (abundance (ind))', col = c(viridisLite::viridis(5)),
               legend.args = list(text='individuals', side=4, line=2.5),
               axis.args =  list(cex.axis=0.8))
  sp::plot(shp2, add=T)
  dev.off()

  #write raster
  raster::writeRaster(ss, here::here("outputs/brts/poisson/predictions_summed_species.grd"), overwrite=TRUE)

  return(s)


}






#' load contributions of reduced models for all fish species
#'
#' @param species_list
#'
#' @return
#' @export
#'

load_contributions_all_fish_species <- function(species_list){

  datalist = list()

  for (i in 1:length(species_list)){
    load(here::here(paste0("outputs/brts/gaussian/mod_", species_list[i], ".RData")))
    summary(tbmod) %>%
      dplyr::mutate(species = species_list[i]) %>%
      dplyr::mutate(var = as.factor(var)) -> dat
    datalist[[i]] <- dat
  }

  data = do.call(rbind, datalist)

  return(data)
}





#' load contributions of reduced models for all shark species
#'
#' @param species_list
#'
#' @return
#' @export
#'

load_contributions_all_shark_species <- function(species_list){

  datalist = list()

  for (i in 1:length(species_list)){
    load(here::here(paste0("outputs/brts/poisson/mod_", species_list[i], ".RData")))
    summary(tbmod) %>%
      dplyr::mutate(species = species_list[i]) %>%
      dplyr::mutate(var = as.factor(var)) -> dat
    datalist[[i]] <- dat
  }

  data = do.call(rbind, datalist)

  return(data)
}




#' plot frequency of variable selection for all fish species
#'
#' @return
#' @export
#'

plot_frequency_var_all_fish_species <- function(data){

  data %>%
    dplyr::group_by(var) %>%
    dplyr::summarise(n= dplyr::n()) -> freq

  plot = ggplot(data = freq, aes(x = reorder(var, n), y = n))+
    geom_bar(stat="identity") +
    ylab("Number of times selected") +
    xlab("") +
    coord_flip()+
    theme(axis.text=element_text(size=14))

  ggplot2::ggsave(here::here(paste0("/outputs/brts/gaussian/frequency_var_all_species.png")), plot, width = 6, height = 7)

}






#' plot frequency of variable selection for all shark species
#'
#' @return
#' @export
#'

plot_frequency_var_all_shark_species <- function(data){

  data %>%
    dplyr::group_by(var) %>%
    dplyr::summarise(n= dplyr::n()) -> freq

  plot = ggplot(data = freq, aes(x = reorder(var, n), y = n))+
    geom_bar(stat="identity") +
    ylab("Number of times selected") +
    xlab("") +
    coord_flip()+
    theme(axis.text=element_text(size=14))

  ggplot2::ggsave(here::here(paste0("/outputs/brts/poisson/frequency_var_all_species.png")), plot, width = 6, height = 7)

}




#' boxplot of contributions for all fish species
#'
#' @return
#' @export
#'

boxplot_contributions_all_fish_species <- function(data){

  data %>%
    dplyr::group_by(var) %>%
    dplyr::summarise(mean= mean(rel.inf)) -> t

  plot = ggplot(data = data, aes(x = reorder(var, rel.inf), y = rel.inf))+
    geom_boxplot() +
    ylab("Relative contributions (%)") +
    xlab("") +
    coord_flip()+
    theme(axis.text=element_text(size=14))

  ggplot2::ggsave(here::here(paste0("/outputs/brts/gaussian/boxplot_contributions_all_species.png")), plot, width = 6, height = 7)

}





#' boxplot of contributions for all shark species
#'
#' @return
#' @export
#'

boxplot_contributions_all_shark_species <- function(data){

  data %>%
    dplyr::group_by(var) %>%
    dplyr::summarise(mean= mean(rel.inf)) -> t

  plot = ggplot(data = data, aes(x = reorder(var, rel.inf), y = rel.inf))+
    geom_boxplot() +
    ylab("Relative contributions (%)") +
    xlab("") +
    coord_flip()+
    theme(axis.text=element_text(size=14))

  ggplot2::ggsave(here::here(paste0("/outputs/brts/poisson/boxplot_contributions_all_species.png")), plot, width = 6, height = 7)

}


#' summarize predictor values for stations (bruvs and uvcs) and region
#'
#' @param shp_rast
#'
#' @return
#' @export
#'

summarize_predictors <- function(shp_rast){

  # Load rasters

  # load sst raster stack for bruvs
  sstbruvs = mspproject::load_sstbruvs_raster()
  sstbruvs = sstbruvs[["sst_mean"]]

  # load sst raster stack for uvcs
  sstuvcs = mspproject::load_sstuvcs_raster()
  sstuvcs = sstuvcs[["sst_mean"]]

  # overlay sst raster for uvcs and bruvs
  #sst_mean = raster::overlay(sstbruvs[["sst_mean"]], sstuvcs[["sst_mean"]], fun = mean)

  # load coral l3 raster (rename as predictor)
  coral_hab_type = mspproject::load_coral_l3_raster()

  # load travel time raster
  travel_time = mspproject::load_travel_time_raster()

  # load dist to reef raster and resample
  dist_reef = mspproject::load_dist_reef_raster()
  dist_reef = raster::resample(dist_reef, travel_time)

  # load population density rasters and resample
  pop_dens_bruvs = mspproject::load_pop_dens_bruvs_raster()
  pop_dens_bruvs = raster::resample(pop_dens_bruvs, travel_time)
  pop_dens_uvcs = mspproject::load_pop_dens_uvcs_raster()
  pop_dens_uvcs = raster::resample(pop_dens_uvcs, travel_time)

  # load coral cover raster and resample
  coralcover = mspproject::load_coral_cover_raster()
  coralcover = raster::resample(coralcover, travel_time)

  # stack rasters
  stackpreds = raster::stack(sstbruvs,  sstuvcs, coral_hab_type, travel_time, dist_reef, pop_dens_bruvs, coralcover)
  names(stackpreds) = c("sstbruvs",  "sstuvcs", "coral_hab_type", "travel_time", "dist_reef", "pop_dens", "coralcover")

  #crop to extent and mask with study area raster
  stackpreds2 = raster::crop(stackpreds, raster::extent(-158, 168.5, -24, -17))
  shp_rast2 = raster::crop(shp_rast, raster::extent(-158, 168.5, -24, -17))
  stackpreds3 = raster::mask(stackpreds2, shp_rast2, inverse=T)

  #mask with coral habitat type
  #coral_hab_type2 = raster::crop(coral_hab_type, raster::extent(-158, 168.5, -24, -17))
  #stackpreds4 = raster::mask(stackpreds3, coral_hab_type2)

  #mask with shark predictions
  mask = raster::raster(here::here("outputs/brts/poisson/predictions_summed_species.grd"))
  mask = raster::crop(mask, raster::extent(-158, 168.5, -24, -17))
  stackpreds4 = raster::mask(stackpreds3, mask)

  # compute stats (mean, min, max) of continuous predictors for bruvs / uvcs/ whole region

  for (var in c( "travel_time", "dist_reef", "pop_dens", "coralcover")){

    assign(paste0("bruvs_min_", var), min(bruvspoly[,var], na.rm=T))
    assign(paste0("bruvs_max_", var), max(bruvspoly[,var], na.rm=T))
    assign(paste0("bruvs_mean_", var), mean(bruvspoly[,var], na.rm=T))

    assign(paste0("uvcs_min_", var), min(uvcspoly[,var], na.rm=T))
    assign(paste0("uvcs_max_", var), max(uvcspoly[,var], na.rm=T))
    assign(paste0("uvcs_mean_", var), mean(uvcspoly[,var], na.rm=T))

    assign(paste0("region_min_", var), min(raster::values(stackpreds4[[var]]), na.rm=T))
    assign(paste0("region_max_", var), max(raster::values(stackpreds4[[var]]), na.rm=T))
    assign(paste0("region_mean_", var), mean(raster::values(stackpreds4[[var]]), na.rm=T))

  }

  #separate treatment for sst because separate regional datasets for uvcs and bruvs
  var = "sst_mean"

  assign(paste0("bruvs_min_", var), min(bruvspoly[,var], na.rm=T))
  assign(paste0("bruvs_max_", var), max(bruvspoly[,var], na.rm=T))
  assign(paste0("bruvs_mean_", var), mean(bruvspoly[,var], na.rm=T))

  assign(paste0("uvcs_min_", var), min(uvcspoly[,var], na.rm=T))
  assign(paste0("uvcs_max_", var), max(uvcspoly[,var], na.rm=T))
  assign(paste0("uvcs_mean_", var), mean(uvcspoly[,var], na.rm=T))

  region_min_sstbruvs = min(raster::values(stackpreds4[["sstbruvs"]]), na.rm=T)
  region_max_sstbruvs = max(raster::values(stackpreds4[["sstbruvs"]]), na.rm=T)
  region_mean_sstbruvs = mean(raster::values(stackpreds4[["sstbruvs"]]), na.rm=T)

  region_min_sstuvcs = min(raster::values(stackpreds4[["sstuvcs"]]), na.rm=T)
  region_max_sstuvcs = max(raster::values(stackpreds4[["sstuvcs"]]), na.rm=T)
  region_mean_sstuvcs = mean(raster::values(stackpreds4[["sstuvcs"]]), na.rm=T)

  # sst_mean plot
  df <- data.frame(name = c("Region_BRUVS", "Region_UVCs", "BRUVS", "UVCs"),
                   mean = c(region_mean_sstbruvs, region_mean_sstuvcs, bruvs_mean_sst_mean, uvcs_mean_sst_mean),
                   min = c(region_min_sstbruvs, region_min_sstuvcs, bruvs_min_sst_mean, uvcs_min_sst_mean),
                   max = c(region_max_sstbruvs, region_max_sstuvcs, bruvs_max_sst_mean, uvcs_max_sst_mean))

  df$name = as.factor(df$name)
  df$name <- factor(df$name, levels = c("Region_BRUVS", "Region_UVCs", "BRUVS", "UVCs"))

  p = ggplot(aes(y = mean, x = name), data = df) +
    geom_point(size=5) +
    geom_segment(aes(x = 1, xend = 1, y = region_min_sstbruvs, yend = region_max_sstbruvs), size=2) +
    geom_segment(aes(x = 2, xend = 2, y = region_min_sstuvcs, yend = region_max_sstuvcs), size=2) +
    geom_segment(aes(x = 3, xend = 3, y = bruvs_min_sst_mean, yend = bruvs_max_sst_mean), size=2) +
    geom_segment(aes(x = 4, xend = 4, y = uvcs_min_sst_mean, yend = uvcs_max_sst_mean), size=2) +
    coord_flip() +
    ylab("sst_mean (°C)") +
    xlab("") +
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=14))

  ggplot2::ggsave(here::here("/outputs/brts/stats_sst_mean.png"), p, width = 6, height = 6)


  # travel_time plot
  df <- data.frame(name = c("Region", "BRUVS", "UVCs"),
                   mean = c(region_mean_travel_time, bruvs_mean_travel_time, uvcs_mean_travel_time),
                   min = c(region_min_travel_time, bruvs_min_travel_time, uvcs_min_travel_time),
                   max = c(region_max_travel_time, bruvs_max_travel_time, uvcs_max_travel_time))

  df$name = as.factor(df$name)
  df$name <- factor(df$name, levels = c("Region", "BRUVS", "UVCs"))

  p = ggplot(aes(y = mean/1000, x = name), data = df) +
    geom_point(size=5) +
    geom_segment(aes(x = 1, xend = 1, y = region_min_travel_time/1000, yend = region_max_travel_time/1000), size=2) +
    geom_segment(aes(x = 2, xend = 2, y = bruvs_min_travel_time/1000, yend = bruvs_max_travel_time/1000), size=2) +
    geom_segment(aes(x = 3, xend = 3, y = uvcs_min_travel_time/1000, yend = uvcs_max_travel_time/1000), size=2) +
    coord_flip() +
    ylab("travel_time (km)") +
    xlab("") +
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=14))

  ggplot2::ggsave(here::here("/outputs/brts/stats_travel_time.png"), p, width = 6, height = 6)



  # dist_reef plot
  df <- data.frame(name = c("Region", "BRUVS", "UVCs"),
                   mean = c(region_mean_dist_reef, bruvs_mean_dist_reef, uvcs_mean_dist_reef),
                   min = c(region_min_dist_reef, bruvs_min_dist_reef, uvcs_min_dist_reef),
                   max = c(region_max_dist_reef, bruvs_max_dist_reef, uvcs_max_dist_reef))

  df$name = as.factor(df$name)
  df$name <- factor(df$name, levels = c("Region", "BRUVS", "UVCs"))

  p = ggplot(aes(y = mean/1000, x = name), data = df) +
    geom_point(size=5) +
    geom_segment(aes(x = 1, xend = 1, y = region_min_dist_reef/1000, yend = region_max_dist_reef/1000), size=2) +
    geom_segment(aes(x = 2, xend = 2, y = bruvs_min_dist_reef/1000, yend = bruvs_max_dist_reef/1000), size=2) +
    geom_segment(aes(x = 3, xend = 3, y = uvcs_min_dist_reef/1000, yend = uvcs_max_dist_reef/1000), size=2) +
    coord_flip() +
    ylab("dist_reef (km)") +
    xlab("") +
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=14))

  ggplot2::ggsave(here::here("/outputs/brts/stats_dist_reef.png"), p, width = 6, height = 6)



  # pop_dens plot
  df <- data.frame(name = c("Region", "BRUVS", "UVCs"),
                   mean = c(region_mean_pop_dens, bruvs_mean_pop_dens, uvcs_mean_pop_dens),
                   min = c(region_min_pop_dens, bruvs_min_pop_dens, uvcs_min_pop_dens),
                   max = c(region_max_pop_dens, bruvs_max_pop_dens, uvcs_max_pop_dens))

  df$name = as.factor(df$name)
  df$name <- factor(df$name, levels = c("Region", "BRUVS", "UVCs"))

  p = ggplot(aes(y = mean, x = name), data = df) +
    geom_point(size=5) +
    geom_segment(aes(x = 1, xend = 1, y = region_min_pop_dens, yend = region_max_pop_dens), size=2) +
    geom_segment(aes(x = 2, xend = 2, y = bruvs_min_pop_dens, yend = bruvs_max_pop_dens), size=2) +
    geom_segment(aes(x = 3, xend = 3, y = uvcs_min_pop_dens, yend = uvcs_max_pop_dens), size=2) +
    coord_flip() +
    ylab("pop_dens (inhabitant / km2)") +
    xlab("") +
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=14))

  ggplot2::ggsave(here::here("/outputs/brts/stats_pop_dens.png"), p, width = 6, height = 6)



  # coralcover plot
  df <- data.frame(name = c("Region", "BRUVS", "UVCs"),
                   mean = c(region_mean_coralcover, bruvs_mean_coralcover, uvcs_mean_coralcover),
                   min = c(region_min_coralcover, bruvs_min_coralcover, uvcs_min_coralcover),
                   max = c(region_max_coralcover, bruvs_max_coralcover, uvcs_max_coralcover))

  df$name = as.factor(df$name)
  df$name <- factor(df$name, levels = c("Region", "BRUVS", "UVCs"))

  p = ggplot(aes(y = mean, x = name), data = df) +
    geom_point(size=5) +
    geom_segment(aes(x = 1, xend = 1, y = region_min_coralcover, yend = region_max_coralcover), size=2) +
    geom_segment(aes(x = 2, xend = 2, y = bruvs_min_coralcover, yend = bruvs_max_coralcover), size=2) +
    geom_segment(aes(x = 3, xend = 3, y = uvcs_min_coralcover, yend = uvcs_max_coralcover), size=2) +
    coord_flip() +
    ylab("coralcover (%)") +
    xlab("") +
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=14))

  ggplot2::ggsave(here::here("/outputs/brts/stats_coralcover.png"), p, width = 6, height = 6)



  # sst_mean plot
  df <- data.frame(name = c("BRUVS", "UVCs"),
                   mean = c(bruvs_mean_sst_mean, uvcs_mean_sst_mean),
                   min = c(bruvs_min_sst_mean, uvcs_min_sst_mean),
                   max = c(bruvs_max_sst_mean, uvcs_max_sst_mean))

  df$name = as.factor(df$name)
  df$name <- factor(df$name, levels = c("BRUVS", "UVCs"))

  p = ggplot(aes(y = mean, x = name), data = df) +
    geom_point(size=5) +
    geom_segment(aes(x = 1, xend = 1, y = bruvs_min_sst_mean, yend = bruvs_max_sst_mean), size=2) +
    geom_segment(aes(x = 2, xend = 2, y = uvcs_min_sst_mean, yend = uvcs_max_sst_mean), size=2) +
    coord_flip() +
    ylab("sst_mean (°C)") +
    xlab("") +
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=14))

  ggplot2::ggsave(here::here("/outputs/brts/stats_sst_mean_no_region.png"), p, width = 6, height = 6)


  # travel_time plot
  df <- data.frame(name = c("BRUVS", "UVCs"),
                   mean = c(bruvs_mean_travel_time, uvcs_mean_travel_time),
                   min = c(bruvs_min_travel_time, uvcs_min_travel_time),
                   max = c(bruvs_max_travel_time, uvcs_max_travel_time))

  df$name = as.factor(df$name)
  df$name <- factor(df$name, levels = c("BRUVS", "UVCs"))

  p = ggplot(aes(y = mean/1000, x = name), data = df) +
    geom_point(size=5) +
    geom_segment(aes(x = 1, xend = 1, y = bruvs_min_travel_time/1000, yend = bruvs_max_travel_time/1000), size=2) +
    geom_segment(aes(x = 2, xend = 2, y = uvcs_min_travel_time/1000, yend = uvcs_max_travel_time/1000), size=2) +
    coord_flip() +
    ylab("travel_time (km)") +
    xlab("") +
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=14))

  ggplot2::ggsave(here::here("/outputs/brts/stats_travel_time_no_region.png"), p, width = 6, height = 6)



  # dist_reef plot
  df <- data.frame(name = c("BRUVS", "UVCs"),
                   mean = c(bruvs_mean_dist_reef, uvcs_mean_dist_reef),
                   min = c(bruvs_min_dist_reef, uvcs_min_dist_reef),
                   max = c(bruvs_max_dist_reef, uvcs_max_dist_reef))

  df$name = as.factor(df$name)
  df$name <- factor(df$name, levels = c("BRUVS", "UVCs"))

  p = ggplot(aes(y = mean/1000, x = name), data = df) +
    geom_point(size=5) +
    geom_segment(aes(x = 1, xend = 1, y = bruvs_min_dist_reef/1000, yend = bruvs_max_dist_reef/1000), size=2) +
    geom_segment(aes(x = 2, xend = 2, y = uvcs_min_dist_reef/1000, yend = uvcs_max_dist_reef/1000), size=2) +
    coord_flip() +
    ylab("dist_reef (km)") +
    xlab("") +
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=14))

  ggplot2::ggsave(here::here("/outputs/brts/stats_dist_reef_no_region.png"), p, width = 6, height = 6)



  # pop_dens plot
  df <- data.frame(name = c("BRUVS", "UVCs"),
                   mean = c(bruvs_mean_pop_dens, uvcs_mean_pop_dens),
                   min = c(bruvs_min_pop_dens, uvcs_min_pop_dens),
                   max = c(bruvs_max_pop_dens, uvcs_max_pop_dens))

  df$name = as.factor(df$name)
  df$name <- factor(df$name, levels = c("BRUVS", "UVCs"))

  p = ggplot(aes(y = mean, x = name), data = df) +
    geom_point(size=5) +
    geom_segment(aes(x = 1, xend = 1, y = bruvs_min_pop_dens, yend = bruvs_max_pop_dens), size=2) +
    geom_segment(aes(x = 2, xend = 2, y = uvcs_min_pop_dens, yend = uvcs_max_pop_dens), size=2) +
    coord_flip() +
    ylab("pop_dens (inhabitant / km2)") +
    xlab("") +
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=14))

  ggplot2::ggsave(here::here("/outputs/brts/stats_pop_dens_no_region.png"), p, width = 6, height = 6)



  # coralcover plot
  df <- data.frame(name = c("BRUVS", "UVCs"),
                   mean = c(bruvs_mean_coralcover, uvcs_mean_coralcover),
                   min = c(bruvs_min_coralcover, uvcs_min_coralcover),
                   max = c(bruvs_max_coralcover, uvcs_max_coralcover))

  df$name = as.factor(df$name)
  df$name <- factor(df$name, levels = c("BRUVS", "UVCs"))

  p = ggplot(aes(y = mean, x = name), data = df) +
    geom_point(size=5) +
    geom_segment(aes(x = 1, xend = 1, y = bruvs_min_coralcover, yend = bruvs_max_coralcover), size=2) +
    geom_segment(aes(x = 2, xend = 2, y = uvcs_min_coralcover, yend = uvcs_max_coralcover), size=2) +
    coord_flip() +
    ylab("coralcover (%)") +
    xlab("") +
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=14))

  ggplot2::ggsave(here::here("/outputs/brts/stats_coralcover_no_region.png"), p, width = 6, height = 6)

}


#' summarize brts results for fish species
#'
#' @param species_list
#'
#' @return
#' @export
#'

summarize_brt_results_fish <- function(species_list){

  library(foreach) #need to load library to use %do% operator
  res = foreach(i=1:length(species_list), .combine=rbind) %do%
    read.table(here::here(paste0("/outputs/brts/gaussian/train_results_", species_list[i], "_gaussian.txt")), header = T)[1, ]

  cat("tree complexity \n")
  cat("mean :", mean(res$TC), "\n")
  cat("range :", range(res$TC), "\n")

  cat("learning rate \n")
  cat("mean :", mean(res$LR), "\n")
  cat("range :", range(res$LR), "\n")

  cat("nb trees \n")
  cat("mean :", mean(res$n.trees), "\n")
  cat("range :", range(res$n.trees), "\n")

  cat("bag fraction \n")
  cat("mean :", mean(res$bag.fract), "\n")
  cat("range :", range(res$bag.fract), "\n")

  cat("Cross-validation deviance \n")
  cat("mean :", mean(res$cv.dev), "\n")
  cat("range :", range(res$cv.dev), "\n")

  cat("Cross-validation deviance standard error \n")
  cat("mean :", mean(res$cv.se), "\n")
  cat("range :", range(res$cv.se), "\n")

  cat("Cross-validation correlation \n")
  cat("mean :", mean(res$corr), "\n")
  cat("range :", range(res$corr), "\n")

  cat("Cross-validation correlation standard error \n")
  cat("mean :", mean(res$corr.se), "\n")
  cat("range :", range(res$corr.se), "\n")

}



#' summarize brts results for shark species
#'
#' @param species_list
#'
#' @return
#' @export
#'

summarize_brt_results_shark <- function(species_list){

  library(foreach) #need to load library to use %do% operator
  res = foreach(i=1:length(species_list), .combine=rbind) %do%
    read.table(here::here(paste0("/outputs/brts/poisson/train_results_", species_list[i], "_poisson.txt")), header = T)[1, ]

  cat("tree complexity \n")
  cat("mean :", mean(res$TC), "\n")
  cat("range :", range(res$TC), "\n")

  cat("learning rate \n")
  cat("mean :", mean(res$LR), "\n")
  cat("range :", range(res$LR), "\n")

  cat("nb trees \n")
  cat("mean :", mean(res$n.trees), "\n")
  cat("range :", range(res$n.trees), "\n")

  cat("bag fraction \n")
  cat("mean :", mean(res$bag.fract), "\n")
  cat("range :", range(res$bag.fract), "\n")

  cat("Cross-validation deviance \n")
  cat("mean :", mean(res$cv.dev), "\n")
  cat("range :", range(res$cv.dev), "\n")

  cat("Cross-validation deviance standard error \n")
  cat("mean :", mean(res$cv.se), "\n")
  cat("range :", range(res$cv.se), "\n")

  cat("Cross-validation correlation \n")
  cat("mean :", mean(res$corr), "\n")
  cat("range :", range(res$corr), "\n")

  cat("Cross-validation correlation standard error \n")
  cat("mean :", mean(res$corr.se), "\n")
  cat("range :", range(res$corr.se), "\n")

}
