# tutoriel https://cran.r-project.org/web/packages/prioritizr/vignettes/prioritizr.html
# https://cran.r-project.org/web/packages/prioritizr/vignettes/saltspring.html

#gurobi installation
#manual download following instructions https://www.gurobi.com/academia/academic-program-and-licenses/ THEN
#install.packages('C:/gurobi911/win64/R/gurobi_9.1-1.zip', repos=NULL)
#install.packages('slam')

#load all functions
devtools::load_all()



##############################################  load prediction rasters  ##############################################

pred_summed_fish = load_summed_species_fish_pred() #fish predictions summed for modeled species masked with sharks predictions
#pred_all_sharks = load_all_sharks_pred() #predictions for all sharks modeled together
pred_summed_sharks = load_summed_species_shark_pred() #shark predictions summed for modeled species

# Load uvcs and bruvs grids
load("3-extract_predictors_at_stations_and_join_to_grid.RData")

#load fish species predictions
pred_fish_species = load_fish_species_pred()

#load shark species predictions
pred_shark_species = load_shark_species_pred()

# load study area raster
r = load_study_area_raster()

#calculate nb of raster cells with prediction
ncell = length(pred_summed_sharks[!is.na(pred_summed_sharks)])
ncell




##############################################  MSP for sharks - min set objective ##############################################



### selection of BLM value (ratio perimeter / surface) for sharks with min set objective
### round 1

# Loop of BLM valuess
blmval = c(0, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000, 10000, 100000)

for (p in blmval){
  print("----------------------------------------------------------")
  print(p)

  #design reserves with min set objective (set target at 0.5)
  reserves = design_sharks_reserves_minset_penalty(r, pred_shark_species, target = 0.5, penalty = p)

  #reformat p to avoid scientifc writing in objects
  p2 = format(p, scientific = FALSE)

  #calculate vector of patch surfaces
  assign(paste0("patch_surf_", p2), calc_patch_surfaces(reserves))

  #calculate mean of patch surfaces
  assign(paste0("mean_surf_", p2), mean(get(paste0("patch_surf_", p2))))

  #one-sample t test to compare the observed mean with the theoretical mean of 781km2
  if (length(get(paste0("patch_surf_", p2))) > 1){
    test = t.test(get(paste0("patch_surf_", p2)), mu = 781)
    print(test)
    #extract p-value
    #the p-value gives an indication on how likely your null hypothesis is (ie the two means are equal).
    #It is also defined as the smallest level of significance for which the data indicate rejection of the null hypothesis.
    assign(paste0("pval_", p2), test$p.value)
  }else{
    print("t-test impossible")
  }
}

#blmval 100000 did not converge so removed

mean_surf = c(mean_surf_0, mean_surf_0.00001, mean_surf_0.0001, mean_surf_0.001, mean_surf_0.01, mean_surf_0.1,
              mean_surf_1, mean_surf_10, mean_surf_100, mean_surf_1000, mean_surf_10000)

pval = c(pval_0, pval_0.00001, pval_0.0001, pval_0.001, pval_0.01, pval_0.1,
              pval_1, pval_10, pval_100, pval_1000, pval_10000)

# boxplot BLM vs pach surface
boxplot_sharks_blm_vs_patch_surf(patch_surf_0, patch_surf_0.00001, patch_surf_0.0001, patch_surf_0.001, patch_surf_0.01, patch_surf_0.1,
                                 patch_surf_1, patch_surf_10, patch_surf_100, patch_surf_1000, patch_surf_10000,
                                 pval, blmval[blmval != 100000], "1", 6200)


# plot BLM vs mean pach surface
plot_sharks_blm_vs_patch_surf(mean_surf, pval, blmval[blmval != 100000], "1", 3000)






### selection of BLM value (ratio perimeter / surface) for sharks with min set objective
### round 2

# Loop of BLM values
blmval = seq(20, 30, 1)

for (p in blmval){
  print("----------------------------------------------------------")
  print(p)

  #design reserves with min set objective (set target at 0.5)
  reserves = design_sharks_reserves_minset_penalty(r, pred_shark_species, target = 0.5, penalty = p)

  #calculate vector of patch surfaces
  assign(paste0("patch_surf_", p), calc_patch_surfaces(reserves))

  #calculate mean of patch surfaces
  assign(paste0("mean_surf_", p), mean(get(paste0("patch_surf_", p))))

  #one-sample t test to compare the observed mean with the theoretical mean of 781km2
  if (length(get(paste0("patch_surf_", p))) > 1){
    test = t.test(get(paste0("patch_surf_", p)), mu = 781)
    print(test)
    #extract p-value
    #the p-value is the proba to reject the null hyp (ie the observed means is equal to the theoretical mean) when it is true
    #smaller pvalue indicate stronger evidence for alternate hypothesis
    assign(paste0("pval_", p), test$p.value)
  }else{
    print("t-test impossible")
  }
}


mean_surf = c(mean_surf_20, mean_surf_21, mean_surf_22, mean_surf_23, mean_surf_24, mean_surf_25, mean_surf_26,
              mean_surf_27, mean_surf_28, mean_surf_29, mean_surf_30)

pval = c(pval_20, pval_21, pval_22, pval_23, pval_24, pval_25, pval_26,
              pval_27, pval_28, pval_29, pval_30)

# boxplot BLM vs pach surface
boxplot_sharks_blm_vs_patch_surf(patch_surf_20, patch_surf_21, patch_surf_22, patch_surf_23, patch_surf_24, patch_surf_25, patch_surf_26,
                                 patch_surf_27, patch_surf_28, patch_surf_29, patch_surf_30,
                                 pval, blmval, "2", 2000)


# plot BLM vs mean pach surface
plot_sharks_blm_vs_patch_surf(mean_surf, pval, blmval, "2", 900)



### selection of BLM value (ratio perimeter / surface) for sharks with min set objective
### round 3

# Loop of BLM values
blmval = seq(22, 23, 0.1)

for (p in blmval){
  print("----------------------------------------------------------")
  print(p)

  #design reserves with min set objective (set target at 0.5)
  reserves = design_sharks_reserves_minset_penalty(r, pred_shark_species, target = 0.5, penalty = p)

  #calculate vector of patch surfaces
  assign(paste0("patch_surf_", p), calc_patch_surfaces(reserves))

  #calculate mean of patch surfaces
  assign(paste0("mean_surf_", p), mean(get(paste0("patch_surf_", p))))

  #one-sample t test to compare the observed mean with the theoretical mean of 781km2
  if (length(get(paste0("patch_surf_", p))) > 1){
    test = t.test(get(paste0("patch_surf_", p)), mu = 781)
    print(test)
    #extract p-value
    #the p-value is the proba to reject the null hyp (ie the observed means is equal to the theoretical mean) when it is true
    #smaller pvalue indicate stronger evidence for alternate hypothesis
    assign(paste0("pval_", p), test$p.value)
  }else{
    print("t-test impossible")
  }
}


mean_surf = c(mean_surf_22, mean_surf_22.1, mean_surf_22.2, mean_surf_22.3, mean_surf_22.4, mean_surf_22.5, mean_surf_22.6,
              mean_surf_22.7, mean_surf_22.8, mean_surf_22.9, mean_surf_23)

pval = c(pval_22, pval_22.1, pval_22.2, pval_22.3, pval_22.4, pval_22.5, pval_22.6,
              pval_22.7, pval_22.8, pval_22.9, pval_23)


# boxplot BLM vs pach surface
boxplot_sharks_blm_vs_patch_surf(patch_surf_22, patch_surf_22.1, patch_surf_22.2, patch_surf_22.3, patch_surf_22.4, patch_surf_22.5, patch_surf_22.6,
                                 patch_surf_22.7, patch_surf_22.8, patch_surf_22.9, patch_surf_23,
                                 pval, blmval, "3", 4000)


# plot BLM vs mean pach surface
plot_sharks_blm_vs_patch_surf(mean_surf, pval, blmval, "3", 900)


######### selected blm = 22.5






### loop for 10 - 100% shark abundance target - min set objective - BLM set to 22.5

for (i in seq(0.1, 1, 0.1)){

  # design reserves
  assign(paste0("reserves_sharks_minset", i), design_sharks_reserves_minset_penalty(r, pred_shark_species, i, penalty = 22.5))

  # calculate proportion of fish biomass in reserves
  assign(paste0("prop_fish_in_sharks_reserves", i), calc_fish_prop_in_reserves(get(paste0("reserves_sharks_minset", i)) , pred_summed_fish)*100)

  # calculate proportion of shark abundance in reserves
  assign(paste0("prop_sharks_in_sharks_reserves", i), calc_shark_prop_in_reserves(get(paste0("reserves_sharks_minset", i)) , pred_summed_sharks)*100)

  # calculate sum of reserve surface in km2
  assign(paste0("surf_sharks_reserves", i), calc_reserve_surface(get(paste0("reserves_sharks_minset", i))))

}
surf_sharks_reserves = c(surf_sharks_reserves0.1, surf_sharks_reserves0.2, surf_sharks_reserves0.3, surf_sharks_reserves0.4, surf_sharks_reserves0.5,
                         surf_sharks_reserves0.6, surf_sharks_reserves0.7, surf_sharks_reserves0.8, surf_sharks_reserves0.9, surf_sharks_reserves1)

prop_fish_in_sharks_reserves = c(prop_fish_in_sharks_reserves0.1, prop_fish_in_sharks_reserves0.2, prop_fish_in_sharks_reserves0.3, prop_fish_in_sharks_reserves0.4, prop_fish_in_sharks_reserves0.5,
                                 prop_fish_in_sharks_reserves0.6, prop_fish_in_sharks_reserves0.7, prop_fish_in_sharks_reserves0.8, prop_fish_in_sharks_reserves0.9, prop_fish_in_sharks_reserves1)

prop_sharks_in_sharks_reserves = c(prop_sharks_in_sharks_reserves0.1, prop_sharks_in_sharks_reserves0.2, prop_sharks_in_sharks_reserves0.3, prop_sharks_in_sharks_reserves0.4, prop_sharks_in_sharks_reserves0.5,
                                 prop_sharks_in_sharks_reserves0.6, prop_sharks_in_sharks_reserves0.7, prop_sharks_in_sharks_reserves0.8, prop_sharks_in_sharks_reserves0.9, prop_sharks_in_sharks_reserves1)

#calc total surface
surf_tot = raster::ncell(pred_summed_sharks[is.na(pred_summed_sharks)==F])

# plot reserves
plot_sharks_reserves_minset(reserves_sharks_minset0.1, target_sharks = 0.1, target_fish = 0.3, prop_fish = prop_fish_in_sharks_reserves0.1, prop_sharks = prop_sharks_in_sharks_reserves0.1)
plot_sharks_reserves_minset(reserves_sharks_minset0.2, target_sharks = 0.2, target_fish = 0.3, prop_fish = prop_fish_in_sharks_reserves0.2, prop_sharks = prop_sharks_in_sharks_reserves0.2)
plot_sharks_reserves_minset(reserves_sharks_minset0.3, target_sharks = 0.3, target_fish = 0.3, prop_fish = prop_fish_in_sharks_reserves0.3, prop_sharks = prop_sharks_in_sharks_reserves0.3)
plot_sharks_reserves_minset(reserves_sharks_minset0.4, target_sharks = 0.4, target_fish = 0.3, prop_fish = prop_fish_in_sharks_reserves0.4, prop_sharks = prop_sharks_in_sharks_reserves0.4)
plot_sharks_reserves_minset(reserves_sharks_minset0.5, target_sharks = 0.5, target_fish = 0.3, prop_fish = prop_fish_in_sharks_reserves0.5, prop_sharks = prop_sharks_in_sharks_reserves0.5)
plot_sharks_reserves_minset(reserves_sharks_minset0.6, target_sharks = 0.6, target_fish = 0.3, prop_fish = prop_fish_in_sharks_reserves0.6, prop_sharks = prop_sharks_in_sharks_reserves0.6)
plot_sharks_reserves_minset(reserves_sharks_minset0.7, target_sharks = 0.7, target_fish = 0.3, prop_fish = prop_fish_in_sharks_reserves0.7, prop_sharks = prop_sharks_in_sharks_reserves0.7)
plot_sharks_reserves_minset(reserves_sharks_minset0.8, target_sharks = 0.8, target_fish = 0.3, prop_fish = prop_fish_in_sharks_reserves0.8, prop_sharks = prop_sharks_in_sharks_reserves0.8, barchart = TRUE)
plot_sharks_reserves_minset(reserves_sharks_minset0.9, target_sharks = 0.9, target_fish = 0.3, prop_fish = prop_fish_in_sharks_reserves0.9, prop_sharks = prop_sharks_in_sharks_reserves0.9)


#Plot contributions of shark reserves to the protection of fish
plot_contribution_shark_reserves_fish(prop_fish_in_sharks_reserves, surf_sharks_reserves, surf_tot)







##############################################  MSP for fish - min set objective ##############################################



### loop for 10 - 100%  fish species biomass target - min set objective

for (i in seq(0.1, 1, 0.1)){

  # design reserves
  assign(paste0("reserves_fish_minset", i), design_fish_species_reserves_minset(r, pred_fish_species, i))

  # calculate proportion of shark abundance in reserves
  assign(paste0("prop_sharks_in_fish_reserves", i), calc_shark_prop_in_reserves(get(paste0("reserves_fish_minset", i)) , pred_summed_sharks)*100)

  # calculate proportion of fish biomass in reserves
  assign(paste0("prop_fish_in_fish_reserves", i), calc_fish_prop_in_reserves(get(paste0("reserves_fish_minset", i)) , pred_summed_fish)*100)

  # calculate proportion of all shark abundance in reserves
  #assign(paste0("prop_all_sharks_minset_fish", i), calc_shark_prop_in_reserves(get(paste0("reserves_fish_minset", i)) , pred_all_sharks)*100)

  # calculate sum of reserve surface in km2
  assign(paste0("surf_fish_reserves", i), calc_reserve_surface(get(paste0("reserves_fish_minset", i))))

}

surf_fish_reserves = c(surf_fish_reserves0.1, surf_fish_reserves0.2, surf_fish_reserves0.3, surf_fish_reserves0.4, surf_fish_reserves0.5,
                       surf_fish_reserves0.6, surf_fish_reserves0.7, surf_fish_reserves0.8, surf_fish_reserves0.9, surf_fish_reserves1)

prop_sharks_in_fish_reserves = c(prop_sharks_in_fish_reserves0.1, prop_sharks_in_fish_reserves0.2, prop_sharks_in_fish_reserves0.3, prop_sharks_in_fish_reserves0.4, prop_sharks_in_fish_reserves0.5,
                                 prop_sharks_in_fish_reserves0.6, prop_sharks_in_fish_reserves0.7, prop_sharks_in_fish_reserves0.8, prop_sharks_in_fish_reserves0.9, prop_sharks_in_fish_reserves1)

prop_fish_in_fish_reserves = c(prop_fish_in_fish_reserves0.1, prop_fish_in_fish_reserves0.2, prop_fish_in_fish_reserves0.3, prop_fish_in_fish_reserves0.4, prop_fish_in_fish_reserves0.5,
                               prop_fish_in_fish_reserves0.6, prop_fish_in_fish_reserves0.7, prop_fish_in_fish_reserves0.8, prop_fish_in_fish_reserves0.9, prop_fish_in_fish_reserves1)

#prop_all_sharks_fish = c(prop_all_sharks_minset_fish0.1, prop_all_sharks_minset_fish0.2, prop_all_sharks_minset_fish0.3, prop_all_sharks_minset_fish0.4, prop_all_sharks_minset_fish0.5, prop_all_sharks_minset_fish0.6, prop_all_sharks_minset_fish0.7, prop_all_sharks_minset_fish0.8, prop_all_sharks_minset_fish0.9, prop_all_sharks_minset_fish1)


#calc total surface
surf_tot = raster::ncell(pred_summed_fish[is.na(pred_summed_fish)==F])

# plot reserves
plot_fish_reserves_minset(reserves_fish_minset0.1, target_fish = 0.1, target_sharks = 0.8, prop_sharks = prop_sharks_in_fish_reserves0.1, prop_fish = prop_fish_in_fish_reserves0.1)
plot_fish_reserves_minset(reserves_fish_minset0.2, target_fish = 0.2, target_sharks = 0.8, prop_sharks = prop_sharks_in_fish_reserves0.2, prop_fish = prop_fish_in_fish_reserves0.2)
plot_fish_reserves_minset(reserves_fish_minset0.3, target_fish = 0.3, target_sharks = 0.8, prop_sharks = prop_sharks_in_fish_reserves0.3, prop_fish = prop_fish_in_fish_reserves0.3, barchart = TRUE)
plot_fish_reserves_minset(reserves_fish_minset0.4, target_fish = 0.4, target_sharks = 0.8, prop_sharks = prop_sharks_in_fish_reserves0.4, prop_fish = prop_fish_in_fish_reserves0.4)
plot_fish_reserves_minset(reserves_fish_minset0.5, target_fish = 0.5, target_sharks = 0.8, prop_sharks = prop_sharks_in_fish_reserves0.5, prop_fish = prop_fish_in_fish_reserves0.5)
plot_fish_reserves_minset(reserves_fish_minset0.6, target_fish = 0.6, target_sharks = 0.8, prop_sharks = prop_sharks_in_fish_reserves0.6, prop_fish = prop_fish_in_fish_reserves0.6)
plot_fish_reserves_minset(reserves_fish_minset0.7, target_fish = 0.7, target_sharks = 0.8, prop_sharks = prop_sharks_in_fish_reserves0.7, prop_fish = prop_fish_in_fish_reserves0.7)
plot_fish_reserves_minset(reserves_fish_minset0.8, target_fish = 0.8, target_sharks = 0.8, prop_sharks = prop_sharks_in_fish_reserves0.8, prop_fish = prop_fish_in_fish_reserves0.8)
plot_fish_reserves_minset(reserves_fish_minset0.9, target_fish = 0.9, target_sharks = 0.8, prop_sharks = prop_sharks_in_fish_reserves0.9, prop_fish = prop_fish_in_fish_reserves0.9)



#Plot contributions of fish reserves to the protection of all sharks
#plot_contribution_fish_reserves_all_sharks(prop_all_sharks_fish, surf_fish, surf_tot)


#Plot contributions of fish reserves to the protection of sharks
plot_contribution_fish_reserves_sharks(prop_sharks_in_fish_reserves, surf_fish_reserves, surf_tot)





#Plot contributions of shark/fish reserves to the protection of fish/sharks
plot_contribution_shark_fish_reserves_fish_sharks(prop_fish_in_sharks_reserves, prop_sharks_in_fish_reserves)



##############################################  MSP for sharks in pre-determined fish reserves - min set objective ##############################################


### remaining shark abundance target - 30% fish biomss target - min set objective - BLM set to 22.5

# design reserves
reserves_sharks_in_fish_msp_minset = design_sharks_reserves_in_fish_msp_minset_penalty(r,
                                                                                       fish_msp = reserves_fish_minset0.3,
                                                                                       pred_sharks = pred_shark_species,
                                                                                       remaining_target_sharks = 80 - prop_sharks_in_fish_reserves0.3,
                                                                                       penalty = 22.5)

# calculate proportion of fish biomass in reserves
prop_fish_in_sharks_reserves_in_fish = calc_fish_prop_in_reserves(reserves_sharks_in_fish_msp_minset, pred_summed_fish)*100

# calculate proportion of shark abundance in reserves
prop_sharks_in_sharks_reserves_in_fish = calc_shark_prop_in_reserves(reserves_sharks_in_fish_msp_minset, pred_summed_sharks)*100

# calculate proportion of summed shark abundance in reserves
#prop_minset_sharks = calc_shark_prop_in_reserves(reserves_sharks_in_fish_msp_minset, pred_summed_sharks)*100

# calculate sum of reserve surface in km2
surf_sharks_in_sharks_reserves_in_fish = calc_reserve_surface(reserves_sharks_in_fish_msp_minset)


# plot reserves
plot_sharks_reserves_in_fish_msp_minset(reserves_sharks_in_fish_msp_minset,
                                        target_sharks = (80 - prop_sharks_in_fish_reserves0.3)/100,
                                        target_fish = 0.3,
                                        prop_fish = prop_fish_in_sharks_reserves_in_fish,
                                        prop_sharks = prop_sharks_in_sharks_reserves_in_fish,
                                        barchart = TRUE)




# Plot sharks and fish reserves in fish msp
plot_sharks_fish_reserves_in_fish_msp_minset(reserves_sharks = reserves_sharks_in_fish_msp_minset,
                                             reserves_fish = reserves_fish_minset0.3,
                                             target_sharks = 80 - prop_sharks_in_fish_reserves0.3,
                                             target_fish = 30,
                                             prop_fish_in_sharks = prop_fish_in_sharks_reserves_in_fish,
                                             prop_sharks_in_fish = prop_sharks_in_fish_reserves0.3,
                                             prop_fish_in_fish = prop_fish_in_fish_reserves0.3,
                                             prop_sharks_in_sharks = prop_sharks_in_sharks_reserves_in_fish,
                                             barchart = TRUE)














##############################################  MSP for sharks and fish - min set objective ##############################################


### 80% shark abundance target - 30% fish biomss target - min set objective - BLM set to 22.5

# design reserves
reserves_sharksandfish_minset = design_sharksandfish_reserves_minset_penalty(r, pred_sharks = pred_shark_species, pred_fish = pred_fish_species,
                                                                             target_sharks = 0.8, target_fish = 0.3, penalty = 22.5)

# calculate proportion of summed fish biomass in reserves
prop_fish_in_sharksandfish_reserves = calc_fish_prop_in_reserves(reserves_sharksandfish_minset, pred_summed_fish)*100

# calculate proportion of summed shark abundance in reserves
prop_sharks_in_sharksandfish_reserves = calc_shark_prop_in_reserves(reserves_sharksandfish_minset, pred_summed_sharks)*100

# calculate sum of reserve surface in km2
surf_sharksandfish_reserves = calc_reserve_surface(reserves_sharksandfish_minset)




# plot reserves
plot_sharksandfish_reserves_minset(reserves_sharksandfish_minset,
                                   target_sharks = 0.8,
                                   target_fish = 0.3,
                                   prop_fish = prop_fish_in_sharksandfish_reserves,
                                   prop_sharks = prop_sharks_in_sharksandfish_reserves,
                                   barchart = TRUE)





################ irreplaceability maps

map_irreplaceability_sharks()

map_irreplaceability_fish()










############################################## sharks and fish reserves comparison: maps ##############################################


# Map of the co-occurrence of shark and fish reserves and barchart of the associated targets
for (i in seq(0.1, 1, 0.1)){

  map_cooccurrence_reserves_fish_shark(get(paste0("reserves_sharks_minset", i)), get(paste0("reserves_fish_minset", i)), target=i)

}






############################################## sharks and fish reserves comparison: diversity indexes  ##############################################


### jaccard index

for (i in seq(0.1, 1, 0.1)){

  assign(paste0("jac", i), calculate_jaccard(get(paste0("reserves_sharks_minset", i)), get(paste0("reserves_fish_minset", i))))

}

# plot jaccard index
png(here::here("outputs/msp/plot_jaccard_similarity_sharks_fish.png"))
plot(seq(10, 100, 10), c(jac0.1, jac0.2, jac0.3, jac0.4, jac0.5, jac0.6, jac0.7, jac0.8, jac0.9, jac1),
     axes = F,
     xlab = "Target of fish or shark protection (%)",
     ylab = "Jaccard similarity index")
axis(side=1, at=seq(10,100,10))
axis(side=2, at=seq(0,1,0.1))
dev.off()

png(here::here("outputs/msp/plot_jaccard_dissimilarity_sharks_fish.png"))
plot(seq(10, 100, 10), 1-c(jac0.1, jac0.2, jac0.3, jac0.4, jac0.5, jac0.6, jac0.7, jac0.8, jac0.9, jac1),
     axes = F,
     xlab = "Target of fish or shark protection (%)",
     ylab = "Jaccard disimilarity index")
axis(side=1, at=seq(10,100,10))
axis(side=2, at=seq(0,1,0.1))
dev.off()



### sorensen, simpson and nestedness indexes

for (i in seq(0.1, 1, 0.1)){

  print(i)
  #sorensen includes nestedness and turnover
  assign(paste0("sorensen", i), calculate_sorensen(get(paste0("reserves_sharks_minset", i)), get(paste0("reserves_fish_minset", i))))
  #simpson includes turnover only
  assign(paste0("simpson", i), calculate_simpson(get(paste0("reserves_sharks_minset", i)), get(paste0("reserves_fish_minset", i))))
  #nestedness is calculated based on sorensen and simpson
  assign(paste0("nestedness", i), get(paste0("sorensen", i)) - get(paste0("simpson", i)))

}

# plot turnover index (simpson)
png(here::here("outputs/msp/plot_turnover_sharks_fish.png"))
plot(seq(10, 100, 10), c(simpson0.1, simpson0.2, simpson0.3, simpson0.4, simpson0.5, simpson0.6, simpson0.7, simpson0.8, simpson0.9, simpson1),
     axes = F,
     xlab = "Target of fish or shark protection (%)",
     ylab = "Turnover index")
axis(side=1, at=seq(10,100,10))
axis(side=2, at=seq(0,1,0.1))
dev.off()

# plot nestedness index
png(here::here("outputs/msp/plot_nestedness_sharks_fish.png"))
plot(seq(10, 100, 10), c(nestedness0.1, nestedness0.2, nestedness0.3, nestedness0.4, nestedness0.5, nestedness0.6, nestedness0.7, nestedness0.8, nestedness0.9, nestedness1),
     axes = F,
     xlab = "Target of fish or shark protection (%)",
     ylab = "Nestedness index")
axis(side=1, at=seq(10,100,10))
axis(side=2, at=seq(0,0.11,0.01))
dev.off()


# plot turnover and nestedness index
png(here::here("outputs/msp/plot_nestedness_turnover_sharks_fish.png"))
par(mar = c(5,5,3,5))
plot(seq(10, 100, 10), c(nestedness0.1, nestedness0.2, nestedness0.3, nestedness0.4, nestedness0.5, nestedness0.6, nestedness0.7, nestedness0.8, nestedness0.9, nestedness1),
     xlab = "Target of fish or shark protection (%)",
     ylab = "",
     col="red",
     pch=16, axes = F)
axis(side=1, at=seq(10,100,10))
axis(side=2, at=seq(0,0.11,0.01), las =2,  col.ticks = "red", col.axis = "red")
mtext("Nestedness", side=2, line= 3.5, col = "red")
par(new=T)
plot(seq(10, 100, 10), c(simpson0.1, simpson0.2, simpson0.3, simpson0.4, simpson0.5, simpson0.6, simpson0.7, simpson0.8, simpson0.9, simpson1),
     xlab = "",
     ylab = "",
     xaxt = "n",
     yaxt = "n",
     col= scales::alpha("blue", 0.6),
     pch=16)
axis(side=4, at=seq(0,1,0.1), las=2, col.ticks = "blue", col.axis = "blue")
mtext("Turnover", side=4, line= 3, col = "blue")
dev.off()



### beta ratio and jaccard (folowing albouy)

for (i in seq(0.1, 1, 0.1)){

  print(i)
  #beta ratio useful to describe the relative contribution of each component (species replacement vs. nestedness) in explaining
  #the overall amount of turnover
  assign(paste0("beta_ratio", i), calculate_beta_ratio(get(paste0("reserves_sharks_minset", i)), get(paste0("reserves_fish_minset", i))))

  #jaccard dissimilarity
  assign(paste0("jac_diss", i), calculate_jaccard_diss(get(paste0("reserves_sharks_minset", i)), get(paste0("reserves_fish_minset", i))))

}


# plot beta ratio and jaccard dissimilarity
png(here::here("outputs/msp/plot_beta_ratio_jaccard_dissim_sharks_fish_ordered_surface.png"))
par(mar = c(5,5,5,5))
plot(surf_fish_reserves[1:9] - surf_sharks_reserves [1:9], #remove target 100%
     c(beta_ratio0.1, beta_ratio0.2, beta_ratio0.3, beta_ratio0.4, beta_ratio0.5, beta_ratio0.6, beta_ratio0.7, beta_ratio0.8, beta_ratio0.9),
     xlab = "Surface fish reserve - surface shark reserve",
     ylab = "",
     col="red",
     pch=16, axes = F, las=2)
axis(side=1, at= surf_fish_reserves - surf_sharks_reserves, las =2)
axis(side=2, at=seq(0,0.14,0.01), las =2,  col.ticks = "red", col.axis = "red")
mtext("Beta ratio", side=2, line= 3.5, col = "red")
par(new=T)
plot(surf_fish_reserves[1:9] - surf_sharks_reserves [1:9], #remove target 100%
     c(jac_diss0.1, jac_diss0.2, jac_diss0.3, jac_diss0.4, jac_diss0.5, jac_diss0.6, jac_diss0.7, jac_diss0.8, jac_diss0.9),
     xlab = "",
     ylab = "",
     xaxt = "n",
     yaxt = "n",
     col= scales::alpha("blue", 0.6),
     pch=16)
axis(side=4, at=seq(0,1,0.1), las=2, col.ticks = "blue", col.axis = "blue")
axis(side=3, at= surf_fish_reserves[1:9] - surf_sharks_reserves [1:9], labels = seq(10, 90, 10), las =2)
mtext("Target of fish or shark protection (%)", side = 3, line = 3)
mtext("Jaccard dissim", side=4, line= 3, col = "blue")
dev.off()


png(here::here("outputs/msp/plot_beta_ratio_jaccard_dissim_sharks_fish_ordered_target.png"))
par(mar = c(5,5,5,5))
plot(seq(10, 90, 10),
     c(beta_ratio0.1, beta_ratio0.2, beta_ratio0.3, beta_ratio0.4, beta_ratio0.5, beta_ratio0.6, beta_ratio0.7, beta_ratio0.8, beta_ratio0.9),
     xlab = "Target of fish or shark protection (%)",
     ylab = "",
     col="red",
     pch=16, axes = F, las=2)
axis(side=1, at= seq(10, 90, 10), las =2)
axis(side=2, at=seq(0,0.14,0.01), las =2,  col.ticks = "red", col.axis = "red")
mtext("Beta ratio", side=2, line= 3.5, col = "red")
par(new=T)
plot(seq(10, 90, 10),
     c(jac_diss0.1, jac_diss0.2, jac_diss0.3, jac_diss0.4, jac_diss0.5, jac_diss0.6, jac_diss0.7, jac_diss0.8, jac_diss0.9),
     xlab = "",
     ylab = "",
     xaxt = "n",
     yaxt = "n",
     col= scales::alpha("blue", 0.6),
     pch=16)
axis(side=4, at=seq(0,1,0.1), las=2, col.ticks = "blue", col.axis = "blue")
axis(side=3, at= seq(10, 90, 10), labels = surf_fish_reserves[1:9] - surf_sharks_reserves [1:9], las =2)
mtext("Surface fish reserve - surface shark reserve", side = 3, line = 3)
mtext("Jaccard dissim", side=4, line= 3, col = "blue")
dev.off()


png(here::here("outputs/msp/plot_beta_ratio_jaccard_dissim_sharks_fish.png"))
par(mar = c(5,5,5,5))
plot(seq(10, 90, 10),
     c(beta_ratio0.1, beta_ratio0.2, beta_ratio0.3, beta_ratio0.4, beta_ratio0.5, beta_ratio0.6, beta_ratio0.7, beta_ratio0.8, beta_ratio0.9),
     xlab = "Target of fish or shark protection (%)",
     ylab = "",
     col="red",
     pch=16, axes = F, las=2)
axis(side=1, at= seq(10, 90, 10), las =2)
axis(side=2, at=seq(0,0.14,0.01), las =2,  col.ticks = "red", col.axis = "red")
mtext("Beta ratio", side=2, line= 3.5, col = "red")
par(new=T)
plot(seq(10, 90, 10),
     c(jac_diss0.1, jac_diss0.2, jac_diss0.3, jac_diss0.4, jac_diss0.5, jac_diss0.6, jac_diss0.7, jac_diss0.8, jac_diss0.9),
     xlab = "",
     ylab = "",
     xaxt = "n",
     yaxt = "n",
     col= scales::alpha("blue", 0.6),
     pch=16)
axis(side=4, at=seq(0,1,0.1), las=2, col.ticks = "blue", col.axis = "blue")
mtext("Jaccard dissim", side=4, line= 3, col = "blue")
dev.off()

png(here::here("outputs/msp/plot_diff_reserve_surface.png"), width = 680, height = 480)
par(mar = c(5,5,5,5))
plot(seq(10, 90, 10),
     surf_fish_reserves[1:9] - surf_sharks_reserves [1:9], #remove target 100%
     xlab = "Target of fish or shark protection (%)",
     ylab = "",
     type="b",
     las =2)
axis(side=1, at= seq(10, 90, 10), las =2)
mtext("Surface fish reserve - surface shark reserve", side=2, line= 3.5)
dev.off()


png(here::here("outputs/msp/plot_diff_reserve_surface_tot_surface.png"), width = 680, height = 480)
par(mar = c(5,5,5,5))
plot(seq(10, 90, 10),
     (surf_fish_reserves[1:9] - surf_sharks_reserves [1:9])/surf_tot, #remove target 100%
     xlab = "Target of fish or shark protection (%)",
     ylab = "",
     type="b",
     las =2)
axis(side=1, at= seq(10, 90, 10), las =2)
mtext("(Surface fish - surface shark) / total surface", side=2, line= 3.5)
dev.off()


png(here::here("outputs/msp/plot_div_reserve_surface.png"), width = 680, height = 380)
par(mar = c(5,5,5,5))
plot(seq(10, 90, 10),
     (surf_fish_reserves[1:9] / surf_sharks_reserves [1:9]), #remove target 100%
     xlab = "Target of fish or shark protection (%)",
     ylab = "",
     type="b",
     las =2)
axis(side=1, at= seq(10, 90, 10), las =2)
mtext("Surface fish / surface shark", side=2, line= 3.5)
dev.off()



### beta ratio and jaccard accross successive targets for sharks (folowing albouy)

for (i in seq(0.1, 0.9, 0.1)){

  print(i)
  #beta ratio useful to describe the relative contribution of each component (species replacement vs. nestedness) in explaining
  #the overall amount of turnover
  assign(paste0("beta_ratio_sharks", i), calculate_beta_ratio(get(paste0("reserves_sharks_minset", i)), get(paste0("reserves_sharks_minset", i+0.1))))

  #jaccard dissimilarity
  assign(paste0("jac_diss_sharks", i), calculate_jaccard_diss(get(paste0("reserves_sharks_minset", i)), get(paste0("reserves_sharks_minset", i+0.1))))

}

png(here::here("outputs/msp/plot_beta_ratio_jaccard_dissim_sharks.png"))
par(mar = c(5,5,5,5))
plot(seq(10, 90, 10),
     c(beta_ratio_sharks0.1, beta_ratio_sharks0.2, beta_ratio_sharks0.3, beta_ratio_sharks0.4, beta_ratio_sharks0.5, beta_ratio_sharks0.6, beta_ratio_sharks0.7, beta_ratio_sharks0.8, beta_ratio_sharks0.9),
     xlab = "",
     ylab = "",
     col="red",
     pch=16, axes = F, las=2)
mtext("Target of shark protection (%)", side=1, line= 3.5)
axis(side=1, at= seq(10, 90, 10), labels = c("10-20", "20-30", "30-40",
                                             "40-50", "50-60", "60-70", "70-80", "80-90", "90-100"), las =2)
axis(side=2, at=seq(0.2,0.6,0.1), las =2,  col.ticks = "red", col.axis = "red")
mtext("Beta ratio", side=2, line= 3.5, col = "red")
par(new=T)
plot(seq(10, 90, 10),
     c(jac_diss_sharks0.1, jac_diss_sharks0.2, jac_diss_sharks0.3, jac_diss_sharks0.4, jac_diss_sharks0.5, jac_diss_sharks0.6, jac_diss_sharks0.7, jac_diss_sharks0.8, jac_diss_sharks0.9),
     xlab = "",
     ylab = "",
     xaxt = "n",
     yaxt = "n",
     col= scales::alpha("blue", 0.6),
     pch=16)
axis(side=4, at=seq(0.2,0.5,0.1), las=2, col.ticks = "blue", col.axis = "blue")
mtext("Jaccard dissim", side=4, line= 3, col = "blue")
dev.off()



### beta ratio and jaccard accross successive targets for fish (folowing albouy)

for (i in seq(0.1, 0.9, 0.1)){

  print(i)
  #beta ratio useful to describe the relative contribution of each component (species replacement vs. nestedness) in explaining
  #the overall amount of turnover
  assign(paste0("beta_ratio_fish", i), calculate_beta_ratio(get(paste0("reserves_fish_minset", i)), get(paste0("reserves_fish_minset", i+0.1))))

  #jaccard dissimilarity
  assign(paste0("jac_diss_fish", i), calculate_jaccard_diss(get(paste0("reserves_fish_minset", i)), get(paste0("reserves_fish_minset", i+0.1))))

}

png(here::here("outputs/msp/plot_beta_ratio_jaccard_dissim_fish.png"))
par(mar = c(5,5,5,5))
plot(seq(10, 90, 10),
     c(beta_ratio_fish0.1, beta_ratio_fish0.2, beta_ratio_fish0.3, beta_ratio_fish0.4, beta_ratio_fish0.5, beta_ratio_fish0.6, beta_ratio_fish0.7, beta_ratio_fish0.8, beta_ratio_fish0.9),
     xlab = "",
     ylab = "",
     col="red",
     pch=16, axes = F, las=2)
mtext("Target of fish protection (%)", side=1, line= 3.5)
axis(side=1, at= seq(10, 90, 10), labels = c("10-20", "20-30", "30-40",
                                             "40-50", "50-60", "60-70", "70-80", "80-90", "90-100"), las =2)
axis(side=2, at=seq(0,0.5,0.1), las =2,  col.ticks = "red", col.axis = "red")
mtext("Beta ratio", side=2, line= 3.5, col = "red")
par(new=T)
plot(seq(10, 90, 10),
     c(jac_diss_fish0.1, jac_diss_fish0.2, jac_diss_fish0.3, jac_diss_fish0.4, jac_diss_fish0.5, jac_diss_fish0.6, jac_diss_fish0.7, jac_diss_fish0.8, jac_diss_fish0.9),
     xlab = "",
     ylab = "",
     xaxt = "n",
     yaxt = "n",
     col= scales::alpha("blue", 0.6),
     pch=16)
axis(side=4, at=seq(0,0.5,0.1), las=2, col.ticks = "blue", col.axis = "blue")
mtext("Jaccard dissim", side=4, line= 3, col = "blue")
dev.off()




