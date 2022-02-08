

#' Load predictions for fish (non masked)
#'
#' @return
#' @export
#'

load_fish_pred <- function(){

  raster::raster(here::here("outputs/brts/gaussian/predictions_all_fish.grd"))

}

#' Load summed predictions for modeled fish species (masked with sharks predictions)
#'
#' @return
#' @export
#'
#
load_summed_species_fish_pred <- function(){

  raster::raster(here::here("outputs/brts/gaussian/predictions_masked_summed_species.grd"))

}



#' Load summed predictions for modeled shark species
#'
#' @return
#' @export
#'
#
load_summed_species_shark_pred <- function(){

  raster::raster(here::here("outputs/brts/poisson/predictions_summed_species.grd"))

}


#' Load predictions for all sharks (all shark species modeled together)
#'
#' @return
#' @export
#'

load_all_sharks_pred <- function(){

  raster::raster(here::here("outputs/brts/poisson/predictions_all_sharks.grd"))

}



#' Load predictions for fish species
#'
#' @return
#' @export
#'

load_fish_species_pred <- function(){

  #load and stack prediction rasters for all fish species
  raster_data = list.files(path=here::here("outputs/brts/gaussian"), pattern="predictions_masked_.*.grd$", full.names=TRUE)

  #remove summed_species raster
  raster_data = raster_data[raster_data != here::here("outputs/brts/gaussian/predictions_masked_summed_species.grd")]

  # read files as rasters
  s = raster::stack(raster_data)

  return(s)


}




#' Load predictions for shark species
#'
#' @return
#' @export
#'

load_shark_species_pred <- function(){

  raster_data = list(here::here("outputs/brts/poisson/predictions_Carcharhinus_amblyrhynchos.grd"),
                     here::here("outputs/brts/poisson/predictions_Triaenodon_obesus.grd"))

  # read files as rasters
  s = raster::stack(raster_data)

  return(s)


}

#' load study area raster with flat cost
#'
#' @return
#' @export
#'

load_study_area_raster <- function(){

  r = mspproject::load_sstbruvs_raster()
  r2 = r[["sst01"]]

  # set flat cost layer
  r2[] = 1
  return(r2)

}



#' Design reserves for sharks based on minimum set objective with boundary penalty
#'
#' @param region
#' @param pred_sharks
#' @param target
#'
#' @return
#' @export
#'

design_sharks_reserves_minset_penalty <- function(region, pred_sharks, target, penalty){

  #resample to ensure same extent between rasters
  r2 = raster::resample(region, pred_sharks)

  #create a problem with the minimum set objective and relative target
  p =  prioritizr::problem(r2, pred_sharks) %>%
    prioritizr::add_min_set_objective() %>% # Minimize the cost of the solution whilst ensuring that all targets are met
    prioritizr::add_relative_targets(target) %>% # relative target eg 0.5 to specify that the solution must secure 50% of planning units for the feature.
    prioritizr::add_boundary_penalties(penalty = penalty) # boundary penalty (to favor solutions that have planning units clumped together into contiguous areas)

  # print number of planning units
  # prioritizr::number_of_planning_units(p)

  # print number of features
  # prioritizr::number_of_features(p)

  # solve the problem (using the default solver)
  library(gurobi)
  s = prioritizr::solve(p)

  #write raster
  raster::writeRaster(s, here::here(paste0("outputs/msp/sharks_reserves_minset_", target*100, "percent.grd")), overwrite=TRUE)

  return(s)
}




#' Boxplot BLM vs pach surface for given round of BLM selection
#'
#' @param pvalue
#' @param blmvalue
#' @param round_number
#' @param yat
#' @param patch_surface1
#' @param patch_surface2
#' @param patch_surface3
#' @param patch_surface4
#' @param patch_surface5
#' @param patch_surface6
#' @param patch_surface7
#' @param patch_surface8
#' @param patch_surface9
#' @param patch_surface10
#' @param patch_surface11
#'
#' @return
#' @export
#'

boxplot_sharks_blm_vs_patch_surf <- function(patch_surface1, patch_surface2, patch_surface3, patch_surface4, patch_surface5, patch_surface6, patch_surface7,
                                             patch_surface8, patch_surface9, patch_surface10, patch_surface11,
                                             pvalue, blmvalue, round_number, yat){

  png(here::here(paste0("outputs/msp/boxplot_sharks_blm_vs_patch_surf_round", round_number, ".png")), width = 960, height = 960)
  par(mar = c(8,6,3,6))
  boxplot(patch_surface1, patch_surface2, patch_surface3, patch_surface4, patch_surface5, patch_surface6, patch_surface7,
          patch_surface8, patch_surface9, patch_surface10, patch_surface11,
          at = seq(1,11,1),
          names = blmvalue,
          las = 2,
          col = "orange",
          axes = FALSE)
  axis(1, cex.axis=2, at = seq(1,11,1), labels= blmvalue, las =2)
  title(ylab = expression(paste("Reserve patch surface (", "km"^"2", ")")), cex.lab = 2, line = 4)
  axis(2, cex.axis=2)
  title(xlab = "BLM values", cex.lab = 2, line = 5)
  text(1, yat, paste("pvalue= \n", round(pvalue[1], 2)), cex = 1.2)
  text(2, yat, paste("pvalue= \n", round(pvalue[2], 2)), cex = 1.2)
  text(3, yat, paste("pvalue= \n", round(pvalue[3], 2)), cex = 1.2)
  text(4, yat, paste("pvalue= \n", round(pvalue[4], 2)), cex = 1.2)
  text(5, yat, paste("pvalue= \n", round(pvalue[5], 2)), cex = 1.2)
  text(6, yat, paste("pvalue= \n", round(pvalue[6], 2)), cex = 1.2)
  text(7, yat, paste("pvalue= \n", round(pvalue[7], 2)), cex = 1.2)
  text(8, yat, paste("pvalue= \n", round(pvalue[8], 2)), cex = 1.2)
  text(9, yat, paste("pvalue= \n", round(pvalue[9], 2)), cex = 1.2)
  text(10, yat, paste("pvalue= \n", round(pvalue[10], 2)), cex = 1.2)
  text(11, yat, paste("pvalue= \n", round(pvalue[11], 2)), cex = 1.2)
  #add reserve surface of 781
  abline(h=781)
  mtext('781', side=2, line=1, at=781, cex = 2)
  dev.off()

}




#' Plot BLM vs mean pach surface for given round of BLM selection
#'
#' @param mean_surface
#' @param pvalue
#' @param round_number
#' @param yat
#' @param blmvalue
#'
#' @return
#' @export
#'

plot_sharks_blm_vs_patch_surf <- function(mean_surface, pvalue, blmvalue, round_number, yat){

  png(here::here(paste0("outputs/msp/plot_sharks_blm_vs_patch_surf_round", round_number, ".png")), width = 960, height = 960)
  par(mar = c(8,8,3,6))
  plot(mean_surface,
       las = 2,
       col = "darkorange", pch = 16, cex = 2,
       axes = FALSE, xlab = "", ylab = "", ylim=c(0,max(mean_surface)))
  axis(1, cex.axis=2, at = seq(1,11,1), labels= blmvalue, las =2)
  title(ylab = expression(paste("Mean reserve patch surface (", "km"^"2", ")")), cex.lab = 2, line = 5.5)
  axis(2, cex.axis=2, las=1)
  title(xlab = "BLM values", cex.lab = 2, line = 5)
  text(1, yat, paste("pvalue= \n", round(pvalue[1], 2)), cex = 1.2)
  text(2, yat, paste("pvalue= \n", round(pvalue[2], 2)), cex = 1.2)
  text(3, yat, paste("pvalue= \n", round(pvalue[3], 2)), cex = 1.2)
  text(4, yat, paste("pvalue= \n", round(pvalue[4], 2)), cex = 1.2)
  text(5, yat, paste("pvalue= \n", round(pvalue[5], 2)), cex = 1.2)
  text(6, yat, paste("pvalue= \n", round(pvalue[6], 2)), cex = 1.2)
  text(7, yat, paste("pvalue= \n", round(pvalue[7], 2)), cex = 1.2)
  text(8, yat, paste("pvalue= \n", round(pvalue[8], 2)), cex = 1.2)
  text(9, yat, paste("pvalue= \n", round(pvalue[9], 2)), cex = 1.2)
  text(10, yat, paste("pvalue= \n", round(pvalue[10], 2)), cex = 1.2)
  text(11, yat, paste("pvalue= \n", round(pvalue[11], 2)), cex = 1.2)
  #add reserve surface of 781
  abline(h = 781, lty = 2)
  mtext('781', side=2, line=1, at=781, cex = 2, las = 1)
  dev.off()

}



#' Design reserves for fish species based on minimum set objective
#'
#' @param region
#' @param pred_fish
#' @param target
#'
#' @return
#' @export
#'

design_fish_species_reserves_minset <- function(region, pred_fish_sp, target){

  #resample to ensure same extent between rasters
  r2 = raster::resample(r, pred_fish_sp)

  #create a problem with the minimum set objective and relative target
  p =  prioritizr::problem(r2, pred_fish_sp) %>%
    prioritizr::add_min_set_objective() %>% # Minimize the cost of the solution whilst ensuring that all targets are met
    prioritizr::add_relative_targets(target) # relative target eg 0.5 to specify that the solution must secure 50% of planning units for the feature.


  # print number of planning units
  # prioritizr::number_of_planning_units(p)

  # print number of features
  # prioritizr::number_of_features(p)


  # solve the problem (using the default solver)
  library(gurobi)
  sol = prioritizr::solve(p)

  #write raster
  raster::writeRaster(sol, here::here(paste0("outputs/msp/fish_reserves_minset_", target*100, "percent.grd")), overwrite=TRUE)

  return(sol)
}






#' Design reserves for sharks and fish based on minimum set objective with boundary penalty
#'
#' @param region
#' @param pred_fish
#' @param target_sharks
#' @param target_fish
#' @param penalty
#' @param pred_sharks
#'
#' @return
#' @export
#'

design_sharksandfish_reserves_minset_penalty <- function(region, pred_sharks, pred_fish, target_sharks, target_fish, penalty){

  #resample to ensure same extent between rasters
  r2 = raster::resample(region, pred_sharks)

  #stack sharks and fish predictions
  pred = raster::stack(pred_sharks, pred_fish)

  #assemble sharks and fish targets
  target = c(rep(target_sharks, raster::nlayers(pred_sharks)), rep(target_fish, raster::nlayers(pred_fish)))

  #create a problem with the minimum set objective and relative target
  p =  prioritizr::problem(r2, pred) %>%
    prioritizr::add_min_set_objective() %>% # Minimize the cost of the solution whilst ensuring that all targets are met
    prioritizr::add_relative_targets(target) %>% # relative target eg 0.5 to specify that the solution must secure 50% of planning units for the feature.
    prioritizr::add_boundary_penalties(penalty = penalty) # boundary penalty (to favor solutions that have planning units clumped together into contiguous areas)

  # print number of planning units
  # prioritizr::number_of_planning_units(p)

  # print number of features
  # prioritizr::number_of_features(p)

  # solve the problem (using the default solver)
  library(gurobi)
  s = prioritizr::solve(p)

  #write raster
  raster::writeRaster(s, here::here(paste0("outputs/msp/sharksandfish_reserves_minset_targetfish", target_fish*100,
                                           "_target_sharks", target_sharks*100,".grd")), overwrite=TRUE)

  return(s)
}





#' Design reserves for sharks outside of pre-defined fish reserves based on minimum set objective with boundary penalty
#'
#' @param region
#' @param fish_msp
#' @param pred_sharks
#' @param remaining_target_sharks
#' @param penalty
#'
#' @return
#' @export
#'

design_sharks_reserves_in_fish_msp_minset_penalty <- function(region, fish_msp, pred_sharks, remaining_target_sharks, penalty){

  #resample to ensure same extent between rasters
  fish_msp2 = raster::resample(fish_msp, pred_sharks)
  r2 = raster::resample(region, pred_sharks)

  #create a problem with the minimum set objective and relative target
  p =  prioritizr::problem(r2, pred_sharks) %>%
    prioritizr::add_min_set_objective() %>% # Minimize the cost of the solution whilst ensuring that all targets are met
    prioritizr::add_relative_targets(remaining_target_sharks/100) %>% # relative target eg 0.5 to specify that the solution must secure 50% of planning units for the feature.
    prioritizr::add_boundary_penalties(penalty = penalty) %>% # boundary penalty (to favor solutions that have planning units clumped together into contiguous areas)
    prioritizr::add_locked_out_constraints(fish_msp2) #lock out constraint to ensure that fish reserves are not selected

  # print number of planning units
  # prioritizr::number_of_planning_units(p)

  # print number of features
  # prioritizr::number_of_features(p)

  # solve the problem (using the default solver)
  library(gurobi)
  s = prioritizr::solve(p)

  #write raster
  raster::writeRaster(s, here::here(paste0("outputs/msp/sharks_reserves_in_fish_msp_minset.grd")), overwrite=TRUE)

  return(s)
}




#' Plot reserves for sharks based on minimum set objective
#'
#' @param reserves
#' @param prop
#' @param target_sharks
#' @param target_fish
#' @param barchart
#'
#' @return
#' @export
#'

plot_sharks_reserves_minset <- function(reserves, target_sharks, target_fish, prop_fish, prop_sharks, barchart = FALSE){

  #read NC land and reproject
  shp = rgdal::readOGR(here::here("data/nc_land/NOUVELLE_CALEDONIE.shp"))
  raster::crs(shp) <- sp::CRS('+init=EPSG:3163') ## lambert new caledonia
  shp2 = sp::spTransform(shp, "+init=epsg:4326")

  #crop to extent
  reserves2 = raster::crop(reserves, raster::extent(-158, 168.5, -24, -17))

  #mask with sharks prediction raster so that brt prediction cells where no reserves are differenciate from the background
  predictions_sharks = load_summed_species_shark_pred()
  predictions_sharks = raster::crop(predictions_sharks, raster::extent(-158, 168.5, -24, -17))
  reserves2 = raster::mask(reserves2, predictions_sharks)

  #load shark species predictions raster
  pred_shark_species = load_shark_species_pred()

  #calculate target in individual shark numbers
  val_sharks = raster::values(pred_shark_species)
  tot_sharks = sum(val_sharks, na.rm=T)
  target_ind = target_sharks*tot_sharks

  #effectively protected shark abundance
  eff_target_ind = (prop_sharks/100)*tot_sharks

  #calculate target in biomass of fish
  val_fish = raster::values(pred_fish_species)
  tot_fish = sum(val_fish, na.rm=T)
  target_t = target_fish*tot_fish/1000

  #effectively protected fish biomass
  eff_target_t = (prop_fish/100)*tot_fish/1000

  png(here::here(paste0(paste0("/outputs/msp/sharks_reserves_minset_" , target_sharks*100, "percent.png"))), width = 800, height = 480)
  raster::plot(reserves2, col = c("light grey", "#39568CFF"), legend=FALSE)
  text(x = 159.5, y = -18.1, col= "#39568CFF", labels = paste0("Target = ", target_sharks*100, "% shark abundance (", round(target_ind), " ind)"))
  text(x = 159.5, y = -18.4, col= "#39568CFF", labels = paste0("Surface = ",  length(reserves[reserves ==1]), " km2"))
  text(x = 159.5, y = -22.5, col= "#39568CFF", labels = paste0("Protected fish biomass = ", round(eff_target_t), " t (", round(prop_fish), "%)"))
  text(x = 159.5, y = -22.8, col= "#39568CFF", labels = paste0("Protected sharks abundance = ", round(eff_target_ind), " ind (", round(prop_sharks), "%)"))
  sp::plot(shp2, add=T)
  dev.off()

  png(here::here(paste0(paste0("/outputs/msp/sharks_reserves_minset_" , target_sharks*100, "percent2.png"))), width = 800, height = 480)
  raster::plot(reserves2, col = c("light grey", "dark blue"), legend=FALSE)
  text(x = 159.5, y = -18.1, col= "black", labels = paste0("Target = ", target_sharks*100, "% shark abundance (", round(target_ind), " ind)"))
  text(x = 159.5, y = -18.4, col= "black", labels = paste0("Surface = ",  length(reserves[reserves ==1]), " km2"))
  text(x = 159.5, y = -22.5, col= "black", labels = paste0("Protected fish biomass = ", round(eff_target_t), " t (", round(prop_fish), "%)"))
  text(x = 159.5, y = -22.8, col= "black", labels = paste0("Protected sharks abundance = ", round(eff_target_ind), " ind (", round(prop_sharks), "%)"))
  sp::plot(shp2, add=T)
  dev.off()

  #barcharts
  if (barchart == TRUE){

    png(here::here(paste0("/outputs/msp/sharks_reserves_minset_" , target_sharks*100, "percent_barchart.png")), width = 960, height = 480)

    par(mar = c(5, 5, 5, 5))

    #barchart sharks at the top
    plot(NULL, xlim=c(0,1.1), ylim=c(0,1), xaxt="n", yaxt="n",
         xlab="", ylab="", bty='n')
    #polygon target
    polygon(c(0, target_sharks, target_sharks, 0), c(0.6,0.6,0.8,0.8), col = "#39568CFF")
    #add target
    text(c(target_sharks), c(0.55),
         c(paste0("Target = " , round(target_sharks*100), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to target
    text(c(target_sharks), c(0.5),
         c(paste0("(" , round(target_ind), " ind)")),
         col=c("black"), cex=1.2)
    #polygon protected
    polygon(c(target_sharks, prop_sharks/100, prop_sharks/100, target_sharks), c(0.6,0.6,target_sharks,target_sharks), col = "#39568CFF")
    #add protected
    text(c(prop_sharks/100), c(0.9),
         c(paste0("Protected = " , round(prop_sharks), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to protected
    text(c(prop_sharks/100), c(0.85),
         c(paste0("(" , round(eff_target_ind), " ind)")),
         col=c("black"), cex=1.2)
    #polygon 100%
    polygon(c(prop_sharks/100, 1, 1, prop_sharks/100), c(0.6,0.6,0.8,0.8), border = "black", col= "white")
    text(c(1+0.05), c(0.55),
         c("100%"),
         col=c("black"), cex=1.2)
    #add title
    text(0.5, 0.9, "Sharks", cex = 1.5)


    #barchart fish at the bottom
    #polygon target
    polygon(c(0, target_fish, target_fish, 0), c(0.1,0.1,target_fish,target_fish), col = "#39568CFF")
    #add target
    text(c(target_fish), c(0.05),
         c(paste0("Target = " , round(target_fish*100), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to target
    text(c(target_fish), c(0),
         c(paste0("(" , round(target_t), " t)")),
         col=c("black"), cex=1.2)
    #polygon protected
    polygon(c(target_fish, prop_fish/100, prop_fish/100, target_fish), c(0.1,0.1,target_fish,target_fish), col = "#39568CFF")
    #add protected
    text(c(prop_fish/100), c(0.05),
         c(paste0("Protected = " , round(prop_fish), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to protected
    text(c(prop_fish/100), c(0),
         c(paste0("(" , round(eff_target_t), " t)")),
         col=c("black"), cex=1.2)
    #polygon 100%
    polygon(c(prop_fish/100, 1, 1, prop_fish/100), c(0.1,0.1,target_fish,target_fish), border = "black", col= "white")
    text(c(1+0.05), c(0.05),
         c("100%"),
         col=c("black"), cex=1.2)
    #add title
    text(0.5, 0.4, "Fish", cex = 1.5)

    dev.off()

    png(here::here(paste0("/outputs/msp/sharks_reserves_minset_" , target_sharks*100, "percent_barchart2.png")), width = 960, height = 480)

    par(mar = c(5, 5, 5, 5))

    #barchart sharks at the top
    plot(NULL, xlim=c(0,1.1), ylim=c(0,1), xaxt="n", yaxt="n",
         xlab="", ylab="", bty='n')
    #polygon target
    polygon(c(0, target_sharks, target_sharks, 0), c(0.6,0.6,0.8,0.8), col = "green")
    #add target
    text(c(target_sharks), c(0.55),
         c(paste0("Target = " , round(target_sharks*100), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to target
    text(c(target_sharks), c(0.5),
         c(paste0("(" , round(target_ind), " ind)")),
         col=c("black"), cex=1.2)
    #polygon protected
    polygon(c(target_sharks, prop_sharks/100, prop_sharks/100, target_sharks), c(0.6,0.6,target_sharks,target_sharks), col = "cyan")
    #add protected
    text(c(prop_sharks/100), c(0.9),
         c(paste0("Protected = " , round(prop_sharks), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to protected
    text(c(prop_sharks/100), c(0.85),
         c(paste0("(" , round(eff_target_ind), " ind)")),
         col=c("black"), cex=1.2)
    #polygon 100%
    polygon(c(prop_sharks/100, 1, 1, prop_sharks/100), c(0.6,0.6,0.8,0.8), border = "black", col= "white")
    text(c(1+0.05), c(0.55),
         c("100%"),
         col=c("black"), cex=1.2)
    #add title
    text(0.5, 0.9, "Sharks", cex = 1.5)


    #barchart fish at the bottom
    #polygon target
    polygon(c(0, target_fish, target_fish, 0), c(0.1,0.1,target_fish,target_fish), col = "green")
    #add target
    text(c(target_fish), c(0.05),
         c(paste0("Target = " , round(target_fish*100), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to target
    text(c(target_fish), c(0),
         c(paste0("(" , round(target_t), " t)")),
         col=c("black"), cex=1.2)
    #polygon protected
    polygon(c(target_fish, prop_fish/100, prop_fish/100, target_fish), c(0.1,0.1,target_fish,target_fish), col = "cyan")
    #add protected
    text(c(prop_fish/100), c(0.05),
         c(paste0("Protected = " , round(prop_fish), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to protected
    text(c(prop_fish/100), c(0),
         c(paste0("(" , round(eff_target_t), " t)")),
         col=c("black"), cex=1.2)
    #polygon 100%
    polygon(c(prop_fish/100, 1, 1, prop_fish/100), c(0.1,0.1,target_fish,target_fish), border = "black", col= "white")
    text(c(1+0.05), c(0.05),
         c("100%"),
         col=c("black"), cex=1.2)
    #add title
    text(0.5, 0.4, "Fish", cex = 1.5)

    dev.off()
  }

}



#' Plot fish reserves based on minimum set objective
#'
#' @param reserves
#' @param prop
#' @param target_fish
#' @param target_sharks
#' @param barchart
#'
#' @return
#' @export
#'

plot_fish_reserves_minset <- function(reserves, target_fish, target_sharks, prop_sharks, prop_fish, barchart = FALSE){

  #read NC land and reproject
  shp = rgdal::readOGR(here::here("data/nc_land/NOUVELLE_CALEDONIE.shp"))
  raster::crs(shp) <- sp::CRS('+init=EPSG:3163') ## lambert new caledonia
  shp2 = sp::spTransform(shp, "+init=epsg:4326")

  #crop to extent
  reserves2 = raster::crop(reserves, raster::extent(-158, 168.5, -24, -17))

  #mask with sharks prediction raster so that brt prediction cells where no reserves are differenciate from the background
  predictions_sharks = load_summed_species_shark_pred()
  predictions_sharks = raster::crop(predictions_sharks, raster::extent(-158, 168.5, -24, -17))
  reserves2 = raster::mask(reserves2, predictions_sharks)

  #load fish species predictions raster
  pred_fish_species = load_fish_species_pred()

  #calculate target in biomass of fish
  val = raster::values(pred_fish_species)
  tot_fish = sum(val, na.rm=T)
  target_t = target_fish*tot_fish/1000

  #effectively protected fish biomass
  eff_target_t = (prop_fish/100)*tot_fish/1000

  #load shark species predictions raster
  pred_shark_species = load_shark_species_pred()

  #calculate target in individual shark numbers
  val = raster::values(pred_shark_species)
  tot_sharks = sum(val, na.rm=T)
  target_ind = target_sharks*tot_sharks

  #effectively protected shark abundance
  eff_target_ind = (prop_sharks/100)*tot_sharks

  png(here::here(paste0(paste0("/outputs/msp/fish_reserves_minset_" , target_fish*100, "percent.png"))), width = 800, height = 480)
  raster::plot(reserves2, col = c("light grey", "#A62098FF"), legend=FALSE)
  text(x = 159.5, y = -18.1, col = "#A62098FF", labels = paste0("Target = ", target_fish*100, "% fish biomass (", round(target_t), " t)"))
  text(x = 159.5, y = -18.4, col = "#A62098FF", labels = paste0("Surface = ",  length(reserves[reserves ==1]), " km2"))
  text(x = 159.5, y = -22.5, col = "#A62098FF", labels = paste0("Protected shark abundance = ", round(eff_target_ind), " ind (", round(prop_sharks), "%)"))
  text(x = 159.5, y = -22.8, col = "#A62098FF", labels = paste0("Protected fish biomass = ", round(eff_target_t), " t (", round(prop_fish), "%)"))
  sp::plot(shp2, add=T)
  dev.off()

  png(here::here(paste0(paste0("/outputs/msp/fish_reserves_minset_" , target_fish*100, "percent2.png"))), width = 800, height = 480)
  raster::plot(reserves2, col = c("light grey", "dark blue"), legend=FALSE)
  text(x = 159.5, y = -18.1, col = "black", labels = paste0("Target = ", target_fish*100, "% fish biomass (", round(target_t), " t)"))
  text(x = 159.5, y = -18.4, col = "black", labels = paste0("Surface = ",  length(reserves[reserves ==1]), " km2"))
  text(x = 159.5, y = -22.5, col = "black", labels = paste0("Protected shark abundance = ", round(eff_target_ind), " ind (", round(prop_sharks), "%)"))
  text(x = 159.5, y = -22.8, col = "black", labels = paste0("Protected fish biomass = ", round(eff_target_t), " t (", round(prop_fish), "%)"))
  sp::plot(shp2, add=T)
  dev.off()

  #barcharts
  if (barchart == TRUE){

    png(here::here(paste0("/outputs/msp/fish_reserves_minset_" , target_fish*100, "percent_barchart.png")), width = 960, height = 480)

    par(mar = c(5, 5, 5, 5))

    #barchart sharks at the top
    plot(NULL, xlim=c(0,1.1), ylim=c(0,1), xaxt="n", yaxt="n",
         xlab="", ylab="", bty='n')
    #polygon target
    polygon(c(0, target_sharks, target_sharks, 0), c(0.6,0.6,0.8,0.8), col = "#A62098FF")
    #add target
    text(c(target_sharks), c(0.55),
         c(paste0("Target = " , round(target_sharks*100), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to target
    text(c(target_sharks), c(0.5),
         c(paste0("(" , round(target_ind), " ind)")),
         col=c("black"), cex=1.2)
    #polygon protected
    polygon(c(0, prop_sharks/100, prop_sharks/100, 0), c(0.6,0.6,0.8,0.8), col = "#A62098FF")
    #add protected
    text(c(prop_sharks/100), c(0.55),
         c(paste0("Protected = " , round(prop_sharks), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to protected
    text(c(prop_sharks/100), c(0.5),
         c(paste0("(" , round(eff_target_ind), " ind)")),
         col=c("black"), cex=1.2)
    #polygon 100%
    polygon(c(target_sharks, 1, 1, target_sharks), c(0.6,0.6,0.8,0.8), border = "black", col= "white")
    text(c(1+0.05), c(0.55),
         c("100%"),
         col=c("black"), cex=1.2)
    #add title
    text(0.5, 0.9, "Sharks", cex = 1.5)


    #barchart fish at the bottom
    #polygon target
    polygon(c(0, target_fish, target_fish, 0), c(0.1,0.1,target_fish,target_fish), col = "#A62098FF")
    #add target
    text(c(target_fish), c(0.05),
         c(paste0("Target = " , round(target_fish*100), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to target
    text(c(target_fish), c(0),
         c(paste0("(" , round(target_t), " t)")),
         col=c("black"), cex=1.2)
    #polygon protected
    polygon(c(target_fish,prop_fish/100,prop_fish/100,target_fish), c(0.1,0.1,0.3,0.3), col = "#A62098FF")
    #add protected
    text(c(prop_fish/100+0.05), c(0.40),
         c(paste0("Protected = " , round(prop_fish), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to protected
    text(c(prop_fish/100+0.05), c(0.35),
         c(paste0("(" , round(eff_target_t), " t)")),
         col=c("black"), cex=1.2)
    #polygon 100%
    polygon(c(prop_fish/100, 1, 1, prop_fish/100), c(0.1,0.1,target_fish,target_fish), border = "black", col= "white")
    text(c(1+0.05), c(0.05),
         c("100%"),
         col=c("black"), cex=1.2)
    #add title
    text(0.5, 0.4, "Fish", cex = 1.5)


    dev.off()


    png(here::here(paste0("/outputs/msp/fish_reserves_minset_" , target_fish*100, "percent_barchart2.png")), width = 960, height = 480)

    par(mar = c(5, 5, 5, 5))

    #barchart sharks at the top
    plot(NULL, xlim=c(0,1.1), ylim=c(0,1), xaxt="n", yaxt="n",
         xlab="", ylab="", bty='n')
    #polygon target
    polygon(c(0, target_sharks, target_sharks, 0), c(0.6,0.6,0.8,0.8), col = "red")
    #add target
    text(c(target_sharks), c(0.55),
         c(paste0("Target = " , round(target_sharks*100), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to target
    text(c(target_sharks), c(0.5),
         c(paste0("(" , round(target_ind), " ind)")),
         col=c("black"), cex=1.2)
    #polygon protected
    polygon(c(0, prop_sharks/100, prop_sharks/100, 0), c(0.6,0.6,0.8,0.8), col = "green")
    #add protected
    text(c(prop_sharks/100), c(0.55),
         c(paste0("Protected = " , round(prop_sharks), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to protected
    text(c(prop_sharks/100), c(0.5),
         c(paste0("(" , round(eff_target_ind), " ind)")),
         col=c("black"), cex=1.2)
    #polygon 100%
    polygon(c(target_sharks, 1, 1, target_sharks), c(0.6,0.6,0.8,0.8), border = "black", col= "white")
    text(c(1+0.05), c(0.55),
         c("100%"),
         col=c("black"), cex=1.2)
    #add title
    text(0.5, 0.9, "Sharks", cex = 1.5)


    #barchart fish at the bottom
    #polygon target
    polygon(c(0, target_fish, target_fish, 0), c(0.1,0.1,target_fish,target_fish), col = "green")
    #add target
    text(c(target_fish), c(0.05),
         c(paste0("Target = " , round(target_fish*100), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to target
    text(c(target_fish), c(0),
         c(paste0("(" , round(target_t), " t)")),
         col=c("black"), cex=1.2)

    #polygon protected
    polygon(c(target_fish,prop_fish/100,prop_fish/100,target_fish), c(0.1,0.1,0.3,0.3), col = "cyan")
    #add protected
    text(c(prop_fish/100+0.05), c(0.40),
         c(paste0("Protected = " , round(prop_fish), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to protected
    text(c(prop_fish/100+0.05), c(0.35),
         c(paste0("(" , round(eff_target_t), " t)")),
         col=c("black"), cex=1.2)

    #polygon 100%
    polygon(c(prop_fish/100, 1, 1, prop_fish/100), c(0.1,0.1,target_fish,target_fish), border = "black", col= "white")
    text(c(1+0.05), c(0.05),
         c("100%"),
         col=c("black"), cex=1.2)
    #add title
    text(0.5, 0.4, "Fish", cex = 1.5)

    dev.off()
  }

}





#' Plot fish and sharks reserves based on minimum set objective
#'
#' @param target_sharks
#' @param target_fish
#' @param prop_fish
#' @param prop_sharks
#' @param reserves
#' @param barchart
#'
#' @return
#' @export
#'

plot_sharksandfish_reserves_minset <- function(reserves, target_sharks, target_fish, prop_fish, prop_sharks, barchart = FALSE){

  #read NC land and reproject
  shp = rgdal::readOGR(here::here("data/nc_land/NOUVELLE_CALEDONIE.shp"))
  raster::crs(shp) <- sp::CRS('+init=EPSG:3163') ## lambert new caledonia
  shp2 = sp::spTransform(shp, "+init=epsg:4326")

  #crop to extent
  reserves2 = raster::crop(reserves, raster::extent(-158, 168.5, -24, -17))

  #mask with sharks prediction raster so that brt prediction cells where no reserves are differenciated from the background
  predictions_sharks = load_summed_species_shark_pred()
  predictions_sharks = raster::crop(predictions_sharks, raster::extent(-158, 168.5, -24, -17))
  reserves2 = raster::mask(reserves2, predictions_sharks)

  #load fish species predictions raster
  pred_fish_species = load_fish_species_pred()

  #calculate target in biomass of fish
  val = raster::values(pred_fish_species)
  tot_fish = sum(val, na.rm=T)
  target_t = target_fish*tot_fish/1000

  #effectively protected fish biomass
  eff_target_t = (prop_fish/100)*tot_fish/1000

  #load shark species predictions raster
  pred_shark_species = load_shark_species_pred()

  #calculate target in individual shark numbers
  val = raster::values(pred_shark_species)
  tot_sharks = sum(val, na.rm=T)
  target_ind = target_sharks*tot_sharks

  #effectively protected shark abundance
  eff_target_ind = (prop_sharks/100)*tot_sharks

  png(here::here(paste0("/outputs/msp/sharksandfish_reserves_minset_targetfish", target_fish*100, "_targetsharks", target_sharks*100, ".png")), width = 800, height = 480)
  raster::plot(reserves2, col = c("light grey", "#F58C46FF"), legend=FALSE)
  text(x = 159.5, y = -17.8, col = "#F58C46FF", labels = paste0("Target = ", target_fish*100, "% fish biomass (", round(target_t), " t)"))
  text(x = 159.5, y = -18.1, col = "#F58C46FF", labels = paste0("Target = ", target_sharks*100, "% sharks abundance (", round(target_ind), " ind)"))
  text(x = 159.5, y = -18.4, col = "#F58C46FF", labels = paste0("Surface = ", length(reserves[reserves ==1]), " km2"))
  text(x = 159.5, y = -22.5, col = "#F58C46FF", labels = paste0("Protected fish biomass = ", round(eff_target_t), " t (", round(prop_fish), "%)"))
  text(x = 159.5, y = -22.8, col = "#F58C46FF", labels = paste0("Protected shark abundance = ", round(eff_target_ind), " ind (", round(prop_sharks), "%)"))
  sp::plot(shp2, add=T)
  dev.off()

  png(here::here(paste0("/outputs/msp/sharksandfish_reserves_minset_targetfish", target_fish*100, "_targetsharks", target_sharks*100, "2.png")), width = 800, height = 480)
  raster::plot(reserves2, col = c("light grey", "dark blue"), legend=FALSE)
  text(x = 159.5, y = -17.8, col = "black", labels = paste0("Target = ", target_fish*100, "% fish biomass (", round(target_t), " t)"))
  text(x = 159.5, y = -18.1, col = "black", labels = paste0("Target = ", target_sharks*100, "% sharks abundance (", round(target_ind), " ind)"))
  text(x = 159.5, y = -18.4, col = "black", labels = paste0("Surface = ", length(reserves[reserves ==1]), " km2"))
  text(x = 159.5, y = -22.5, col = "black", labels = paste0("Protected fish biomass = ", round(eff_target_t), " t (", round(prop_fish), "%)"))
  text(x = 159.5, y = -22.8, col = "black", labels = paste0("Protected shark abundance = ", round(eff_target_ind), " ind (", round(prop_sharks), "%)"))
  sp::plot(shp2, add=T)
  dev.off()

  #barcharts
  if (barchart == TRUE){

    png(here::here(paste0("/outputs/msp/sharksandfish_reserves_minset_targetfish", target_fish*100, "_targetsharks", target_sharks*100, "_barchart.png")), width = 960, height = 480)

    par(mar = c(5, 5, 5, 5))

    #barchart sharks at the top
    plot(NULL, xlim=c(0,1.1), ylim=c(0,1), xaxt="n", yaxt="n",
         xlab="", ylab="", bty='n')
    #polygon target
    polygon(c(0, target_sharks, target_sharks, 0), c(0.6,0.6,0.8,0.8), col = "#F58C46FF")
    #add target
    text(c(target_sharks), c(0.55),
         c(paste0("Target = " , round(target_sharks*100), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to target
    text(c(target_sharks), c(0.5),
         c(paste0("(" , round(target_ind), " ind)")),
         col=c("black"), cex=1.2)
    #polygon protected
    polygon(c(target_sharks, prop_sharks/100, prop_sharks/100, target_sharks), c(0.6,0.6,target_sharks,target_sharks), col = "#F58C46FF")
    #add protected
    text(c(prop_sharks/100), c(0.9),
         c(paste0("Protected = " , round(prop_sharks), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to protected
    text(c(prop_sharks/100), c(0.85),
         c(paste0("(" , round(eff_target_ind), " ind)")),
         col=c("black"), cex=1.2)
    #polygon 100%
    polygon(c(prop_sharks/100, 1, 1, prop_sharks/100), c(0.6,0.6,0.8,0.8), border = "black", col= "white")
    text(c(1+0.05), c(0.55),
         c("100%"),
         col=c("black"), cex=1.2)
    #add title
    text(0.5, 0.9, "Sharks", cex = 1.5)


    #barchart fish at the bottom
    #polygon target
    polygon(c(0, target_fish, target_fish, 0), c(0.1,0.1,target_fish,target_fish), col = "#F58C46FF")
    #add target
    text(c(target_fish), c(0.05),
         c(paste0("Target = " , round(target_fish*100), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to target
    text(c(target_fish), c(0),
         c(paste0("(" , round(target_t), " t)")),
         col=c("black"), cex=1.2)
    #polygon protected
    polygon(c(target_fish, prop_fish/100, prop_fish/100, target_fish), c(0.1,0.1,target_fish,target_fish), col = "#F58C46FF")
    #add protected
    text(c(prop_fish/100), c(0.05),
         c(paste0("Protected = " , round(prop_fish), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to protected
    text(c(prop_fish/100), c(0),
         c(paste0("(" , round(eff_target_t), " t)")),
         col=c("black"), cex=1.2)
    #polygon 100%
    polygon(c(prop_fish/100, 1, 1, prop_fish/100), c(0.1,0.1,target_fish,target_fish), border = "black", col= "white")
    text(c(1+0.05), c(0.05),
         c("100%"),
         col=c("black"), cex=1.2)
    #add title
    text(0.5, 0.4, "Fish", cex = 1.5)

    dev.off()

    png(here::here(paste0("/outputs/msp/sharksandfish_reserves_minset_targetfish", target_fish*100, "_targetsharks", target_sharks*100, "_barchart2.png")), width = 960, height = 480)

    par(mar = c(5, 5, 5, 5))

    #barchart sharks at the top
    plot(NULL, xlim=c(0,1.1), ylim=c(0,1), xaxt="n", yaxt="n",
         xlab="", ylab="", bty='n')
    #polygon target
    polygon(c(0, target_sharks, target_sharks, 0), c(0.6,0.6,0.8,0.8), col = "green")
    #add target
    text(c(target_sharks), c(0.55),
         c(paste0("Target = " , round(target_sharks*100), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to target
    text(c(target_sharks), c(0.5),
         c(paste0("(" , round(target_ind), " ind)")),
         col=c("black"), cex=1.2)
    #polygon protected
    polygon(c(target_sharks, prop_sharks/100, prop_sharks/100, target_sharks), c(0.6,0.6,target_sharks,target_sharks), col = "cyan")
    #add protected
    text(c(prop_sharks/100), c(0.9),
         c(paste0("Protected = " , round(prop_sharks), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to protected>
    text(c(prop_sharks/100), c(0.85),
         c(paste0("(" , round(eff_target_ind), " ind)")),
         col=c("black"), cex=1.2)
    #polygon 100%
    polygon(c(prop_sharks/100, 1, 1, prop_sharks/100), c(0.6,0.6,0.8,0.8), border = "black", col= "white")
    text(c(1+0.05), c(0.55),
         c("100%"),
         col=c("black"), cex=1.2)
    #add title
    text(0.5, 0.9, "Sharks", cex = 1.5)


    #barchart fish at the bottom
    #polygon target
    polygon(c(0, target_fish, target_fish, 0), c(0.1,0.1,target_fish,target_fish), col = "green")
    #add target
    text(c(target_fish), c(0.05),
         c(paste0("Target = " , round(target_fish*100), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to target
    text(c(target_fish), c(0),
         c(paste0("(" , round(target_t), " t)")),
         col=c("black"), cex=1.2)
    #polygon protected
    polygon(c(target_fish, prop_fish/100, prop_fish/100, target_fish), c(0.1,0.1,target_fish,target_fish), col = "cyan")
    #add protected
    text(c(prop_fish/100), c(0.05),
         c(paste0("Protected = " , round(prop_fish), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to protected
    text(c(prop_fish/100), c(0),
         c(paste0("(" , round(eff_target_t), " t)")),
         col=c("black"), cex=1.2)
    #polygon 100%
    polygon(c(prop_fish/100, 1, 1, prop_fish/100), c(0.1,0.1,target_fish,target_fish), border = "black", col= "white")
    text(c(1+0.05), c(0.05),
         c("100%"),
         col=c("black"), cex=1.2)
    #add title
    text(0.5, 0.4, "Fish", cex = 1.5)

    dev.off()
  }

}




#' Plot sharks reserves in fish msp based on minimum set objective
#'
#' @param target_sharks
#' @param prop_fish
#' @param reserves
#' @param target_fish
#' @param barchart
#'
#' @return
#' @export
#'


plot_sharks_reserves_in_fish_msp_minset <- function(reserves, target_sharks, target_fish, prop_fish, prop_sharks, barchart = FALSE){

  #read NC land and reproject
  shp = rgdal::readOGR(here::here("data/nc_land/NOUVELLE_CALEDONIE.shp"))
  raster::crs(shp) <- sp::CRS('+init=EPSG:3163') ## lambert new caledonia
  shp2 = sp::spTransform(shp, "+init=epsg:4326")

  #crop to extent
  reserves2 = raster::crop(reserves, raster::extent(-158, 168.5, -24, -17))

  #mask with sharks prediction raster so that brt prediction cells where no reserves are differenciated from the background
  predictions_sharks = load_summed_species_shark_pred()
  predictions_sharks = raster::crop(predictions_sharks, raster::extent(-158, 168.5, -24, -17))
  reserves2 = raster::mask(reserves2, predictions_sharks)

  #load shark species predictions raster
  pred_shark_species = load_shark_species_pred()

  #calculate target in individual shark numbers
  val = raster::values(pred_shark_species)
  tot_sharks = sum(val, na.rm=T)
  target_ind = target_sharks*tot_sharks

  #effectively protected shark abundance
  eff_target_ind = (prop_sharks/100)*tot_sharks

  #load fish species predictions raster
  pred_fish_species = load_fish_species_pred()

  #calculate target in biomass of fish
  val = raster::values(pred_fish_species)
  tot_fish = sum(val, na.rm=T)
  target_t = target_fish*tot_fish/1000

  #effectively protected fish biomass
  eff_target_t = (prop_fish/100)*tot_fish/1000

  png(here::here("/outputs/msp/sharks_reserves_in_fish_msp_minset.png"), width = 800, height = 480)
  raster::plot(reserves2, col = c("light grey", "#39568CFF"), legend=FALSE)
  text(x = 159.5, y = -18.1, col = "#39568CFF", labels = paste0("Target = ", round(target_sharks*100), "% sharks abundance (", round(target_ind), " ind)"))
  text(x = 159.5, y = -18.4, col = "#39568CFF", labels = paste0("Surface = ", length(reserves[reserves ==1]), " km2"))
  text(x = 159.5, y = -22.5, col = "#39568CFF", labels = paste0("Protected fish biomass = ", round(eff_target_t), " t (", round(prop_fish), "%)"))
  text(x = 159.5, y = -22.8, col = "#39568CFF", labels = paste0("Protected shark abundance = ", round(eff_target_ind), " ind (", round(prop_sharks), "%)"))
  sp::plot(shp2, add=T)
  dev.off()

  png(here::here("/outputs/msp/sharks_reserves_in_fish_msp_minset2.png"), width = 800, height = 480)
  raster::plot(reserves2, col = c("light grey", "dark blue"), legend=FALSE)
  text(x = 159.5, y = -18.1, col = "black", labels = paste0("Target = ", round(target_sharks*100), "% sharks abundance (", round(target_ind), " ind)"))
  text(x = 159.5, y = -18.4, col = "black", labels = paste0("Surface = ", length(reserves[reserves ==1]), " km2"))
  text(x = 159.5, y = -22.5, col = "black", labels = paste0("Protected fish biomass = ", round(eff_target_t), " t (", round(prop_fish), "%)"))
  text(x = 159.5, y = -22.8, col = "black", labels = paste0("Protected shark abundance = ", round(eff_target_ind), " ind (", round(prop_sharks), "%)"))
  sp::plot(shp2, add=T)
  dev.off()

  #barcharts
  if (barchart == TRUE){

    png(here::here("/outputs/msp/sharks_reserves_in_fish_msp_minset_barchart.png"), width = 960, height = 480)

    par(mar = c(5, 5, 5, 5))

    #barchart sharks at the top
    plot(NULL, xlim=c(0,1.1), ylim=c(0,1), xaxt="n", yaxt="n",
         xlab="", ylab="", bty='n')
    #polygon target
    polygon(c(0, target_sharks, target_sharks, 0), c(0.6,0.6,0.8,0.8), col = "#39568CFF")
    #add target
    text(c(target_sharks), c(0.55),
         c(paste0("Target = " , round(target_sharks*100), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to target
    text(c(target_sharks), c(0.5),
         c(paste0("(" , round(target_ind), " ind)")),
         col=c("black"), cex=1.2)
    #polygon 100%
    polygon(c(target_sharks, 1, 1, target_sharks), c(0.6,0.6,0.8,0.8), border = "black", col= "white")
    text(c(1+0.05), c(0.55),
         c("100%"),
         col=c("black"), cex=1.2)
    #add title
    text(0.5, 0.9, "Sharks", cex = 1.5)


    #barchart fish at the bottom
    #polygon target
    polygon(c(0, target_fish, target_fish, 0), c(0.1,0.1,0.3,0.3), col = "#39568CFF")
    #add target
    text(c(target_fish-0.03), c(0.05),
         c(paste0("Target = " , round(target_fish*100), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to target
    text(c(target_fish-0.03), c(0),
         c(paste0("(" , round(target_t), " t)")),
         col=c("black"), cex=1.2)
    #polygon protected
    polygon(c(target_fish,prop_fish/100,prop_fish/100,target_fish), c(0.1,0.1,0.3,0.3), col = "#39568CFF")
    #add protected
    text(c(prop_fish/100+0.05), c(0.05),
         c(paste0("Protected = " , round(prop_fish), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to protected
    text(c(prop_fish/100+0.05), c(0),
         c(paste0("(" , round(eff_target_t), " t)")),
         col=c("black"), cex=1.2)
    #polygon 100%
    polygon(c(prop_fish/100, 1, 1, prop_fish/100), c(0.1,0.1,0.3,0.3), border = "black", col= "white")
    text(c(1+0.05), c(0.05),
         c("100%"),
         col=c("black"), cex=1.2)
    #add title
    text(0.5, 0.4, "Fish", cex = 1.5)

    dev.off()

    png(here::here("/outputs/msp/sharks_reserves_in_fish_msp_minset_barchart2.png"), width = 960, height = 480)

    par(mar = c(5, 5, 5, 5))

    #barchart sharks at the top
    plot(NULL, xlim=c(0,1.1), ylim=c(0,1), xaxt="n", yaxt="n",
         xlab="", ylab="", bty='n')
    #polygon target
    polygon(c(0, target_sharks, target_sharks, 0), c(0.6,0.6,0.8,0.8), col = "green")
    #add target
    text(c(target_sharks), c(0.55),
         c(paste0("Target = " , round(target_sharks*100), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to target
    text(c(target_sharks), c(0.5),
         c(paste0("(" , round(target_ind), " ind)")),
         col=c("black"), cex=1.2)
    #polygon 100%
    polygon(c(target_sharks, 1, 1, target_sharks), c(0.6,0.6,0.8,0.8), border = "black", col= "white")
    text(c(1+0.05), c(0.55),
         c("100%"),
         col=c("black"), cex=1.2)
    #add title
    text(0.5, 0.9, "Sharks", cex = 1.5)


    #barchart fish at the bottom
    #polygon target
    polygon(c(0, target_fish, target_fish, 0), c(0.1,0.1,0.3,0.3), col = "green")
    #add target
    text(c(target_fish-0.03), c(0.05),
         c(paste0("Target = " , round(target_fish*100), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to target
    text(c(target_fish-0.03), c(0),
         c(paste0("(" , round(target_t), " t)")),
         col=c("black"), cex=1.2)
    #polygon protected
    polygon(c(target_fish,prop_fish/100,prop_fish/100,target_fish), c(0.1,0.1,0.3,0.3), col = "cyan")
    #add protected
    text(c(prop_fish/100+0.05), c(0.05),
         c(paste0("Protected = " , round(prop_fish), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to protected
    text(c(prop_fish/100+0.05), c(0),
         c(paste0("(" , round(eff_target_t), " t)")),
         col=c("black"), cex=1.2)
    #polygon 100%
    polygon(c(prop_fish/100, 1, 1, prop_fish/100), c(0.1,0.1,0.3,0.3), border = "black", col= "white")
    text(c(1+0.05), c(0.05),
         c("100%"),
         col=c("black"), cex=1.2)
    #add title
    text(0.5, 0.4, "Fish", cex = 1.5)

    dev.off()

  }

}





#'  Plot sharks and fish reserves in fish msp based on minimum set objective
#'
#' @param target_sharks
#' @param prop_fish
#' @param reserves
#'
#' @return
#' @export
#'


plot_sharks_fish_reserves_in_fish_msp_minset <- function(reserves_sharks, reserves_fish, target_sharks, target_fish,
                                                         prop_fish_in_sharks, prop_sharks_in_fish,
                                                         prop_fish_in_fish, prop_sharks_in_sharks,
                                                         barchart = FALSE){

  #read NC land and reproject
  shp = rgdal::readOGR(here::here("data/nc_land/NOUVELLE_CALEDONIE.shp"))
  raster::crs(shp) <- sp::CRS('+init=EPSG:3163') ## lambert new caledonia
  shp2 = sp::spTransform(shp, "+init=epsg:4326")

  #crop to extent
  reserves_sharks2 = raster::crop(reserves_sharks, raster::extent(-158, 168.5, -24, -17))
  reserves_fish2 = raster::crop(reserves_fish, raster::extent(-158, 168.5, -24, -17))

  #mask with sharks prediction raster so that brt prediction cells where no reserves are differenciated from the background
  predictions_sharks = load_summed_species_shark_pred()
  predictions_sharks = raster::crop(predictions_sharks, raster::extent(-158, 168.5, -24, -17))
  reserves_sharks2 = raster::mask(reserves_sharks2, predictions_sharks)
  reserves_fish2 = raster::mask(reserves_fish2, predictions_sharks)

  #load shark species predictions raster
  pred_shark_species = load_shark_species_pred()

  #calculate target in individual shark numbers
  val = raster::values(pred_shark_species)
  tot_sharks = sum(val, na.rm=T)
  target_ind = (target_sharks/100)*tot_sharks

  #effectively protected shark abundance in fish reserves
  eff_target_ind = (prop_sharks_in_fish/100)*tot_sharks

  #effectively protected shark abundance in sharks reserves
  eff_target_ind2 = (prop_sharks_in_sharks/100)*tot_sharks

  #load fish species predictions raster
  pred_fish_species = load_fish_species_pred()

  #calculate target in biomass of fish
  val = raster::values(pred_fish_species)
  tot_fish = sum(val, na.rm=T)
  target_t = (target_fish/100)*tot_fish/1000

  #effectively protected fish biomass in sharks reserves
  eff_target_t = (prop_fish_in_sharks/100)*tot_fish/1000

  #effectively protected fish biomass in fish reserves
  eff_target_t2 = (prop_fish_in_fish/100)*tot_fish/1000

  #overlay sharks and fish reserves for plotting
  reserves_sharks22 = reserves_sharks2*2
  reserves_fish_sharks = reserves_fish2 + reserves_sharks22

  png(here::here(paste0("/outputs/msp/sharks_fish_reserves_in_fish_msp_minset.png")), width = 800, height = 480)
  raster::plot(reserves_fish_sharks, col = c("light grey", "#A62098FF", "#39568CFF"), legend=FALSE)
  text(x = 159.7, y = -18.4, col = "#39568CFF", labels = paste0("Target = ", round(target_sharks), "% sharks abundance (", round(target_ind), " ind)"))
  text(x = 159.7, y = -18.7, col = "#39568CFF", labels = paste0("Surface = ", length(reserves_sharks[reserves_sharks ==1]), " km2"))
  text(x = 159.7, y = -22.9, col = "#39568CFF", labels = paste0("Protected fish biomass = ", round(eff_target_t), " t (", round(prop_fish_in_sharks), "%)"))
  text(x = 159.7, y = -23.2, col = "#39568CFF", labels = paste0("Protected shark abundance = ", round(eff_target_ind2), " ind (", round(prop_sharks_in_sharks), "%)"))
  text(x = 159.7, y = -17.8, col = "#A62098FF", labels = paste0("Target = ", round(target_fish), "% fish biomass (", round(target_t), " t)"))
  text(x = 159.7, y = -18.1, col = "#A62098FF", labels = paste0("Surface = ",  length(reserves_fish[reserves_fish ==1]), " km2"))
  text(x = 159.7, y = -22.3, col = "#A62098FF", labels = paste0("Protected shark abundance = ", round(eff_target_ind), " ind (", round(prop_sharks_in_fish), "%)"))
  text(x = 159.7, y = -22.6, col = "#A62098FF", labels = paste0("Protected fish biomass = ", round(eff_target_t2), " t (", round(prop_fish_in_fish), "%)"))
  sp::plot(shp2, add=T)
  dev.off()



  #barcharts
  if (barchart == TRUE){

    png(here::here(paste0("/outputs/msp/sharks_fish_reserves_in_fish_msp_minset_barchart.png")), width = 960, height = 480)

    par(mar = c(5, 5, 5, 5))

    #barchart sharks at the top
    plot(NULL, xlim=c(0,1.1), ylim=c(0,1), xaxt="n", yaxt="n",
         xlab="", ylab="", bty='n')
    #polygon protected in fish
    polygon(c(0, prop_sharks_in_fish/100, prop_sharks_in_fish/100, 0), c(0.6,0.6,0.8,0.8), col = "#A62098FF")
    #add protected in fish
    text(c(prop_sharks_in_fish/100), c(0.55),
         c(paste0("Protected = " , round(prop_sharks_in_fish), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to protected in fish
    text(c(prop_sharks_in_fish/100), c(0.5),
         c(paste0("(" , round(eff_target_ind), " ind)")),
         col=c("black"), cex=1.2)
    #polygon protected in sharks
    polygon(c(prop_sharks_in_fish/100, prop_sharks_in_fish/100+prop_sharks_in_sharks/100, prop_sharks_in_fish/100+prop_sharks_in_sharks/100, prop_sharks_in_fish/100), c(0.8,0.8,0.6,0.6), col = "#39568CFF")
    #add protected in sharks
    text(c(prop_sharks_in_fish/100+prop_sharks_in_sharks/100), c(0.9),
         c(paste0("Protected = " , round(prop_sharks_in_sharks), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to protected in sharks
    text(c(prop_sharks_in_fish/100+prop_sharks_in_sharks/100), c(0.85),
         c(paste0("(" , round(eff_target_ind2), " ind)")),
         col=c("black"), cex=1.2)
    #polygon target
    polygon(c(prop_sharks_in_fish/100, target_sharks/100+prop_sharks_in_fish/100, target_sharks/100+prop_sharks_in_fish/100, prop_sharks_in_fish/100), c(0.8,0.8,0.6,0.6), col = "#39568CFF")
    #add target
    text(c(target_sharks/100+prop_sharks_in_fish/100), c(0.55),
         c(paste0("Target = " , round(target_sharks+prop_sharks_in_fish), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to target
    text(c(target_sharks/100+prop_sharks_in_fish/100), c(0.5),
         c(paste0("(" , round(target_ind+eff_target_ind), " ind)")),
         col=c("black"), cex=1.2)
    #polygon 100%
    polygon(c(target_sharks/100+prop_sharks_in_fish/100, 1, 1, target_sharks/100+prop_sharks_in_fish/100), c(0.6,0.6,0.8,0.8), border = "black", col= "white")
    text(c(1+0.05), c(0.555),
         c("100 %"),
         col=c("black"), cex=1.2)
    #add title
    text(0.5, 0.9, "Sharks", cex = 1.5)


    #barchart fish at the bottom
    #polygon protected in fish
    polygon(c(0, prop_fish_in_fish/100, prop_fish_in_fish/100, 0), c(0.1,0.1,target_fish/100,target_fish/100), col = "#A62098FF")
    #add protected in fish
    text(c(prop_fish_in_fish/100), c(0.40),
         c(paste0("Protected = " , round(prop_fish_in_fish), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to protected in fish
    text(c(prop_fish_in_fish/100), c(0.35),
         c(paste0("(" , round(eff_target_t2), " t)")),
         col=c("black"), cex=1.2)
    #add target
    text(c(target_fish/100), c(0.05),
         c(paste0("Target = " , round(target_fish), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to target
    text(c(target_fish/100), c(0),
         c(paste0("(" , round(target_t), " t)")),
         col=c("black"), cex=1.2)
    #polygon protected in sharks
    polygon(c(target_fish/100, target_fish/100+prop_fish_in_sharks/100, target_fish/100+prop_fish_in_sharks/100, target_fish/100), c(0.1,0.1,target_fish/100,target_fish/100), col = "#39568CFF")
    #add protected
    text(c(target_fish/100+prop_fish_in_sharks/100), c(0.05),
         c(paste0("Protected = " , round(prop_fish_in_sharks), " %")),
         col=c("black"), cex=1.2)
    #add number corresponding to protected
    text(c(target_fish/100+prop_fish_in_sharks/100), c(0),
         c(paste0("(" , round(eff_target_t), " t)")),
         col=c("black"), cex=1.2)
    #polygon 100%
    polygon(c(target_fish/100+prop_fish_in_sharks/100, 1, 1, target_fish/100+prop_fish_in_sharks/100), c(0.1,0.1,target_fish/100,target_fish/100), border = "black", col= "white")
    text(c(1+0.05), c(0.05),
         c("100 %"),
         col=c("black"), cex=1.2)
    #add title
    text(0.5, 0.4, "Fish", cex = 1.5)

    dev.off()
  }

}



#' calculate proportion fish biomass in reserves
#'
#' @param reserves
#' @param pred_fish
#'
#' @return
#' @export
#'

calc_fish_prop_in_reserves <- function(reserves, pred_fish){

  # get fish cells in reserves
  pred_fish_in_reserves = raster::mask(pred_fish, reserves, maskvalue=0)

  # calc proportion
  prop_fish_in_reserves = sum(raster::values(pred_fish_in_reserves), na.rm=T) / sum(raster::values(pred_fish), na.rm=T)

  return(prop_fish_in_reserves)

}




#' calculate proportion shark abundance in reserves
#'
#' @param reserves
#' @param pred_fish
#'
#' @return
#' @export
#'

calc_shark_prop_in_reserves <- function(reserves, pred_sharks){

  # get sharks cells in reserves
  pred_sharks_in_reserves = raster::mask(pred_sharks, reserves, maskvalue=0)

  # calc proportion
  prop_sharks_in_reserves = sum(raster::values(pred_sharks_in_reserves), na.rm=T) / sum(raster::values(pred_sharks), na.rm=T)

  return(prop_sharks_in_reserves)

}




#' Calculate reserve surface in km2
#'
#' @param reserve
#'
#' @return
#' @export
#'

calc_reserve_surface <- function(reserve){

  # ncells with reserve
  ncells = length(reserve[reserve ==1])

  # each cell is 1km2 so ncells is the surface in km2
  return(ncells)

}





#' Plot contributions of reserves for sharks to the protection of fish
#'
#' @param prop_sharks
#' @param surf_sharks
#' @param surf_tot
#'
#' @return
#' @export
#'

plot_contribution_shark_reserves_fish <- function(prop_sharks, surf_sharks, surf_tot){

  # plot 1: plot proportion sharks vs fish protected
  png(here::here("outputs/msp/plot1_sharks_reserves_vs_fish.png"))
  plot(seq(10, 100, 10), prop_sharks,
       ylab = "Proportion of fish biomass protected",
       xlab = "Proportion of shark abundance protected",
       xlim = c(0,100), ylim = c(0,100))
  ### add bissectrice
  lines(1:100, 1:100)
  axis(side=1, at=seq(0,100,10))
  axis(side=2, at=seq(0,100,10))
  dev.off()

  # plot 2: plot surface sharks reserves vs fish protected
  png(here::here("outputs/msp/plot2_sharks_reserves_vs_fish.png"))
  plot(surf_sharks, prop_sharks,
       ylab = "Proportion of fish biomass protected",
       xlab = expression(paste("Surface of sharks reserves (", "km"^"2", ")")),
       ylim = c(0,100))
  dev.off()

  # plot 3: plot surface sharks reserves vs fish protected with surface
  png(here::here("outputs/msp/plot3_sharks_reserves_vs_fish.png"))
  par(mar = c(5, 4, 7, 2))
  plot(surf_sharks/surf_tot*100, prop_sharks,
       ylab = "Proportion of fish biomass protected",
       xlab = "Proportion of sharks abundance protected",
       axes = F, xlim = c(0,100), ylim = c(0,100))
  axis(3, at = round(surf_sharks/surf_tot*100), las=2)
  axis(2, at = seq(0, 100, 10), las=2)
  axis(1, at = c(0, surf_sharks/surf_tot*100), labels = seq(0, 100, 10), las =2)
  title("Surface of sharks reserves (%)", cex.main=1, font.main= 1)
  box()
  dev.off()

  # plot 4: plot surface sharks reserves vs fish protected with surface in percent on regular x axis
  png(here::here("outputs/msp/plot4_sharks_reserves_vs_fish.png"))
  plot(seq(10, 100, 10), prop_sharks,
       ylab = "Proportion of fish biomass protected",
       xlab = "Proportion of shark abundance protected",
       xlim = c(0,100), ylim = c(0,100), axes = F)
  ### add bissectrice
  lines(1:100, 1:100)
  axis(side=1, at=seq(0,100,10))
  axis(side=2, at=seq(0,100,10))
  axis(3, at = seq(0, 100, 10), labels = c(0, round(surf_sharks/surf_tot*100)), las=2)
  title("Surface of sharks reserves (%)", cex.main=1, font.main= 1, line = 3)
  box()
  dev.off()

  # plot 5: plot surface sharks reserves vs fish protected with surface in cells of 1 km2 on regular x axis
  png(here::here("outputs/msp/plot5_sharks_reserves_vs_fish.png"))
  par(mar =  c(5, 4, 6, 2))
  plot(seq(10, 100, 10), prop_sharks,
       ylab = "Proportion of fish biomass protected",
       xlab = "Proportion of shark abundance protected",
       xlim = c(0,100), ylim = c(0,100), axes = F)
  ### add bissectrice
  lines(1:100, 1:100)
  axis(side=1, at=seq(0,100,10))
  axis(side=2, at=seq(0,100,10))
  axis(3, at = seq(0, 100, 10), labels = c(0, surf_sharks), las=2)
  title(expression(paste("Surface of sharks reserves (", "km"^"2", ")")), cex.main=1, font.main= 1, line = 5)
  box()
  dev.off()


  # plot 6: plot surface sharks reserves vs fish protected with surface in cells of 1 km2 on regular x axis
  # with added y axis for proportion of fish target

  #calculate total fish biomass
  val_fish_t = raster::values(pred_fish_species)
  tot_fish_t = sum(val_fish_t, na.rm=T) / 1000

  #calculate proportion of fish target (30%) corresponding to 10 - 100% of fish biomass
  fish_target_t = 0.3*tot_fish_t
  prop_fish_target = round((seq(0,100,10) * tot_fish_t) / fish_target_t)

  png(here::here("outputs/msp/plot6_sharks_reserves_vs_fish.png"), width = 700, height = 560)
  par(mar =  c(5, 7, 6, 7))
  plot(seq(10, 100, 10), prop_sharks,
       ylab = " ",
       xlab = "Shark target",
       xlim = c(0,100), ylim = c(0,100), axes = F)
  ### add bissectrice
  lines(1:100, 1:100)
  axis(side=1, at=seq(0,100,10), labels = paste(seq(0,100,10), "%"))
  text(x = -18, y = 50, labels = "Fish biomass protected", srt = 90, xpd = TRUE)
  axis(side=2, at=seq(0,100,10), las = 2, labels = paste(seq(0,100,10), "%"))
  axis(3, at = seq(0, 100, 10), labels = c(0, surf_sharks), las = 2)
  axis(side=4, at=seq(0,100,10), labels = paste(prop_fish_target, "%"), las = 2)
  title(expression(paste("Surface of sharks reserves (", "km"^"2", ")")), cex.main=1, font.main= 1, line = 5)
  text(x = 120, y = 50, labels = "Fish target protected", srt = -90, xpd = TRUE)
  abline(v = 80, col = "blue", lty = 2)
  abline(h = prop_sharks[8], col = "blue", lty = 2)
  box()
  dev.off()

  cat("% of fish target protected corresponding to fish biomass protected:", (prop_sharks[8]* tot_fish_t) / fish_target_t)

}







#' Plot contributions of fish reserves to the protection of sharks
#'
#' @param prop_fish
#' @param surf_fish
#' @param surf_tot
#'
#' @return
#' @export
#'

plot_contribution_fish_reserves_sharks <- function(prop_fish, surf_fish, surf_tot){

  # plot 1: plot proportion fish vs sharks protected
  png(here::here("outputs/msp/plot1_fish_reserves_vs_sharks.png"))
  plot(seq(10, 100, 10), prop_fish,
       ylab = "Proportion of sharks abundance protected",
       xlab = "Proportion of fish biomass protected",
       xlim = c(0,100), ylim = c(0,100))
  ### add bissectrice
  lines(1:100, 1:100)
  axis(side=1, at=seq(0,100,10))
  axis(side=2, at=seq(0,100,10))
  dev.off()


  # plot 2: plot surface fish reserves vs sharks protected
  png(here::here("outputs/msp/plot2_fish_reserves_vs_sharks.png"))
  plot(surf_fish, prop_fish,
       ylab = "Proportion of sharks abundance protected",
       xlab = expression(paste("Surface of fish reserves (", "km"^"2", ")")),
       ylim = c(0,100))
  dev.off()


  # plot 3: plot surface fish reserves vs sharks protected with surface
  png(here::here("outputs/msp/plot3_fish_reserves_vs_sharks.png"))
  par(mar = c(5, 4, 7, 2))
  plot(surf_fish/surf_tot*100, prop_fish,
       ylab = "Proportion of sharks abundance protected",
       xlab = "Proportion of fish biomass protected",
       axes = F, xlim = c(0,100), ylim = c(0,100))
  axis(3, at = round(surf_fish/surf_tot*100), las=2)
  axis(2, at = seq(0, 100, 10), las=2)
  axis(1, at = c(0, surf_fish/surf_tot*100), labels = seq(0, 100, 10), las =2)
  title("Surface of fish reserves (%)", cex.main=1, font.main= 1)
  box()
  dev.off()


  # plot 4: plot surface fish reserves vs sharks protected with surface in percent on regular x axis
  png(here::here("outputs/msp/plot4_fish_reserves_vs_sharks.png"))
  plot(seq(10, 100, 10), prop_fish,
       xlab = "Proportion of fish biomass protected",
       ylab = "Proportion of shark abundance protected",
       xlim = c(0,100), ylim = c(0,100), axes = F)
  ### add bissectrice
  lines(1:100, 1:100)
  axis(side=1, at=seq(0,100,10))
  axis(side=2, at=seq(0,100,10))
  axis(3, at = seq(0, 100, 10), labels = c(0, round(surf_fish/surf_tot*100)), las=2)
  title("Surface of fish reserves (%)", cex.main=1, font.main= 1, line = 3)
  box()
  dev.off()


  # plot 5: plot surface fish reserves vs sharks protected with surface in cells of 1 km2 on regular x axis
  png(here::here("outputs/msp/plot5_fish_reserves_vs_sharks.png"))
  par(mar =  c(5, 4, 6, 2))
  plot(seq(10, 100, 10), prop_fish,
       xlab = "Proportion of fish biomass protected",
       ylab = "Proportion of shark abundance protected",
       xlim = c(0,100), ylim = c(0,100), axes = F)
  ### add bissectrice
  lines(1:100, 1:100)
  axis(side=1, at=seq(0,100,10))
  axis(side=2, at=seq(0,100,10))
  axis(3, at = seq(0, 100, 10), labels = c(0, surf_fish), las=2)
  title(expression(paste("Surface of fish reserves (", "km"^"2", ")")), cex.main=1, font.main= 1, line = 5)
  box()
  dev.off()

  # plot 6: plot surface fish reserves vs shark protected with surface in cells of 1 km2 on regular x axis
  # with added y axis for proportion of shark target

  #calculate total shark biomass
  val_shark_ind = raster::values(pred_shark_species)
  tot_shark_ind = sum(val_shark_ind, na.rm=T)

  #calculate proportion of shark target (80%) corresponding to 10 - 100% of shark abundance
  shark_target_ind = 0.8*tot_shark_ind
  prop_shark_target = round((seq(0,100,10) * tot_shark_ind) / shark_target_ind)

  png(here::here("outputs/msp/plot6_fish_reserves_vs_sharks.png"), width = 700, height = 560)
  par(mar =  c(5, 7, 6, 7))
  plot(seq(10, 100, 10), prop_fish,
       ylab = " ",
       xlab = "Fish target",
       xlim = c(0,100), ylim = c(0,100), axes = F)
  ### add bissectrice
  lines(1:100, 1:100)
  axis(side=1, at=seq(0,100,10), labels = paste(seq(0,100,10), "%"))
  text(x = -18, y = 50, labels = "Shark abundance protected", srt = 90, xpd = TRUE)
  axis(side=2, at=seq(0,100,10), las = 2, labels = paste(seq(0,100,10), "%"))
  axis(3, at = seq(0, 100, 10), labels = c(0, surf_fish), las = 2)
  axis(side=4, at=seq(0,100,10), labels = paste(prop_shark_target, "%"), las = 2)
  title(expression(paste("Surface of fish reserves (", "km"^"2", ")")), cex.main=1, font.main= 1, line = 5)
  text(x = 120, y = 50, labels = "Shark target protected", srt = -90, xpd = TRUE)
  abline(v = 30, col = "blue", lty = 2)
  abline(h = prop_fish[3], col = "blue", lty = 2)
  box()
  dev.off()

  cat("% of shark target protected corresponding to shark abundance protected:", (prop_fish[3]* tot_shark_ind) / shark_target_ind)

}








#' Plot contributions of reserves for sharks to the protection of fish
#'
#' @param prop_sharks
#' @param surf_sharks
#' @param surf_tot
#'
#' @return
#' @export
#'

plot_contribution_shark_fish_reserves_fish_sharks <- function(prop_sharks, prop_fish){

  #calculate total fish biomass
  val_fish_t = raster::values(pred_fish_species)
  tot_fish_t = sum(val_fish_t, na.rm=T) / 1000

  #calculate proportion of fish target (30%) corresponding to 10 - 100% of fish biomass
  fish_target_t = 0.3*tot_fish_t
  prop_fish_target = round((seq(0,100,10) * tot_fish_t) / fish_target_t)

  #calculate total shark abundance
  val_shark_ind = raster::values(pred_shark_species)
  tot_shark_ind = sum(val_shark_ind, na.rm=T)

  #calculate proportion of shark target (80%) corresponding to 10 - 100% of shark abundance
  shark_target_ind = 0.8*tot_shark_ind
  prop_shark_target = round((seq(0,100,10) * tot_shark_ind) / shark_target_ind)

  png(here::here("outputs/msp/plot_contribution_shark_fish_reserves_fish_sharks.png"), width = 700, height = 560)
  par(mar =  c(5, 7, 6, 7))
  #sharks line
  plot(seq(0, 100, 10), c(0,prop_sharks),
       ylab = " ",
       xlab = "Target",
       xlim = c(0,100), ylim = c(0,100), axes = F, type = "b", col= "red")
  #fish line
  lines(seq(0, 100, 10), c(0,prop_fish), col = "blue")
  points(seq(0, 100, 10), c(0,prop_fish), col = "blue")
  #Fish axis
  axis(side=1, at=seq(0,100,10), labels = paste(seq(0,100,10), "%"))
  text(x = -18, y = 50, labels = "Fish target protected", srt = 90, xpd = TRUE)
  axis(side=2, at=seq(0,100,10), labels = paste(prop_fish_target, "%"), las = 2)
  #Sharks axis
  text(x = 118, y = 50, labels = "Shark target protected", srt = -90, xpd = TRUE)
  axis(side=4, at=seq(0,100,10), labels = paste(prop_shark_target, "%"), las = 2)
  #sharks abline
  y_fish = (237*70)/233
  segments(80, -5, 80, y_fish, col = "blue", lty = 2)
  segments(-5, y_fish, 80, y_fish, col = "blue", lty = 2)
  #fish abline
  y_sharks = (39*30)/38
  segments(30, -5, 30, y_sharks, col = "red", lty = 2)
  segments(30, y_sharks, 105, y_sharks, col = "red", lty = 2)
  #legend
  legend("topleft", c("Sharks msp", "Fish msp"), col = c("blue", "red"), lty = c(1,1), text.col = c("blue", "red"))
  box()
  dev.off()

}




#' Calculate patch surfaces in km2
#'
#' @param reserve
#'
#' @return
#' @export
#'

calc_patch_surfaces <- function(reserves){

  #project
  reserves2 = raster::projectRaster(reserves, crs = '+init=EPSG:3163', method= "ngb") ## lambert new caledonia

  #replace 0s (no reserve) with NAs
  reserves2[reserves2==0] <- NA

  #landscapemetrics::check_landscape(reserves2)

  #calculate patch surface
  s = landscapemetrics::lsm_p_area(reserves2) #in Hectares
  surfaces_km2 = s$value * 0.01 #in km2

  return(surfaces_km2)
}




#' calculate Jaccard similarity index between fish and shark reserves
#'
#' @param raster_sharks
#' @param raster_fish
#'
#' @return
#' @export
#'

calculate_jaccard <- function(raster_sharks, raster_fish){

  #crop to extent
  raster_sharks = raster::crop(raster_sharks, raster::extent( 157, 170, -23.5, -17.5))

  # Calculate the intersection of the two rasters, this is given by adding
  # the binary rasters together -> 2 indicates intersection
  combination = raster_sharks + raster_fish
  intersection = combination == 2

  # Union is all the area covered by the both rasters
  union = combination >= 1

  # Jaccard index
  jac = raster::freq(intersection, value = 1) / raster::freq(union, value = 1)

  return(jac)
}




#' calculate Jaccard dissimilarity index between fish and shark reserves (following Albouy)
#'
#' @param raster_sharks
#' @param raster_fish
#'
#' @return
#' @export
#'

calculate_jaccard_diss <- function(rastershark, rasterfish){

  #compute intersection of the two rasters

  rastershark = rastershark*2
  rasternew = rastershark + rasterfish

  # following Albouy et al 2012

  a = raster::freq(rasternew, value = 3) #nb of cells in intersection

  b = raster::freq(rasternew, value = 1) #nb of cells in rasterfish but not rastershark

  c = raster::freq(rasternew, value = 2) #nb of cells in rastershark but not rasterfish

  # Jaccard index
  jac = (b + c) / (a + b + c)

  return(jac)
}



#' calculate sorensen dissimilarity index (incorporates both true spatial turnover and nestedness) based on Baselga 2010
#'
#' @param raster_sharks
#' @param raster_fish
#'
#' @return
#' @export
#'

calculate_sorensen <- function(rastershark, rasterfish){

  #compute intersection of the two rasters

  rastershark = rastershark*2
  rasternew = rastershark + rasterfish

  # following Baselga et al 2010 - Partitioning the turnover and nestedness components of beta diversity

  a = raster::freq(rasternew, value = 3) #nb of cells in intersection

  b = raster::freq(rasternew, value = 1) #nb of cells in rasterfish but not rastershark

  c = raster::freq(rasternew, value = 2) #nb of cells in rastershark but not rasterfish

  #sorensen dissimilarity index (incorporates both true spatial turnover and nestedness)
  sorensen = (b + c) / (2*a + b + c)

  return(sorensen)
}


#' calculate simpson dissimilarity index to describe spatial turnover based on Baselga 2010
#'
#' @param raster_sharks
#' @param raster_fish
#'
#' @return
#' @export
#'

calculate_simpson <- function(rastershark, rasterfish){

  #compute intersection of the two rasters

  rastershark = rastershark*2
  rasternew = rastershark + rasterfish

  # following Baselga et al 2010 - Partitioning the turnover and nestedness components of beta diversity

  a = raster::freq(rasternew, value = 3) #nb of cells in intersection

  b = raster::freq(rasternew, value = 1) #nb of cells in rasterfish but not rastershark

  c = raster::freq(rasternew, value = 2) #nb of cells in rastershark but not rasterfish

  #simpson dissimilarity index to describe spatial turnover
  simpson = min(b, c) / (a + min(b, c))

  return(simpson)

}



#' calculate beta ratio following albouy et al 2012
#'
#' @param raster_sharks
#' @param raster_fish
#'
#' @return
#' @export
#'

calculate_beta_ratio <- function(rastershark, rasterfish){

  #compute intersection of the two rasters

  rastershark = rastershark*2
  rasternew = rastershark + rasterfish

  # following Albouy et al 2012

  a = raster::freq(rasternew, value = 3) #nb of cells in intersection

  b = raster::freq(rasternew, value = 1) #nb of cells in rasterfish but not rastershark

  c = raster::freq(rasternew, value = 2) #nb of cells in rastershark but not rasterfish

  #beta ratio
  #useful to describe the relative contribution of each component (species replacement vs. nestedness) in explaining
  #the overall amount of turnover
  beta_ratio = ((max(b, c) - min(b, c)) / (a + b + c)) * a / (a + 2 * min(b, c))

  return(beta_ratio)

}






#' Map irreplaceability for sharks
#'
#' @return
#' @export
#'

map_irreplaceability_sharks <- function(){

  # stack shark reserves for all targets
  stack_sharks = raster::stack(reserves_sharks_minset0.1, reserves_sharks_minset0.2, reserves_sharks_minset0.3,
                               reserves_sharks_minset0.4, reserves_sharks_minset0.5, reserves_sharks_minset0.6,
                               reserves_sharks_minset0.7, reserves_sharks_minset0.8, reserves_sharks_minset0.9)

  #crop to extent
  stack_sharks = raster::crop(stack_sharks, raster::extent(-158, 168.5, -24, -17))

  #calc number of times each raster cell is selected as a reserve solution
  #(1: selected in one solution, 2: selected in 2 solutions ...)
  irrep = raster::calc(stack_sharks,fun=sum)

  # mask stack so that there are NAs outside the study region
  mask = raster::raster(here::here("outputs/brts/poisson/predictions_summed_species.grd"))
  mask = raster::crop(mask, raster::extent(-158, 168.5, -24, -17))
  irrep = raster::mask(irrep, mask)

  #plot irreplaceability 1 - frequency selected in legend

  png(here::here("/outputs/msp/map_irreplaceability_sharks.png"),  width = 960, height = 480)

  #layout
  layout.matrix <- matrix(c(1, 2), ncol = 2)
  layout(mat = layout.matrix, heights = c(1,1), widths =c(5,1))

  #map
  arg = list(at=seq(0, 9, 1), labels=seq(0, 9, 1))
  par(mar = c(2, 0.5, 1, 1))
  raster::plot(irrep,  axis.args =  arg, col = c("light grey", viridisLite::viridis(9)),
               legend.args = list(text='Irreplaceability sharks', side=4, line=2), box = F, legend = F)

  #barplot of cell numbers per color
  for (i in seq(0, 9, 1)){
    assign(paste0("nc_sharks", i),  raster::ncell(irrep[irrep >= i & irrep < i+0.1]))
  }

  ncells = c(nc_sharks0, nc_sharks1, nc_sharks2, nc_sharks3, nc_sharks4, nc_sharks5, nc_sharks6, nc_sharks7,
             nc_sharks8, nc_sharks9)

  par(mar = c(4, 21, 1, 1))
  barplot(ncells,
          col = c("light grey", viridisLite::viridis(9)),
          cex.names = 1.2,
          las=2, ann = FALSE, horiz = T, border = NA)
  axis(2, at=c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7, 7.9, 9.1, 10.3, 11.5), labels=seq(0, 9, 1),  las=2, tick = F, line=NA)
  title(xlab = "Surface (km2)", cex.lab = 1, line = 3)

  dev.off()

  #write raster
  raster::writeRaster(irrep, here::here("outputs/msp/irreplaceability_sharks.grd"), overwrite=TRUE)


  #plot irreplaceability 2 - target in legend

  png(here::here("/outputs/msp/map_irreplaceability_sharks2.png"),  width = 960, height = 480)

  #layout
  layout.matrix <- matrix(c(1, 2), ncol = 2)
  layout(mat = layout.matrix, heights = c(1,1), widths =c(5,1))

  #map
  arg = list(at=seq(0, 9, 1), labels=seq(0, 9, 1))
  par(mar = c(2, 0.5, 1, 1))
  raster::plot(irrep,  axis.args =  arg, col = rev(c(viridisLite::viridis(10))),
               legend.args = list(text='Irreplaceability sharks', side=4, line=2), box = F, legend = F)

  #barplot of cell numbers per color
  for (i in seq(0, 9, 1)){
    assign(paste0("nc_sharks", i),  raster::ncell(irrep[irrep >= i & irrep < i+0.1]))
  }

  ncells = c(nc_sharks0, nc_sharks1, nc_sharks2, nc_sharks3, nc_sharks4, nc_sharks5, nc_sharks6, nc_sharks7,
             nc_sharks8, nc_sharks9)

  par(mar = c(4, 21, 1, 1))
  barplot(ncells,
          col = rev(c(viridisLite::viridis(10))),
          cex.names = 1.2,
          las=2, ann = FALSE, horiz = T, border = NA)
  axis(2, at=c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7, 7.9, 9.1, 10.3, 11.5), labels=paste(rev(seq(10, 100, 10)), "%"),  las=2, tick = F, line=NA)
  title(xlab = "Surface (km2)", cex.lab = 1, line = 3)

  dev.off()


  #plot irreplaceability 3 - target in legend with cumulated surface

  png(here::here("/outputs/msp/map_irreplaceability_sharks3.png"),  width = 960, height = 480)

  #layout
  layout.matrix <- matrix(c(1, 2), ncol = 2)
  layout(mat = layout.matrix, heights = c(1,1), widths =c(5,1))

  #map
  arg = list(at=seq(0, 9, 1), labels=seq(0, 9, 1))
  par(mar = c(2, 0.5, 1, 1))
  raster::plot(irrep,  axis.args =  arg, col = rev(c(viridisLite::viridis(10))),
               legend.args = list(text='Irreplaceability sharks', side=4, line=2), box = F, legend = F)

  #barplot of cell numbers per color
  for (i in seq(0, 9, 1)){
    assign(paste0("nc_sharks", i),  raster::ncell(irrep[irrep >= i & irrep < i+0.1]))
  }

  ncells = c(nc_sharks0, nc_sharks1, nc_sharks2, nc_sharks3, nc_sharks4, nc_sharks5, nc_sharks6, nc_sharks7,
             nc_sharks8, nc_sharks9)

  ncells2 = rev(ncells)

  data = as.matrix(data.frame(ncells2))

  par(mar = c(4, 21, 1, 4))
  barplot(data,
          col = c(viridisLite::viridis(10)),
          cex.names = 1.2,
          las=2, ann = FALSE, horiz = F, border = NA, names = F)
  axis(4, at=c(ncells2[1], sum(ncells2[1:2]), sum(ncells2[1:3]), sum(ncells2[1:4]), sum(ncells2[1:5]),
               sum(ncells2[1:6]), sum(ncells2[1:7]), sum(ncells2[1:8]), sum(ncells2[1:9]), sum(ncells2[1:10])),
       labels=paste(seq(10, 100, 10), "%"),  las=2, tick = F, line=NA)
  title(xlab = "Cumulated surface (km2)", cex.lab = 1, line = 2)

  dev.off()

  #write raster
  raster::writeRaster(irrep, here::here("outputs/msp/irreplaceability_sharks.grd"), overwrite=TRUE)

  return(irrep)
}




#' Map irreplaceability for fish
#'
#' @return
#' @export
#'


map_irreplaceability_fish <- function(){

  # stack shark reserves for all targets
  stack_fish = raster::stack(reserves_fish_minset0.1, reserves_fish_minset0.2, reserves_fish_minset0.3,
                             reserves_fish_minset0.4, reserves_fish_minset0.5, reserves_fish_minset0.6,
                             reserves_fish_minset0.7, reserves_fish_minset0.8, reserves_fish_minset0.9)

  #crop to extent
  stack_fish = raster::crop(stack_fish, raster::extent(-158, 168.5, -24, -17))

  #calc number of times each raster cell is selected as a reserve solution
  #(1: selected in one solution, 2: selected in 2 solutions ...)
  irrep = raster::calc(stack_fish,fun=sum)

  # mask stack so that there are NAs outside the study region
  mask = raster::raster(here::here("outputs/brts/poisson/predictions_summed_species.grd"))
  mask = raster::crop(mask, raster::extent(-158, 168.5, -24, -17))
  irrep = raster::mask(irrep, mask)

  #plot irreplaceability 1 - frequency in legend

  png(here::here("/outputs/msp/map_irreplaceability_fish.png"),  width = 960, height = 480)

  #layout
  layout.matrix <- matrix(c(1, 2), ncol = 2)
  layout(mat = layout.matrix, heights = c(1,1), widths =c(5,1))

  #map
  arg = list(at=seq(0, 9, 1), labels=seq(0, 9, 1))
  par(mar = c(2, 0.5, 1, 1))
  raster::plot(irrep,  axis.args =  arg, col = c("light grey", viridisLite::viridis(9)),
               legend.args = list(text='Irreplaceability fish', side=4, line=2), box = F, legend = F)

  #barplot of cell numbers per color
  for (i in seq(0, 9, 1)){
    assign(paste0("nc_fish", i),  raster::ncell(irrep[irrep >= i & irrep < i+0.1]))
  }

  ncells = c(nc_fish0, nc_fish1, nc_fish2, nc_fish3, nc_fish4, nc_fish5, nc_fish6, nc_fish7,
             nc_fish8, nc_fish9)

  par(mar = c(4, 21, 1, 1))
  barplot(ncells,
          col = c("light grey", viridisLite::viridis(9)),
          cex.names = 1.2,
          las=2, ann = FALSE, horiz = T, border = NA)
  axis(2, at=c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7, 7.9, 9.1, 10.3, 11.5), labels=seq(0, 9, 1),  las=2, tick = F, line=NA)
  title(xlab = "Surface (km2)", cex.lab = 1, line = 3)

  dev.off()

  #plot irreplaceability 2 - target in legend

  png(here::here("/outputs/msp/map_irreplaceability_fish2.png"),  width = 960, height = 480)

  #layout
  layout.matrix <- matrix(c(1, 2), ncol = 2)
  layout(mat = layout.matrix, heights = c(1,1), widths =c(5,1))

  #map
  arg = list(at=seq(0, 9, 1), labels=seq(0, 9, 1))
  par(mar = c(2, 0.5, 1, 1))
  raster::plot(irrep,  axis.args =  arg, col = rev(c(viridisLite::viridis(10))),
               legend.args = list(text='Irreplaceability fish', side=4, line=2), box = F, legend = F)

  #barplot of cell numbers per color
  for (i in seq(0, 9, 1)){
    assign(paste0("nc_fish", i),  raster::ncell(irrep[irrep >= i & irrep < i+0.1]))
  }

  ncells = c(nc_fish0, nc_fish1, nc_fish2, nc_fish3, nc_fish4, nc_fish5, nc_fish6, nc_fish7,
             nc_fish8, nc_fish9)

  par(mar = c(4, 21, 1, 1))
  barplot(ncells,
          col = rev(c(viridisLite::viridis(10))),
          cex.names = 1.2,
          las=2, ann = FALSE, horiz = T, border = NA)
  axis(2, at=c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7, 7.9, 9.1, 10.3, 11.5), labels=paste(rev(seq(10, 100, 10)), "%"),  las=2, tick = F, line=NA)
  title(xlab = "Surface (km2)", cex.lab = 1, line = 3)

  dev.off()



  #plot irreplaceability 3 - target in legend with cumulated surface

  png(here::here("/outputs/msp/map_irreplaceability_fish3.png"),  width = 960, height = 480)

  #layout
  layout.matrix <- matrix(c(1, 2), ncol = 2)
  layout(mat = layout.matrix, heights = c(1,1), widths =c(5,1))

  #map
  arg = list(at=seq(0, 9, 1), labels=seq(0, 9, 1))
  par(mar = c(2, 0.5, 1, 1))
  raster::plot(irrep,  axis.args =  arg, col = rev(c(viridisLite::viridis(10))),
               legend.args = list(text='Irreplaceability fish', side=4, line=2), box = F, legend = F)

  #barplot of cell numbers per color
  for (i in seq(0, 9, 1)){
    assign(paste0("nc_fish", i),  raster::ncell(irrep[irrep >= i & irrep < i+0.1]))
  }

  ncells = c(nc_fish0, nc_fish1, nc_fish2, nc_fish3, nc_fish4, nc_fish5, nc_fish6, nc_fish7,
             nc_fish8, nc_fish9)

  ncells2 = rev(ncells)

  data = as.matrix(data.frame(ncells2))

  par(mar = c(4, 21, 1, 4))
  barplot(data,
          col = c(viridisLite::viridis(10)),
          cex.names = 1.2,
          las=2, ann = FALSE, horiz = F, border = NA, names = F)
  axis(4, at=c(ncells2[1], sum(ncells2[1:2]), sum(ncells2[1:3]), sum(ncells2[1:4]), sum(ncells2[1:5]),
               sum(ncells2[1:6]), sum(ncells2[1:7]), sum(ncells2[1:8]), sum(ncells2[1:9]), sum(ncells2[1:10])),
       labels=paste(seq(10, 100, 10), "%"),  las=2, tick = F, line=NA)
  title(xlab = "Cumulated surface (km2)", cex.lab = 1, line = 2)

  dev.off()

  #write raster
  raster::writeRaster(irrep, here::here("outputs/msp/irreplaceability_fish.grd"), overwrite=TRUE)

  return(irrep)
}



#' Map of the co-occurrence of shark and fish reserves and barchart of the associated targets
#'
#' @param rastershark
#' @param rasterfish
#' @param target
#'
#' @return
#' @export
#'

map_cooccurrence_reserves_fish_shark <- function(rastershark, rasterfish, target){

  # mask fish and shark rasters so that there are NAs outside the study region
  mask = raster::raster(here::here("outputs/brts/poisson/predictions_summed_species.grd"))
  rasterfish = raster::mask(rasterfish, mask)
  rastershark = raster::mask(rastershark, mask)

  rastershark = rastershark*2
  rasternew = rastershark + rasterfish

  #3: cells in rasterfish and rastershark
  #1: cells in rasterfish but not rastershark
  #2: cells in rastershark but not rasterfish



  #### map + barplot cell numbers

  png(here::here(paste0("/outputs/msp/map_cooccurrence_reserves_fish_shark_target_", target*100, ".png")), width = 960, height = 480)

  #layout
  layout.matrix <- matrix(c(1, 2), ncol = 2)
  layout(mat = layout.matrix, heights = c(1,1), widths =c(5,1))

  #map
  arg = list(at=c(0.5, 1.5, 2.5, 3.5))

  par(mar = c(2, 0.5, 1, 1))
  raster::plot(rasternew, breaks = c(0,0.5,1.5,2.5, 3.5),  axis.args =  arg, col = c("light grey", viridisLite::viridis(3)), legend = F, box = F,
               xlim = c(-158, 168.5), ylim = c(-23, -17.8))
  text(x = 160.5, y = -17.5, labels = paste("Target =", target*100, "%"))

  #barplot of cell numbers per color
  ncell0 = raster::ncell(rasternew[rasternew == 0])
  ncell1 = raster::ncell(rasternew[rasternew == 1])
  ncell2 = raster::ncell(rasternew[rasternew == 2])
  ncell3 = raster::ncell(rasternew[rasternew == 3])

  ncells = c(ncell0, ncell1, ncell2, ncell3)

  par(mar = c(5, 23, 1, 1))
  barplot(ncells,
          col = c("light grey", viridisLite::viridis(3)),
          cex.names = 1.2,
          las=2, ann = FALSE, horiz = T, border = NA)
  axis(2, at=c(0.7, 1.9, 3.1, 4.3), labels=c("No \n reserve", "Fish \n reserve \n only",
                                             "Shark \n reserve \n only", "Shark \n & fish \n reserve "),  las=2, tick = F, line=NA)
  title(xlab = "Surface (cells)", cex.lab = 1, line = 4)

  dev.off()



  #### barchart

  png(here::here(paste0("/outputs/msp/barchart_cooccurrence_reserves_fish_shark_target_", target*100, ".png")), width = 960, height = 480)

  par(mar = c(5, 5, 5, 5))

  ## barchart of shark target

  #load shark predictions and mask
  predictions_sharks = load_shark_species_pred()
  predictions_sharks = raster::crop(predictions_sharks, raster::extent(-158, 168.5, -24, -17))

  #calculate target in individual shark numbers
  val = raster::values(predictions_sharks)
  tot = sum(val, na.rm=T)
  target_ind = round(target*tot)

  #calculate surface of shark reserves
  ncell_sharks =  ncell3 + ncell2

  #calculate proportion of shark target in shark & fish reserves
  prop_shark3 = round((target_ind * (ncell3 / (ncell_sharks))) / target_ind,2)

  #calculate proportion of shark target in shark reserve only
  prop_shark2 = round((target_ind * (ncell2 / (ncell_sharks))) / target_ind,2)

  #plot at the top
  plot(NULL, xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n",
       xlab="", ylab="", bty='n')
  polygon(c(0, prop_shark3, prop_shark3, 0), c(0.6,0.6,0.8,0.8), col = viridisLite::viridis(3)[3])
  polygon(c(prop_shark3, 1, 1, prop_shark3), c(0.6,0.6,0.8,0.8), col = viridisLite::viridis(3)[2])
  #add proportions
  text(c(prop_shark3/2, prop_shark2/2 + prop_shark3), c(0.7,0.7),
       c(paste0(100*prop_shark3, "%"), paste0(100*prop_shark2, "%")),
       col=c("black", "white"), cex=1.2)
  #add targets
  text(c(prop_shark3/2, prop_shark2/2 + prop_shark3), c(0.65,0.65),
       c(paste0(round(prop_shark3*target_ind), "ind"), paste0(round(prop_shark2*target_ind), "ind")),
       col=c("black", "white"), cex=1.2)
  #add title
  text(0.5, 0.9, paste0("Proportion of shark target (", target*100, " % - ", target_ind, "ind) in reserve type") , cex = 1.5)


  ## barchart of fish target

  #load fish species predictions raster
  pred_fish_species = load_fish_species_pred()

  #calculate target in biomass of fish
  val = raster::values(pred_fish_species)
  tot = sum(val, na.rm=T)
  target_kg = target*tot
  target_t = round(target_kg / 1000)

  #calculate surface of fish reserves
  ncell_fish =  ncell3 + ncell1

  #calculate proportion of fish target in fish & fish reserves
  prop_fish3 = round((target_t * (ncell3 / (ncell_fish))) / target_t,2)

  #calculate proportion of fish target in fish reserve only
  prop_fish1 = round((target_t * (ncell1 / (ncell_fish))) / target_t,2)

  #plot at the bottom
  polygon(c(0, prop_fish3, prop_fish3, 0), c(0.1,0.1,0.3,0.3), col = viridisLite::viridis(3)[3])
  polygon(c(prop_fish3, 1, 1, prop_fish3), c(0.1,0.1,0.3,0.3), col = viridisLite::viridis(3)[1])
  #add proportions
  text(c(prop_fish3/2, prop_fish1/2 + prop_fish3), c(0.2,0.2),
       c(paste0(100*prop_fish3, "%"), paste0(100*prop_fish1, "%")),
       col=c("black", "white"), cex=1.2)
  #add targets
  text(c(prop_fish3/2, prop_fish1/2 + prop_fish3), c(0.15,0.15),
       c(paste0(round(prop_fish3*target_t), "t"), paste0(round(prop_fish1*target_t), "t")),
       col=c("black", "white"), cex=1.2)
  #add title
  text(0.5, 0.4, paste0("Proportion of fish target (", target*100, " % - ", target_t, "t) in reserve type") , cex = 1.5)

  dev.off()

}
