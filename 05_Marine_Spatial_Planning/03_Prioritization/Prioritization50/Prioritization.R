library(gurobi)
library(prioritizr)
library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)
library(topsis)
library(ggplot2)
library(ggrepel)

load("05_Marine_Spatial_Planning/02_formating_MSP3D_inputs/Rdata/bound_data.rdata")
load("05_Marine_Spatial_Planning/02_formating_MSP3D_inputs/Rdata/pu_data.rdata")
load("05_Marine_Spatial_Planning/02_formating_MSP3D_inputs/Rdata/rij_data.rdata")
load("05_Marine_Spatial_Planning/02_formating_MSP3D_inputs/Rdata/feat_data.rdata")
load("05_Marine_Spatial_Planning/02_formating_MSP3D_inputs/Rdata/features_depth.rdata")
load("05_Marine_Spatial_Planning/02_formating_MSP3D_inputs/Rdata/pu_sf.rdata")



cl = makeCluster(5)
registerDoParallel(cl)

blmval = c(0)
targets =c(rep(0.5, 52))

hierarchical_results50 <- data.frame(blmval=numeric(length(blmval)), total_boundary_length=numeric(length(blmval)), total_cost=numeric(length(blmval)))

for (i in 1:length(blmval)){

  #reformat p to avoid scientific writing in objects
  p2 = format(blmval[[i]], scientific = FALSE)
  
  # create output directory
  dir.create(paste("05_Marine_Spatial_Planning/03_Prioritization/Prioritization50/Solution50_blm_",p2, sep=""))
  path <- paste("05_Marine_Spatial_Planning/03_Prioritization/Prioritization50/Solution50_blm_",p2, sep="")
  
  # create the problem
  problem <-
    problem(pu_data$cost, features = feat_data, rij_matrix = rij_data) %>%
    add_min_set_objective() %>%
    add_relative_targets(targets) %>%
    add_boundary_penalties(blmval[[i]],0.5,data=bound_data) %>%
    add_binary_decisions()%>%
    add_gurobi_solver(gap=0.02, threads = 5)
  
  # generate a solution
  solution50 <- solve(problem)
  
  save(solution50, file=paste(path,"/solution50.rdata", sep=""))
  
  # check if target is met for each feature
  feature_target50 <- eval_target_coverage_summary(problem, solution50)
  save(feature_target50, file=paste(path,"/feature_target50.rdata", sep=""))
  
  # prepare data for plotting
  ## add solution to planning unit data
  pu_data50 <- pu_data
  pu_data50$solution_1 <- c(solution50)
  
  save(pu_data50, file=paste(path,"/pu_data50.rdata", sep=""))
  
  # calculate total cost and total boundary length of the solution
  sol50_cost <-  eval_cost_summary(problem, solution50)$cost
  sol50_boundary <- eval_boundary_summary(problem, solution50, data=bound_data)$boundary
  
  save(sol50_cost, file=paste(path,"/sol50_cost.rdata", sep=""))
  save(sol50_boundary, file=paste(path,"/sol50_boundary.rdata", sep=""))
  
  hierarchical_results50[i, "blmval"] <- blmval[[i]]
  hierarchical_results50[i, "total_boundary_length"] <- sol50_boundary
  hierarchical_results50[i, "total_cost"] <- sol50_cost
  
  ## lets create a list (one element per depth) with a copy of the solution
  ## values that correspond to the planning units at each depth
  sol_data <-
    lapply(seq_along(features_depth), function(i) {
      x <- pu_sf
      x$id_2d <- seq_len(nrow(x))
      x$id_depth <- names(features_depth)[[i]]
      x <- left_join(
        x,
        pu_data50 %>% dplyr::select(id_2d, id_depth, solution_1),
        by = c("id_2d", "id_depth")
      )
      x$solution_1
    }) %>%
    setNames(paste0("solution_1_", names(features_depth)))
  
  ## let's now create a version of the 2d planning unit data and insert
  ## a column for each depth, indicating (using 0s and 1s) if the
  ## planning unit was prioritized at a specific depth
  sol50_2d_data <-
    pu_sf %>%
    bind_cols(do.call(tibble, sol_data))
  
  save(sol50_2d_data, file=paste(path,"/sol50_2d_data.rdata", sep=""))
  
  surface <- data.frame(depth1=sol50_2d_data$solution_1_depth_1, depth2=sol50_2d_data$solution_1_depth_2, depth3=sol50_2d_data$solution_1_depth_3)
  surface$surface <- rowSums(surface[,1:3])
  sol50_surface <- nrow(as.data.frame(surface$surface[surface$surface>0]))
  save(sol50_surface, file=paste(path,"/sol50_surface.rdata", sep=""))
  
  sol50_depth1 <- sol50_2d_data %>%
    filter(solution_1_depth_1 == 1)
  plot(sol50_depth1[,"solution_1_depth_1"])
  
  sol50_depth2 <- sol50_2d_data %>%
    filter(solution_1_depth_2 == 1)
  plot(sol50_depth2[,"solution_1_depth_2"])
  
  sol50_depth3 <- sol50_2d_data %>%
    filter(solution_1_depth_3 == 1)
  plot(sol50_depth3[,"solution_1_depth_3"])
  
  
  save(sol50_depth1, file=paste(path,"/sol50_depth1.rdata", sep=""))
  save(sol50_depth2, file=paste(path,"/sol50_depth2.rdata", sep=""))
  save(sol50_depth3, file=paste(path,"/sol50_depth3.rdata", sep=""))
  
}


stopCluster(cl)


save(hierarchical_results50, file="05_Marine_Spatial_Planning/03_Prioritization/Prioritization50/Rdata/hierarchical_results50.rdata")

