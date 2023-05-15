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
targets =c(rep(0.4, 52))

hierarchical_results40 <- data.frame(blmval=numeric(length(blmval)), total_boundary_length=numeric(length(blmval)), total_cost=numeric(length(blmval)))

for (i in 1:length(blmval)){

  #reformat p to avoid scientific writing in objects
  p2 = format(blmval[[i]], scientific = FALSE)
  
  # create output directory
  dir.create(paste("05_Marine_Spatial_Planning/03_Prioritization/Prioritization40/Solution40_blm_",p2, sep=""))
  path <- paste("05_Marine_Spatial_Planning/03_Prioritization/Prioritization40/Solution40_blm_",p2, sep="")
  
  # create the problem
  problem <-
    problem(pu_data$cost, features = feat_data, rij_matrix = rij_data) %>%
    add_min_set_objective() %>%
    add_relative_targets(targets) %>%
    add_boundary_penalties(blmval[[i]],0.5,data=bound_data) %>%
    add_binary_decisions()%>%
    add_gurobi_solver(gap=0.02, threads = 5)
  
  # generate a solution
  solution40 <- solve(problem)
  
  save(solution40, file=paste(path,"/solution40.rdata", sep=""))
  
  # check if target is met for each feature
  feature_target40 <- eval_target_coverage_summary(problem, solution40)
  save(feature_target40, file=paste(path,"/feature_target40.rdata", sep=""))
  
  # prepare data for plotting
  ## add solution to planning unit data
  pu_data40 <- pu_data
  pu_data40$solution_1 <- c(solution40)
  
  save(pu_data40, file=paste(path,"/pu_data40.rdata", sep=""))
  
  # calculate total cost and total boundary length of the solution
  sol40_cost <-  eval_cost_summary(problem, solution40)$cost
  sol40_boundary <- eval_boundary_summary(problem, solution40, data=bound_data)$boundary
  
  save(sol40_cost, file=paste(path,"/sol40_cost.rdata", sep=""))
  save(sol40_boundary, file=paste(path,"/sol40_boundary.rdata", sep=""))
  
  hierarchical_results40[i, "blmval"] <- blmval[[i]]
  hierarchical_results40[i, "total_boundary_length"] <- sol40_boundary
  hierarchical_results40[i, "total_cost"] <- sol40_cost
  
  ## lets create a list (one element per depth) with a copy of the solution
  ## values that correspond to the planning units at each depth
  sol_data <-
    lapply(seq_along(features_depth), function(i) {
      x <- pu_sf
      x$id_2d <- seq_len(nrow(x))
      x$id_depth <- names(features_depth)[[i]]
      x <- left_join(
        x,
        pu_data40 %>% dplyr::select(id_2d, id_depth, solution_1),
        by = c("id_2d", "id_depth")
      )
      x$solution_1
    }) %>%
    setNames(paste0("solution_1_", names(features_depth)))
  
  ## let's now create a version of the 2d planning unit data and insert
  ## a column for each depth, indicating (using 0s and 1s) if the
  ## planning unit was prioritized at a specific depth
  sol40_2d_data <-
    pu_sf %>%
    bind_cols(do.call(tibble, sol_data))
  
  save(sol40_2d_data, file=paste(path,"/sol40_2d_data.rdata", sep=""))
  
  surface <- data.frame(depth1=sol40_2d_data$solution_1_depth_1, depth2=sol40_2d_data$solution_1_depth_2, depth3=sol40_2d_data$solution_1_depth_3)
  surface$surface <- rowSums(surface[,1:3])
  sol40_surface <- nrow(as.data.frame(surface$surface[surface$surface>0]))
  save(sol40_surface, file=paste(path,"/sol40_surface.rdata", sep=""))
  
  sol40_depth1 <- sol40_2d_data %>%
    filter(solution_1_depth_1 == 1)
  plot(sol40_depth1[,"solution_1_depth_1"])
  
  sol40_depth2 <- sol40_2d_data %>%
    filter(solution_1_depth_2 == 1)
  plot(sol40_depth2[,"solution_1_depth_2"])
  
  sol40_depth3 <- sol40_2d_data %>%
    filter(solution_1_depth_3 == 1)
  plot(sol40_depth3[,"solution_1_depth_3"])
  
  
  save(sol40_depth1, file=paste(path,"/sol40_depth1.rdata", sep=""))
  save(sol40_depth2, file=paste(path,"/sol40_depth2.rdata", sep=""))
  save(sol40_depth3, file=paste(path,"/sol40_depth3.rdata", sep=""))
  
}


stopCluster(cl)


save(hierarchical_results40, file="05_Marine_Spatial_Planning/03_Prioritization/Prioritization40/Rdata/hierarchical_results40.rdata")

