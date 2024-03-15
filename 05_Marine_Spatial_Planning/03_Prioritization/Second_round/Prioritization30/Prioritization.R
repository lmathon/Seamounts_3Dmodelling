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

# transform bound_data to km²
bound_data$boundary <- bound_data$boundary/1000000

cl = makeCluster(5)
registerDoParallel(cl)

blmval = c(0, 0.0001, 0.001, 0.01, 0.1, 1, 10, 100)
targets =c(rep(0.3, 52))

#load("05_Marine_Spatial_Planning/03_Prioritization/Prioritization30/Rdata/hierarchical_results.rdata")
hierarchical_results <- data.frame(blmval=numeric(length(blmval)), total_boundary_length=numeric(length(blmval)), total_cost=numeric(length(blmval)))

for (i in 1:length(blmval)){

  #reformat p to avoid scientifc writing in objects
  p2 = format(blmval[[i]], scientific = FALSE)
  
  # create output directory
  dir.create(paste("05_Marine_Spatial_Planning/03_Prioritization/Second_round/Prioritization30/Solution30_blm_",p2, sep=""))
  path <- paste("05_Marine_Spatial_Planning/03_Prioritization/Second_round/Prioritization30/Solution30_blm_",p2, sep="")
  
  # create the problem
  problem <-
    problem(pu_data$cost, features = feat_data, rij_matrix = rij_data) %>%
    add_min_set_objective() %>%
    add_relative_targets(targets) %>%
    add_boundary_penalties(blmval[[i]],0.5,data=bound_data) %>%
    add_binary_decisions()%>%
    add_gurobi_solver(gap=0.02, threads = 5)
  
  # generate a solution
  solution <- solve(problem)
  
  save(solution, file=paste(path,"/solution30.rdata", sep=""))
  
  # check if target is met for each feature
  feature_target30 <- eval_target_coverage_summary(problem, solution)
  save(feature_target30, file=paste(path,"/feature_target30.rdata", sep=""))
  
  # prepare data for plotting
  ## add solution to planning unit data
  pu_data$solution_1 <- c(solution)
  
  save(pu_data, file=paste(path,"/pu_data30.rdata", sep=""))
  
  # calculate total cost and total boundary length of the solution
  sol_cost <-  eval_cost_summary(problem, solution)$cost
  sol_boundary <- eval_boundary_summary(problem, solution, data=bound_data)$boundary
  
  save(sol_cost, file=paste(path,"/sol30_cost.rdata", sep=""))
  save(sol_boundary, file=paste(path,"/sol30_boundary.rdata", sep=""))
  
  hierarchical_results[i, "blmval"] <- blmval[[i]]
  hierarchical_results[i, "total_boundary_length"] <- sol_boundary
  hierarchical_results[i, "total_cost"] <- sol_cost
  
  ## lets create a list (one element per depth) with a copy of the solution
  ## values that correspond to the planning units at each depth
  sol_data <-
    lapply(seq_along(features_depth), function(i) {
      x <- pu_sf
      x$id_2d <- seq_len(nrow(x))
      x$id_depth <- names(features_depth)[[i]]
      x <- left_join(
        x,
        pu_data %>% dplyr::select(id_2d, id_depth, solution_1),
        by = c("id_2d", "id_depth")
      )
      x$solution_1
    }) %>%
    setNames(paste0("solution_1_", names(features_depth)))
  
  ## let's now create a version of the 2d planning unit data and insert
  ## a column for each depth, indicating (using 0s and 1s) if the
  ## planning unit was prioritized at a specific depth
  sol_2d_data <-
    pu_sf %>%
    bind_cols(do.call(tibble, sol_data))
  
  save(sol_2d_data, file=paste(path,"/sol30_2d_data.rdata", sep=""))
  
  surface <- data.frame(depth1=sol_2d_data$solution_1_depth_1, depth2=sol_2d_data$solution_1_depth_2, depth3=sol_2d_data$solution_1_depth_3)
  surface$surface <- rowSums(surface[,1:3])
  sol_surface <- nrow(as.data.frame(surface$surface[surface$surface>0]))
  save(sol_surface, file=paste(path,"/sol30_surface.rdata", sep=""))
  
  sol_depth1 <- sol_2d_data %>%
    filter(solution_1_depth_1 == 1)
  plot(sol_depth1[,"solution_1_depth_1"])
  
  sol_depth2 <- sol_2d_data %>%
    filter(solution_1_depth_2 == 1)
  plot(sol_depth2[,"solution_1_depth_2"])
  
  sol_depth3 <- sol_2d_data %>%
    filter(solution_1_depth_3 == 1)
  plot(sol_depth3[,"solution_1_depth_3"])
  
  
  save(sol_depth1, file=paste(path,"/sol30_depth1.rdata", sep=""))
  save(sol_depth2, file=paste(path,"/sol30_depth2.rdata", sep=""))
  save(sol_depth3, file=paste(path,"/sol30_depth3.rdata", sep=""))
  
}


stopCluster(cl)


save(hierarchical_results, file="05_Marine_Spatial_Planning/03_Prioritization/Second_round/Prioritization30/Rdata/hierarchical_results30.rdata")

hierarchical_results$blmval <- c("0", "1e-04", "1e-03", "1e-02", "1e-01", "1", "10", "100")

hierarchical_results$blmval <- as.factor(hierarchical_results$blmval)

# calculate TOPSIS scores
topsis_results <- topsis(
  decision = hierarchical_results %>%
    dplyr::select(total_cost, total_boundary_length) %>%
    as.matrix(),
  weights = c(1, 1),
  impacts = c("-", "-"))

topsis_results <- cbind(hierarchical_results, topsis_results[,2:3])

save(topsis_results, file="05_Marine_Spatial_Planning/03_Prioritization/Second_round/Prioritization30/Rdata/topsis_results30.rdata")         
write.csv(topsis_results, "05_Marine_Spatial_Planning/03_Prioritization/Second_round/Prioritization30/Rdata/topsis_results30.csv", row.names = FALSE)

hierarchical_results$color <- c("black","black","black","black","black","black","#018571","black")

# create plot to visualize trade-offs and show selected candidate prioritization
result_plot <- ggplot(data = hierarchical_results, aes(x = total_boundary_length, y = total_cost))+
                        #geom_line() +
                        geom_point(size = 3, color=hierarchical_results$color) +
                        geom_text_repel(aes(label = blmval),color=hierarchical_results$color,size=3, min.segment.length = 0.2, force = 5) +
                        xlab("Total boundary length of prioritization") +
                        ylab("Total cost of prioritization") +
                        theme_bw()+
                        theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
                              panel.border = element_rect(colour = "black", size=1, fill=NA),
                              legend.title = element_blank())

ggsave(result_plot, file="05_Marine_Spatial_Planning/03_Prioritization/Second_round/Prioritization30/result_plot30.png")
save(result_plot, file="05_Marine_Spatial_Planning/03_Prioritization/Second_round/Prioritization30/Rdata/result_plot30.rdata")
