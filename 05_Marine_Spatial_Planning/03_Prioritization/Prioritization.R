library(gurobi)
library(prioritizr)
library(tidyverse)
library(parallel)
library(foreach)
library(doParallel)

load("05_Marine_Spatial_Planning/02_formating_MSP3D_inputs/Rdata/bound_data.rdata")
load("05_Marine_Spatial_Planning/02_formating_MSP3D_inputs/Rdata/pu_data.rdata")
load("05_Marine_Spatial_Planning/02_formating_MSP3D_inputs/Rdata/rij_data.rdata")
load("05_Marine_Spatial_Planning/02_formating_MSP3D_inputs/Rdata/feat_data.rdata")
load("05_Marine_Spatial_Planning/02_formating_MSP3D_inputs/Rdata/features_depth.rdata")
load("05_Marine_Spatial_Planning/02_formating_MSP3D_inputs/Rdata/pu_sf.rdata")



cl = makeCluster(5)
registerDoParallel(cl)



# create the problem
p <-
  problem(pu_data$cost, features = feat_data, rij_matrix = rij_data) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_boundary_penalties(0.00001,0.5,data=bound_data) %>%
  add_binary_decisions()%>%
  #add_contiguity_constraints()%>%
  add_gurobi_solver(gap=0, threads = 4)
print(p)


# generate a solution
s <- solve(p)

# prepare data for plotting
## add solution to planning unit data
pu_data$solution_1 <- c(s)

print(attr(s, "objective"))
print(attr(s, "status"))

#print(eval_target_coverage_summary(p, s[, "solution_1_depth1"]), width = Inf)


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

save(sol_2d_data, file="05_Marine_Spatial_Planning/03_Prioritization/Rdata/sol_2d_data.rdata")


sol_depth1 <- sol_2d_data %>%
  filter(solution_1_depth_1 == 1)
plot(sol_depth1[,"solution_1_depth_1"])

sol_depth2 <- sol_2d_data %>%
  filter(solution_1_depth_2 == 1)
plot(sol_depth2[,"solution_1_depth_2"])

sol_depth3 <- sol_2d_data %>%
  filter(solution_1_depth_3 == 1)
plot(sol_depth3[,"solution_1_depth_3"])



stopCluster(cl)
