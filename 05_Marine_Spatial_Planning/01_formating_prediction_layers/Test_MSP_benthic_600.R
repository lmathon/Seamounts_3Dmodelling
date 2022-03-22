library(prioritizr)
library(gurobi)

raster_benthic <- raster("02_formating_data/00_Prediction_raster/Raster_df_predictions/raster_benthic.tif")
raster_benthic[raster_benthic>1] <- 1

raster_200 <- stack("05_Marine_Spatial_Planning/01_formating_prediction_layers/0-200/raster_0_200.tif")
raster_400 <- stack("05_Marine_Spatial_Planning/01_formating_prediction_layers/200-400/raster_200_400.tif")
raster_600 <- stack("05_Marine_Spatial_Planning/01_formating_prediction_layers/400-600/raster_400_600.tif")


pb <- problem(raster_benthic, raster_600)%>%
  add_min_set_objective() %>%
  add_relative_targets(0.15) %>%
  add_boundary_penalties(penalty = 300, edge_factor = 0.5)%>%
  add_default_solver(gap = 0) %>%
  add_contiguity_constraints()

sol <- solve(pb)


















data(sim_pu_polygons)
head(sim_pu_polygons@data)
spplot(sim_pu_polygons, "cost", main = "Planning unit cost",
       xlim = c(-0.1, 1.1), ylim = c(-0.1, 1.1))


data(sim_features)
plot(sim_features, main = paste("Feature", seq_len(nlayers(sim_features))),
     nr = 2)

p1 <- problem(sim_pu_polygons, features = sim_features,
              cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_relative_targets(0.15) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0)
s1 <- solve(p1)
