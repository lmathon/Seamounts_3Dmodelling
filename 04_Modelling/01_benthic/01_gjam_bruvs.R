library(gjam)
library(tidyverse)


########################################################################################################################################
## Model on abundances
load("02_formating_data/01_Benthic/Rdata/bruvs_species_matrix.rdata")


rownames(bruvs_species) <- bruvs_species$Station

bruvs_species <- bruvs_species[,-1]
bruvs_species <- bruvs_species[,colSums(bruvs_species) > 0]

bruvs_species <- as.data.frame(gjamTrimY(bruvs_species[,1:ncol(bruvs_species)], maxCols = 50)$y)

# load predictors
load("00_metadata/bruvs_explanatory_variables.rdata")

bruvs_var$Habitat <- as.factor(bruvs_var$Habitat)
rownames(bruvs_var) <- bruvs_var$Station

# define model
formula <- as.formula(~ Habitat+Salinity +SuspendedParticulateMatter + EastwardVelocity + 
                        NorthwardVelocity + SSTmean + seafloorTemp + Chla + SummitDepth + 
                        ValleyDepth + SummitAreaKm2 + SummitRugosity + BottomDepth + 
                        TravelTime + ReefMinDist + LandMinDist)

types <- c(rep('DA', 51))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species, modelList= ml )

# outputs model
plotPars = list(GRIDPLOTS=T, SAVEPLOTS = T)
gjamPlot( output = gjam,  plotPars = plotPars)

gjam$fit$DIC



## DIC = 19460.44
## xPred pretty good
## yPred could be better
## 4 species clusters
## sensitivity high for Habitat, SummitArea, SummitDepth, ValleyDepth, BottomDepth, SSTmean, ReefMinDist, NorthwardVelocity, LandMinDist



formula <- as.formula(~ Habitat + NorthwardVelocity + SSTmean +SummitDepth + 
                        ValleyDepth + SummitAreaKm2 + BottomDepth + 
                        TravelTime + ReefMinDist + LandMinDist)

types <- c(rep('DA', 51))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species, modelList= ml )

# outputs model
plotPars = list(GRIDPLOTS=T, SAVEPLOTS = T)
gjamPlot( output = gjam,  plotPars = plotPars)

gjam$fit$DIC










##########################################################################################
# load new data for predictions
load("02_formating_data/00_Prediction_raster/Rdata/df_seamount_islands.rdata")

test_df <- df_seamount_islands
test_df <- test_df %>% select(-Height)
test_df <- test_df %>% filter(BottomDepth < 600)
test_df <- test_df %>%
  mutate(Habitat = case_when(
    Habitat == 4 ~ "DeepSlope",
    Habitat == 1 ~ "Seamount",
    Habitat == 2 ~ "Seamount",
    Habitat == 3 ~ "Seamount"
  ))

new_df <- test_df %>%
  select(Habitat,EastwardVelocity,NorthwardVelocity,SSTmean,SSTsd,seafloorTemp,
         Chla,SummitDepth,ValleyDepth,SummitAreaKm2,BottomDepth,TravelTime)

new_data1 <- list(xdata=new_df, nsim=50)
# predict on new data 
p1 <- gjamPredict(output = gjam, newdata = new_data1, FULL = TRUE)
predictions <- as.data.frame(p1$sdList$yMu)
predictions <- cbind(predictions, test_df)
save(predictions, file="gjamOutput/predictions.rdata")



