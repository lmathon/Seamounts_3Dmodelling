library(gjam)
library(rsq)
library(tidyverse)
library(ecodist)
library(modelr)


########################################################################################################################################
## Load abundance data
load("03_preliminary_analyses/bruvs_species_selected.rdata")

abund <- as.matrix(bruvs_species)
table(abund)

# load predictors
load("00_metadata/bruvs_explanatory_variables.rdata")

bruvs_var$Habitat <- as.factor(bruvs_var$Habitat)
rownames(bruvs_var) <- bruvs_var$Station


######################################################################################################################################
# Best model

formula <- as.formula(~ SSTmax + I(SSTmax^3) + BottomDepth + I(BottomDepth^2) +
                        EastwardVelocity + I(EastwardVelocity^3) +
                        ReefMinDist + I(ReefMinDist^2) + Salinity + I(Salinity^2) +
                        NorthwardVelocity + Habitat + Chla)

types <- c(rep('DA', ncol(bruvs_species)))
ml <- list(ng = 2500, burnin = 500, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species, modelList= ml )

# outputs model
plotPars = list(GRIDPLOTS=T, SAVEPLOTS = T)
gjamPlot( output = gjam,  plotPars = plotPars)

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## Avec ng=2500 et burnin=500 et especes 40
## DIC = 12307.46
## pearson r = 0.68


save(gjam, file="04_Modelling/01_benthic/01_BRUVs/GJAM_Output_bruvs/gjam_model.rdata")



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
  select(Habitat,Salinity,SuspendedParticulateMatter,EastwardVelocity,NorthwardVelocity,SSTmean,Chla,BottomDepth, 
         TravelTime,ReefMinDist)

new_data1 <- list(xdata=new_df, nsim=50)
# predict on new data 
p1 <- gjamPredict(output = gjam, newdata = new_data1, FULL = TRUE)
predictions <- as.data.frame(p1$sdList$yMu)
predictions <- cbind(predictions, test_df)
save(predictions, file="gjamOutput/predictions.rdata")

