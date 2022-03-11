library(gjam)
library(tidyverse)
library(ecodist)
library(modelr)


########################################################################################################################################
## Load abundance data
load("03_preliminary_analyses/bruvs_species_selected.rdata")


# load predictors
load("00_metadata/bruvs_explanatory_variables.rdata")

bruvs_var$Habitat <- as.factor(bruvs_var$Habitat)
rownames(bruvs_var) <- bruvs_var$Station


######################################################################################################################################
# Full  model
formula <- as.formula(~ Habitat+Salinity +SuspendedParticulateMatter + EastwardVelocity + 
                        NorthwardVelocity + SSTmean + Chla+ BottomDepth + 
                        TravelTime + ReefMinDist)

types <- c(rep('DA', ncol(bruvs_species)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

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

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

plot(df$pred ~ df$obs)

lm <- lm(df$pred ~ df$obs)
rsq(lm)
rmse(lm, df)

## DIC = 20253.97
## xPred pretty good
## pearson r = 0.51
## R² = 0.26
## RMSE = 0.82
## 4 species clusters


######################################################################################################################################
# Full  model + Quadratic
formula <- as.formula(~ Habitat+Salinity +SuspendedParticulateMatter + EastwardVelocity + 
                        NorthwardVelocity + SSTmean + Chla+ BottomDepth + 
                        TravelTime + ReefMinDist + I(Salinity^2) + I(SuspendedParticulateMatter^2) + I(EastwardVelocity^2) + 
                        I(NorthwardVelocity^2) + I(SSTmean^2) + I(Chla^2) + I(BottomDepth^2) + 
                        I(TravelTime^2) + I(ReefMinDist^2))

types <- c(rep('DA', ncol(bruvs_species)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

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

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

plot(df$pred ~ df$obs)

lm <- lm(df$pred ~ df$obs)
rsq(lm)
rmse(lm, df)

## DIC = 20253.97
## xPred pretty good
## pearson r = 0.62
## R² = 0.38
## RMSE = 0.91
## 4 species clusters


######################################################################################################################################
# Full  model + selected Quadratic
formula <- as.formula(~ Habitat+Salinity +SuspendedParticulateMatter + EastwardVelocity + 
                        NorthwardVelocity + SSTmean + Chla+ BottomDepth + 
                        TravelTime + ReefMinDist + I(NorthwardVelocity^2) + I(SSTmean^2) + 
                        I(TravelTime^2))

types <- c(rep('DA', ncol(bruvs_species)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

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

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

plot(df$pred ~ df$obs)

lm <- lm(df$pred ~ df$obs)
rsq(lm)
rmse(lm, df)

## DIC = 20646.54
## xPred pretty good
## pearson r = 0.54
## R² = 0.29
## RMSE = 0.86
## 4 species clusters


######################################################################################################################################
# Full  Full model + Full Quadratic
formula <- as.formula(~ Habitat+Salinity +SuspendedParticulateMatter + EastwardVelocity + seafloorTemp +
                        NorthwardVelocity + SSTmean + Chla+ BottomDepth + SummitDepth + SummitRugosity + SummitAreaKm2 +
                        TravelTime + ReefMinDist + I(NorthwardVelocity^2) + I(SSTmean^2) + I(seafloorTemp^2) +
                        I(TravelTime^2) + I(SummitDepth^2) + I(SummitRugosity^2))

types <- c(rep('DA', ncol(bruvs_species)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

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

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

plot(df$pred ~ df$obs)

lm <- lm(df$pred ~ df$obs)
rsq(lm)
rmse(lm, df)

## DIC = 20000
## xPred pretty good
## pearson r = 0.64
## R² = 0.41
## RMSE = 0.93
## 4 species clusters

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



