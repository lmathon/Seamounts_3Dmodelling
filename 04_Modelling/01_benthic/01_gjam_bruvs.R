library(gjam)
library(rsq)
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
# Habitat
formula <- as.formula(~ Habitat)

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

## DIC = 19419.28
## sensitivity Habitat = 410
## pearson r = 0.30

######################################################################################################################################
# Latitude
formula <- as.formula(~ Latitude)

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

## DIC = 14809.41
## sensitivity Latitude = 591
## pearson r = 0.28

# Latitude^2
formula <- as.formula(~ Latitude + I(Latitude^2))

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

## DIC = 18705.64
## sensitivity Latitude^2 lower 
## pearson r = 0.30


# Latitude^3
formula <- as.formula(~ Latitude + I(Latitude^3))

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

## DIC = 18705.64
## sensitivity Latitude^3 lower 
## pearson r = 0.30


######################################################################################################################################
# Salinity
formula <- as.formula(~ Salinity)

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

## DIC = 19788.6
## sensitivity Salinity = 1100
## pearson r = 0.27

# Salinity^2
formula <- as.formula(~ Salinity + I(Salinity^2))

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

## DIC = 18048.23
## sensitivity Salinity^2 = 1350 (better) 
## pearson r = 0.32


# Salinity^3
formula <- as.formula(~ Salinity + I(Salinity^3))

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

## DIC = 17751.29
## sensitivity Salinity^3 lower 
## pearson r = 0.31


######################################################################################################################################
# EastwardVelocity
formula <- as.formula(~ EastwardVelocity)

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

## DIC = 17710.76
## sensitivity EastwardVelocity = 186
## pearson r = 0.24

# EastwardVelocity^2
formula <- as.formula(~ EastwardVelocity + I(EastwardVelocity^2))

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

## DIC = 19360.27
## sensitivity EastwardVelocity^2 = 986 (better) 
## pearson r = 0.24


# EastwardVelocity^3
formula <- as.formula(~ EastwardVelocity + I(EastwardVelocity^3))

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

## DIC = 17586.84
## sensitivity EastwardVelocity^3 lower 
## pearson r = 0.25

######################################################################################################################################
# NorthwardVelocity
formula <- as.formula(~ NorthwardVelocity)

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

## DIC = 19155.34
## sensitivity NorthwardVelocity = 1570
## pearson r = 0.26

# NorthwardVelocity^2
formula <- as.formula(~ NorthwardVelocity + I(NorthwardVelocity^2))

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

## DIC = 19195.36
## sensitivity NorthwardVelocity^2 lower
## pearson r = 0.28


# NorthwardVelocity^3
formula <- as.formula(~ NorthwardVelocity + I(NorthwardVelocity^3))

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

## DIC = 15959.21
## sensitivity NorthwardVelocity^3 lower 
## pearson r = 0.27

######################################################################################################################################
# SuspendedParticulateMatter
formula <- as.formula(~ SuspendedParticulateMatter)

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

## DIC = 19207.3
## sensitivity SuspendedParticulateMatter = 286
## pearson r = 0.27

# SuspendedParticulateMatter^2
formula <- as.formula(~ SuspendedParticulateMatter + I(SuspendedParticulateMatter^2))

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

## DIC = 21230.18
## sensitivity SuspendedParticulateMatter^2 lower
## pearson r = 0.30


# SuspendedParticulateMatter^3
formula <- as.formula(~ SuspendedParticulateMatter + I(SuspendedParticulateMatter^3))

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

## DIC = 17566.67
## sensitivity SuspendedParticulateMatter^3 lower 
## pearson r = 0.29

######################################################################################################################################
# SSTmax
formula <- as.formula(~ SSTmax)

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

## DIC = 17776.81
## sensitivity SSTmax = 580
## pearson r = 0.27

# SSTmax^2
formula <- as.formula(~ SSTmax + I(SSTmax^2))

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

## DIC = 17505.19
## sensitivity SSTmax^2 lower
## pearson r = 0.31


# SSTmax^3
formula <- as.formula(~ SSTmax + I(SSTmax^3))

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

## DIC = 17604.64
## sensitivity SSTmax^3 lower 
## pearson r = 0.33


######################################################################################################################################
# SSTmean
formula <- as.formula(~ SSTmean)

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

## DIC = 17845.97
## sensitivity SSTmean = 1320
## pearson r = 0.27

# SSTmean^2
formula <- as.formula(~ SSTmean + I(SSTmean^2))

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

## DIC = 16835.67
## sensitivity SSTmean^2 lower
## pearson r = 0.31


# SSTmean^3
formula <- as.formula(~ SSTmean + I(SSTmean^3)+ I(SSTmean^2))

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

## DIC = 21730.15
## sensitivity SSTmean^3 lower 
## pearson r = 0.31

######################################################################################################################################
# seafloorTemp
formula <- as.formula(~ seafloorTemp)

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

## DIC = 18640.33
## sensitivity seafloorTemp = 180
## pearson r = 0.29

# seafloorTemp^2
formula <- as.formula(~ seafloorTemp + I(seafloorTemp^2))

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

## DIC = 18617.24
## sensitivity seafloorTemp^2 =519
## pearson r = 0.31


# seafloorTemp^3
formula <- as.formula(~ seafloorTemp + I(seafloorTemp^3))

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

## DIC = 19459.56
## sensitivity seafloorTemp^3 lower 
## pearson r = 0.35

######################################################################################################################################
# Chla
formula <- as.formula(~ Chla)

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

## DIC = 17620.73
## sensitivity Chla = 426
## pearson r = 0.25

# Chla^2
formula <- as.formula(~ Chla + I(Chla^2))

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

## DIC = 20410.19
## sensitivity Chla^2 lower
## pearson r = 0.30


# Chla^3
formula <- as.formula(~ Chla + I(Chla^3))

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

## DIC = 18039.77
## sensitivity Chla^3 lower 
## pearson r = 0.29

######################################################################################################################################
# SummitAreaKm2
formula <- as.formula(~ SummitAreaKm2)

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

## DIC = 20600.7
## sensitivity SummitAreaKm2 = 1830
## pearson r = 0.29

# SummitAreaKm2^2
formula <- as.formula(~ SummitAreaKm2 + I(SummitAreaKm2^2))

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

## DIC = 16978.42
## sensitivity SummitAreaKm2^2 2070
## pearson r = 0.34


# SummitAreaKm2^3
formula <- as.formula(~ SummitAreaKm2 + I(SummitAreaKm2^3))

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

## DIC = 16131.48
## sensitivity SummitAreaKm2^3 lower 
## pearson r = 0.23













######################################################################################################################################
# SummitRugosity
formula <- as.formula(~ SummitRugosity)

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

## DIC = 18074.49
## sensitivity SummitRugosity = 1890
## pearson r = 0.37

# SummitRugosity^2
formula <- as.formula(~ SummitRugosity + I(SummitRugosity^2))

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

## DIC = 18571.27
## sensitivity SummitRugosity^2 lower
## pearson r = 0.40


# SummitRugosity^3
formula <- as.formula(~ SummitRugosity + I(SummitRugosity^3))

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

## DIC = 16417.58
## sensitivity SummitRugosity^3 lower 
## pearson r = 0.39



######################################################################################################################################
# BottomDepth
formula <- as.formula(~ BottomDepth)

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

## DIC = 18282.87
## sensitivity BottomDepth = 1810
## pearson r = 0.35

# BottomDepth^2
formula <- as.formula(~ BottomDepth + I(BottomDepth^2))

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

## DIC = 18571.27
## sensitivity BottomDepth^2 = 2720
## pearson r = 0.42


# BottomDepth^3
formula <- as.formula(~ BottomDepth + I(BottomDepth^3))

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

## DIC = 15901.19
## sensitivity BottomDepth^3 lower 
## pearson r = 0.37

######################################################################################################################################
# TravelTime
formula <- as.formula(~ TravelTime)

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

## DIC = 20197.17
## sensitivity TravelTime = 1490
## pearson r = 0.28

# TravelTime^2
formula <- as.formula(~ TravelTime + I(TravelTime^2))

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

## DIC = 17774.36
## sensitivity TravelTime^2 lower
## pearson r = 0.29


# TravelTime^3
formula <- as.formula(~ TravelTime + I(TravelTime^3))

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

## DIC = 19662.21
## sensitivity TravelTime^3 lower 
## pearson r = 0.30

######################################################################################################################################
# ReefMinDist
formula <- as.formula(~ ReefMinDist)

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

## DIC = 18224.12
## sensitivity ReefMinDist = 588
## pearson r = 0.29

# ReefMinDist^2
formula <- as.formula(~ ReefMinDist + I(ReefMinDist^2))

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

## DIC = 17171.68
## sensitivity ReefMinDist^2 lower
## pearson r = 0.32


# ReefMinDist^3
formula <- as.formula(~ ReefMinDist + I(ReefMinDist^3))

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

## DIC = 18471.63
## sensitivity ReefMinDist^3 lower 
## pearson r = 0.30

######################################################################################################################################
# LandMinDist
formula <- as.formula(~ LandMinDist)

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

## DIC = 19262.74
## sensitivity LandMinDist = 306
## pearson r = 0.27

# LandMinDist^2
formula <- as.formula(~ LandMinDist + I(LandMinDist^2))

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

## DIC = 18226.84
## sensitivity LandMinDist^2 lower
## pearson r = 0.31


# LandMinDist^3
formula <- as.formula(~ LandMinDist + I(LandMinDist^3))

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

## DIC = 20955.76
## sensitivity LandMinDist^3 lower 
## pearson r = 0.30



######################################################################################################################################
# Latitude + SSTmean + BottomDepth + SummitAreaKm2
formula <- as.formula(~ Latitude + SSTmean + BottomDepth + SummitAreaKm2)

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

## DIC = 28629.99
## pearson r = 0.48

######################################################################################################################################
# Latitude^2 + SSTmean^3 + BottomDepth^2 + SummitAreaKm2^2
formula <- as.formula(~ Latitude + I(Latitude^2) + SSTmean + I(SSTmean^3) + BottomDepth + I(BottomDepth^2) +
                        SummitAreaKm2 + I(SummitAreaKm2^2))

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

## DIC = 14213.76
## pearson r = 0.59

######################################################################################################################################
# Latitude^2 + SSTmean^3 + BottomDepth^2 + SummitAreaKm2^2 + EastwardVelocity^3 + SummitRugosity + LandMinDist^2 
formula <- as.formula(~ Latitude + I(Latitude^2) + SSTmean + I(SSTmean^3) + BottomDepth + I(BottomDepth^2) +
                        SummitAreaKm2 + I(SummitAreaKm2^2) + EastwardVelocity + I(EastwardVelocity^3) +
                        SummitRugosity + LandMinDist + I(LandMinDist^2))

types <- c(rep('DA', ncol(bruvs_species)))
ml <- list(ng = 2500, burnin = 500, typeNames = types)

# fit model
gjam1 <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species, modelList= ml )

# outputs model
plotPars = list(GRIDPLOTS=T, SAVEPLOTS = T)
gjamPlot( output = gjam1,  plotPars = plotPars)

gjam1$fit$DIC
gjam1[["inputs"]][["designTable"]]
gjam1[["parameters"]][["sensTable"]]

yobs <- gjam1[["inputs"]][["y"]]
ypred <- gjam1[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## Avec ng=2500 et burnin=500 et especes 40
## DIC = 15220.6
## pearson r = 0.69


######################################################################################################################################
# Latitude^2 + SSTmean^3 + BottomDepth^2 + SummitAreaKm2^2 + EastwardVelocity^3 + SummitRugosity + LandMinDist^2 + Salinity^2 + NorthwardVelocity 
formula <- as.formula(~ Latitude + I(Latitude^2) + SSTmean + I(SSTmean^3) + BottomDepth + I(BottomDepth^2) +
                        SummitAreaKm2 + I(SummitAreaKm2^2) + EastwardVelocity + I(EastwardVelocity^3) +
                        SummitRugosity + LandMinDist + I(LandMinDist^2) + Salinity + I(Salinity^2) +
                        NorthwardVelocity)

types <- c(rep('DA', ncol(bruvs_species)))
ml <- list(ng = 2500, burnin = 500, typeNames = types)

# fit model
gjam2 <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species, modelList= ml )

# outputs model
plotPars = list(GRIDPLOTS=T, SAVEPLOTS = T)
gjamPlot( output = gjam2,  plotPars = plotPars)

gjam2$fit$DIC
gjam2[["inputs"]][["designTable"]]
gjam2[["parameters"]][["sensTable"]]

yobs <- gjam2[["inputs"]][["y"]]
ypred <- gjam2[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## Avec ng=2500 et burnin=500 et especes 40
## DIC = 12339.86
## pearson r = 0.70


######################################################################################################################################
# Latitude^2 + SSTmean^3 + BottomDepth^2 + EastwardVelocity^3 + SummitRugosity + LandMinDist^2 + Salinity^2 + NorthwardVelocity + Habitat
formula <- as.formula(~ Latitude + I(Latitude^2) + SSTmean + I(SSTmean^3) + BottomDepth + I(BottomDepth^2) +
                        EastwardVelocity + I(EastwardVelocity^3) +
                        SummitRugosity + LandMinDist + I(LandMinDist^2) + Salinity + I(Salinity^2) +
                        NorthwardVelocity + Habitat)

types <- c(rep('DA', ncol(bruvs_species)))
ml <- list(ng = 2500, burnin = 500, typeNames = types)

# fit model
gjam3 <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species, modelList= ml )

# outputs model
plotPars = list(GRIDPLOTS=T, SAVEPLOTS = T)
gjamPlot( output = gjam3,  plotPars = plotPars)

gjam3$fit$DIC
gjam3[["inputs"]][["designTable"]]
gjam3[["parameters"]][["sensTable"]]

yobs <- gjam3[["inputs"]][["y"]]
ypred <- gjam3[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## Avec ng=2500 et burnin=500 et especes 30
## DIC = 20333.31
## pearson r = 0.645

## Avec ng=2500 et burnin=500 et especes 40
## DIC = 13259.01
## pearson r = 0.69



######################################################################################################################################
# Latitude^2 + SSTmean^3 + BottomDepth^2 + EastwardVelocity^3 + SummitRugosity + LandMinDist^2 + Salinity^2 + NorthwardVelocity + Habitat
# + Chla^2
formula <- as.formula(~ Latitude + I(Latitude^2) + SSTmean + I(SSTmean^3) + BottomDepth + I(BottomDepth^2) +
                        EastwardVelocity + I(EastwardVelocity^3) +
                        SummitRugosity + LandMinDist + I(LandMinDist^2) + Salinity + I(Salinity^2) +
                        NorthwardVelocity + Habitat + Chla + I(Chla^2))

types <- c(rep('DA', ncol(bruvs_species)))
ml <- list(ng = 2500, burnin = 500, typeNames = types)

# fit model
gjam4 <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species, modelList= ml )

# outputs model
plotPars = list(GRIDPLOTS=T, SAVEPLOTS = T)
gjamPlot( output = gjam4,  plotPars = plotPars)

gjam4$fit$DIC
gjam4[["inputs"]][["designTable"]]
gjam4[["parameters"]][["sensTable"]]

yobs <- gjam4[["inputs"]][["y"]]
ypred <- gjam4[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## Avec ng=2500 et burnin=500 et especes 40
## DIC = 13976.81
## pearson r = 0.69

######################################################################################################################################
# Latitude^2 + SSTmean^3 + BottomDepth^2 + EastwardVelocity^3 + SummitRugosity + LandMinDist^2 + Salinity^2 + NorthwardVelocity + Habitat
# + SuspendedParticulateMatter^2
formula <- as.formula(~ Latitude + I(Latitude^2) + SSTmean + I(SSTmean^3) + BottomDepth + I(BottomDepth^2) +
                        EastwardVelocity + I(EastwardVelocity^3) +
                        SummitRugosity + LandMinDist + I(LandMinDist^2) + Salinity + I(Salinity^2) +
                        NorthwardVelocity + Habitat + SuspendedParticulateMatter + I(SuspendedParticulateMatter^2))

types <- c(rep('DA', ncol(bruvs_species)))
ml <- list(ng = 2500, burnin = 500, typeNames = types)

# fit model
gjam5 <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species, modelList= ml )

# outputs model
plotPars = list(GRIDPLOTS=T, SAVEPLOTS = T)
gjamPlot( output = gjam5,  plotPars = plotPars)

gjam5$fit$DIC
gjam5[["inputs"]][["designTable"]]
gjam5[["parameters"]][["sensTable"]]

yobs <- gjam5[["inputs"]][["y"]]
ypred <- gjam5[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs 

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## Avec ng=2500 et burnin=500 et especes 40
## DIC = 12821.18
## pearson r = 0.70

######################################################################################################################################
# Latitude^2 + SSTmean^3 + BottomDepth^2 + EastwardVelocity^3 + LandMinDist^2 + Salinity^2 + Habitat + SuspendedParticulateMatter^2 + Chla^2
formula <- as.formula(~ Latitude + I(Latitude^2) + SSTmean + I(SSTmean^3) + BottomDepth + I(BottomDepth^2) +
                        LandMinDist + I(LandMinDist^2) + Salinity + I(Salinity^2) + SuspendedParticulateMatter + I(SuspendedParticulateMatter^2) +
                        EastwardVelocity + I(EastwardVelocity^3) + Habitat + Chla + I(Chla^2))
                        

types <- c(rep('DA', ncol(bruvs_species)))
ml <- list(ng = 2500, burnin = 500, typeNames = types)

# fit model
gjam6 <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species, modelList= ml )

# outputs model
plotPars = list(GRIDPLOTS=T, SAVEPLOTS = T)
gjamPlot( output = gjam6,  plotPars = plotPars)

gjam6$fit$DIC
gjam6[["inputs"]][["designTable"]]
gjam6[["parameters"]][["sensTable"]]

yobs <- gjam6[["inputs"]][["y"]]
ypred <- gjam6[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## Avec ng=2500 et burnin=500 et especes 40
## DIC = 13890.9
## pearson r = 0.68


######################################################################################################################################
# Latitude^2 + SSTmean^3 + BottomDepth^2 + EastwardVelocity^3 + SummitRugosity + LandMinDist^2 + Salinity^2 + NorthwardVelocity + Habitat
# + SuspendedParticulateMatter^2 + SummitAreaKm2
formula <- as.formula(~ Latitude + I(Latitude^2) + SSTmean + I(SSTmean^3) + BottomDepth + I(BottomDepth^2) +
                        EastwardVelocity + I(EastwardVelocity^3) + SummitAreaKm2 +
                        SummitRugosity + LandMinDist + I(LandMinDist^2) + Salinity + I(Salinity^2) +
                        NorthwardVelocity + Habitat + SuspendedParticulateMatter + I(SuspendedParticulateMatter^2))

types <- c(rep('DA', ncol(bruvs_species)))
ml <- list(ng = 2500, burnin = 500, typeNames = types)

# fit model
gjam7 <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species, modelList= ml )

# outputs model
plotPars = list(GRIDPLOTS=T, SAVEPLOTS = T)
gjamPlot( output = gjam7,  plotPars = plotPars)

gjam7$fit$DIC
gjam7[["inputs"]][["designTable"]]
gjam7[["parameters"]][["sensTable"]]

yobs <- gjam7[["inputs"]][["y"]]
ypred <- gjam7[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs 

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## Avec ng=2500 et burnin=500 et especes 40
## DIC = 13179.91
## pearson r = 0.70




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



