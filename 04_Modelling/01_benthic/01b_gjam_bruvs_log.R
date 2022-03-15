library(gjam)
library(rsq)
library(tidyverse)
library(ecodist)
library(modelr)


########################################################################################################################################
## Load abundance data
load("03_preliminary_analyses/bruvs_species_selected.rdata")

bruvs_species_log <- log(bruvs_species+1)

# load predictors
load("00_metadata/bruvs_explanatory_variables.rdata")

bruvs_var$Habitat <- as.factor(bruvs_var$Habitat)
rownames(bruvs_var) <- bruvs_var$Station


######################################################################################################################################
# Habitat
formula <- as.formula(~ Habitat)

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 6394.053
## sensitivity Habitat = 285
## pearson r = 0.42

######################################################################################################################################
# Latitude
formula <- as.formula(~ Latitude)

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 6668.7
## sensitivity Latitude = 365
## pearson r = 0.38

# Latitude^2
formula <- as.formula(~ Latitude + I(Latitude^2))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 5825.167
## sensitivity Latitude^2 lower 
## pearson r = 0.42


# Latitude^3
formula <- as.formula(~ Latitude + I(Latitude^3))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 5904.052
## sensitivity Latitude^3 lower 
## pearson r = 0.43


######################################################################################################################################
# Salinity
formula <- as.formula(~ Salinity)

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 5820.965
## sensitivity Salinity = 664
## pearson r = 0.38

# Salinity^2
formula <- as.formula(~ Salinity + I(Salinity^2))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 6195.742
## sensitivity Salinity^2 = lower 
## pearson r = 0.42


# Salinity^3
formula <- as.formula(~ Salinity + I(Salinity^3))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 5909.615
## sensitivity Salinity^3 lower 
## pearson r = 0.42


######################################################################################################################################
# EastwardVelocity
formula <- as.formula(~ EastwardVelocity)

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 7551.275
## sensitivity EastwardVelocity = 31
## pearson r = 0.29

# EastwardVelocity^2
formula <- as.formula(~ EastwardVelocity + I(EastwardVelocity^2))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 6512.491
## sensitivity EastwardVelocity^2 lower 
## pearson r = 0.31


# EastwardVelocity^3
formula <- as.formula(~ EastwardVelocity + I(EastwardVelocity^3))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 6317.08
## sensitivity EastwardVelocity^3 lower 
## pearson r = 0.33

######################################################################################################################################
# NorthwardVelocity
formula <- as.formula(~ NorthwardVelocity)

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 5987.374
## sensitivity NorthwardVelocity = 424
## pearson r = 0.32

# NorthwardVelocity^2
formula <- as.formula(~ NorthwardVelocity + I(NorthwardVelocity^2))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 6210.621
## sensitivity NorthwardVelocity^2 lower
## pearson r = 0.35


# NorthwardVelocity^3
formula <- as.formula(~ NorthwardVelocity + I(NorthwardVelocity^3))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 6063.029
## sensitivity NorthwardVelocity^3 lower 
## pearson r = 0.33

######################################################################################################################################
# SuspendedParticulateMatter
formula <- as.formula(~ SuspendedParticulateMatter)

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 8082.693
## sensitivity SuspendedParticulateMatter = 14
## pearson r = 0.34

# SuspendedParticulateMatter^2
formula <- as.formula(~ SuspendedParticulateMatter + I(SuspendedParticulateMatter^2))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 5777.513
## sensitivity SuspendedParticulateMatter^2 lower
## pearson r = 0.41


# SuspendedParticulateMatter^3
formula <- as.formula(~ SuspendedParticulateMatter + I(SuspendedParticulateMatter^3))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 6468.289
## sensitivity SuspendedParticulateMatter^3 lower 
## pearson r = 0.39

######################################################################################################################################
# SSTmax
formula <- as.formula(~ SSTmax)

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 7047.115
## sensitivity SSTmax = 141
## pearson r = 0.38

# SSTmax^2
formula <- as.formula(~ SSTmax + I(SSTmax^2))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 6080.79
## sensitivity SSTmax^2 lower
## pearson r = 0.42


# SSTmax^3
formula <- as.formula(~ SSTmax + I(SSTmax^3))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 6483.925
## sensitivity SSTmax^3 lower 
## pearson r = 0.45


######################################################################################################################################
# SSTmean
formula <- as.formula(~ SSTmean)

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 6139.809
## sensitivity SSTmean = 340
## pearson r = 0.38

# SSTmean^2
formula <- as.formula(~ SSTmean + I(SSTmean^2))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 5855.355
## sensitivity SSTmean^2 lower
## pearson r = 0.42


# SSTmean^3
formula <- as.formula(~ SSTmean + I(SSTmean^3))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 6332.498
## sensitivity SSTmean^3 =210 
## pearson r = 0.42

######################################################################################################################################
# seafloorTemp
formula <- as.formula(~ seafloorTemp)

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 6430.718
## sensitivity seafloorTemp = 54
## pearson r = 0.36

# seafloorTemp^2
formula <- as.formula(~ seafloorTemp + I(seafloorTemp^2))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 6368.655
## sensitivity seafloorTemp^2 =449
## pearson r = 0.43


# seafloorTemp^3
formula <- as.formula(~ seafloorTemp + I(seafloorTemp^3))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 6414.726
## sensitivity seafloorTemp^3 lower 
## pearson r = 0.46

######################################################################################################################################
# Chla
formula <- as.formula(~ Chla)

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 6624.885
## sensitivity Chla = 143
## pearson r = 0.32

# Chla^2
formula <- as.formula(~ Chla + I(Chla^2))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 7003.785
## sensitivity Chla^2 lower
## pearson r = 0.38


# Chla^3
formula <- as.formula(~ Chla + I(Chla^3))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 7798.242
## sensitivity Chla^3 lower 
## pearson r = 0.38

######################################################################################################################################
# SummitAreaKm2
formula <- as.formula(~ SummitAreaKm2)

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 5858.375
## sensitivity SummitAreaKm2 = 823
## pearson r = 0.42

# SummitAreaKm2^2
formula <- as.formula(~ SummitAreaKm2 + I(SummitAreaKm2^2))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 6266.68
## sensitivity SummitAreaKm2^2 
## pearson r = 0.47


# SummitAreaKm2^3
formula <- as.formula(~ SummitAreaKm2 + I(SummitAreaKm2^3))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 5948.955
## sensitivity SummitAreaKm2^3 lower 
## pearson r = 0.48



######################################################################################################################################
# SummitRugosity
formula <- as.formula(~ SummitRugosity)

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 6854.276
## sensitivity SummitRugosity = 143
## pearson r = 0.52

# SummitRugosity^2
formula <- as.formula(~ SummitRugosity + I(SummitRugosity^2))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 5495.76
## sensitivity SummitRugosity^2 lower
## pearson r = 0.57


# SummitRugosity^3
formula <- as.formula(~ SummitRugosity + I(SummitRugosity^3))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 5448.507
## sensitivity SummitRugosity^3 lower 
## pearson r = 0.56



######################################################################################################################################
# BottomDepth
formula <- as.formula(~ BottomDepth)

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 5968.486
## sensitivity BottomDepth = 202
## pearson r = 0.50

# BottomDepth^2
formula <- as.formula(~ BottomDepth + I(BottomDepth^2))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 5896.764
## sensitivity BottomDepth^2 lower
## pearson r = 0.60


# BottomDepth^3
formula <- as.formula(~ BottomDepth + I(BottomDepth^3))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 5299.623
## sensitivity BottomDepth^3 lower 
## pearson r = 0.54

######################################################################################################################################
# TravelTime
formula <- as.formula(~ TravelTime)

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 5712.475
## sensitivity TravelTime = 576
## pearson r = 0.34

# TravelTime^2
formula <- as.formula(~ TravelTime + I(TravelTime^2))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 5712.219
## sensitivity TravelTime^2 lower
## pearson r = 0.38


# TravelTime^3
formula <- as.formula(~ TravelTime + I(TravelTime^3))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 6145.323
## sensitivity TravelTime^3 lower 
## pearson r = 0.36

######################################################################################################################################
# ReefMinDist
formula <- as.formula(~ ReefMinDist)

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 6402.199
## sensitivity ReefMinDist = 257
## pearson r = 0.38

# ReefMinDist^2
formula <- as.formula(~ ReefMinDist + I(ReefMinDist^2))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 5958.095
## sensitivity ReefMinDist^2 lower
## pearson r = 0.44


# ReefMinDist^3
formula <- as.formula(~ ReefMinDist + I(ReefMinDist^3))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 6367.166
## sensitivity ReefMinDist^3 lower 
## pearson r = 0.42

######################################################################################################################################
# LandMinDist
formula <- as.formula(~ LandMinDist)

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 6117.579
## sensitivity LandMinDist = 95
## pearson r = 0.34

# LandMinDist^2
formula <- as.formula(~ LandMinDist + I(LandMinDist^2))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 6049.839
## sensitivity LandMinDist^2 lower
## pearson r = 0.42


# LandMinDist^3
formula <- as.formula(~ LandMinDist + I(LandMinDist^3))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 5897.263
## sensitivity LandMinDist^3 lower 
## pearson r = 0.42



######################################################################################################################################
# Latitude + SSTmean + BottomDepth + SummitAreaKm2
formula <- as.formula(~ Latitude + SSTmean + BottomDepth + SummitAreaKm2)

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

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

## DIC = 5224.744
## pearson r = 0.66

######################################################################################################################################
# Latitude^2 + SSTmean^3 + BottomDepth^2 + SummitAreaKm2^2
formula <- as.formula(~ Latitude + I(Latitude^2) + SSTmean + I(SSTmean^3) + BottomDepth + I(BottomDepth^2) +
                        SummitAreaKm2 + I(SummitAreaKm2^2))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

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

## DIC = 3202.51
## pearson r = 0.75

######################################################################################################################################
# Latitude^2 + SSTmean^3 + BottomDepth^2 + SummitAreaKm2^2 + EastwardVelocity^3 + SummitRugosity + LandMinDist^2 
formula <- as.formula(~ Latitude + I(Latitude^2) + SSTmean + I(SSTmean^3) + BottomDepth + I(BottomDepth^2) +
                        SummitAreaKm2 + I(SummitAreaKm2^2) + EastwardVelocity + I(EastwardVelocity^3) +
                        SummitRugosity + LandMinDist + I(LandMinDist^2))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 2500, burnin = 500, typeNames = types)

# fit model
gjam1 <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

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
## DIC = 1846.691
## pearson r = 0.79


######################################################################################################################################
# Latitude^2 + SSTmean^3 + BottomDepth^2 + SummitAreaKm2^2 + EastwardVelocity^3 + SummitRugosity + LandMinDist^2 + Salinity^2 + NorthwardVelocity 
formula <- as.formula(~ Latitude + I(Latitude^2) + SSTmean + I(SSTmean^3) + BottomDepth + I(BottomDepth^2) +
                        SummitAreaKm2 + I(SummitAreaKm2^2) + EastwardVelocity + I(EastwardVelocity^3) +
                        SummitRugosity + LandMinDist + I(LandMinDist^2) + Salinity + I(Salinity^2) +
                        NorthwardVelocity)

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 2500, burnin = 500, typeNames = types)

# fit model
gjam2 <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

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
## DIC = 1336.385
## pearson r = 0.79


######################################################################################################################################
# Latitude^2 + SSTmean^3 + BottomDepth^2 + EastwardVelocity^3 + SummitRugosity + LandMinDist^2 + Salinity^2 + NorthwardVelocity + Habitat
formula <- as.formula(~ Latitude + I(Latitude^2) + SSTmean + I(SSTmean^3) + BottomDepth + I(BottomDepth^2) +
                        EastwardVelocity + I(EastwardVelocity^3) +
                        SummitRugosity + LandMinDist + I(LandMinDist^2) + Salinity + I(Salinity^2) +
                        NorthwardVelocity + Habitat)

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 2500, burnin = 500, typeNames = types)

# fit model
gjam3 <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

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

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 2500, burnin = 500, typeNames = types)

# fit model
gjam4 <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

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
## DIC = 2620.962
## pearson r = 0.79

######################################################################################################################################
# Latitude^2 + SSTmean^3 + BottomDepth^2 + EastwardVelocity^3 + SummitRugosity + LandMinDist^2 + Salinity^2 + NorthwardVelocity + Habitat
# + SuspendedParticulateMatter^2
formula <- as.formula(~ Latitude + I(Latitude^2) + SSTmean + I(SSTmean^3) + BottomDepth + I(BottomDepth^2) +
                        EastwardVelocity + I(EastwardVelocity^3) +
                        SummitRugosity + LandMinDist + I(LandMinDist^2) + Salinity + I(Salinity^2) +
                        NorthwardVelocity + Habitat + SuspendedParticulateMatter + I(SuspendedParticulateMatter^2))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 2500, burnin = 500, typeNames = types)

# fit model
gjam5 <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

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
## DIC = 3320.561
## pearson r = 0.79

######################################################################################################################################
# Latitude^2 + SSTmean^3 + BottomDepth^2 + EastwardVelocity^3 + LandMinDist^2 + Salinity^2 + Habitat + SuspendedParticulateMatter^2 + Chla^2
formula <- as.formula(~ Latitude + I(Latitude^2) + SSTmean + I(SSTmean^3) + BottomDepth + I(BottomDepth^2) +
                        LandMinDist + I(LandMinDist^2) + Salinity + I(Salinity^2) + SuspendedParticulateMatter + I(SuspendedParticulateMatter^2) +
                        EastwardVelocity + I(EastwardVelocity^3) + Habitat + Chla + I(Chla^2))


types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 2500, burnin = 500, typeNames = types)

# fit model
gjam6 <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

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
## DIC = 2995.567
## pearson r = 0.79


######################################################################################################################################
# Latitude^2 + SSTmean^3 + BottomDepth^2 + EastwardVelocity^3 + SummitRugosity + LandMinDist^2 + Salinity^2 + NorthwardVelocity + Habitat
# + SuspendedParticulateMatter^2 + SummitAreaKm2
formula <- as.formula(~ Latitude + I(Latitude^2) + SSTmean + I(SSTmean^3) + BottomDepth + I(BottomDepth^2) +
                        EastwardVelocity + I(EastwardVelocity^3) + SummitAreaKm2 +
                        SummitRugosity + LandMinDist + I(LandMinDist^2) + Salinity + I(Salinity^2) +
                        NorthwardVelocity + Habitat + SuspendedParticulateMatter + I(SuspendedParticulateMatter^2))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 2500, burnin = 500, typeNames = types)

# fit model
gjam7 <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

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
## DIC = 5564.18
## pearson r = 0.80


######################################################################################################################################
# SSTmax^3 + BottomDepth^2 + SummitRugosity^2 + seafloorTemp^3 + SummitAreaKm2^3
formula <- as.formula(~ SSTmax + I(SSTmax^3) + BottomDepth + I(BottomDepth^2) +
                        SummitAreaKm2 + I(SummitAreaKm2^3) + SummitRugosity + I(SummitRugosity^2) +
                        seafloorTemp + I(seafloorTemp^3))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 2500, burnin = 500, typeNames = types)

# fit model
gjam8 <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

# outputs model
plotPars = list(GRIDPLOTS=T, SAVEPLOTS = T)
gjamPlot( output = gjam8,  plotPars = plotPars)

gjam8$fit$DIC
gjam8[["inputs"]][["designTable"]]
gjam8[["parameters"]][["sensTable"]]

yobs <- gjam8[["inputs"]][["y"]]
ypred <- gjam8[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs 

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## Avec ng=2500 et burnin=500 et especes 40
## DIC = 5564.18
## pearson r = 0.74

######################################################################################################################################
# SSTmax^3 + BottomDepth^2 + SummitRugosity^2 + seafloorTemp^3 + SummitAreaKm2 + ReefMinDist^2 + SuspendedParticulateMatter^2 + Habitat +
# Salinity^2
formula <- as.formula(~ SSTmax + I(SSTmax^3) + BottomDepth + I(BottomDepth^2) +
                        SummitAreaKm2 + SummitRugosity + I(SummitRugosity^2) +
                        seafloorTemp + I(seafloorTemp^3) + ReefMinDist + I(ReefMinDist^2) +
                        SuspendedParticulateMatter + I(SuspendedParticulateMatter^2) +
                        Habitat + Salinity + I(Salinity^2))

types <- c(rep('CA', ncol(bruvs_species_log)))
ml <- list(ng = 2500, burnin = 500, typeNames = types)

# fit model
gjam9 <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species_log, modelList= ml )

# outputs model
plotPars = list(GRIDPLOTS=T, SAVEPLOTS = T)
gjamPlot( output = gjam9,  plotPars = plotPars)

gjam9$fit$DIC
gjam9[["inputs"]][["designTable"]]
gjam9[["parameters"]][["sensTable"]]

yobs <- gjam9[["inputs"]][["y"]]
ypred <- gjam9[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs 

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## Avec ng=2500 et burnin=500 et especes 40
## DIC = 2943.193
## pearson r = 0.78

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



