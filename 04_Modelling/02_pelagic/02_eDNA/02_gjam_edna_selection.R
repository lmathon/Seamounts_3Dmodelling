library(gjam)
library(rsq)
library(tidyverse)
library(ecodist)
library(modelr)


########################################################################################################################################
## Load abundance data
load("03_preliminary_analyses/edna_motus_selected_pelagic.rdata")

edna_motus <- edna_motus[,colSums(edna_motus) > 0]

# load predictors
load("00_metadata/edna_explanatory_variables_pelagic.rdata")

edna_var$Habitat <- as.factor(edna_var$Habitat)
rownames(edna_var) <- edna_var$Station
edna_var$BottomDepth <- as.numeric(edna_var$BottomDepth)
edna_var$SamplingDepth <- as.numeric(edna_var$Sampling_Depth)


######################################################################################################################################
# Habitat
formula <- as.formula(~ Habitat)

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC =  -1226.631
## sensitivity Habitat = 195
## pearson r = 0.28

######################################################################################################################################
# Latitude
formula <- as.formula(~ Latitude)

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -1344.214
## sensitivity Latitude = 167
## pearson r = 0.32

# Latitude^2
formula <- as.formula(~ Latitude + I(Latitude^2))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -1436.607
## sensitivity Latitude^2 =500 
## pearson r = 0.30


# Latitude^3
formula <- as.formula(~ Latitude + I(Latitude^3))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -960.9698
## sensitivity Latitude^3 1000 
## pearson r = 0.31


######################################################################################################################################
# Salinity
formula <- as.formula(~ Salinity)

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -1338.687
## sensitivity Salinity = 201
## pearson r = 0.36

# Salinity^2
formula <- as.formula(~ Salinity + I(Salinity^2))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -1573.933
## sensitivity Salinity^2 346
## pearson r = 0.33


# Salinity^3
formula <- as.formula(~ Salinity + I(Salinity^3))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -1047.86
## sensitivity Salinity^3 lower 
## pearson r = 0.36


######################################################################################################################################
# EastwardVelocity
formula <- as.formula(~ EastwardVelocity)

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -357.6025
## sensitivity EastwardVelocity = 118
## pearson r = 0.29

# EastwardVelocity^2
formula <- as.formula(~ EastwardVelocity + I(EastwardVelocity^2))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -1411.815
## sensitivity EastwardVelocity^2 373 
## pearson r = 0.36


# EastwardVelocity^3
formula <- as.formula(~ EastwardVelocity + I(EastwardVelocity^3))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -441.2644
## sensitivity EastwardVelocity^3 lower 
## pearson r = 0.31

######################################################################################################################################
# NorthwardVelocity
formula <- as.formula(~ NorthwardVelocity)

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -572.5725
## sensitivity NorthwardVelocity = 230
## pearson r = 0.3

# NorthwardVelocity^2
formula <- as.formula(~ NorthwardVelocity + I(NorthwardVelocity^2))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -1740.358
## sensitivity NorthwardVelocity^2 436
## pearson r = 0.23


# NorthwardVelocity^3
formula <- as.formula(~ NorthwardVelocity + I(NorthwardVelocity^3))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -1288.365
## sensitivity NorthwardVelocity^3 lower 
## pearson r = 0.28

######################################################################################################################################
# SuspendedParticulateMatter
formula <- as.formula(~ SuspendedParticulateMatter)

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -1018.027
## sensitivity SuspendedParticulateMatter = 107
## pearson r = 0.29

# SuspendedParticulateMatter^2
formula <- as.formula(~ SuspendedParticulateMatter + I(SuspendedParticulateMatter^2))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -13576.39
## sensitivity SuspendedParticulateMatter^2 322
## pearson r = 0.38


# SuspendedParticulateMatter^3
formula <- as.formula(~ SuspendedParticulateMatter + I(SuspendedParticulateMatter^3))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -719.9609
## sensitivity SuspendedParticulateMatter^3 2710 
## pearson r = 0.36

######################################################################################################################################
# SSTmax
formula <- as.formula(~ SSTmax)

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -2386.432
## sensitivity SSTmax = 190
## pearson r = 0.33

# SSTmax^2
formula <- as.formula(~ SSTmax + I(SSTmax^2))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC =  -1078.713
## sensitivity SSTmax^2 = 191
## pearson r = 0.30


# SSTmax^3
formula <- as.formula(~ SSTmax + I(SSTmax^3))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -1508.661
## sensitivity SSTmax^3 353 
## pearson r = 0.34


######################################################################################################################################
# SSTmean
formula <- as.formula(~ SSTmean)

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -1376.386
## sensitivity SSTmean = 165
## pearson r = 0.27

# SSTmean^2
formula <- as.formula(~ SSTmean + I(SSTmean^2))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -1571.306
## sensitivity SSTmean^2 196
## pearson r = 0.30


# SSTmean^3
formula <- as.formula(~ SSTmean + I(SSTmean^3))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC =  -472.3391
## sensitivity SSTmean^3 255 
## pearson r = 0.30

######################################################################################################################################
# seafloorTemp
formula <- as.formula(~ seafloorTemp)

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -2139.329
## sensitivity seafloorTemp = 138
## pearson r = 0.27

# seafloorTemp^2
formula <- as.formula(~ seafloorTemp + I(seafloorTemp^2))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -667.6953
## sensitivity seafloorTemp^2 lower
## pearson r = 0.29


# seafloorTemp^3
formula <- as.formula(~ seafloorTemp + I(seafloorTemp^3))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -1033.369
## sensitivity seafloorTemp^3 317 
## pearson r = 0.30

######################################################################################################################################
# Chla
formula <- as.formula(~ Chla)

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -1454.295
## sensitivity Chla = 250
## pearson r = 0.36

# Chla^2
formula <- as.formula(~ Chla + I(Chla^2))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -1120.172
## sensitivity Chla^2 256
## pearson r = 0.41


# Chla^3
formula <- as.formula(~ Chla + I(Chla^3))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -1064.866
## sensitivity Chla^3 1000 
## pearson r = 0.46

######################################################################################################################################
# SummitAreaKm2
formula <- as.formula(~ SummitAreaKm2)

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 425.96
## sensitivity SummitAreaKm2 = 147
## pearson r = 0.31

# SummitAreaKm2^2
formula <- as.formula(~ SummitAreaKm2 + I(SummitAreaKm2^2))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -635.9086
## sensitivity SummitAreaKm2^2 1100
## pearson r = 0.29


# SummitAreaKm2^3
formula <- as.formula(~ SummitAreaKm2 + I(SummitAreaKm2^3))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -1977.416
## sensitivity SummitAreaKm2^3 lower 
## pearson r = 0.30



######################################################################################################################################
# SummitRugosity
formula <- as.formula(~ SummitRugosity)

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC =  -864.7685
## sensitivity SummitRugosity = 207
## pearson r = 0.29

# SummitRugosity^2
formula <- as.formula(~ SummitRugosity + I(SummitRugosity^2))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC =  -803.8459
## sensitivity SummitRugosity^2 228
## pearson r = 0.29


# SummitRugosity^3
formula <- as.formula(~ SummitRugosity + I(SummitRugosity^3))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -1050.99
## sensitivity SummitRugosity^3 lower 
## pearson r = 0.34



######################################################################################################################################
# BottomDepth
formula <- as.formula(~ BottomDepth)

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -1298.92
## sensitivity BottomDepth = 73
## pearson r = 0.23

# BottomDepth^2
formula <- as.formula(~ BottomDepth + I(BottomDepth^2))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -1341.405
## sensitivity BottomDepth^2 109
## pearson r = 0.26


# BottomDepth^3
formula <- as.formula(~ BottomDepth + I(BottomDepth^3))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -927.5638
## sensitivity BottomDepth^3 lower 
## pearson r = 0.26

######################################################################################################################################
# TravelTime
formula <- as.formula(~ TravelTime)

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -895.3141
## sensitivity TravelTime = 184
## pearson r = 0.38

# TravelTime^2
formula <- as.formula(~ TravelTime + I(TravelTime^2))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -10638.51
## sensitivity TravelTime^2 267
## pearson r = 0.35


# TravelTime^3
formula <- as.formula(~ TravelTime + I(TravelTime^3))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -1474.992
## sensitivity TravelTime^3 lower 
## pearson r = 0.41

######################################################################################################################################
# ReefMinDist
formula <- as.formula(~ ReefMinDist)

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -1321.94
## sensitivity ReefMinDist = 444
## pearson r = 0.29

# ReefMinDist^2
formula <- as.formula(~ ReefMinDist + I(ReefMinDist^2))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -251.7863
## sensitivity ReefMinDist^2 lower
## pearson r = 0.26


# ReefMinDist^3
formula <- as.formula(~ ReefMinDist + I(ReefMinDist^3))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -675.8681
## sensitivity ReefMinDist^3 lower 
## pearson r = 0.28

######################################################################################################################################
# LandMinDist
formula <- as.formula(~ LandMinDist)

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -617.924
## sensitivity LandMinDist = 197
## pearson r = 0.33

# LandMinDist^2
formula <- as.formula(~ LandMinDist + I(LandMinDist^2))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -1606.963
## sensitivity LandMinDist^2 600
## pearson r = 0.42


# LandMinDist^3
formula <- as.formula(~ LandMinDist + I(LandMinDist^3))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = 71.84065
## sensitivity LandMinDist^3 900 
## pearson r = 0.43


######################################################################################################################################
# sampling_depth
formula <- as.formula(~ SamplingDepth)

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -480.5658
## sensitivity sampling_depth = 322
## pearson r = 0.24

# sampling_depth^2
formula <- as.formula(~ SamplingDepth + I(SamplingDepth^2))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC =  -661.6443
## sensitivity sampling_depth^2 lower
## pearson r = 0.28


# sampling_depth^3
formula <- as.formula(~ SamplingDepth + I(SamplingDepth^3))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

gjam$fit$DIC
gjam[["inputs"]][["designTable"]]
gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -1289.12
## sensitivity sampling_depth^3 lower 
## pearson r = 0.25

######################################################################################################################################
# Habitat + Salinity^2 + EastwardVelocity^2 + SuspendedParticulateMatter^3 + NorthwardVelocity^2 + SSTmax^3 + 
# seafloorTemp^3 + Chla^3 + SummitAreaKm2^2 + SummitRugosity^2 + ReefMinDist + SamplingDepth

formula <- as.formula(~ Habitat + Salinity + I(Salinity^2) + EastwardVelocity + I(EastwardVelocity^2) + SuspendedParticulateMatter + 
                        I(SuspendedParticulateMatter^3) + NorthwardVelocity + I(NorthwardVelocity^2) + SSTmax + I(SSTmax^3) + 
                        seafloorTemp + I(seafloorTemp^3) + Chla + I(Chla^3) + SummitAreaKm2 + I(SummitAreaKm2^2) + SummitRugosity + 
                        I(SummitRugosity^2) + ReefMinDist + SamplingDepth)

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

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

## DIC = -1429.837
## pearson r = 0.52

######################################################################################################################################
# Habitat + Salinity^2 + SuspendedParticulateMatter^3 + NorthwardVelocity^2 + SSTmax^3 + 
# seafloorTemp^3 + Chla^3 + SummitAreaKm2^2 + SummitRugosity^2 + ReefMinDist + SamplingDepth

formula <- as.formula(~ Habitat + Salinity + I(Salinity^2) + SuspendedParticulateMatter + 
                        I(SuspendedParticulateMatter^3) + NorthwardVelocity + I(NorthwardVelocity^2) + SSTmax + I(SSTmax^3) + 
                        seafloorTemp + I(seafloorTemp^3) + Chla + I(Chla^3) + SummitAreaKm2 + I(SummitAreaKm2^2) + SummitRugosity + 
                        I(SummitRugosity^2) + ReefMinDist + SamplingDepth)

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

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

## DIC = -1668.836
## pearson r = 0.69

######################################################################################################################################
######################################################################################################################################
# Habitat + Salinity^2 + SuspendedParticulateMatter^3 + NorthwardVelocity^2 + SSTmax^3 + 
# seafloorTemp^3 + Chla^3 + SummitRugosity^2 + ReefMinDist + SamplingDepth

formula <- as.formula(~ Habitat + Salinity + I(Salinity^2) + SuspendedParticulateMatter + 
                        I(SuspendedParticulateMatter^3) + NorthwardVelocity + I(NorthwardVelocity^2) + SSTmax + I(SSTmax^3) + 
                        seafloorTemp + I(seafloorTemp^3) + Chla + I(Chla^3) + SummitRugosity + 
                        I(SummitRugosity^2) + ReefMinDist + SamplingDepth)


types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam1 <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

# outputs model
plotPars = list(GRIDPLOTS=T, SAVEPLOTS = T)
gjamPlot( output = gjam1,  plotPars = plotPars)

gjam1$fit$DIC
gjam1[["inputs"]][["designTable"]]
gjam1[["parameters"]][["sensTable"]]

yobs <- gjam1[["inputs"]][["y"]]
ypred <- gjam1[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs 

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -1598.286
## pearson r = 0.59


######################################################################################################################################
# Habitat + Salinity^2 + SuspendedParticulateMatter^3 + NorthwardVelocity^2 + SSTmax + 
# seafloorTemp^3 + Chla^3 + SummitRugosity^2 + ReefMinDist + SamplingDepth

formula <- as.formula(~ Habitat + Salinity + I(Salinity^2) + SuspendedParticulateMatter + 
                        I(SuspendedParticulateMatter^3) + NorthwardVelocity + I(NorthwardVelocity^2) + SSTmax + 
                        seafloorTemp + I(seafloorTemp^3) + Chla + I(Chla^3) + SummitRugosity + 
                        I(SummitRugosity^2) + ReefMinDist + SamplingDepth)

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 2500, burnin = 500, typeNames = types)

# fit model
gjam2 <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

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

## DIC = -743.2289
## pearson r = 0.58


######################################################################################################################################
# Habitat + Salinity^2 + SuspendedParticulateMatter + NorthwardVelocity^2 + SSTmax + 
# seafloorTemp^3 + Chla^3 + SummitRugosity^2 + ReefMinDist + SamplingDepth + SummitAreaKm2

formula <- as.formula(~ Habitat + Salinity + I(Salinity^2) + SuspendedParticulateMatter + 
                        SummitAreaKm2 + NorthwardVelocity + I(NorthwardVelocity^2) + SSTmax + 
                        seafloorTemp + I(seafloorTemp^3) + Chla + I(Chla^3) + SummitRugosity + 
                        I(SummitRugosity^2) + ReefMinDist + SamplingDepth)

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam3 <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

# outputs model
plotPars = list(GRIDPLOTS=T, SAVEPLOTS = T)
gjamPlot( output = gjam3,  plotPars = plotPars)

gjam3$fit$DIC
gjam3[["inputs"]][["designTable"]]
gjam3[["parameters"]][["sensTable"]]

yobs <- gjam3[["inputs"]][["y"]]
ypred <- gjam3[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs 

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC =  -1916.298
## pearson r = 0.63



######################################################################################################################################
# Habitat + Salinity^2 + SuspendedParticulateMatter + NorthwardVelocity^2 + SSTmax + 
# seafloorTemp^3 + Chla + SummitRugosity^2 + ReefMinDist + SamplingDepth + SummitAreaKm2

formula <- as.formula(~ Habitat + Salinity + I(Salinity^2) + SuspendedParticulateMatter + 
                        SummitAreaKm2 + NorthwardVelocity + I(NorthwardVelocity^2) + SSTmax + 
                        seafloorTemp + I(seafloorTemp^3) + Chla + SummitRugosity + 
                        I(SummitRugosity^2) + ReefMinDist + SamplingDepth)


types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam5 <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

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

## DIC = -2653.518
## pearson r = 0.58

######################################################################################################################################
# Habitat + Salinity^2 + SuspendedParticulateMatter + NorthwardVelocity^2 + SSTmax + 
# seafloorTemp^3 + Chla + SummitRugosity^2 + ReefMinDist + SamplingDepth 

formula <- as.formula(~ Habitat + Salinity + I(Salinity^2) + SuspendedParticulateMatter + 
                        NorthwardVelocity + I(NorthwardVelocity^2) + SSTmax + 
                        seafloorTemp + I(seafloorTemp^3) + Chla + SummitRugosity + 
                        I(SummitRugosity^2) + ReefMinDist + SamplingDepth)


types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 500, burnin = 50, typeNames = types)

# fit model
gjam6 <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

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

## DIC = -2968.317
## pearson r = 0.58

