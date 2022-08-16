library(gjam)
library(rsq)
library(tidyverse)
library(ecodist)
library(modelr)


########################################################################################################################################
## Load abundance data
load("03_preliminary_analyses/02_Species_MOTU_selection/edna_motus_selected.rdata")

edna_motus <- edna_motus[,colSums(edna_motus) > 0]

# load predictors
load("00_metadata/edna_explanatory_variables_benthic.rdata")

edna_var$Habitat <- as.factor(edna_var$Habitat)
rownames(edna_var) <- edna_var$Station
edna_var$BottomDepth <- as.numeric(edna_var$BottomDepth)


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

## DIC =  -10598.6
## sensitivity Habitat = 1090
## pearson r = 0.26

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

## DIC = -8844.494
## sensitivity Latitude = 463
## pearson r = 0.24

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

## DIC = -11676.84
## sensitivity Latitude^2 =620 
## pearson r = 0.27


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

## DIC = -9212.617
## sensitivity Latitude^3 lower 
## pearson r = 0.26


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

## DIC = -10128.14
## sensitivity Salinity = 368
## pearson r = 0.29

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

## DIC = -11382.58
## sensitivity Salinity^2 lower
## pearson r = 0.31


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

## DIC = -13100.69
## sensitivity Salinity^3 lower 
## pearson r = 0.44


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

## DIC = -10290.85
## sensitivity EastwardVelocity = 231
## pearson r = 0.32

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

## DIC = -11727.01
## sensitivity EastwardVelocity^2 lower 
## pearson r = 0.37


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

## DIC = -11921.67
## sensitivity EastwardVelocity^3 lower 
## pearson r = 0.32

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

## DIC = -11556.22
## sensitivity NorthwardVelocity = 196
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

## DIC = -10661.43
## sensitivity NorthwardVelocity^2 lower
## pearson r = 0.32


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

## DIC = -13257.35
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

## DIC = -11027.33
## sensitivity SuspendedParticulateMatter = 350
## pearson r = 0.26

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
## sensitivity SuspendedParticulateMatter^2 lower
## pearson r = 0.28


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

## DIC = 17566.67
## sensitivity SuspendedParticulateMatter^3 lower 
## pearson r = 0.25

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

## DIC = -11932.28
## sensitivity SSTmax = 227
## pearson r = 0.3

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

## DIC = -12699.94
## sensitivity SSTmax^2 = 641
## pearson r = 0.32


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

## DIC = -12439.13
## sensitivity SSTmax^3 lower 
## pearson r = 0.33


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

## DIC = -12089.95
## sensitivity SSTmean = 383
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

## DIC = -12088.73
## sensitivity SSTmean^2 lower
## pearson r = 0.26


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

## DIC = -11068.49
## sensitivity SSTmean^3 lower 
## pearson r = 0.33

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

## DIC = -9386.451
## sensitivity seafloorTemp = 365
## pearson r = 0.23

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

## DIC = -11139.92
## sensitivity seafloorTemp^2 =541
## pearson r = 0.30


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

## DIC = -10315.14
## sensitivity seafloorTemp^3 lower 
## pearson r = 0.25

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

## DIC = -13568.08
## sensitivity Chla = 267
## pearson r = 0.26

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

## DIC = -10510.56
## sensitivity Chla^2 lower
## pearson r = 0.26


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

## DIC = -14375.17
## sensitivity Chla^3 lower 
## pearson r = 0.28

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

## DIC = -13008.32
## sensitivity SummitAreaKm2 = 398
## pearson r = 0.26

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

## DIC = -10093.8
## sensitivity SummitAreaKm2^2 1540
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

## DIC = -12217.17
## sensitivity SummitAreaKm2^3 lower 
## pearson r = 0.27



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

## DIC =  -10546.3
## sensitivity SummitRugosity = 305
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

## DIC = -11670.3
## sensitivity SummitRugosity^2 lower
## pearson r = 0.39


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

## DIC = -11907.45
## sensitivity SummitRugosity^3 lower 
## pearson r = 0.33



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

## DIC = -12325.84
## sensitivity BottomDepth = 408
## pearson r = 0.33

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

## DIC = -11581.19
## sensitivity BottomDepth^2 lower
## pearson r = 0.36


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

## DIC = -10058.31
## sensitivity BottomDepth^3 lower 
## pearson r = 0.31

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

## DIC =  -11107.42
## sensitivity TravelTime = 450
## pearson r = 0.27

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
## sensitivity TravelTime^2 lower
## pearson r = 0.25


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

## DIC = -10534.31
## sensitivity TravelTime^3 lower 
## pearson r = 0.23

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

## DIC = -11795.41
## sensitivity ReefMinDist = 325
## pearson r = 0.27

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

## DIC = -12622.75
## sensitivity ReefMinDist^2 lower
## pearson r = 0.3


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

## DIC = -12753.71
## sensitivity ReefMinDist^3 lower 
## pearson r = 0.23

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

## DIC = -11421.37
## sensitivity LandMinDist = 176
## pearson r = 0.27

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

## DIC = -10913.32
## sensitivity LandMinDist^2 lower
## pearson r = 0.27


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

## DIC = -11659.17
## sensitivity LandMinDist^3 lower 
## pearson r = 0.25



######################################################################################################################################
# Habitat + Salinity^3 + SuspendedParticulateMatter^2 + NorthwardVelocity^3

formula <- as.formula(~ Habitat + Salinity + I(Salinity^3) + SuspendedParticulateMatter + I(SuspendedParticulateMatter^2) +
                        NorthwardVelocity + I(NorthwardVelocity^3))

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

## DIC = -11494.31
## pearson r = 0.48

######################################################################################################################################
# Habitat + Salinity^3 + SuspendedParticulateMatter^2 + NorthwardVelocity^3 + SSTmax^2 + BottomDepth

formula <- as.formula(~ Habitat + Salinity + I(Salinity^3) + SuspendedParticulateMatter + I(SuspendedParticulateMatter^2) +
                        NorthwardVelocity + I(NorthwardVelocity^3) + SSTmax + I(SSTmax^2) + BottomDepth)

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

## DIC = -13659.14
## pearson r = 0.52

######################################################################################################################################
# Habitat + Salinity^3 + SuspendedParticulateMatter^2 + NorthwardVelocity^3 + SSTmax^2 + BottomDepth^2

formula <- as.formula(~ Habitat + Salinity + I(Salinity^3) + SuspendedParticulateMatter + I(SuspendedParticulateMatter^2) +
                        NorthwardVelocity + I(NorthwardVelocity^3) + SSTmax + I(SSTmax^2) + BottomDepth + I(BottomDepth^2))


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

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -12803.93
## pearson r = 0.53


######################################################################################################################################
# Habitat + Salinity^3 + SuspendedParticulateMatter^2 + NorthwardVelocity^3 + SSTmax^2 + BottomDepth^2 + SummitRugosity^2

formula <- as.formula(~ Habitat + Salinity + I(Salinity^3) + SuspendedParticulateMatter + I(SuspendedParticulateMatter^2) +
                        NorthwardVelocity + I(NorthwardVelocity^3) + SSTmean + I(SSTmean^2) + BottomDepth + I(BottomDepth^2) +
                        SummitRugosity + I(SummitRugosity^2))

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

## DIC = -15506.66
## pearson r = 0.58


######################################################################################################################################
# Habitat + Salinity^3 + SuspendedParticulateMatter^2 + NorthwardVelocity^3 + SSTmax^2 + BottomDepth^2 + SummitRugosity^2 + seafloorTemp

formula <- as.formula(~ Habitat + Salinity + I(Salinity^3) + SuspendedParticulateMatter + I(SuspendedParticulateMatter^2) +
                        NorthwardVelocity + I(NorthwardVelocity^3) + SSTmean + I(SSTmean^2) + BottomDepth + I(BottomDepth^2) +
                        SummitRugosity + I(SummitRugosity^2) + seafloorTemp)

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

# compute correlation between ypred and yobs (removing "other")

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## DIC = -12888.02
## pearson r = 0.54



######################################################################################################################################
# Habitat + Salinity^3 + SuspendedParticulateMatter^2 + NorthwardVelocity^3 + SSTmax^2 + BottomDepth^2 + SummitRugosity^2 + ReefMinDist^2

formula <- as.formula(~ Habitat + Salinity + I(Salinity^3) + SuspendedParticulateMatter + I(SuspendedParticulateMatter^2) +
                        NorthwardVelocity + I(NorthwardVelocity^3) + SSTmean + I(SSTmean^2) + BottomDepth + I(BottomDepth^2) +
                        SummitRugosity + I(SummitRugosity^2) + ReefMinDist + I(ReefMinDist^2))

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

## DIC = -13817.42
## pearson r = 0.55

######################################################################################################################################
# Habitat + Salinity^3 + SuspendedParticulateMatter^2 + NorthwardVelocity^3 + SSTmax^2 + BottomDepth^2 + SummitRugosity^2 + ReefMinDist^2 + seafloorTemp^2

formula <- as.formula(~ Habitat + Salinity + I(Salinity^3) + SuspendedParticulateMatter + I(SuspendedParticulateMatter^2) +
                        NorthwardVelocity + I(NorthwardVelocity^3) + SSTmean + I(SSTmean^2) + BottomDepth + I(BottomDepth^2) +
                        SummitRugosity + I(SummitRugosity^2) + ReefMinDist + I(ReefMinDist^2) + seafloorTemp +
                        I(seafloorTemp^2))


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

## DIC = -13273.19
## pearson r = 0.56

######################################################################################################################################
#all variables simple
formula <- as.formula(~ Habitat + SSTmax + Chla + ReefMinDist + BottomDepth + EastwardVelocity + Salinity +  
                        SSTmean + SummitDepth + SummitAreaKm2 + NorthwardVelocity + seafloorTemp + SuspendedParticulateMatter +
                        SummitRugosity + TravelTime + LandMinDist)

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 2500, burnin = 500, typeNames = types)

# fit model
gjam4 <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

# outputs model
plotPars = list(GRIDPLOTS=T, SAVEPLOTS = T)
gjamPlot( output = gjam4,  plotPars = plotPars)

gjam4$fit$DIC
gjam4[["inputs"]][["designTable"]]
gjam4[["parameters"]][["sensTable"]]

yobs <- gjam4[["inputs"]][["y"]]
ypred <- gjam4[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## Avec ng=2500 et burnin=500 et especes 40
## DIC = 20574.24
## pearson r = 0.61