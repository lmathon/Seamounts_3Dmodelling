library(gjam)
library(tidyverse)

########################################################################################################################################
## Model on presence-absence
load("02_formating_data/01_Bottom/Rdata/bruvs_species_matrix.rdata")


rownames(bruvs_species) <- bruvs_species$Station

bruvs_species[,-1][bruvs_species[,-1] > 1] <- 1
bruvs_species <- bruvs_species[,-1]
bruvs_species <- bruvs_species[,colSums(bruvs_species) > 0]

bruvs_species <- as.data.frame(gjamTrimY(bruvs_species[,1:ncol(bruvs_species)], maxCols = 50)$y)

# load predictors
load("00_metadata/bruvs_explanatory_variables.rdata")

bruvs_var$Habitat <- as.factor(bruvs_var$Habitat)
rownames(bruvs_var) <- bruvs_var$Station

# define model
formula <- as.formula(~ Habitat+EastwardVelocity+NorthwardVelocity+Salinity+SuspendedParticulateMatter+
                        SSTmax+SSTmean+SSTmin+SSTsd+seafloorTemp+Chla+SummitDepth+ValleyDepth+SummitAreaKm2+
                        SummitRugosity+BottomDepth+TravelTime)

types <- c(rep('PA', 51))
ml <- list(ng = 2000, burnin = 500, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species, modelList= ml )

# outputs model
plotPars = list(GRIDPLOTS=T, SAVEPLOTS = T, outfolder = "04_Modelling/01_bottom/test_gjam_bruvs/")
gjamPlot( output = gjam,  plotPars = plotPars)

gjam$fit$DIC

gjam$parameters$betaMu         # S by M coefficient matrix unstandardized
gjam$parameters$betaSe         # S by M coefficient SE
gjam$parameters$betaStandXmu   # S by M standardized for X
gjam$parameters$betaStandXWmu  # (S-F) by M standardized for W/X, centered factors

gjam$parameters$betaTable        # SM by stats posterior summary
gjam$parameters$betaStandXTable  # SM by stats posterior summary
gjam$parameters$betaStandXWTable # (S-F)M by stats posterior summary

gjam$parameters$sensBeta         # sensitivity to response variables
gjam$parameters$sensTable        # sensitivity to predictor variables

gjam$parameters$sigMu            # S by S covariance matrix omega


# load new data for predictions
load("02_formating_data/00_Prediction_raster/Rdata/df_seamount_islands.rdata")

test_df <- df_seamount_islands
test_df <- test_df %>% select(-Height)
test_df <- test_df %>% filter(BottomDepth < 600)
test_df <- test_df %>%
  mutate(Habitat = case_when(
    Habitat == 4 ~ "DeepSlope",
    Habitat == 1 ~ "Summit50",
    Habitat == 2 ~ "Summit250",
    Habitat == 3 ~ "Summit500"
  ))

new_df <- test_df %>%
  select(Habitat,EastwardVelocity,NorthwardVelocity,Salinity,SuspendedParticulateMatter,
           SSTmax,SSTmean,SSTmin,SSTsd,seafloorTemp,Chla,SummitDepth,ValleyDepth,SummitAreaKm2,
           SummitRugosity,BottomDepth,TravelTime)

new_data1 <- list(xdata=new_df, nsim=50)
# predict on new data (doesn't work for P-A data)
p1 <- gjamPredict(output = gjam, newdata = new_data1, FULL = TRUE)




########################################################################################################################################
## Model on abundances
load("02_formating_data/01_Bottom/Rdata/bruvs_species_matrix.rdata")


rownames(bruvs_species) <- bruvs_species$Station

bruvs_species <- bruvs_species[,-1]
bruvs_species <- bruvs_species[,colSums(bruvs_species) > 0]

bruvs_species <- as.data.frame(gjamTrimY(bruvs_species[,1:ncol(bruvs_species)], maxCols = 50)$y)

# load predictors
load("00_metadata/bruvs_explanatory_variables.rdata")

bruvs_var$Habitat <- as.factor(bruvs_var$Habitat)
rownames(bruvs_var) <- bruvs_var$Station

# define model
formula <- as.formula(~ Habitat+EastwardVelocity+NorthwardVelocity+Salinity+SuspendedParticulateMatter+
                        SSTmax+SSTmean+SSTmin+SSTsd+seafloorTemp+Chla+SummitDepth+ValleyDepth+SummitAreaKm2+
                        SummitRugosity+BottomDepth+TravelTime)

types <- c(rep('DA', 51))
ml <- list(ng = 2000, burnin = 500, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_species, modelList= ml )

# outputs model
plotPars = list(GRIDPLOTS=T, SAVEPLOTS = T, outfolder = "04_Modelling/01_bottom/test_gjam_bruvs/")
gjamPlot( output = gjam,  plotPars = plotPars)

gjam$fit$DIC

gjam$parameters$betaMu         # S by M coefficient matrix unstandardized
gjam$parameters$betaSe         # S by M coefficient SE
gjam$parameters$betaStandXmu   # S by M standardized for X
gjam$parameters$betaStandXWmu  # (S-F) by M standardized for W/X, centered factors

gjam$parameters$betaTable        # SM by stats posterior summary
gjam$parameters$betaStandXTable  # SM by stats posterior summary
gjam$parameters$betaStandXWTable # (S-F)M by stats posterior summary

gjam$parameters$sensBeta         # sensitivity to response variables
gjam$parameters$sensTable        # sensitivity to predictor variables

gjam$parameters$sigMu            # S by S covariance matrix omega


# load new data for predictions
load("02_formating_data/00_Prediction_raster/Rdata/df_seamount_islands.rdata")

test_df <- df_seamount_islands
test_df <- test_df %>% select(-Height)
test_df <- test_df %>% filter(BottomDepth < 600)
test_df <- test_df %>%
  mutate(Habitat = case_when(
    Habitat == 4 ~ "DeepSlope",
    Habitat == 1 ~ "Summit50",
    Habitat == 2 ~ "Summit250",
    Habitat == 3 ~ "Summit500"
  ))

new_df <- test_df %>%
  select(Habitat,EastwardVelocity,NorthwardVelocity,Salinity,SuspendedParticulateMatter,
         SSTmax,SSTmean,SSTmin,SSTsd,seafloorTemp,Chla,SummitDepth,ValleyDepth,SummitAreaKm2,
         SummitRugosity,BottomDepth,TravelTime)

new_data1 <- list(xdata=new_df, nsim=50)
# predict on new data 
p1 <- gjamPredict(output = gjam, newdata = new_data1, FULL = TRUE)
predictions <- as.data.frame(p1$sdList$yMu)
predictions <- cbind(predictions, test_df)
save(predictions, file="gjamOutput/predictions.rdata")






#################################################################################################################################
## Non utilise pour l'instant

load("02_formating_data/01_Bottom/Rdata/bruvs_abundance_all.rdata")
load("02_formating_data/01_Bottom/Rdata/bruvs_abundance_chondri.rdata")

colnames(bruvs_abundance_chondri) <- c("Station", "abundance_chondri", "Logabundance_chondri")

bruvs_ab <- left_join(bruvs_abundance_all, bruvs_abundance_chondri)
bruvs_ab[is.na(bruvs_ab)] <- 0

rownames(bruvs_ab) <- bruvs_ab$Station
bruvs_ab <- bruvs_ab[,-c(1,3,5)]
