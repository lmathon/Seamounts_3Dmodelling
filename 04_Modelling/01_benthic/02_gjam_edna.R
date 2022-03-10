library(gjam)
library(tidyverse)


########################################################################################################################################
## Model on abundances
load("02_formating_data/01_Benthic/Rdata/edna_motus_matrix_benthic.rdata")
load("02_formating_data/01_Benthic/Rdata/edna_richness_benthic.rdata")

edna_motus <- edna_motus[,colSums(edna_motus) > 0]

edna_motus <- as.data.frame(gjamTrimY(edna_motus[,1:ncol(edna_motus)], maxCols = 100)$y)

# load predictors
load("00_metadata/edna_explanatory_variables_benthic.rdata")

edna_var$Habitat <- as.factor(edna_var$Habitat)
rownames(edna_var) <- edna_var$Station
edna_var$BottomDepth <- as.numeric(edna_var$BottomDepth)


# define model
formula <- as.formula(~ Habitat+EastwardVelocity+NorthwardVelocity+SSTmean+SSTsd+seafloorTemp+
                        Chla+SummitDepth+ValleyDepth+SummitAreaKm2+BottomDepth+TravelTime) # + ReefMinDist

types <- c(rep('CC', ncol(edna_motus)))

#effMat <- matrix(replicate(ncol(edna_motus), rowSums(edna_motus)), nrow = nrow(edna_motus))
#ef  <- list( columns = 1:ncol(edna_motus), values = effMat)

ml <- list(ng = 2000, burnin = 500, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

# outputs model
plotPars = list(GRIDPLOTS=T, SAVEPLOTS = T)
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
test_df <- test_df %>% filter(BottomDepth < 600)
test_df <- test_df %>%
  mutate(Habitat = case_when(
    Habitat == 4 ~ "DeepSlope",
    Habitat == 1 ~ "Seamount",
    Habitat == 2 ~ "Seamount",
    Habitat == 3 ~ "Seamount"
  ))
# sample 500 rows fo test dataset
test_df <- test_df[sample(1:nrow(test_df), 500), ]

new_df <- test_df %>%
  select(Habitat,EastwardVelocity,NorthwardVelocity,SSTmean,SSTsd,seafloorTemp,
         Chla,SummitDepth,ValleyDepth,SummitAreaKm2,BottomDepth,TravelTime)

new_data1 <- list(xdata=new_df, nsim=50)
# predict on new data 
p1 <- gjamPredict(output = gjam, newdata = new_data1, FULL = TRUE)
predictions <- as.data.frame(p1$sdList$yMu)
predictions <- cbind(predictions, test_df)
save(predictions, file="gjamOutput/predictions.rdata")






