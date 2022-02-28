library(gjam)
library(tidyverse)

load("02_formating_data/01_Bottom/Rdata/bruvs_abundance_all.rdata")
load("02_formating_data/01_Bottom/Rdata/bruvs_abundance_chondri.rdata")
load("02_formating_data/01_Bottom/Rdata/bruvs_species_matrix.rdata")

colnames(bruvs_abundance_chondri) <- c("Station", "abundance_chondri", "Logabundance_chondri")

bruvs_data <- left_join(bruvs_abundance_all, bruvs_abundance_chondri)
bruvs_data[is.na(bruvs_data)] <- 0

rownames(bruvs_species) <- bruvs_species$Station

bruvs_species[,-1][bruvs_species[,-1] > 1] <- 1

bruvs_data <- left_join(bruvs_data, bruvs_species)
rownames(bruvs_data) <- bruvs_data$Station
bruvs_data <- bruvs_data[,-1]

load("00_metadata/bruvs_explanatory_variables.rdata")

bruvs_data_log <- bruvs_data[,-c(1,3)]
bruvs_data <- bruvs_data[,-c(2,4)]
bruvs_data <- bruvs_data[,colSums(bruvs_data) > 0]
bruvs_data_log <- bruvs_data_log[,colSums(bruvs_data_log) > 0]

bruvs_var$Habitat <- as.factor(bruvs_var$Habitat)
rownames(bruvs_var) <- bruvs_var$Station

formula <- as.formula(~ Habitat+EastwardVelocity+NorthwardVelocity+Salinity+SuspendedParticulateMatter+
                        SSTmax+SSTmean+SSTmin+SSTsd+seafloorTemp+Chla+SummitDepth+ValleyDepth+SummitAreaKm2+
                        SummitRugosity+BottomDepth)

types <- c('DA','DA', rep('PA', 189))
#S <- 10
#n <- 1500
#ef <- list( columns = 1:S, values = round(runif(n,.5,5),1) )
types <- c('DA','DA', rep('PA', 8))
ml <- list(ng = 2000, burnin = 500, typeNames = types)

gjam <- gjam(formula = formula, xdata = bruvs_var, ydata = bruvs_data[,c(1:10)], modelList= ml )

gjamPlot( output = gjam, plotPars = list(GRIDPLOTS=T) )


load("02_formating_data/00_Prediction_raster/Rdata/df_seamount_islands.rdata")

test_df <- df_seamount_islands
test_df$BottomDepth <- test_df$BottomDepth*(-1)
test_df$SummitDepth <- test_df$SummitDepth*(-1)
test_df$ValleyDepth <- test_df$ValleyDepth*(-1)
test_df$Habitat <- as.factor(test_df$Habitat)
test_df <- test_df %>% select(-Height)
test_df <- test_df %>% filter(!is.na(BottomDepth))
test_df <- test_df %>% filter(!is.na(EastwardVelocity))
test_df <- test_df %>% filter(!is.na(Chla))

newdata <- list(xdata=test_df, nsim=50)
p1 <- gjamPredict(output = gjam, newdata = newdata)
plot(bruvs_data[,types == 'DA'], p1$sdList$yMu[,types == 'DA'],ylab = 'Predicted',cex=.1)
abline(0,1)

