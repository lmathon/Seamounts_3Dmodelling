library(gjam)
library(rsq)
library(tidyverse)
library(ecodist)
library(modelr)
library(raster)


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
sens <- gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")

## Avec ng=2500 et burnin=500 et especes 40
## DIC = 12307.46
## pearson r = 0.68


save(gjam, file="04_Modelling/01_benthic/01_BRUVs/GJAM_Output_bruvs/gjam_model.rdata")

sens$variable <- rownames(sens)
sens <- sens[-14,]
sens$variable <- gsub("HabitatDeepSlope", "Habitat", sens$variable)
sens$group <- c(1,1,2,2,3,3,4,4,5,5,6,7,8)

total <- sens %>%
  select(group, Estimate) %>%
  group_by(group) %>%
  summarise_all(funs(sum)) 
names(total) <- c("group", "total")
sens_tot <- left_join(total, sens[,c("group", "variable")])
sens_tot <- sens_tot %>% distinct(group, .keep_all=T)

plot_sens <- ggplot(sens_tot, aes(reorder(variable, total), total))+
  geom_bar(stat = "identity", width = 0.5)+
  ylab("Sensitivity") +
  xlab("") +
  theme_bw()+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank())+
  coord_flip()


ggsave(plot_sens, file="04_Modelling/01_benthic/01_BRUVs/GJAM_Output_bruvs/gjamOutput/plot_sens.png", width=6, height = 5)



##########################################################################################
# load new data for predictions
load("02_formating_data/00_Prediction_raster/Raster_df_predictions/df_benthic.rdata")

df_benthic <- na.omit(df_benthic)

new_df <- df_benthic %>%
  select(x, y, Habitat,Salinity,EastwardVelocity,NorthwardVelocity,SSTmax,Chla,BottomDepth, 
         ReefMinDist)

new_df$Habitat <- as.factor(new_df$Habitat)

new_df$`I(Salinity^2)` <- (new_df$Salinity)^2
new_df$`I(BottomDepth^2)` <- (new_df$BottomDepth)^2
new_df$`I(ReefMinDist^2)` <- (new_df$ReefMinDist)^2
new_df$`I(EastwardVelocity^3)` <- (new_df$EastwardVelocity)^3
new_df$`I(SSTmax^3)` <- (new_df$SSTmax)^3


new_data1 <- list(xdata=new_df, nsim=500)

# predict on new data 
p1 <- gjamPredict(output = gjam, newdata = new_data1)
predictions <- as.data.frame(p1$sdList$yMu)
predictions <- cbind(predictions, df_benthic)
save(predictions, file="04_Modelling/01_benthic/01_BRUVs/GJAM_Output_bruvs/predictions.rdata")


raster_predict_abundance <- predictions[,1:17]

coordinates(raster_predict_abundance) <- ~x+y
gridded(raster_predict_abundance) <- TRUE
raster_predict_abundance <- stack(raster_predict_abundance)

plot(raster_predict_abundance)
