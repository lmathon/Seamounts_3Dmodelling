library(gjam)
library(tidyverse)
library(raster)
library(terra)


########################################################################################################################################
## Model on abundances
load("03_preliminary_analyses/02_Species_MOTU_selection/edna_motus_selected_pelagic.rdata")

edna_motus <- edna_motus[,colSums(edna_motus) > 0]

# load predictors
load("00_metadata/edna_explanatory_variables_pelagic.rdata")

edna_var$Habitat <- as.factor(edna_var$Habitat)
rownames(edna_var) <- edna_var$Station
edna_var$BottomDepth <- as.numeric(edna_var$BottomDepth)
edna_var$Sampling_Depth <- as.numeric(edna_var$Sampling_Depth)
edna_var$SamplingDepth <- edna_var$Sampling_Depth


# define model
formula <- as.formula(~ Habitat + Salinity + I(Salinity^2) + SuspendedParticulateMatter + SummitAreaKm2 +
                        NorthwardVelocity + I(NorthwardVelocity^2) + EastwardVelocity + SSTmean + 
                        seafloorTemp + I(seafloorTemp^3) + SummitRugosity + 
                        I(SummitRugosity^2) + BottomDepth + ReefMinDist + TravelTime + SamplingDepth)

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 2500, burnin = 500, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

# outputs model
plotPars = list(GRIDPLOTS=T, SAVEPLOTS = T, ncluster=2)
gjamPlot( output = gjam,  plotPars = plotPars)

gjam$fit$DIC

gjam[["inputs"]][["designTable"]]
sens <- gjam[["parameters"]][["sensTable"]]

yobs <- gjam[["inputs"]][["y"]]
ypred <- gjam[["prediction"]][["ypredMu"]]

# compute correlation between ypred and yobs 

df <- data.frame(obs=as.vector(yobs), pred=as.vector(ypred))
cor.test(df$pred, df$obs, method = "pearson")


save(gjam, file="04_Modelling/02_pelagic/02_eDNA/02_GJAM_eDNA/GJAM_Output_edna/gjam_model.rdata")


sens$variable <- rownames(sens)
sens <- sens[-18,]
sens$variable <- gsub("HabitatDeepSlope", "Habitat", sens$variable)
sens$group <- c(1,1,2,3,4,4,5,6,7,7,8,8,9,10,11,12,13)

total <- sens %>%
  dplyr::select(group, Estimate) %>%
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


ggsave(plot_sens, file="04_Modelling/02_pelagic/02_eDNA/02_GJAM_eDNA/GJAM_Output_edna/gjamOutput/plot_sens.png", width=5, height = 7)

##########################################################################################
# load new data for predictions
load("04_Modelling/02_pelagic/02_eDNA/02_GJAM_eDNA/GJAM_Output_edna/gjam_model.rdata")
load("02_formating_data/00_Prediction_raster/Raster_df_predictions/df_pelagic.rdata")

df_pelagic <- df_pelagic %>%
  filter(!is.na(Sampling_Depth))

df_pelagic <- na.omit(df_pelagic)
df_pelagic$SamplingDepth <- df_pelagic$Sampling_Depth

new_df <- df_pelagic %>%
  dplyr::select(x, y, Habitat,Salinity,SuspendedParticulateMatter,NorthwardVelocity,SSTmean,SummitRugosity,SamplingDepth, 
         ReefMinDist, seafloorTemp, TravelTime, SummitAreaKm2, BottomDepth, EastwardVelocity)

new_df$Habitat <- as.factor(new_df$Habitat)
new_df$SamplingDepth <- as.numeric(new_df$SamplingDepth)

new_df$`I(Salinity^2)` <- (new_df$Salinity)^2
new_df$`I(SummitRugosity^2)` <- (new_df$SummitRugosity)^2
new_df$`I(seafloorTemp^3)` <- (new_df$seafloorTemp)^3
new_df$`I(NorthwardVelocity^2)` <- (new_df$NorthwardVelocity)^2


new_data1 <- list(xdata=new_df, nsim=500)

# predict on new data 
p1 <- gjamPredict(output = gjam, newdata = new_data1)
predictions <- as.data.frame(p1$sdList$yMu)
predictions <- cbind(predictions, df_pelagic)
save(predictions, file="04_Modelling/02_pelagic/02_eDNA/02_GJAM_eDNA/GJAM_Output_edna/predictions.rdata")


predict <- predictions[,c(1:12,34)]

for (i in 1:10) {
  df <- predict[,c(i,11,12,13)]
  colnames(df)<- c("motu", "x", "y", "SamplingDepth")
  df <- spread(df, key = "SamplingDepth", value="motu")
  
  coordinates(df) <- ~x+y
  gridded(df) <- TRUE
  df <- stack(df)
  
  df <- rast(df)
  terra::writeRaster(df,filename = paste0("04_Modelling/02_pelagic/02_eDNA/02_GJAM_eDNA/GJAM_Output_edna/raster_predict_pelagic_motu", i, ".tif"))
  
}




