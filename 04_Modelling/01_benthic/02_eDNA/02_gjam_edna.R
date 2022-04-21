library(gjam)
library(tidyverse)
library(terra)


########################################################################################################################################
## Model on abundances
load("03_preliminary_analyses/edna_motus_selected.rdata")

edna_motus <- edna_motus[,colSums(edna_motus) > 0]

# load predictors
load("00_metadata/edna_explanatory_variables_benthic.rdata")

edna_var$Habitat <- as.factor(edna_var$Habitat)
rownames(edna_var) <- edna_var$Station
edna_var$BottomDepth <- as.numeric(edna_var$BottomDepth)


# define model
formula <- as.formula(~ Habitat + Salinity + I(Salinity^3) + SuspendedParticulateMatter + 
                        I(SuspendedParticulateMatter^2) + NorthwardVelocity + 
                        I(NorthwardVelocity^3) + SSTmean + I(SSTmean^2) + 
                        BottomDepth + I(BottomDepth^2) + SummitRugosity + I(SummitRugosity^2))

types <- c(rep('CC', ncol(edna_motus)))
ml <- list(ng = 2500, burnin = 500, typeNames = types)

# fit model
gjam <- gjam(formula = formula, xdata = edna_var, ydata = edna_motus, modelList= ml )

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


save(gjam, file="04_Modelling/01_benthic/02_eDNA/GJAM_Output_edna/gjam_model.rdata")


sens$variable <- rownames(sens)
sens <- sens[-14,]
sens$variable <- gsub("HabitatDeepSlope", "Habitat", sens$variable)
sens$group <- c(1,1,2,2,3,3,4,4,5,5,6,6,7)

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


ggsave(plot_sens, file="04_Modelling/01_benthic/02_eDNA/GJAM_Output_edna/gjamOutput/plot_sens.png", width=6, height = 5)

##########################################################################################
# load new data for predictions
load("04_Modelling/01_benthic/02_eDNA/GJAM_Output_edna/gjam_model.rdata")
load("02_formating_data/00_Prediction_raster/Raster_df_predictions/df_benthic.rdata")

df_benthic <- na.omit(df_benthic)

new_df <- df_benthic %>%
  dplyr::select(x, y, Habitat,Salinity,SuspendedParticulateMatter,NorthwardVelocity,SSTmean,SummitRugosity,BottomDepth, 
         ReefMinDist)

new_df$Habitat <- as.factor(new_df$Habitat)

new_df$`I(Salinity^3)` <- (new_df$Salinity)^3
new_df$`I(BottomDepth^2)` <- (new_df$BottomDepth)^2
new_df$`I(SummitRugosity^2)` <- (new_df$SummitRugosity)^2
new_df$`I(SuspendedParticulateMatter^2)` <- (new_df$SuspendedParticulateMatter)^2
new_df$`I(NorthwardVelocity^3)` <- (new_df$NorthwardVelocity)^3
new_df$`I(SSTmean^2)` <- (new_df$SSTmean)^2


new_data1 <- list(xdata=new_df, nsim=500)

# predict on new data 
p1 <- gjamPredict(output = gjam, newdata = new_data1)
predictions <- as.data.frame(p1$sdList$yMu)
predictions <- cbind(predictions, df_benthic)
save(predictions, file="04_Modelling/01_benthic/02_eDNA/GJAM_Output_edna/predictions.rdata")


df <- predictions[,1:16]
coordinates(df) <- ~x+y
gridded(df) <- TRUE
raster_pred <- stack(df)
names(raster_pred) <- paste("MOTU_", 1:14, sep="")

terra::plot(raster_pred, nc=3, nr=5, axes=FALSE,axis.args=list( cex.axis=0.7))



