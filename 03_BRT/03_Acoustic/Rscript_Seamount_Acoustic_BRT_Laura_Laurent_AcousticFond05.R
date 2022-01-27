# INIT : source du 00_Initialisation ----
rm(list=ls(all=TRUE))


#load libraries
if (!require("readxl")) install.packages("readxl")
if (!require("writexl")) install.packages("writexl")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("jtools")) install.packages("jtools")
if (!require("ggstance")) install.packages("ggstance")

if (!require("rms")) install.packages("rms")
if (!require("fitdistrplus")) install.packages("fitdistrplus")
if (!require("huxtable")) install.packages("huxtable")
if (!require("parallel")) install.packages("parallel")
if (!require("foreach")) install.packages("foreach")
if (!require("doParallel")) install.packages("doParallel")
if (!require("dplyr")) install.packages("dplyr")
if (!require("here")) install.packages("here")

## read acoustic cells
Acoustic= read_excel("Acoustic_Data_All_SiteWithDistanceLandReefTravelTime.xlsx", sheet = "Acoustic")
Acoustic=as.data.frame(Acoustic)


# regardons la donnee
names(Acoustic)
head(Acoustic)
summary(Acoustic) 
### Il y a des na dans les donnees AcousticFond, et du coup la variable a ete lue comme un caractere au lieu d'une variable numerique
## Site, Day et Habitat ont ete lus comme des caracteres, et pas des facteurs

Acoustic$AcousticFond=as.numeric(Acoustic$AcousticFond) # on transforme la variable en numerique
Acoustic$Site=as.factor(Acoustic$Site) # on transforme la variable en facteur
Acoustic$Day=as.factor(Acoustic$Day) # on transforme la variable en facteur
Acoustic$Habitat=as.factor(Acoustic$Habitat) # on transforme la variable en facteur
summary(Acoustic) ## les variables ont le bon format, mais il reste des NA (pour l'acoustique fond lorsqu'il n"y a pas de fond, et pour les valeurs de hauteur de mont etc lorsqu'on est en transit)

# le travel time est en seconde, transformons en heure
Acoustic$TravelTimeHour=Acoustic$TravelTime.seconds / 3600
Acoustic=Acoustic[-c(105)] # remove variable 105 (travel time in second)
summary(Acoustic)

# transformons les distances reef & land en km
Acoustic$ShortestDistanceLand=Acoustic$ShortestDistanceLand/1000
Acoustic$ShortestDistanceReef=Acoustic$ShortestDistanceReef/1000
summary(Acoustic)

# concentrons nous sur la valeur de l'acoustique sur le fond, et donc enlevons les lignes avec NA pour cette variable
# on va ainsi obtenir un jeu de donnees partout ou le fond etait detectable par le ek60 (- de 800m)

Acoustic_fond=Acoustic[is.na(Acoustic$AcousticFond)==FALSE,] # la fonction is.na retourne vrai ou faux en fonction des NA
summary(Acoustic_fond) # on a bien enleve les NA de la variable acoustique fond. mais du coup le facteur habitat a zero valeur pour le niveau transit
Acoustic_fond$Habitat=droplevels(Acoustic_fond$Habitat) # cette fonction enleve les niveaux non-utilises d'un facteur
summary(Acoustic_fond)
head(Acoustic_fond)


### regardons la distribution de la variable qui nous interesse
#hist(Acoustic_fond) si on veut regarder toutes les variables
hist(Acoustic_fond$AcousticFond) 
summary(Acoustic_fond$AcousticFond)
plotdist(Acoustic_fond$AcousticFond, histo = TRUE, demp = TRUE)

# il y a quelques tres grosses valeurs. Des erreurs dans le nettoyage de la donnee brute?
br = seq(0,3000,by=100)
ranges = paste(head(br,-1), br[-1], sep=" - ")
freq   = hist(Acoustic_fond$AcousticFond, breaks=br, include.lowest=TRUE, plot=FALSE)
data.frame(range = ranges, frequency = freq$counts)
# on voit que la vaste majorite des donnees est avec une valeur inferieure a 100. Il me semble que tu mavais dit que sa>80 etait suspect?
#a moins que ct sa>20? bref, la il fait avoir une connaissance
# de la donnee pour decider de ce qu'on enleve car on pense que c une erreur

# j'enleve toutes les lignes ou la valeur acousticfond est superieure a 100

dim(Acoustic_fond) ## 3328 lignes et 104 colonnes avant d'avoir enleve les valeurs >100

Acoustic_fond=Acoustic_fond[Acoustic_fond$AcousticFond < 100,]
dim(Acoustic_fond) # on a donc enleve 64 lignes sur 3328

# regardons a nouveau la distribution des donnees
summary(Acoustic_fond$AcousticFond)
plotdist(Acoustic_fond$AcousticFond, histo = TRUE, demp = TRUE)

# c'est deja mieux. On a une distribution "de poisson" avec un grand nombre de faibles valeurs et un petit nombre de grandes valeur.
# la distribution de poisson est normalement pour des valeurs entieres positives. Nous on a des valeurs decimales positives
# quoi qu'il en soit, nous n'avons clairement pas une distribution gaussienne (normale). Normalisons en appliquant la transformation log (x+1).
# le "+1" est car log(0) est - infini alors que log(0+1) est zero

Acoustic_fond$LogAcousticFond=log(Acoustic_fond$AcousticFond + 1)


# regardons la distribution de cette variable transformee
summary(Acoustic_fond$LogAcousticFond)
plotdist(Acoustic_fond$LogAcousticFond, histo = TRUE, demp = TRUE)

# on voit qu'on a bien normalise

descdist(Acoustic_fond$LogAcousticFond, boot = 1000)

fnorm <- fitdist(Acoustic_fond$LogAcousticFond, "norm")
par(mfrow = c(2, 2))
plot.legend <- c("Normal")
denscomp(list(fnorm), legendtext = plot.legend)
qqcomp(list(fnorm), legendtext = plot.legend)
cdfcomp(list(fnorm), legendtext = plot.legend)
ppcomp(list(fnorm), legendtext = plot.legend)

par(mfrow = c(1, 1))

#rename lat long
colnames(Acoustic_fond)[colnames(Acoustic_fond) == "x"] <- "Longitude"
colnames(Acoustic_fond)[colnames(Acoustic_fond) == "y"] <- "Latitude"

summary(Acoustic_fond)

# Maintenant que le jeu de donnees est pret, analysons l'acoustique du fond en fonction
#des predicteurs environnementaux par BRT (Boosted Regression Trees)


#################
################## BRT AVEC LAURA

####### VOIR LA BIBLIO ICI
###############    https://rspatial.org/raster/sdm/9_sdm_brt.html


############################################

# creer un repertoire de sortie

dir.exists("BRT_Output")
dir.create("BRT_Output")



# charger les fonctions BRT necessaires
source("ScriptR_Functions_BRT_Laura_Laurent.R")


# definir le jeu de donnee, les variables "reponse" (Y) si on voulait analyser plus qu'une variable reponse, et les variables predicteur (X)

myData=Acoustic_fond
myResponse=c("LogAcousticFond")

myPredictor=c("Longitude","Latitude", "Day", "Habitat", "SummitDepth", "Height", "SummitAreasKm2", "SummitRugosity","BottomDepth",
              "SSTmean1k", "SSTmin1k", "SSTmax1k", "SSTsd1k", "ChlorA", "EastwardVelocity", "NorthwardVelocity",
              "Salinity", "SeaFloorPotentialTemperature", "SuspendedParticulateMatter", "ShortestDistanceReef", "ShortestDistanceLand",
              "TravelTimeHour")

myPredictorNumeric=c("Longitude","Latitude","SummitDepth", "Height", "SummitAreasKm2", "SummitRugosity","BottomDepth",
              "SSTmean1k", "SSTmin1k", "SSTmax1k", "SSTsd1k", "ChlorA", "EastwardVelocity", "NorthwardVelocity",
              "Salinity", "SeaFloorPotentialTemperature", "SuspendedParticulateMatter", "ShortestDistanceReef", "ShortestDistanceLand",
              "TravelTimeHour")


#myPredictor=c("SSTmin1k","BottomDepth","ShortestDistanceLand", "ChlorA", "Day", "Habitat")

#myPredictorNumeric=c("SSTmin1k","BottomDepth","ShortestDistanceLand", "ChlorA")




# verifier les corelelations entre predicteurs numeriques
cort = cor(na.omit(myData[,myPredictorNumeric]))
cort

# summitdepth & summit rugosity sont tres correlles (0.92). garder summit depth car couche profondeur pas super pour calcul rug
# sstmean et sstmin sont tres correlles (0.94), sstmean et sstmax aussi (0.87), mais moins de correlation entre sstmin et sstmax (0.76). garder sstmean only?
# chlor A tres correlle a suspended matter (0.92). garder chlorA
# travel time et distance to land tres correlles (0.97)/ garder travel time qui represente l'humain
# je fais le calcul en gardant ces correlations. Nous verrons si necessaire de supprimer certaines variables plus tard



# calcul parallele
# il faut faire bcp de calcul pour un BRT et ces calculs sont paralelisables
# cad qu'il peuvent etre fait independemments par les differents coeurs de l'ordinateur
# il faut donc d'abord savoir combien de coeur est sur la machine
# puis typiquement, on calcule sur n-1 coeur, et on laisse un coeur pour pouvoir encore communiquer avec la machine

cores = detectCores()
cores    # ma machine a 8 coeurs


# Setting the different parameters to combine
tree.complexity = c(1:5)
learning.rate = c(0.01, 0.005, 0.001)
bag.fraction = c(0.5,0.75)

#tree.complexity = c(1:3)
#learning.rate = c(0.01, 0.005)
#bag.fraction = c(0.5)


#Make cluster for parallel processing
cores = detectCores()
cl = makeCluster(cores-1)
registerDoParallel(cl)


#loop on responses for optimisation

#for (responseName in myResponse[1:length(myResponse)]){ # in case there are several response variables
  
responseName=myResponse # in case there is only one response variable
  print(paste("----------------------", responseName))


  ### Optimisation
  
  # define empty output matrix
  output <- matrix(ncol=9)
  
  # Run the function through the for loop to test all parameter combinations
  
  #source("R_BRT_Fonctions/R_Function_optimize_gaussian_brts_Laura_Laurent.R")
  #source("ScriptR_Functions_BRT_Laura_Laurent.R")
  
  par_output =  foreach(i = tree.complexity, .packages=c("foreach")) %dopar% {
    foreach(j = learning.rate, .packages=c("foreach")) %dopar% {
      foreach(k = bag.fraction, .packages=c("foreach")) %dopar% {
        #need to load package within foreach loop
        #devtools::load_all("./R_BRT_Fonctions/") ##### fonction qui charge toutes les fonctions d'un dossier R
        #source("ScriptR_Functions_BRT_Laura_Laurent.R")
        #source("R_BRT_Fonctions/R_Function_optimize_gaussian_brts_Laura_Laurent.R")
        # model name
        nam = paste0("Model_",responseName,"_tc_", i, "_lr_", j, "_bf_", k)
        # model optimization
        t = optimize_gaussian_brts(tree.com = i,learn = j, bag.f = k, myData, responseName, myPredictor)
        # assign results to output matrix
        if(!is.null(t$interaction.depth)){ output = rbind(output, c(nam, unlist(t))) }
        output
      }
    }
  }
  
  
  
  
  # extract best brts parameters
  best_parameters = extract_best_parameters_par(par_output, responseName, "gaussian")
  best_parameters
  
  
  ### Fit best gaussian BRT with fixed nb of trees
  
  # Fit best model
  mod_best_fixed = fit_best_gaussian_brt_fixed(myData, responseName, best_parameters, myPredictor)
  mod_best_fixed
  summary(mod_best_fixed)
  names(mod_best_fixed)
  mod_best_fixed$contributions
  
  # Make plot of variable contributions best fixed model
  make_contribution_reduced_plot(mod_best_fixed, responseName, "gaussian")
  
  
  # Get variables with contributions > 5%
  var_sup5_best_fixed = get_variables_contrib_sup5(mod_best_fixed)
  
  
  ### Fit best gaussian BRT REDUCED (same as brt)
  
  # Refit after dropping predictors with contributions < 5%
  mod_best_fixed_reduced = fit_best_reduced_gaussian_brt_fixed(myData, responseName, best_parameters,
                                                   preds = var_sup5_best_fixed)
  mod_best_fixed_reduced
  summary(mod_best_fixed_reduced)
  names(mod_best_fixed_reduced)
  mod_best_fixed_reduced$contributions
  mod_best_fixed_reduced$var.names
  
  # Make plot of variable contributions reduced model
  make_contribution_reduced_plot(mod_best_fixed_reduced, responseName, "gaussian")
  
  # Partial dependance plots reduced model
  partial_dependance_plots3(mod_best_fixed_reduced, responseName, "gaussian")
  
  # Refit a gbmStep after dropping predictors with contributions < 5%
  mod_best_gbmStep_reduced = fit_best_reduced_gaussian_brt_gbmStep(myData, responseName, best_parameters,
                                                               preds = var_sup5_best_fixed)
  
  names(mod_best_gbmStep_reduced)
  mod_best_gbmStep_reduced$cv.statistics
  mod_best_gbmStep_reduced$shrinkage
  mod_best_gbmStep_reduced$n.trees
  mod_best_gbmStep_reduced$contributions
  mod_best_fixed_reduced$contributions

  
  #explore interactions for best reduced model (must be a gbmStep - dont work with gbmfixed)
  find.int <- dismo::gbm.interactions(mod_best_gbmStep_reduced)
  find.int$interactions
  find.int$rank.list
  
  # the greatest interaction (7.90) is between var 9 (SSTsd1k) and var 1 (BottomDepth)
  # the second greatest 6.92 is between var 6 (SSTsd1k) and var 1 (BottomDepth)
  # etc
  
  #dev.new()
  
  png(paste0("BRT_Output/", "InteractionPlotsBestModel.png"), width = 1200, height = 600)
  
  par(mfrow=c(1,2))
  dismo::gbm.perspec(mod_best_gbmStep_reduced, 5, 2, z.range=c(0,2))
  dismo::gbm.perspec(mod_best_gbmStep_reduced, 1, 3, z.range=c(1,3.5))
  
  dev.off()
  
  par(mfrow=c(1,1))
  
  #Stop cluster
  stopCluster(cl)
  
  # attention, il y a 2 packages, gbm et dismo, et avec les memes fonctions. utiliser gbm:: et dismo:: pour choisir
  # exemple
  
  ?dismo::gbm.plot
  
  png(paste0("BRT_Output/", "InteractionPlotsBestModelDismo.png"), width = 1200, height = 600)
  
  dismo::gbm.plot(mod_best_gbmStep_reduced, n.plots=5, plot.layout=c(3, 2), write.title = FALSE)
  
  dev.off()
  
  ?gbm::plot.gbm
  
  gbm::plot.gbm(mod_best_gbmStep_reduced, i.var=c(1))
  gbm::plot.gbm(mod_best_gbmStep_reduced, i.var=c(1,6),level.plot=FALSE)
  
  
  
  #########
  ######### LA SUIT EST SI ON VEUT PREDIRE SUR DES CARTES, PAS ENCORE LE CAS
  ######## JE DEPLACE DONC LA FIN DU CALCUL PARALLE ICI
  ######### IDEM DEPLACEMENT DE LA BOUCLE EVENTUELLE
  
  
  ### Predict REDUCED BRT on study area
  
  # Predict based on reduced model
  pred_fish = predict_brt(mod_fish_reduced, "uvcs", "gaussian", sp,
                          preds = var_sup5_fish, rast)
  
  # Map prediction
  map_brt_prediction(pred_fish, sp, "gaussian")
  
  map_brt_prediction_exp_transf(pred_fish, sp, "gaussian")
  
  map_brt_prediction_quantile_cols(pred_fish, sp, "gaussian")
  
 # } ### BOUCLE de FOR


#Stop cluster
stopCluster(cl)



###########
############
#######



descdist(Acoustic_fond$AcousticFond, boot = 1000)

# a priori on est vers une distribution gamma, mais log normal et weibull peuvent aussi (beta est pour des valeurs entre 0 et 1)
# ca correspond bien au blog https://stats.stackexchange.com/questions/190763/how-to-decide-which-glm-family-to-use
# "if you are dealing with continuous non-negative outcome, then you could consider the Gamma distribution, or Inverse Gaussian distribution."

fw <- fitdist(Acoustic_fond$AcousticFond, "weibull")
fg <- fitdist(Acoustic_fond$AcousticFond, "gamma")
fln <- fitdist(Acoustic_fond$AcousticFond, "lnorm")

par(mfrow = c(2, 2))
plot.legend <- c("Weibull", "lognormal", "gamma")
denscomp(list(fw, fln, fg), legendtext = plot.legend)
qqcomp(list(fw, fln, fg), legendtext = plot.legend)
cdfcomp(list(fw, fln, fg), legendtext = plot.legend)
ppcomp(list(fw, fln, fg), legendtext = plot.legend)


# j'avoue etre en limite de mes connaissances pour le choix de la distrubution. la lognormal est bien sur le graphe pp et les densite theoriques, le gamma sur qqplot


###
names(Acoustic_fond)
glm.variables= glm(AcousticFond ~ Day+Habitat+SummitDepth+ValleyDepth+
                     Height+SummitAreasKm2+SummitRugosity+
                     +BottomDepth+SSTmean1k + SSTmin1k+SSTmax1k+
                     SSTsd1k+ChlorA+EastwardVelocity+NorthwardVelocity+Salinity+SeaFloorPotentialTemperature+
                     SuspendedParticulateMatter+ShortestDistanceReef+ShortestDistanceLand, family=Gamma, data=Acoustic_fond)
anova(glm.variables)
summary(glm.variables) 

# on enleve valley depth car la hauteur c'est la difference entre sommet et vallee
glm.variables.gamma= glm(AcousticFond ~ Day+Habitat+SummitDepth+
                           Height+SummitAreasKm2+SummitRugosity+
                           +BottomDepth+SSTmean1k + SSTmin1k+SSTmax1k+
                           SSTsd1k+ChlorA+EastwardVelocity+NorthwardVelocity+Salinity+SeaFloorPotentialTemperature+
                           SuspendedParticulateMatter+ShortestDistanceReef+ShortestDistanceLand, family=Gamma, data=Acoustic_fond)
anova(glm.variables.gamma)
summary(glm.variables.gamma) 

glm.variables.lnor= glm(AcousticFond ~ Day+Habitat+SummitDepth+
                          Height+SummitAreasKm2+SummitRugosity+
                          +BottomDepth+SSTmean1k + SSTmin1k+SSTmax1k+
                          SSTsd1k+ChlorA+EastwardVelocity+NorthwardVelocity+Salinity+SeaFloorPotentialTemperature+
                          SuspendedParticulateMatter+ShortestDistanceReef+ShortestDistanceLand, family=gaussian(link="log"), data=Acoustic_fond)
anova(glm.variables.lnor)
summary(glm.variables.lnor) 


# le AIC du gamma est plus petit que le AIC du lognormal, a priori le gamma est mieux
AIC(glm.variables.lnor) 
AIC(glm.variables.gamma) 

# aide a interpretation
# https://cran.r-project.org/web/packages/jtools/vignettes/summ.html

#gamma
summary(glm.variables.gamma) 
summ(glm.variables.gamma)
summ(glm.variables.gamma, confint = TRUE, digits = 3)
summ(glm.variables.gamma, confint = TRUE, ci.width = .5)
summ(glm.variables.gamma, confint = TRUE, pvals = FALSE)

effect_plot(glm.variables.gamma, pred = SummitDepth, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables.gamma, pred = Height, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables.gamma, pred = SummitRugosity, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables.gamma, pred = BottomDepth, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables.gamma, pred = SSTmin1k, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables.gamma, pred = SSTsd1k, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables.gamma, pred = EastwardVelocity, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables.gamma, pred = NorthwardVelocity, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables.gamma, pred = Salinity, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables.gamma, pred = SuspendedParticulateMatter, interval = TRUE, plot.points = TRUE)
effect_plot(glm.variables.gamma, pred = ShortestDistanceLand, interval = TRUE, plot.points = TRUE)
plot_summs(glm.variables.gamma, scale = TRUE, inner_ci_level = .9) # ne marche pas


###lognormal
summary(glm.variables.lnor) 
summ(glm.variables.lnor)
summ(glm.variables.lnor, confint = TRUE, digits = 3)
summ(glm.variables.lnor, confint = TRUE, ci.width = .5)
summ(glm.variables.lnor, confint = TRUE, pvals = FALSE)

plot_summs(glm.variables.lnor, scale = TRUE, inner_ci_level = .9)

effect_plot(glm.variables.lnor, pred = Day, interval = TRUE, plot.points = TRUE) ## effet positif
effect_plot(glm.variables.lnor, pred = SummitDepth, interval = TRUE, plot.points = TRUE) ## effet positif
effect_plot(glm.variables.lnor, pred = Height, interval = TRUE, plot.points = TRUE) ##positif
effect_plot(glm.variables.lnor, pred = SummitAreasKm2, interval = TRUE, plot.points = TRUE) ## effet positif
effect_plot(glm.variables.lnor, pred = SummitRugosity , interval = TRUE, plot.points = TRUE) ## effet negatif
effect_plot(glm.variables.lnor, pred = BottomDepth, interval = TRUE, plot.points = TRUE) ##negatif
effect_plot(glm.variables.lnor, pred = SSTmin1k, interval = TRUE, plot.points = TRUE) ##negatif
effect_plot(glm.variables.lnor, pred = SSTmax1k, interval = TRUE, plot.points = TRUE) ## negatif
effect_plot(glm.variables.lnor, pred = SSTsd1k, interval = TRUE, plot.points = TRUE) ## positif
effect_plot(glm.variables.lnor, pred = EastwardVelocity, interval = TRUE, plot.points = TRUE) ##positif
effect_plot(glm.variables.lnor, pred = NorthwardVelocity, interval = TRUE, plot.points = TRUE) ##positif
effect_plot(glm.variables.lnor, pred = Salinity, interval = TRUE, plot.points = TRUE) ## negatif
effect_plot(glm.variables.lnor, pred = SeaFloorPotentialTemperature, interval = TRUE, plot.points = TRUE) ## positif
effect_plot(glm.variables.lnor, pred = SuspendedParticulateMatter, interval = TRUE, plot.points = TRUE) ## positif
effect_plot(glm.variables.lnor, pred = ShortestDistanceLand, interval = TRUE, plot.points = TRUE) ## negatif



export_summs(glm.variables.lnor, scale = TRUE)






###################
#################
###############




ess= glm(AcousticFond ~ Day*Habitat*SummitDepth+
                          Height*SummitAreasKm2*SummitRugosity*
           BottomDepth*SSTmean1k * SSTmin1k+SSTmax1k*
                          SSTsd1k*ChlorA*EastwardVelocity*NorthwardVelocity*Salinity*SeaFloorPotentialTemperature*
                          SuspendedParticulateMatter*ShortestDistanceReef*ShortestDistanceLand, family=gaussian(link="log"), data=Acoustic_fond)
anova(ess)
summary(ess) 










?boxplot

## boxplot on a formula:
#boxplot(count ~ spray, data = InsectSprays, col = "lightgray")

boxplot(AcousticSurface ~ Habitat, data = Acoustic, col = "lightgray",na.rm=TRUE)
boxplot(AcousticSurface ~ Habitat, data = Acoustic, col = "lightgray",na.rm=TRUE,ylim=c(0,20))


boxplot(AcousticFond ~ Habitat, data = Acoustic, col = "lightgray",na.rm=TRUE)
boxplot(AcousticFond ~ Habitat, data = Acoustic, col = "lightgray",na.rm=TRUE,ylim=c(0,35))## il y a pas mal de tres grosses valeurs. peut etre faire le menage?
boxplot(AcousticFond ~ Site, data = Acoustic, col = "lightgray",na.rm=TRUE,ylim=c(0,100))## eponge semble tres variable par rapport aux autres sites
boxplot(AcousticFond ~ Day, data = Acoustic, col = "lightgray",na.rm=TRUE,ylim=c(0,35))


## With less effort (slightly different) using factor *interaction*:
#boxplot(len ~ dose:supp, data = ToothGrowth,
#        boxwex = 0.5, col = c("orange", "yellow"),
 #       main = "Guinea Pigs' Tooth Growth",
#        xlab = "Vitamin C dose mg", ylab = "tooth length",
 #       sep = ":", lex.order = TRUE, ylim = c(0, 35), yaxs = "i")


boxplot(AcousticFond ~ Site:Habitat, data = Acoustic, col = "lightgray",na.rm=TRUE,ylim=c(0,220))## dur a voir les labels, rotation avec les graphes de la biblio ggplot



ggplot(Acoustic, aes(Site, AcousticFond)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#un peu plus joli
ggplot(Acoustic, aes(Site, AcousticFond)) + geom_jitter(colour="lightgrey") + geom_boxplot(outlier.shape=NA, fill=NA)  +
  scale_y_continuous(limits=c(0,50)) +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("Sa fond")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))

# tu peux stocker dans un objet pour faire du multiplot plus tard
mongraphe1=ggplot(Acoustic, aes(Site, AcousticFond)) + geom_jitter(colour="lightgrey") + geom_boxplot(outlier.shape=NA, fill=NA)  +
  scale_y_continuous(limits=c(0,50)) +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("Sa fond")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))

mongraphe1


ggplot(Acoustic, aes(Habitat, AcousticFond)) + geom_jitter(colour="lightgrey") + geom_boxplot(outlier.shape=NA, fill=NA)  +
  scale_y_continuous(limits=c(0,50)) +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("Sa fond")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))


# ici on enleve les transitsbelow2500m 
ggplot(Acoustic[Acoustic$Habitat!="TransitBelow2500m",], aes(Habitat, AcousticFond)) + geom_jitter(colour="lightgrey") + geom_boxplot(outlier.shape=NA, fill=NA)  +
  scale_y_continuous(limits=c(0,50)) +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("Sa fond")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))


ggplot(aes(y = AcousticFond, x = Site, fill = Habitat), data = Acoustic[Acoustic$Habitat!="TransitBelow2500m",]) +
  geom_jitter(colour="lightgrey") +
  geom_boxplot(outlier.shape=NA) +
  scale_y_continuous(limits=c(0,50)) +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("Sa fond") +
  theme(axis.text.x=element_text(angle=45, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))


## un graphe a trois facteurs, site x habitat x moment_journee
ggplot(aes(y = AcousticFond, x = Site, fill = Habitat), data = Acoustic[Acoustic$Habitat!="TransitBelow2500m",]) +
  geom_jitter(colour="lightgrey") +
  geom_boxplot(outlier.shape=NA) +
  scale_y_continuous(limits=c(0,50)) +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("Sa fond") +
  theme(axis.text.x=element_text(angle=45, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))+
  facet_wrap(~as.factor(Day), nrow=2)



## anova a 1 facteur

anova.habitat= aov(AcousticFond ~ Habitat, data=Acoustic[Acoustic$Habitat!="TransitBelow2500m",]) 
summary(anova.habitat) # ce resultat dit qu'il n'y a pas de differences statistiquement significative entre pente et sommet (sans tenir compte des autres facteurs)

ggplot(Acoustic[Acoustic$Habitat!="TransitBelow2500m",], aes(Habitat, AcousticFond)) + geom_jitter(colour="lightgrey") + geom_boxplot(outlier.shape=NA, fill=NA)  +
  scale_y_continuous(limits=c(0,50)) +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("Sa fond")+
  theme(axis.text.x=element_text(angle=90, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))


## anova a 2 facteurs

anova.Site.habitat= aov(AcousticFond ~ Site*Habitat, data=Acoustic[Acoustic$Habitat!="TransitBelow2500m",]) 
summary(anova.Site.habitat) # cette analyse de variance a 2 facteurs dit que l'interaction site x habitat n'est pas significative, que le signal n'est pas significativement different entre pente/sommet, mais qu'il y a des differences significatives entre sites

ggplot(aes(y = AcousticFond, x = Site, fill = Habitat), data = Acoustic[Acoustic$Habitat!="TransitBelow2500m",]) +
  geom_jitter(colour="lightgrey") +
  geom_boxplot(outlier.shape=NA) +
  scale_y_continuous(limits=c(0,50)) +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("Sa fond") +
  theme(axis.text.x=element_text(angle=45, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))

ggplot(aes(y = AcousticFond, x = Site), data = Acoustic[Acoustic$Habitat!="TransitBelow2500m",]) +
  geom_jitter(colour="lightgrey") +
  geom_boxplot(outlier.shape=NA) +
  scale_y_continuous(limits=c(0,50)) +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("Sa fond") +
  theme(axis.text.x=element_text(angle=45, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))

# post-hoc pairwise comparison de Tukey pour determiner quel site est significativement different de quel autre.
# on voit par exemple que Crypthelia et Antigonia sont significativement differents (probabibilite ajustee de 0.007)
TukeyHSD(anova.Site.habitat, "Site")



# la meme analyse avec une transformation log. Cette fois tout est significatif: il faut regarder l'interaction
anova.Site.habitat= aov(log(AcousticFond+1) ~ Site*Habitat, data=Acoustic[Acoustic$Habitat!="TransitBelow2500m",]) 
summary(anova.Site.habitat) # cette anlyse de variance a 2 facteurs dit que l'interaction site x habitat n'est pas significative, que le signal n'est pas significativement different entre pente/sommet, mais qu'il y a des differences significatives entre sites


ggplot(aes(y = log(AcousticFond+1), x = Site, fill=Habitat), data = Acoustic[Acoustic$Habitat!="TransitBelow2500m",]) +
  geom_jitter(colour="lightgrey") +
  geom_boxplot(outlier.shape=NA) +
  scale_y_continuous(limits=c(0,7.5)) +
  theme(panel.background=element_rect(fill=NA, color="black")) +
  xlab("Site") + ylab("Sa fond") +
  theme(axis.text.x=element_text(angle=45, vjust=0.5),axis.title=element_text(size=14,face="bold"), axis.text=element_text(size=12))

TukeyHSD(anova.Site.habitat, "Site:Habitat")


####### GLM


glm.Site.habitat= glm(AcousticFond ~ Site*Habitat, family=gaussian, data=Acoustic[Acoustic$Habitat!="TransitBelow2500m",])

glm.Site.habitat
anova(glm.Site.habitat)
summary(glm.Site.habitat)

##
glm.Site.habitat= glm(AcousticFond ~ Site+Habitat, family=gaussian, data=Acoustic[Acoustic$Habitat!="TransitBelow2500m",])

glm.Site.habitat
anova(glm.Site.habitat)
summary(glm.Site.habitat)

##
glm.Site.habitat.profondeur= glm(AcousticFond ~ Site+Habitat+BottomDepth, family=gaussian, data=Acoustic[Acoustic$Habitat!="TransitBelow2500m",])

glm.Site.habitat.profondeur
anova(glm.Site.habitat.profondeur)
summary(glm.Site.habitat.profondeur)

anova(glm.Site.habitat,glm.Site.habitat.profondeur)

AIC(glm.Site.habitat.profondeur)
AIC(glm.Site.habitat)


###
names(Acoustic)
glm.variables= glm(AcousticFond ~ Habitat+Day+SummitDepth+ValleyDepth+BottomDepth+SummitAreasKm2+SSTmean1k, family=gaussian, data=Acoustic[Acoustic$Habitat!="TransitBelow2500m",])
anova(glm.variables)
summary(glm.variables)  ### je te laisse ajouter toutes les variables et analyser celles qui sont significatives, et faire les graphes qui vont bien

# un glm avec une famille gaussienne = pareil qu'un lm
lm.variables= lm(AcousticFond ~ Habitat+Day+SummitDepth+ValleyDepth+BottomDepth+SummitAreasKm2+SSTmean1k, data=Acoustic[Acoustic$Habitat!="TransitBelow2500m",])
anova(lm.variables)
summary(lm.variables)  ### je te laisse ajouter toutes les variables et analyser celles qui sont significatives, et faire les graphes qui vont bien


# aide a interpretation
# https://cran.r-project.org/web/packages/jtools/vignettes/summ.html

summ(lm.variables)
summ(glm.variables)

summ(lm.variables, confint = TRUE, digits = 3)
summ(lm.variables, confint = TRUE, ci.width = .5)
summ(lm.variables, confint = TRUE, pvals = FALSE)

effect_plot(lm.variables, pred = BottomDepth, interval = TRUE, plot.points = TRUE)

?effect_plot


lm.variables.log= lm(log(AcousticFond+1) ~ Habitat+Day+SummitDepth+ValleyDepth+BottomDepth+SummitAreasKm2+SSTmean1k, data=Acoustic[Acoustic$Habitat!="TransitBelow2500m",])
anova(lm.variables.log)
summary(lm.variables.log)  ### je te laisse ajouter toutes les variables et analyser celles qui sont significatives, et faire les graphes qui vont bien
summ(lm.variables.log)
summ(lm.variables.log, confint = TRUE, digits = 3)
effect_plot(lm.variables.log, pred = BottomDepth, interval = TRUE, plot.points = TRUE)

summ(lm.variables)
plot_summs(lm.variables)
plot_summs(lm.variables, scale = TRUE)
plot_summs(lm.variables, scale = TRUE, inner_ci_level = .9)

plot_summs(lm.variables.log, scale = TRUE, inner_ci_level = .9)


Acoustic$AcousticFondLog=log(Acoustic$AcousticFond+1)

lm.variables.log= lm(AcousticFondLog ~ Habitat+Day+SummitDepth+ValleyDepth+BottomDepth+SummitAreasKm2+SSTmean1k, data=Acoustic[Acoustic$Habitat!="TransitBelow2500m",])
plot_summs(lm.variables.log, scale = TRUE, inner_ci_level = .9)
plot_summs(lm.variables, scale = TRUE, inner_ci_level = .9)
plot_summs(lm.variables,lm.variables.log, scale = TRUE, inner_ci_level = .9)


plot_summs(lm.variables, scale = TRUE, inner_ci_level = .9)
plot_summs(lm.variables.log, scale = TRUE, inner_ci_level = .9)

summ(lm.variables, confint = TRUE, digits = 3)
summ(lm.variables.log, confint = TRUE, digits = 3)

effect_plot(lm.variables.log, pred = HabitatSummit, interval = TRUE, plot.points = TRUE)

?effect_plot
#transformation variable
#https://rcompanion.org/handbook/I_12.html


summ(lm.variables.log, confint = TRUE, digits = 3)


###https://www.r-bloggers.com/2016/07/introduction-to-the-rms-package/
##https://rdrr.io/cran/rms/man/plot.Predict.html

ols.variables.log= ols(AcousticFondLog ~ Habitat+Day+SummitDepth+ValleyDepth+BottomDepth+SummitAreasKm2+SSTmean1k, data=Acoustic[Acoustic$Habitat!="TransitBelow2500m",], x=TRUE, y=TRUE)
ols.variables.log
summary(ols.variables.log)
anova(ols.variables.log)
plot(anova(ols.variables.log), what='proportion chisq')
plot(Predict(ols.variables.log))
vif(ols.variables.log) # test for multicolinearity
Predict(ols.variables.log)
an <- anova(ols.variables.log)
plot(Predict(ols.variables.log), anova=an, pval=TRUE)

ddist <- datadist(Acoustic$Habitat,Day,SummitDepth,ValleyDepth,BottomDepth,SummitAreasKm2+SSTmean1k)
options(datadist='ddist')

datadist(Acoustic$Habitat)


#######https://rdrr.io/cran/rms/man/plot.Predict.html
# a creuser car exemple interessant mais jarrive pas a le faire marcher
Acoustic2=Acoustic[Acoustic$Habitat!="TransitBelow2500m",]
Acoustic2$AcousticFondLog=log(Acoustic2$AcousticFond+1)
Acoustic2$Habitat=as.factor(Acoustic2$Habitat)
Acoustic2$Day=as.factor(Acoustic2$Day)

ddist <- datadist(Acoustic$Habitat,Acoustic$Day,Acoustic$SummitDepth,Acoustic$ValleyDepth,Acoustic$BottomDepth,Acoustic$SummitAreasKm2+Acoustic$SSTmean1k)
options(datadist='ddist')

ols.variables.log= ols(AcousticFondLog ~ Habitat+Day+SummitDepth+ValleyDepth+BottomDepth+SummitAreasKm2+SSTmean1k, data=Acoustic2, x=TRUE, y=TRUE)
summary(ols.variables.log)

anova(ols.variables.log)
plot(anova(ols.variables.log), what='aic')
plot(anova(ols.variables.log), what='')
plot(anova(ols.variables.log), what='partial R2')
plot(anova(ols.variables.log), what='proportion R2')
plot(anova(ols.variables.log), what='proportion chisq')
plot(Predict(ols.variables.log))




