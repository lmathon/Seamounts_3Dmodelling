# Seamounts_3Dmodelling

Code to analyse eDNA using MOTUs (Teleo), BRUVS and acoustic data, model diversity on seamounts and compute 3D marine spatial planning.  

### 00_metadata
 
The [00_metadata](00_metadata/) folder contains the field metadata for each eDNA sample : sample code, date, station, site, province, country, project, sample type, volume, depth of sampling, habitat and coordinates. 

This folder contains the formated explanatory variables for each dataset (BRUVS, eDNA benthic, eDNA pelagic, acoustic benthic and acoustic pelagic), that will be used in the statistical analyses.

The folder [environmental](00_metadata/environmental) contains all the shapefiles and source information for each environmental variable extracted. 


### 01_Raw_data

This folder contains the raw BRUVS and acoustic data, and the raw explanatory variables for each dataset.


#### Clean_eDNA

This folder contains the scripts used to read eDNA raw bioinformatic outputs and apply cleaning filters. 

The folder [data_MOTUs/NouvelleCaledonie](01_Raw_data/data_MOTUs/NouvelleCaledonie) contains two types of files:  
1. one \*teleo_table_motu.csv containing the occurences of each MOTU - output of *SWARM*
2. one \*teleo_ecotag_ncbi_motu.csv file containing the taxonomical assignation of each MOTU - output of *ecotag*

These files are available for samples belonging to the project, and sometimes for blank samples (empty wells on the same sequencing plate) and other samples (sequenced in the same batch as samples from the project).

The sequencing metadata file is stored in the [data_MOTUs/NouvelleCaledonie/metadata](01_Raw_data/data_MOTUs/NouvelleCaledonie/metadata) folder. This file contains the association of each sample name (SPYGEN identification) to each sequencing run. This file is necessary to apply some quality filters.

The folder [LULU](01_Raw_data/LULU) contains the outputs of the LULU post-clustering curation algorithm.

The folder [Rdata](01_Raw_data/Rdata) contains the rdata outputs from the cleaning scripts, and the final eDNA dataset.


#### Summit_computation

This folder contains the scripts used to compute summit area and rugosity for each seamount sampled.



### 02_formating_data


#### 00_Prediction_raster

This folder contains the scripts used for the construction of the final prediction layer that will be used to predict pelagic and benthic diversity, and further compute marine spatial planning.

In these scripts, we compute and assemble all metadata associated with seamounts and deep slopes on which we will predict biodiversity; we select the cells we want to keep for predictions; and we compute distance to coast and reef and travel times.

The final predictions shapefiles and dataframes are in [Raster_df_predictions](02_formating_data/00_Prediction_raster/Raster_df_predictions). The folders [rasters](02_formating_data/00_Prediction_raster/rasters) and [Rdata](02_formating_data/00_Prediction_raster/Rdata) contain intermediate outputs.

#### 01_Benthic

Contains the scripts to format benthic diversity measures and the associated Rdata. 

#### 02_Pelagic

Contains the scripts to format pelagic diversity measures and the associated Rdata. 


#### 03_explanatory_variables

Contains the scripts used to format explanatory variable, compute pairwise correlations and select uncorrelated variables for further analyses.


### 03_preliminary_analysis

Contains all the scripts used for the selection of eDNA MOTUs and BRUVS species to include in the GJAM models.
Contains the scripts for the exploration of pelagic profiles with acoustic end eDNA data.

### 04_Modelling

#### 01_Benthic

Contains the scripts used to model and predict benthic diversity with GJAM and BRT models (for BRUVS, eDNA and acoustic). Contains the outputs of each model and the predictions layers. 

#### 02_Pelagic

Contains the scripts used to model and predict pelagic diversity with GJAM and BRT models (for eDNA and acoustic). Contains the outputs of each model and the predictions layers. 

### 05_Marine_Spatial_Planning

#### 01_formating_prediction_layers

Contains the script used to assemble all the predictions in the 3 depth layers (0-200, 200-400 and 400-600m), and the associated Rdata.

#### 02_formating_MSP3D_inputs

Contains the script used to create all the objects needed in the input of the marine spatial planning with prioritizr : the features, the planning units with their identitication, the boundary between planning units, and the matrix of feature values within each planning unit.

#### 03_Prioritization

Contains the scripts used to run the prioritization with the prioritizr package, with different BLM values. Each Solution folder contains the cost and boundary length computed with the given BLM value. The Rdata folder contains the outputs of the hierachical ploting and ranking of all the solutions.


### 06_Figures

Contains the scripts to produced the figures to include in the publication.


If you encouter any error or have any question, please contact me & fill a detailed issue on this repo. 
