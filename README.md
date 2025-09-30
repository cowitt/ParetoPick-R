# 1. Background
ParetoPick-R is part of the post processing in the [OPTAIN Project](https://www.optain.eu/). It shall facilitate the analysis of the Pareto front across objectives and support decision making for measure implementation.
It provides a dashboard for the user to supply their own data, visualise it and alter a range of parameters. 
The code allows the user to select variables to be analysed in a correlation analysis and a cluster algorithm. 

* Variables considered by the cluster algorithm, the first 3 are produced seperately for each measure (in a call to convert_optain)
  1. **share_con** - ratio between area covered by measure and area available for measure implementation (per measure type) 
  2. **channel_frac** - fraction of water from measure hru draining straight into the channel (per measure type) 
  3. **moran** - Moran's I (per measure type) 
  4. **linE** - ratio of structural to management options 
  5. **lu_share** - share of area used for "land use" measures (buffer, grassslope and hedge) in area available for measure implementation


The cluster algorithm relies on Principal Component Analysis (PCA) and kmeans/kmedoid. While the variables that can be considered in 
the algorithm are fixed, several settings such as outlier treatment and the number of tested principal components, can be set by the user. 
ParetoPick-R also integrates an Analytical Hierarchy Process (AHP) allowing the user to determine weights for the objectives based on pair-wise comparisons. The results of the clustering and the AHP can be combined and the app provides a range of methods for visualising the results.

Please note that the Python code used in this app was written by [S. White](https://github.com/SydneyEWhite).

# 2. Requirements
  * R version 4.4.2 or higher
  * package "promises" version 1.3.2 or higher
  * package "tmap" has shown to lead to conflicts, it is recommended to either remove it(e.g. via remove.packages("tmap")) or to upgrade it to at least version 4.0

# 3. Folder and File Structure
The tool consists of six folders: input, app, data, output, data_for_container* and python_files*. 

The folder “data for container” stores the default configuration file, called config.ini, which is used by the external python executables. During a reset of the app, this file is used to restore the config.ini in the input folder.
All files supplied through by the user are stored in the data folder, these are the outputs of the previous MOO [Strauch and Schürz, 2024](https://www.optain.eu/sites/default/files/delivrables/OPTAIN%20D5.1%20-%20Common%20optimisation%20protocol.pdf).
The output folder stores all files produced during the correlation and cluster analysis. When selecting to save specific optima, these are also this folder to a file called selected_optima.csv. The python_files folder contains all Python-based parts of the software. These are three executables, correlation_matrix.exe, kmeans.exe and kmedoid.exe as well as an _internal folder that creates a temporary Python environment including all necessary dependencies.

```
.
├── app
│   ├── ui.R
│   ├── server.R
│   ├── global.R
│   └── convert_optain.R
├── python_files
│   ├── kmeans.exe
│   ├── kmedoid.exe
│   ├── correlation_matrix.exe
│   └── _internal
├── input
│   └── config.ini
├── data
├── data_for_container
│   └── config.ini (for hard reset)
└── output
```
The input folder is used for storing all data required to run the tool. These input files are created by the software and regularly accessed and modified after all required data files have been provided by the user.

*In the forthcoming portable version [OPTAIN_Pareto_Demo](https://github.com/MartynLLM/OPTAIN_Pareto_Demo), the app was converted into a fully R-based software and the python_files and data for container folders have been removed.


## 3.1 Files created and used in the process
(stored in input folder)

* object_names.RDS: the names of the four objectives
* var_corr_par.csv: (created in convert_optain.R) objectives and variables considered in correlation and cluster analysis
* nswrm_priorities.RDS: (created in covert_optain.R based on measure_location.csv) measures and their priority of implementation
* hru_in_optima.RDS: created in convert_optain.R based on measure_location.csv) connection between activated HRUs and optima/measure allocation across all HRUs for all optima
* all_var.RDS: all variables produced in the clustering
* pca_content.RDS: variables considered in the clustering after highly correlated variables have been removed from all_var
* config.ini: used for communicating with the external Python processes
* buffers.RDS: names of measures that require a buffer to improve their visibility in maps
* units.RDS

## 3.2 Scripts
ParetoPick-R is built using a standard structure for dividing shiny functionalities among scripts. The five R scripts contained in the app folder are: ui.R, app.R, server.R, global.R, and convert_optain.R.

Each script serves a specific purpose in the software’s architecture:
* ui.R: This script establishes the UI of the app. It organises the app's layout, including input controls for sliders, clustering parameters and visualisation options. Additionally, it specifies the locations for displaying plots, tables, and clustering results.
* server.R: This is the core backend functionality containing the server-side logic of the software. It captures user inputs, processes data, performs calculations and updates outputs. It relies on reactive expressions to efficiently manage data flow and calls external functions from functions.R alongside defining its own to create dynamic visualisations and tables.
* functions.R: This script defines all custom functions used throughout the app. Most of them are used for formatting, data manipulation and plotting, while a few are for adapting config.ini to control the external Python processes. The codebase is easier to maintain when consolidating the most important and frequently used function definitions.
* global.R: This short script defines global paths and app settings. It installs and/or loads packages and sets constants such as file paths, default parameters and any configuration options that need to be accessible across the entire app. It's kept concise to focus on app-wide settings.
* convert_optain.R: This script is needed for the desktop version only. It handles all data preparation. It reads the required data files and prepares the input data for the clustering analysis.

This design separates functionality, creating a modular software simpler to develop and maintain. The convert_optain.R file maintains uniform data formatting for clustering across OPTAIN studies.


# 4. Required input files and data structure
(with example data structures from the Schwarzer Schöps catchment)
1. __pareto_fitness.txt__
  * comma delineated, four columns representing the objectives that were maximised in optimisation
  * can be either comma separated OR space separated
  * EITHER
```
-6880.0 -0.052 59069.165 0.0
-6875.0 -0.052 59068.499 -477.81743
-6850.0 -0.052 59065.513 -14.7785
-6749.0 -0.053 59097.725 -28858.69644
-6681.0 -0.054 59125.122 -67853.89737
-6765.0 -0.053 59099.121 -25536.89511
``` 
  * OR

```
-6880.0, -0.052, 59069.165, 0.0
-6875.0, -0.052, 59068.499, -477.81743
-6850.0, -0.052, 59065.513, -14.7785
-6749.0, -0.053, 59097.725, -28858.69644
-6681.0, -0.054, 59125.122, -67853.89737
-6765.0, -0.053, 59099.121, -25536.89511
```
2. __pareto_genomes.txt__
  * list delineating activated (2) and non-activated (1) hydrological response units (hrus)
  * can be either comma separated OR space separated
  * EITHER
```
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 
1 1 2 1 1 1 1 2 1 1 1 1 1 1 2 2 
1 1 1 1 1 1 1 1 1 2 2 2 1 1 1 1
```

  * OR

```
1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 
1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
```

** Please make sure that the two files align. If there are x rows (=optima) in pareto_fitness.txt there should be x columns (or rows, the app understands both) in pareto_genomes.txt **

3. __hru.con__
  * connection file created with SWAT+ Editor/SWATmeasR containing details on HRU size and location
  * this file has to contain the columns: id, area, lat, lon

4. __measure_location.csv__
  * csv - comma separated table with four columns: id, name, nswrm, obj_id
```
id,	name,	nswrm,	obj_id
1,	buffer_1,	buffer,	479
2,	buffer_10,	buffer,	281
3,	buffer_11,	buffer,	509, 511
107,	lowtillcc_111,	lowtillcc,	513, 514
108,	lowtillcc_112,	lowtillcc,	527
294,	pond_1,	pond,	997
```
5. __hru shapefile__ consisting of: hru.shp, hru.dbf, hru.prj, hru.shx
  * shapfile used in SWAT+ modelling allowing the matching of HRU location and activation
6. __sq_fitness.txt__
  * optional
  * four values indicating the status quo of objectives, must have same order as pareto_fitness.txt
  * can be either comma separated OR space separated
  * EITHER
```
-6880 -0.052 59069.165 0
```
  * OR
```
-6880, -0.052, 59069.165, 0
```
7. __rout_unit.con__
  * connection file created with SWAT+ Editor/SWATmeasR delineating the transport of water between HRUs, channel and aquifer
  * this file has to contain the columns: obj_id, obj_typ_1, area, frac_1

## 4.1 Input files for reduced functionalities

There are four levels of functionality based on input data availability.
If all input data files are available from a coupled model workflow based on SWAT+ and CoMOLA or if all files can be reproduced, then all functionalities of ParetoPick-R (inlcuding both slider types, clustering and plotting) can be used.
The table below outlines the four levels of functionality, their differences and required input files. 

| Level                 | Description                                          | Required Input Files                                   |                               
|:----------------------|:-----------------------------------------------------|:-------------------------------------------------------|
| Basic Functionality   | Visualisations & the AHP  <br> are working but without <br> measure sliders, clustering and <br>map plotting | Only the base file: <br> pareto_fitness.txt|
| Full Visualisation    | All sliders, Visualisations <br> & the AHP are working but neither <br> map plotting nor Clustering | pareto_fitness.txt <br> measure_location.csv <br> hru_in_optima.RDS |
| Full Connection to Decision Space  | All sliders, maps & the AHP are working <br> only the clustering cannot be performed | pareto_fitness.txt <br> measure_location.csv <br> hru_in_optima.RDS <br> hru.con <br> hru.shp/.dbf/.prj/.shx |
| Full Functionality    | All sliders, maps, the AHP & the <br> clustering are working | All files named above. <br> The Data Prep has to run before the <br> Clustering can be performed |



This manual will be expanded with a detailed explanation of the steps required to reproduce the data, specifically: hru_in_optima.RDS and measure_location.csv required for the measure sliders and hru.shp and hru.con required for the mapping.
Since rout_unit.con is the only additional file needed to perform the Data Preparation for the clustering (write var_corr_par.csv), a method for replacing it for other projects shall also be developed. This might potentially include turning off the cluster variable "fraction of water".

# 5. Process
### Data Preparation tab
This tab allows you to either only provide pareto_fitness.txt (optionally also sq_fitness.txt) and the objective names or to provide all required datasets and perform the Data Preparation. The option to "Run Prep" becomes available when all files have been found after clicking "Check Files".
"Run Prep" calls the convert_optain R script and writes var_corr_par.csv into the input folder. Var_corr_par.csv contains all variables considered in the clustering. Depending on the measures implemented, different variables are included. You can find their description in the glossary.
The clustering is later run across these variables.

If you want this tab lets you select those measures that require a buffer in maps to enhance their visibility (note that elements in the downloaded maps tend to be a bit smaller than shown in the app).

Please note that it is not straightforward to change the objectives names at a later point without performing a hard reset and rerunning the data preparation first. 
You can force a change of objective names with the following steps:

1. delete object_names.RDS from the input folder

2. manually change the names in the first four columns in var_corr_par.csv in the input folder

3. manually change the names in the first four columns of the newest file in the output folder with a name like kmeans_data_w_clusters_representativesolutions.csv

### Cluster Tabs
The clustering, both manually or under default settings writes two .csv files to the output folder. One is called correlation_matrix.csv, the other is called kmeans_data_w_clusters_representativesolutions.csv or kmedoid_data_w_clusters_representativesolutions.csv or similar depending on which 
cluster method you chose and if outliers were tested. 

It is important to note that:
1. **these files are overwritten** each time the clustering is run again, save them in another location if you would like to keep your clustering results
2. in the following tabs, **only the most recent** of the <kmeans_data_w_clusters_re....>csv file is read! If you would like to process an older one, you have to remove all newer ones from the output folder
 


# 6. Assumptions and Planned Features

## 6.1 General 
  * covert_optain.R is hard coded to measures, if these measures are not specifically named in this script they cannot be processed
  * the current default settings produce reasonable cluster outputs across all catchments but the default currently does not perform outlier testing 
  * in AHP, the initial state of the pairwise comparison as "Equal" amplifies the mathematical definition of inconsistency, therefore only when at least three sliders are NOT set to "Equal" is inconsistency considered at all
  * stratified variables do not work in the tool, the sliders cannot be moved
 

## 6.2 Planned Features/Missing
  * scaled_filtered_data() and filtered_data() use two different functions that do almost the exact same, merging would increase efficiency
  * clearer error messages for aborted/failed clustering needed
  * processing speed of Visualisation tab went down with matched selection
  * add a small spinner to the Check Data button to clarify that it takes a while
  * description of reproducing data 
  * dynamic clustering with other variables, different var_corr_par.csv unlinked from SWAT+ and CoMOLA  
