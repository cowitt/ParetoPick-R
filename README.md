# 1. Background
ParetoPick-R is part of the post processing in the [OPTAIN Project](https://www.optain.eu/). It shall facilitate the analysis of the Pareto front across objectives and support decision making for measure implementation.
It provides a dashboard for the user to supply their own data, visualise it and alter a range of parameters. 
The code allows the user to select variables to be analysed in a correlation analysis and a cluster algorithm. 

## Cluster Variables
The algorithm considers five variables:
1. **share_con** - ratio of area covered by measure to available area (per measure type) 
2. **channel_frac** - fraction of measure HRU water draining directly to channel (per measure type) 
3. **moran** - Moran's I (per measure type) 
4. **linE** - ratio of structural to management options 
5. **lu_share** - share of land use measures (buffer, grassslope, hedge) in available area

ParetoPick-R employs Principal Component Analysis (PCA) and kmeans/kmedoid clustering, with customisable settings for outlier treatment and component selection. It integrates an Analytical Hierarchy Process (AHP) for objective weighting based on pairwise comparisons. The clustering and AHP results can be combined using various visualisation methods.

Python code: [S. White](https://github.com/SydneyEWhite)

# 2. Requirements
  * R version 4.4.2 or higher
  * package "promises" version 1.3.2 or higher
  * package "tmap" remove or upgrade to version 4.0+ to avoid conflicts


# 3. Folder and File Structure

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
**Folder purposes:**
- **app**: UI and server logic
- **python_files**: Python executables for analysis
- **input**: Configuration and processed data
- **data**: User-supplied outputs from multi-objective optimisation
- **output**: Analysis results and selected optima
- **data_for_container**: Default configuration for reset functionality


Files supplied through by the user are stored in the data folder, these are the outputs of the previous MOO [Strauch and Schürz, 2024](https://www.optain.eu/sites/default/files/delivrables/OPTAIN%20D5.1%20-%20Common%20optimisation%20protocol.pdf).


*In the forthcoming portable version [OPTAIN_Pareto_Demo](https://github.com/MartynLLM/OPTAIN_Pareto_Demo), the app was converted into a fully R-based software and the python_files and data for container folders have been removed.


## 3.1 Files created during processing
(stored in input folder)

* **object_names.RDS**: objective names
* **var_corr_par.csv**: objectives and variables for analysis
* **nswrm_priorities.RDS**: measures and implementation priority
* **hru_in_optima.RDS**: HRU-optimum connections
* **all_var.RDS**: all clustering variables
* **pca_content.RDS**: variables after correlation filtering
* **config.ini**: Python process configuration
* **buffers.RDS**: measures requiring buffer for map visibility
* **units.RDS**: unit definitions

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

There are four levels of functionality based on data availability.
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
Allows to either only provide pareto_fitness.txt (optionally also sq_fitness.txt) and the objective names, to provide all required datasets and perform the Data Preparation or to provide subsets of the required data that you reproduce following the OPTAIN templates. See previous section for Details.

Users can identify measures requiring buffer visualisation in maps. (note that elements in the downloaded maps tend to be a bit smaller than shown in the app).

**Note**: Changing objective names without a Hard Reset requires: (1) delete object_names.RDS, (2) manually update names in var_corr_par.csv, (3) update names in the newest kmeans/kmedoid output file.

### Clusterin Tabs
Clustering (manually & default) generates two correlation_matrix.csv and kmeans/kmedoid_data_w_clusters_representativesolutions.csv  

**Important**:
1. Files are overwritten each clustering run—save externally if retention is needed
2. Only the most recent kmeans/kmedoid output file is read; remove older versions to reprocess a previous result


# 6. Assumptions and Planned Features

## 6.1 Current Limitations
* convert_optain.R requires specific measure names; unmapped measures cannot be processed
* Default settings optimise clustering across catchments without outlier testing
* AHP inconsistency calculation requires ≥3 sliders set to non-"Equal" values
* Stratified variables are not supported


## 6.2 Planned Features
  * write/load full scenario run from previous uses
  * optimum number display in AHP
  * dynamic regression line with R2 in scatter plot in red, other R2 in blue
  * optima selection via direct number input
  * add information on objectives on hover through link to glossary
  * remove minus sign in all tables
  * scaled_filtered_data() and filtered_data() use two different functions that do almost the exact same, merging would increase efficiency
  * clearer error messages for aborted/failed clustering needed
  * add a small spinner to the Check Data button to clarify that it takes a while
  * description of reproducing data 
  * dynamic clustering with other variables, different var_corr_par.csv unlinked from SWAT+ and CoMOLA  
  * remove superfluous priority file writing, remove hru.con requirement for lat lon 
