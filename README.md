# 1. Introduction
ParetoPick-R has been developed for post-processing multi-objective optimisation outputs. <img align = "right" width="150" height="200" alt="Image" src="https://github.com/user-attachments/assets/cf993a43-162e-46ef-80d5-71439fb9d84a" />
It facilitates the detailed analysis of Pareto fronts for four objectives and supports decision making for spatial optimisation.
It provides a dashboard for the user to supply their own data, visualise and explore it, alter a range of parameters and perform clustering and an Analytical Hierarchy Process.

The code allows the user to select variables to be analysed in a correlation analysis and a cluster algorithm. 

ParetoPick-R has been developed as part of the [OPTAIN Project](https://www.optain.eu/).


# 2. Deployment, required input files and data structure

## 2.1 Requirements for use in R/Rstudio
  * R version 4.4.2 or higher
  * package "promises" version 1.3.2 or higher
  * package "tmap" remove or upgrade to version 4.0+ to avoid conflicts

## 2.2 Input files for different levels of functionalities

The following files (their detailed structure is described in the next section) can be uploaded in the Data Preparation tab, depending on which of these files are uploaded, different level of functionalities become available:
  * **pareto_fitness**: describes the performance of individual optimas across the four objectives. Providing this file and the objective names allows to use the Visualisation and AHP tab including the objective sliders.
* **pareto_genomes** & **lookup table**: describes the connection between decision and objective space. Providing both these files, additionally to pareto fitness, activates the decision space/measure sliders in Visualisation and AHP tab. If you would like to assess a more complex decision space with individual elements spanning several spatial elements and competing activation, you might consider reproducing a measure_location file and copying it to the data Folder
* **shapefile**: Spatial representation of the decision space, providing this four-part file allows to use the mapping functionalities of the app.
* **cluster parameters**: contains pareto fitness and descriptors for each of the optima e.g. describing the decision space. The app allows to select the parameters from this file that shall be used in the clustering.

## 2.3 Data structures

1. __pareto_fitness.txt__
  * float
  * four columns that provide the objectives values
  * rows are the different Pareto optima
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
  * integer from 1 to 99
  * columns are different Pareto optima (== rows in pareto_fitness.txt)
  * row numbers have to align with the id column of the shapefile (1st row == id 1)
  * all integers have to be included in lookup table below
  * if using SWAT+/CoMOLA: list delineating activated (2) and non-activated (1) hydrological response units (hrus), aligning with measure_location.csv
  * can be either comma separated OR space separated

** Please make sure that pareto_fitness.txt, pareto_genomes.txt, lookup table and shape files align! **

  * EITHER
```
1 1 3 5 5 1 1 7 5 1 1 1 1 1 1 5
1 6 2 1 1 1 1 2 1 1 1 1 1 1 2 2 
1 1 1 1 6 1 1 1 1 2 2 2 1 1 1 9
```

  * OR

```
1, 1, 3, 5, 5, 1, 1, 7, 5, 1, 1, 1, 1, 1, 1, 5,
1, 6, 2, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 2, 2, 
1, 1, 1, 1, 6, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 9,
```


3. __lookup_table.csv/.txt__
  * integer and string of respective decision space unit/measure/implementation
  * in .txt rows with: "integer = string"
  * in .csv: 2 columns without header/rownames: 1st the integer used in pareto_genomes.txt, 2nd the string denoting the respective measure
  * not required for automated workflow and MOO from SWAT+/CoMOLA

  * EXAMPLE .txt (example from the [Crosslink Project](https://www.biodiversa.eu/2022/10/31/crosslink/))
```
1 = Scen0
2 = Scen20
3 = Scen40
4 = Scen60
5 = Scen80
6 = Scen100
7 = Scen20reduct
8 = Terrestrial

```


4. __shapefile__ consisting of: *.shp, *.dbf, *.prj, *.shx
  * has to contain an id column 
  * the id column has to align with pareto_genomes - the first row of the genome codes the activation of id 1
  * the shapefile should contain valid simple feature geometries (points, lines, or polygons)
  * any CRS is supported, data will be used with CRS EPSG:4326 (WGS84), consider reprojecting your data

5. __cluster-parameters.csv__
  * float
  * rows are Pareto optima
  * columns should contain the Pareto fitness and cluster variables
  * column names can contain spaces and the column names of pareto fitness have to align with what is provided in the Data Preparation tab
  * optional for automated workflow and MOO from SWAT+/CoMOLA
5. __sq_fitness.txt__
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

6. __rout_unit.con__
  * only for automated workflow and MOO from SWAT+/CoMOLA
  * connection file created with SWAT+ Editor/SWATmeasR delineating the transport of water between HRUs, channel and aquifer
  * this file has to contain the columns: obj_id, obj_typ_1, area, frac_1

7. __measure_location.csv__
  * only for automated workflow and MOO from SWAT+/CoMOLA
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

# 3. Process
### Data Preparation tab
The one file that has to be uploaded to allow any functionality is a file describing the Pareto fitness. Also, the objective names have to be provided, these have to aligning with the four columns in this file. Further functionalities become available when other files are uploaded. Please note that the files are only uploaded once the individual upload buttons are pressed.

For users of a SWAT+/CoMOLA workflow, an automated cluster and input data processing is available (see [section 5](#5-pre-set-cluster-variables-for-swatcomolaoptain-workflow) of this Readme).

**Visualisation Options**
Users can identify measures requiring buffer visualisation in maps. (note that elements in the downloaded maps tend to be a bit smaller than shown in the app).

**Note**: Changing objective names without a Hard Reset requires: (1) delete object_names.RDS, (2) manually update names in var_corr_par.csv/cluster_params.csv, (3) update names in the newest kmeans/kmedoid output file or delete these/this file/s.

### Clustering Tabs
Clustering (manually & default) generates two correlation_matrix.csv and kmeans/kmedoid_data_w_clusters_representativesolutions.csv  

ParetoPick-R employs Principal Component Analysis (PCA) and kmeans/kmedoid clustering, with customisable settings for outlier treatment and component selection. It integrates an Analytical Hierarchy Process (AHP) for objective weighting based on pairwise comparisons. The clustering and AHP results can be combined using various visualisation methods.

Original cluster code (in Python): [S. White](https://github.com/SydneyEWhite)


**Important**:
1. Files are overwritten each clustering run—save externally if retention is needed
2. Only the most recent kmeans/kmedoid output file is read; remove older versions to reprocess a previous result


# 4. Folder and File Structure

```
.
├── app
│   ├── ui.R
│   ├── server.R
│   ├── global.R
│   └── convert_optain.R
├── input
├── data
└── output
```
**Folder purposes:**
- **app**: UI and server logic
- **input**: Configuration and processed data
- **data**: User-supplied outputs from multi-objective optimisation
- **output**: Analysis results and selected optima

Files supplied through by the user are stored in the data folder, these are the outputs of the previous MOO, e.g. from SWAT+/CoMOLA [Strauch and Schürz, 2024](https://doi.org/10.5281/zenodo.11473793).


## 4.1 Files created during processing
(stored in input folder)

* **object_names.RDS**: objective names
* **var_corr_par.csv**: objectives and variables for analysis
* **nswrm_priorities.RDS**: measures and implementation priority
* **hru_in_optima.RDS**: HRU-optimum connections
* **all_var.RDS**: all clustering variables
* **pca_content.RDS**: variables after correlation filtering
* **buffers.RDS**: measures requiring buffer for map visibility
* **units.RDS**: unit definitions

## 4.2 Scripts
ParetoPick-R is built using a standard structure for dividing shiny functionalities among scripts. The five R scripts contained in the app folder are: ui.R, app.R, server.R, global.R, and convert_optain.R.

Each script serves a specific purpose in the software’s architecture:
* ui.R: This script establishes the UI of the app. It organises the app's layout, including input controls for sliders, clustering parameters and visualisation options. Additionally, it specifies the locations for displaying plots, tables, and clustering results.
* server.R: This is the core backend functionality containing the server-side logic of the software. It captures user inputs, processes data, performs calculations and updates outputs. It relies on reactive expressions to efficiently manage data flow and calls external functions from functions.R alongside defining its own to create dynamic visualisations and tables.
* functions.R: This script defines all custom functions used throughout the app. Most of them are used for formatting, data manipulation and plotting, while a few are for adapting reactive values for the clustering. The codebase is easier to maintain when consolidating the most important and frequently used function definitions.
* global.R: This short script defines global paths and app settings. It installs and/or loads packages and sets constants such as file paths, default parameters and any configuration options that need to be accessible across the entire app. It's kept concise to focus on app-wide settings.
* convert_optain.R: This script is needed for applications relying on a SWAT+/CoMOLA workflow only. It handles all data preparation. It reads the required data files and prepares the input data for the clustering analysis.



# 5. Pre-set Cluster Variables for SWAT+/CoMOLA/OPTAIN workflow

The algorithm considers five variables:
1. **share_con** - ratio of area covered by measure to available area (per measure type) 
2. **channel_frac** - fraction of measure HRU water draining directly to channel (per measure type) 
3. **moran** - Moran's I (per measure type) 
4. **linE** - ratio of structural to management options 
5. **lu_share** - share of land use measures (buffer, grassslope, hedge) in available area


# 6. Assumptions and Planned Features

## 6.1 Current Limitations
* (OPTAIN - specific) convert_optain.R requires specific measure names; unmapped measures cannot be processed
* AHP inconsistency calculation requires ≥3 sliders set to non-"Equal" values
* Stratified variables (as sometimes happens through rounding) are not supported and there is no error message


## 6.2 Planned Features for Version 1.1.0
  * write/load full scenario run from previous uses
  * optimum number display in AHP
  * (OPTAIN-specific) remove superfluous priority file writing, replace hru.con requirement for lat lon 
  * dynamic printing of progress during clustering
  * render measure_location.csv obsolete
  * hru_in_optima.RDS as txt and with lookup
 
Other
  * this Readme needs a better eplanation of the levels of functionality, aligned with the Data Preparation tab as a table
  * dynamic regression line with R2 in scatter plot in red, other R2 in blue
  * optima selection via direct number input
  * add information on objectives on hover through link to glossary
  * scaled_filtered_data() and filtered_data() use two different functions that do almost the exact same, merging would increase efficiency
  * clearer error messages for aborted/failed clustering needed
  * add a small spinner to the Check Data button to clarify that it takes a while
  * description of reproducing data 
