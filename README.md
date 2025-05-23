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

# Requirements
  * R version 4.4.2 or higher
  * package "promises" version 1.3.2 or higher
  * package "tmap" has shown to lead to conflicts, it is recommended to either remove it(e.g. via remove.packages("tmap")) or to upgrade it to at least version 4.0

# Folder Structure
the project consists of six folders

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

# Required input files and data structure 
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

# Process
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
 

## Files created and used in the process
(stored in input folder)
* var_corr_par.csv (created in convert_optain.R)
* hru_in_optima.RDS (created in convert_optain.R based on measure_location.csv, connection between activated HRUs and optima)
* nswrm_priorities.csv (created in covert_optain.R based on measure_location.csv)
* object_names.RDS
* pca_content.RDS
* all_var.RDS
* units.RDS
* buffers.RDS



# Assumptions

* General 
  * as "land use" measures currently only hedge, buffer and grassslope, terrace, floodres, rip_forest, afforest - if more needed global.R, functions.R (write_corr) and convert_optain require adapting
  * nswrm_priorities() function currently considers: pond, constr_wetland, wetland, hedge, buffer, grassslope, lowtillcc, lowtill, notill, droughtplt, terrace, floodres, rip_forest, afforest and intercrop
  * do the current default settings produce reasonable cluster outputs across all catchments? the default currently does not perform outlier testing 
  * in AHP, the initial state of the pairwise comparison as "Equal" amplifies the mathematical definition of inconsistency, therefore only when at least three sliders are NOT set to "Equal" is inconsistency considered at all
  * stratified variables such as lowflow in the Schoeps do not work in the tool, the sliders cannot be moved
  * range_controlled() controls for the objectives min value being less than 0.0005 (*1000) or over 10000 (--> rounding), this might not be applicable for all case studies 

 

# To do
  * docker image with Linux executables OR translation to fully R-based clustering
  * pull AHP plot up with placeholder
  * add a small spinner to the Check Data button to clarify that it takes a while  