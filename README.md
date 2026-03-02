# 1. Introduction
ParetoPick-R has been developed for post-processing multi-objective optimisation outputs. <img align = "right" width="150" height="200" alt="Image" src="https://github.com/user-attachments/assets/cf993a43-162e-46ef-80d5-71439fb9d84a" />
It facilitates the detailed analysis of Pareto fronts for four objectives and supports decision making.
It provides a dashboard for the user to supply their own data, visualise and explore it, produce maps, alter a range of parameters and perform clustering and an Analytical Hierarchy Process.

The code allows the user to select variables to be analysed in a correlation analysis and a cluster algorithm. 

ParetoPick-R has been developed as part of the [OPTAIN Project](https://www.optain.eu/).


# 2. Deployment, required input files and data structure

## 2.1 Requirements for use in R/Rstudio
  * R version 4.4.2 or higher
  * package "promises" version 1.3.2 or higher
  * remove or upgrade (>4.0) package "tmap" to avoid conflicts
  * recommended to use renv::restore()

## 2.2 Input files for different levels of functionalities

The following files (their detailed structure is described in the next section) can be uploaded in the Data Preparation tab, depending on which of these files are uploaded, different level of functionalities become available:
  * **Pareto fitness**: describes the performance of individual optimas across four objectives. Providing this file and the objective names allows to use the Visualisation and AHP tab including the objective sliders.
* **Pareto genome** & **lookup table**: describes the connection between decision and objective space. Providing both these files, additionally to pareto fitness, activates the decision space/measure sliders in Visualisation and AHP tab. If you would like to assess a more complex decision space with individual elements spanning several spatial elements and competing activation, you might consider reproduce a measure_location file.
* **shapefile**: Spatial representation of the decision space, providing this four-part file allows to use the mapping functionalities of the app.
* **Cluster variables**: contains pareto fitness and descriptors for each of the optima e.g. describing the decision space. The app allows to select the variables from this file that shall be used in the clustering.

## 2.3 Data structures

1. __Pareto Fitness (.txt)__ <a name="fitness-structure"></a>
  * float32
  * four columns for each of the objectives
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
2. __Pareto Genomes (.txt)__ <a name="genome-structure"></a>
  * integer from 1 to 99
  * columns are different Pareto optima (== rows in pareto_fitness.txt)
  * row numbers have to align with the id column of the shapefile (1st row == id 1)
  * all integers have to be included in lookup table below
  * if using SWAT+/CoMOLA: list delineating activated (2) and non-activated (1) hydrological response units (hrus), aligning with measure_location.csv
  * can be either comma separated OR space separated

**Please make sure that pareto_fitness.txt, pareto_genomes.txt, lookup table and shape files align!**

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


3. __lookup table (.txt or .csv)__<a name="lookup-structure"></a>
  * integer and string of respective decision space unit/measure/implementation
  * in .txt rows with: "integer = string", note the space before and after the equal sign
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


4. __Shapefile__ consisting of: *.shp, *.dbf, *.prj, *.shx <a name="shapefile-structure"></a>

  * has to contain an id column 
  * the id column has to align with pareto_genomes - the first row of the genome codes the activation of id 1
  * the shapefile should contain valid simple feature geometries (points, lines or polygons)
  * any CRS is supported, data will be used with CRS EPSG:4326 (WGS84), consider reprojecting your data
  * if cluster parameters shall be calculated by the tool, an area column is required

5. __Cluster Parameters (.csv)__<a name="cluster-structure"></a>
  * float32
  * rows are Pareto optima
  * columns should contain the Pareto fitness and cluster variables
  * column names can contain spaces and the column names of pareto fitness have to align with what is provided in the Data Preparation tab
  * optional for automated workflow and MOO from SWAT+/CoMOLA
5. __status quo fitness (.txt)__<a name="sq-structure"></a>
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

6. __rout_unit.con__<a name="rout-structure"></a>
  * only for automated workflow and MOO from SWAT+/CoMOLA
  * connection file created with SWAT+ Editor/SWATmeasR delineating the transport of water between HRUs, channel and aquifer
  * this file has to contain the columns: obj_id, obj_typ_1, area
  * has to contain one header line followed by one line with column names, no empty lines in first three lines
```
SWAT+ input file updated with SWATmeasR at 2023-12-29 17:47:43.710246
      id  name                gis_id          area           lat           lon          elev    obj_id               wst       cst      ovfl      rule   out_tot     obj_typ_1  obj_id_1     hyd_typ_1        frac_1     obj_typ_2  obj_id_2     hyd_typ_2        frac_2     obj_typ_3  obj_id_3     hyd_typ_3        frac_3     obj_typ_4  obj_id_4     hyd_typ_4        frac_4     obj_typ_5  obj_id_5     hyd_typ_5        frac_5     obj_typ_6  obj_id_6     hyd_typ_6        frac_6     obj_typ_7  obj_id_7     hyd_typ_7        frac_7     obj_typ_8  obj_id_8     hyd_typ_8        frac_8     obj_typ_9  obj_id_9     hyd_typ_9        frac_9    obj_typ_10  obj_id_10    hyd_typ_10       frac_10    obj_typ_11  obj_id_11    hyd_typ_11       frac_11    obj_typ_12  obj_id_12    hyd_typ_12       frac_12
       1  rtu0001                  0       0.14805      51.12234      14.74782     243.08908         1     s51119n14755e         0         0         0         3            ru      1856           tot       0.72164            ru      4694           tot       0.27836           aqu         1           rhg       1.00000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
    
```

7. __measure_location.csv__<a name="msrs-structure"></a>
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

8. __hru.con__<a name="con-structure"></a>
  * only for automated workflow and MOO from SWAT+/CoMOLA
  * SWAT+ input file
  * has to contain columns id, area, lat, lon
```
SWAT+ input file updated with SWATmeasR at 2024-09-18 12:54:56.599093
      id  name                gis_id          area           lat           lon          elev    obj_id               wst       cst      ovfl      rule   out_tot
       1  hru0001                  1       0.76279      45.70575       9.81739     364.90688         1      s45784n9822e         0         0         0         0
       2  hru0002                  2       0.74251      45.70370       9.82486     293.29695         2      s45784n9822e         0         0         0         0

```
## 2.4 Automated Clustering
It is possible to work with cluster variables produced within the tool. 

 **share_con** - ratio of area covered by measure to available area (per measure type) 

This variable is the share of area implemented for individual decision space variables (aka measures) in each individual optima.
The user has to supply a shapefile with an id and an area column. The area can be provided in any unit, the id column maps the spatial units to the genome.

## 2.5 Automated Data Processing and Clustering with a SWAT+/CoMOLA workflow

For users of a SWAT+/CoMOLA workflow, an automated cluster and input data processing is available.

The algorithm considers five variables:
1. **share_con** - ratio of area covered by measure to available area (per measure type) 
2. **channel_frac** - fraction of measure HRU water draining directly to channel (per measure type) 
3. **moran** - Moran's I (per measure type) 
4. **linE** - ratio of structural to management options 
5. **lu_share** - share of land use measures (buffer, grassslope, hedge) in available area

# 3. Process
### Data Preparation tab
Unless otherwise specified, you may use any file name. However, ensure the file is in the correct format.

The one file that has to be uploaded to allow any functionality is a file describing the Pareto fitness. Additionally, the objective names have to be provided. These names have to align with the four columns in this file. Further functionalities become available when other files are uploaded. The app will tell you which functionalities are available at each step.

**Visualisation Options**: Users can identify measures requiring a buffer in maps because they might be too small otherwise. (note that elements in the downloaded maps tend to be a bit smaller than shown in the app).

**Note**: Changing objective names without a Hard Reset requires: (1) delete object_names.RDS, (2) manually update names in var_corr_par.csv/cluster_params.csv, (3) update names in the newest kmeans/kmedoid output file or delete these/this file/s.

### Clustering Tabs
Clustering (manually & default) generates two files - correlation_matrix.csv and kmeans/kmedoid_data_w_clusters_representativesolutions.csv  

ParetoPick-R employs Principal Component Analysis (PCA) and kmeans/kmedoid clustering, with customisable settings for outlier treatment and component selection. It integrates an Analytical Hierarchy Process (AHP) for objective weighting based on pairwise comparisons. The clustering and AHP results can be combined using various visualisation methods.

Original cluster code (in Python): [S. White](https://github.com/SydneyEWhite)


**Important**:
1. Files are overwritten each clustering run—save externally if retention is needed
2. Only the most recent kmeans/kmedoid output file is read/analysed within the tool; (re)move newer versions from output folder to reprocess an older result


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

Files uploaded in the Data Preparation tab are stored in the data folder, these are the outputs of the previous MOO (e.g. from SWAT+/CoMOLA [Strauch and Schürz, 2024](https://doi.org/10.5281/zenodo.11473793)).


## 4.1 Files created during processing
(stored in input folder)

* **object_names.RDS**: objective names
* **nswrm_priorities.RDS**: measures and implementation priority
* **hru_in_optima.RDS**: HRU-optimum connections
* **all_var.RDS**: all clustering variables
* **pca_content.RDS**: variables after correlation filtering
* **buffers.RDS**: measures requiring buffer for map visibility
* **units.RDS**: unit definitions
* **hru.con**: shapefile center for plot settings (same name as what automated workflow uses for producing cluster variables)
* **cluster_params.csv**: parameters for cluster variables
* **var_corr_par.csv**: objectives and parameters for cluster analysis (SWAT+/CoMOLA only)


## 4.2 Scripts
ParetoPick-R is built using a standard structure for dividing shiny functionalities among scripts. The five R scripts contained in the app folder are: ui.R, app.R, server.R, global.R, and convert_optain.R.

Each script serves a specific purpose in the software’s architecture:
* ui.R: This script establishes the UI of the app. It organises the app's layout, including input controls for sliders, clustering parameters and visualisation options. Additionally, it specifies the locations for displaying plots, tables, and clustering results.
* server.R: This is the core backend functionality containing the server-side logic of the software. It captures user inputs, processes data, performs calculations and updates outputs. It relies on reactive expressions to efficiently manage data flow and calls external functions from functions.R alongside defining its own to create dynamic visualisations and tables.
* functions.R: This script defines all custom functions used throughout the app.
* global.R: This short script defines global paths and app settings.  
* convert_optain.R: This script is needed for applications relying on a SWAT+/CoMOLA workflow only. It prepares hru_in_optima.RDS with competing activation of individual spatial units (defined in measure_location.csv) and the cluster parameters.


# 5. Assumptions and Planned Features

## 5.1 Current Limitations
* hard-coded to FOUR objectives, less than four can be assessed by introducing a dummy variable but more is not possible atm
* convert_optain.R when using the automated SWAT+/CoMOLA workflow is limited to the hard coded measure names; unmapped measures cannot be processed. The distinction between linear and management measures cannot be automated.
* Stratified variables (as sometimes happens through rounding) are not supported for sliders and there is no error message
* not all input files supplied by the user are checked for consistency, focus on the most important files: fitness and genome


## 5.2 Planned Features for Version 1.1.0
  * debounce slider settings in visualisation tab
  * write/load full scenario run from previous uses
  * dynamic printing of progress during clustering
  * easier reuse of cluster results with selection and renaming
   
Other
  * optimum number display in AHP
  * dynamic regression line with R2 in scatter plot in red, other R2 in blue
  * optima selection via direct number input
  * scaled_filtered_data() and filtered_data() use two different functions that do almost the exact same, merging would increase efficiency
  * clearer error messages for aborted/failed clustering needed
