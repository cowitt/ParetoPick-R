######################### GLOBAL ###################################
# comments: we assume four variables delineating pareto front
# Project: Clustering Pareto solutions/Multi-objective visualisation
# author: cordula.wittekind@ufz.de
####################################################################
## loading new packages
foo1 <- function(x) {
  for (i in x) {
    if (!requireNamespace(i, quietly = TRUE)) {
      install.packages(i, dependencies = TRUE, quiet = TRUE)
    }
    if (!requireNamespace(i, quietly = TRUE) && i == "leaflet.extras") {
      if (!requireNamespace("remotes", quietly = TRUE)) {
        install.packages("remotes", quiet = TRUE)
      }
      remotes::install_github("trafficonese/leaflet.extras", dependencies = T, quiet = T)
    }
    library(i, character.only = TRUE)
  }
}

## check if any packages are missing (not only here but also for external convert_optain)
foo1(c("cluster", "corrplot", "DT","fpc", "fs", "fst", 
       "geosphere",  "ggplot2",  "gridExtra", "ini", "leaflet","leaflet.extras", "leafsync",
       "patchwork", "plotly",  "processx",  "quanteda", "scales", "sf", "shiny",
       "shinycssloaders", "shinydashboard", "shinyjs", "shinyWidgets", 
       "sp",# dependency = foreign 
       "spdep", #dependency = Matrix
       "svglite", "tmap",
       "tidyverse", #dependency = stringr & tidyr
       "viridis", "webshot"))#Docker needs webshot2, only works in Chrome

if (!webshot::is_phantomjs_installed()) {
  webshot::install_phantomjs()
}


options(shiny.maxRequestSize = 1000*1024^2)

options(warn = -1)
source("functions.R")

save_dir <- "../data/"
input_dir <- "../input/"
output_dir <- "../output/"
pareto_path <- "../data/pareto_fitness.txt" #used too frequently..
if(!dir.exists(save_dir)){  dir.create(save_dir)}
if(!dir.exists(output_dir)){  dir.create(output_dir)}