######################### GLOBAL ###################################
# comments: we assume four variables delineating pareto front
# Project: Clustering Pareto solutions/Multi-objective visualisation
# author: cordula.wittekind@ufz.de
####################################################################
## loading new packages
# foo1 <- function(x) {
#   for (i in x) {
#     if (!requireNamespace(i, quietly = TRUE)) {
#       install.packages(i, dependencies = TRUE, quiet = TRUE)
#     }
#     if (!requireNamespace(i, quietly = TRUE) && i == "leaflet.extras") {
#       if (!requireNamespace("remotes", quietly = TRUE)) {
#         install.packages("remotes", quiet = TRUE)
#       }
#       remotes::install_github("trafficonese/leaflet.extras", dependencies = T, quiet = T)
#     }
#     library(i, character.only = TRUE)
#   }
# }

# foo1 <- function(x) {
#   for (i in x) {
#     library(i, character.only = TRUE)
#   }
# }

## check if any packages are missing (not only here but also for external convert_optain)
packages <- c("cluster", "corrplot","chromote", "DT", "fpc", "fs", "fst",
              "geosphere", "ggplot2", "gridExtra", "ini", "leaflet",
              "leaflet.extras", "leafsync", "patchwork", "plotly",
              "processx", "quanteda", "scales", "sf", "shiny",
              "shinycssloaders", "shinydashboard", "shinyjs", "shinyWidgets",
              "sp", "spdep", "svglite", "tmap", "tidyverse", "viridis",
              "webshot2")

invisible(lapply(packages, library, character.only = TRUE))


options(shiny.maxRequestSize = 1000*1024^2)
b <- Chromote$new(Chrome$new(path = '/usr/bin/google-chrome',
                             args = c('--no-sandbox', '--disable-dev-shm-usage', '--disable-setuid-sandbox')
  )
)
set_default_chromote_object(b)
options(warn = -1)
source("functions.R")

save_dir <- "../data/"
input_dir <- "../input/"
output_dir <- "../output/"
pareto_path <- "../data/pareto_fitness.txt" #used too frequently..
if(!dir.exists(save_dir)){  dir.create(save_dir)}
if(!dir.exists(output_dir)){  dir.create(output_dir)}