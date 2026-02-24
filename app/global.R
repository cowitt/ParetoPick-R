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
    library(i, character.only = TRUE)
  }
}

## check if any packages are missing (not only here but also for external convert_optain)
foo1(c("cluster", "corrplot", "DT","fpc", "fs", "fst", 
       "geosphere",  "ggplot2",  "gridExtra", "ini", "leaflet","leaflet.extras", "leafsync",
        "patchwork", "plotly",  "processx",  "quanteda",
       "scales", "sf", "shiny", "shinycssloaders", "shinydashboard", "shinyjs",
     "shinyWidgets",  "sp", "spdep", "svglite", "tmap","tidyverse",  "viridis", "webshot2"))

# Configure Chrome/Chromium path for webshot2/chromote
if (file.exists("/usr/bin/google-chrome")) {
  Sys.setenv(CHROMOTE_CHROME = "/usr/bin/google-chrome", CHROME_BIN = "/usr/bin/google-chrome")
} else if (file.exists("/usr/bin/chromium")) {
  Sys.setenv(CHROMOTE_CHROME = "/usr/bin/chromium", CHROME_BIN = "/usr/bin/chromium")
} else if (file.exists("/usr/bin/chromium-browser")) {
  Sys.setenv(CHROMOTE_CHROME = "/usr/bin/chromium-browser", CHROME_BIN = "/usr/bin/chromium-browser")
}


options(shiny.maxRequestSize = 1000*1024^2)

options(warn = -1)
source("functions.R")

# Default to nested 'default' case study when not overridden by server.R session mapping
base_dir <- normalizePath("..", winslash = "/", mustWork = FALSE)
default_folder <- file.path(base_dir, "default")
save_dir <- file.path(default_folder, "data")
input_dir <- file.path(default_folder, "input")
output_dir <- file.path(default_folder, "output")
# Fallback: use original flat dirs if default/ has no data
if (!file.exists(file.path(save_dir, "pareto_fitness.txt")) &&
    file.exists(file.path(base_dir, "data", "pareto_fitness.txt"))) {
  save_dir <- file.path(base_dir, "data")
  input_dir <- file.path(base_dir, "input")
  output_dir <- file.path(base_dir, "output")
}
pareto_path <- file.path(save_dir, "pareto_fitness.txt")
if(!dir.exists(save_dir)){  dir.create(save_dir, recursive = TRUE)}
if(!dir.exists(output_dir)){  dir.create(output_dir, recursive = TRUE)}