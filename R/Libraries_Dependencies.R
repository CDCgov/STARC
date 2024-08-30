if (!require("devtools")) install.packages("devtools");library(devtools)
if (!require("remotes")) install.packages("remotes");library(remotes)
#if (!require("spatialutils")) devtools::install_github("matthewjwhittle/spatialutils") 
if (!require("rgeoboundaries")) remotes::install_github("wmgeolab/rgeoboundaries") 
if (!require("rhdx")) remotes::install_gitlab("dickoa/rhdx")  
if (!require("pacman")) install.packages("pacman"); library(pacman)  

p_load( 
  'dplyr',
  #'rgdal',
  'sf',
  'sp',
  "tmap",
  "tmaptools",
  "raster",
  #'doParallel', 
  "exactextractr",
  "terra",
  "h3jsr",
  #"spatialutils",
  "shiny",
  "bs4Dash", 
  "shinyWidgets",
  "htmlwidgets",
  #"shinythemes",
  "shinydashboard",
  "beepr",
  "OpenStreetMap",
  "rgeoboundaries", 
  "crsuggest", 
  "leaflet",
  "leaflet.extras",
  "stringr",
  "openxlsx",
  "RColorBrewer",
  "dismo",
  "spdep",
  "purrr",
  "rmapshaper"
)

#Not using (will keep devtools"   &   "remotes" )
#"devtools"      "remotes"       "spatialutils"  "pacman"        "tidyverse"     "rgdal"         "doParallel"    "spatialutils"  "shinythemes"   "OpenStreetMap"