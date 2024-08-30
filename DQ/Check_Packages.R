p_load( 
  'NCmisc',
  'stringr',
  'dplyr'
)


checkPacks<-function(path){
  
  ## get all R files in your directory
  ## by the way, extract R code from Rmd: http://felixfan.github.io/extract-r-code/
  #files<-list.files(path)[str_detect(list.files(path), ".R$")]
  
  ## extract all functions and which package they are from 
  ## using NCmisc::list.functions.in.file
  funs<-   list.functions.in.file(path)#  unlist(lapply(paste0(path, "/", files), list.functions.in.file))
  packs<-funs %>% names()
  
  ## "character" functions such as reactive objects in Shiny
  characters<-packs[str_detect(packs, "^character")]
  
  ## user defined functions in the global environment
  globals<-packs[str_detect(packs, "^.GlobalEnv")]
  
  ## functions that are in multiple packages' namespaces 
  multipackages<-packs[str_detect(packs, ", ")]
  
  ## get just the unique package names from multipackages
  mpackages<-multipackages %>%
    str_extract_all(., "[a-zA-Z0-9]+") %>%
    unlist() %>%
    unique()
  mpackages<-mpackages[!mpackages %in% c("c", "package")]
  
  ## functions that are from single packages
  packages<-packs[str_detect(packs, "package:") & !packs %in% multipackages] %>%
    str_replace(., "[0-9]+$", "") %>%
    str_replace(., "package:", "") 
  
  ## unique packages
  packages_u<-packages %>%
    unique() %>%
    union(., mpackages)
  
  return(list(packs=packages_u))
  
}
wd= getwd()

files<-list.files(file.path(wd,"R"),full.names=T)[str_detect(list.files(file.path(wd,"R"),full.names=T), ".Rmd$")]  
dir.create(paste0(wd,"/DQ/r_files"), showWarnings = FALSE)
lapply(files, 
       function(x) knitr::purl(x, 
                               output = paste0(
                                 wd,
                                 "/DQ/r_files/",
                                 gsub(".Rmd", 
                                      ".R",
                                      basename(x )
                                 )
                                )))
files2<-list.files(paste0(wd,"/DQ/r_files/"),
                   full.names=T)[str_detect(list.files(paste0(wd,"/DQ/r_files/"),full.names=T), ".R$")]  

paths <- c(paste0(wd,"/app.R"),
           files2)
  
libraries_used <- unique(unlist(lapply(paths, function(x) checkPacks(x)$packs)))

packages_loading <- c("devtools", "remotes", "spatialutils", "rgeoboundaries", "rhdx", "pacman",
  'tidyverse',
  'dplyr',
  'rgdal',
  'sf',
  'sp',
  "tmap",
  "tmaptools",
  "raster",
  'doParallel', 
  "exactextractr",
  "terra",
  "h3jsr",
  "spatialutils",
  "shiny",
  "bs4Dash", 
  "shinyWidgets",
  "htmlwidgets",
  "shinythemes",
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
  "rmapshaper"
)

packages_loading[! packages_loading %in% libraries_used]





