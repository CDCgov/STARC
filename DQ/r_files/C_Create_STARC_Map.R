## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Banner when running App
if( exists("app_run")) {
   showModal(modalDialog("Generating Maps", footer=NULL))
}


## ----setup, include=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------
time_start <- Sys.time()
if(! exists("app_run")) {
#Establish Root Directory
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
#Parameters
countries="Burundi"
}



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Set Paths
country<- countries 
country_folder_in <- paste("Inputs/Country_Files/",country,sep="")
country_folder_out <- paste("Outputs/Country_Outputs/",country,sep="")

# Read road layer and ctry border file
roads = read_sf(paste(country_folder_in, "/gis_osm_roads_free_1.shp",sep="" ))
ctry_border = read_sf(paste(country_folder_in, "/borders/admin_0_rgeo.shp",sep="" ))
regions = read_sf(paste(country_folder_in, "/borders/admin_1_rgeo.shp",sep="" ))

# Auto define best crs projection using crsuggest. Then transform border file
country_project = suggest_crs(ctry_border)[1,]
country_crs = country_project$crs_code
ctry_border =st_transform(ctry_border, crs=as.numeric(country_crs))


STARC= read_sf( paste(country_folder_out, "/STARC_Map.shp",sep="" ))

STARC = STARC %>% mutate(strc_cd = factor(strc_cd, levels = c("S1.1","S1.2","S2.1","S2.2",
                                    "S1.3","S2.3","S3.1","S3.2","S3.3",
                                    "S1.4","S2.4","S4.1","S4.2","S3.4","S4.3","S4.4",
                                    "S5.1","S5.2","S5.3","S5.4","S6.1","S6.2","S6.3","S6.4"),
                                             labels = c("S1.1","S1.2","S2.1","S2.2",
                                    "S1.3","S2.3","S3.1","S3.2","S3.3",
                                    "S1.4","S2.4","S4.1","S4.2","S3.4","S4.3","S4.4",
                                    "S5.1","S5.2","S5.3","S5.4","S6.1","S6.2","S6.3","S6.4")))



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Use OpenStreetMap library to read in the OSM given extend of ctry_border file
osm_NLD <- tmaptools::read_osm(ctry_border, ext=1.1)

# Assigning an ordered numeric ID to the STARC Codes. This is needed for tmap_polygons() to recognize the order of STARC codes when assigning pallette colors 
strc_cdz=sort(unique(STARC$strc_cd))
numz=seq(1,length(strc_cdz),1)



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Condense the STARC map to expedite map making. 
    #Create a terra spatial vector from the STARC file. Then aggregate the STARC units by the numerical strcc_ode id created above
terra_STARC <- vect(STARC) 
dissolved_terra_STARC <- terra::aggregate(terra_STARC, by="strc_cd", cores=6,dissolve=T,fun="sum", count=T)

# Reconvert terra spatial file to sf so can use it in TMAP
STARC_unify <- sf::st_as_sf(dissolved_terra_STARC)
 STARC_unify2 <- rmapshaper::ms_simplify(input = as(STARC_unify, 'Spatial')) %>%
   st_as_sf()

 STARC_unify2 <- rmapshaper::ms_simplify(input = STARC_unify,
                                        keep=0.8,
                                        keep_shapes=T) %>%
   st_as_sf()

Road_layer <- roads %>% filter(Road_class=="Primary" | Road_class=="Secondary")

# st_write(STARC_unify, paste0(country_folder_out, "/mSTARC_unify.shp" ))
st_write(STARC_unify2, paste0(country_folder_out, "/map_starc_unify.shp" ), append = FALSE)



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
map2 <- tm_shape(osm_NLD) + tm_rgb()+
  tm_shape(ctry_border) + 
    tm_borders(lwd =1) + 
    tm_fill(col="white",alpha=0.6)+
  tm_shape(STARC_unify) + 
    tm_polygons(col="strc_cd", 
              border.alpha=0, 
              palette="Spectral",
              style = "cat",
              # breaks = starc_pallette$code,
              title="STARC Codes", 
              legend.hist=F) +
  tm_shape(regions) + 
    tm_borders(lwd =0.5, col="black") +
  tm_shape(Road_layer) +
    tm_lines(lwd=0.5, col="Road_class", palette=c("brown", "blue"), lty="dashed",legend.col.show=T, legend.lwd.show=F) + 
  tm_layout(
    main.title=paste(country,": STARC MAP", sep=""),
    main.title.position = c('center', 'top'),
    legend.outside = TRUE, 
    legend.outside.position = "right")+ 
  tm_compass(type = "8star", size = 4, position = c("right", "top"))+
  tm_scale_bar()
#map2


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tmap_save(map2,paste0(country_folder_out, "/STARC_MAP_R_",country, ".png" ), dpi=900)

tmap_options(check.and.fix = TRUE)
tmap_save(map2, paste0(country_folder_out, "/STARC_MAP_R_",country, ".html" ))


beep(1)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# End banner when running App
if( exists("app_run")) {
removeModal()
}

