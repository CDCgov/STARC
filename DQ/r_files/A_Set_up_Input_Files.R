## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
source("R/Libraries_Dependencies.R")



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Banner when running App
if( exists("app_run")) {
   showModal(modalDialog("Generating Inputs", footer=NULL))
}


## ----setup, include=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------
if(! exists("app_run")) {
#Establish Root Directory
knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
#Parameters
countries="Burundi"
continent="africa"
regen_inputs="True"
Low_rast_rest="True"
input_runs=c("OSM Roads", "Pop Raster", "Hex Grids")
}


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Set directory
if("OSM Roads" %in% input_runs){
  run_osm_shp_TF="True"
} else{run_osm_shp_TF="False"}

if("Pop Raster" %in% input_runs){
  run_pop_rast_TF="True"
} else{run_pop_rast_TF="False"}

if("Hex Grids" %in% input_runs){
  run_hex_grid_TF="True"
} else{run_hex_grid_TF="False"}



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Set directory
dest_borders <- paste0("Inputs/Country_Files/", countries, "/borders")
dir.create(dest_borders, recursive = TRUE, showWarnings =F)
# If the relevant Border file does not exist, download it
if (!any(grepl('admin_0_rgeo.shp$',list.files(dest_borders)))){
  ctry= geoboundaries(countries, adm_lvl=0,type="HPSCU", quiet=F)
  st_write(ctry, paste0(dest_borders, "/admin_0_rgeo.shp"),append=FALSE)
} else {ctry = st_read( paste0(dest_borders, "/admin_0_rgeo.shp"))}

if (!any(grepl('admin_1_rgeo.shp$',list.files(dest_borders)))){
  sub_reg1= geoboundaries(countries, adm_lvl=1,type="HPSCU", quiet=F)
  st_write(sub_reg1, paste0(dest_borders, "/admin_1_rgeo.shp"),append=FALSE)
}

if (!any(grepl('admin_2_rgeo.shp$',list.files(dest_borders)))){
  sub_reg2= geoboundaries(countries, adm_lvl=2,type="HPSCU", quiet=F)
  st_write(sub_reg2, paste0(dest_borders, "/admin_2_rgeo.shp"),append=FALSE)
}
# Use crsuggest:suggest_crs() to select CRS to transform this border file to. This defines CRS for rest of analysis 
country_project = suggest_crs(ctry, gcs = 4326)[1,]
country_crs = country_project$crs_code
# transform to suggested crs
ctry_border =st_transform(ctry, crs=as.numeric(country_crs))



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#osm_layer_name will normally just be the country's name, but there are exceptions (see below)
osm_layer_name = gsub(" ","-",ifelse(countries=="Haiti" | countries=="Dominican Republic", "haiti-and-domrep", 
                        ifelse(countries=="South Africa" | countries=="Lesotho", "south-africa",
                               ifelse(countries=="Democratic Republic of the Congo", "congo-democratic-republic", ifelse(countries=="Senegal" | countries=="Gambia", "senegal-and-gambia", countries)))))

countries_osm_label = osm_layer_name





## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Inspitation source: https://gis.stackexchange.com/questions/375345/dividing-polygon-into-parts-which-have-equal-area-using-r
split_poly <- function(sf_poly, n_areas){
  # create random points
  points_rnd <- st_sample(sf_poly, size = 10000)
  #k-means clustering
  points <- do.call(rbind, st_geometry(points_rnd)) %>%
    as_tibble() %>% setNames(c("lon","lat"))
  k_means <- kmeans(points, centers = n_areas)
  # create voronoi polygons
  voronoi_polys <- dismo::voronoi(k_means$centers, ext = sf_poly)
  # clip to sf_poly
  crs(voronoi_polys) <- crs(sf_poly)
  voronoi_sf <- st_as_sf(voronoi_polys)
  equal_areas <- st_intersection(voronoi_sf, sf_poly)
  equal_areas$area <- st_area(equal_areas)
  return(equal_areas)
}



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
get_inputs <- function(country,run_osm_shp,run_pop_rast, run_hex_grid, replace){
  
  paste("Processing ", country, "/n")
  #Establish directories
  temp_dir <- tempdir()
  country_folder <- country
  dest_dir <- paste("Inputs/Country_Files",country_folder,sep="/")
  dir.create(dest_dir, recursive = TRUE, showWarnings =F)
  
  ## Download OMS Road files
  #Only proceed if parameter run_osm_shp set to true
  if (run_osm_shp=="True"){  
    #if File exists and replace is fale, do not proceed
    if( replace=="False" & any(grepl('.shp.zip$',list.files(dest_dir)))){
      paste("gis_osm_roads_free_1.shp already exists for: ",country_folder, " and replace = F",sep="" )
    }  else{
      #Format correct link to download osm file from geofabrik
      countries_osm_label = osm_layer_name#NOTE For some countries you may need to adjust the osm_layer_name
      osm_path <- paste("http://download.geofabrik.de/",tolower(continent),"/",tolower(countries_osm_label), "-latest-free.shp.zip",sep="")
      #Download File
      if(grepl('http',osm_path)){
        dest <- paste0(dest_dir,"/",basename(osm_path))
        utils::download.file(osm_path, dest, quiet=F, method = "libcurl")#,download.file.method="curl", url.method="curl",
        osm_path <- dest
      }
      #Find out what type of file (should be zip)
      file_names <- grep('gis_osm_roads_free_1', unzip(osm_path, list=TRUE)$Name, 
                         ignore.case=TRUE, value=TRUE)
      #Untar file based on type
      if(grepl('.tgz$|.tar.gz$',osm_path)){
        utils::untar(osm_path, files=file_names, exdir = dest_dir)
      } else if(grepl('.zip$',osm_path)){
        utils::unzip(osm_path, files=file_names, exdir = dest_dir)
      } else{
        stop('Unsupported filetype')
      }
      
      shape_name = grep('.shp$',list.files(dest_dir),value=T)
      
      #Categorize primary-tertiary road layers as defined in the protocol 
      shp_osm_readz <- read_sf(dsn = paste(dest_dir,"/gis_osm_roads_free_1.shp",sep=""))
      primary <- c("primary", "primary_link", "service")
      secondary <- c("secondary", "secondary_link", "tertiary", "tertiary_link")
      tertiary <- c("bridleway", "cycleway", "footway", "living_street", "path", "pedestrian", "steps", "track", "track_grade1", "track_grade2", "track_grade3", "track_grade4", "track_grade5","unclassified", "unknown")
      #Apply Categories to data to create "Road_class" column
      osm_dataz<- shp_osm_readz %>% dplyr::mutate(Road_class = ifelse(fclass %in% primary, "Primary", 
                                                                      ifelse(fclass %in% secondary, "Secondary",
                                                                             ifelse(fclass %in% tertiary, "Tertiary",
                                                                                    NA)))
      ) %>% dplyr::select(fclass,Road_class, geometry )  %>% dplyr::filter(! is.na(Road_class) )
      #Transform osm data to the crs that the national border was transformed to
      osm_dataz =st_transform(osm_dataz, crs=as.numeric(country_crs))
      #Save the osm data 
      st_write(osm_dataz, paste(dest_dir,"/gis_osm_roads_free_1.shp",sep=""), overwrite=T, append=F)
      #Save a text file updating the time that the shape file for OSM roads was updated 
      text_directory <- paste(dest_dir, "/osm_last_updated.txt", sep="")
      fileConn<-file(text_directory)
      writeLines(c("Last Updated",paste(Sys.Date() )), fileConn)
      close(fileConn)

      }# end else
  }
  ## End OSM Section
  
  ## Download OMS Population Raster Files
  if(run_pop_rast=="True"){
    if( replace=="False" & any(grepl('pop.tif',list.files(dest_dir)))){
      paste("population raster already exists for: ",country_folder, " and replace = F",sep="" )
    } else {
      # Establish data type (Room to allow for multiple different Pop sources to choose from for download...TBD)
      data_types = c("Population Raster")
      data_type="Population Raster"
      data_typez = ifelse(data_type==data_types[1], " high-resolution-population-density-maps-demographic-estimates", NA)
      data_typez_comb = paste(tolower(country), data_typez, sep="")
      # Search rhdx file index
      df_connect = rhdx::search_datasets(data_typez_comb, 
                                         rows =1) %>%  pluck(1)## search dataset in HDX, limit the results to two rows
      # Exceptions for Haiti 
      if(country=="Haiti"){
        pop_tif_url=df_connect$data$resources[which( grepl("population_hti_2018-10-01",df_connect$data$resources) & 
                                                       grepl("geotiff.zip" , df_connect$data$resources)
        )]  [[1]]$url
      } else{
        pop_tif_url=df_connect$data$resources[which( grepl("general",df_connect$data$resources) & 
                                                       grepl("geotiff.zip" , df_connect$data$resources)
        )]  [[1]]$url
      }
      # Establish output path
      pop_pathz <- paste0(dest_dir,"/", country,"_hdx_pop.zip")
      # Download the file
      utils::download.file(pop_tif_url,pop_pathz,method = "libcurl")
      # Identify file types
      file_names <- grep('general', unzip(pop_pathz, list=TRUE)$Name,  ignore.case=TRUE, value=TRUE)
      file_names<- file_names[grepl("^[^.]*[.][^.]*$", file_names)] #filter down to only files that have one "."
      # Untar based on file type
      if(grepl('.tgz$|.tar.gz$',pop_pathz)){
        utils::untar(pop_pathz, files=file_names, exdir = dest_dir)
      } else if(grepl('.zip$',pop_pathz)){
        utils::unzip(pop_pathz, files=file_names, exdir = dest_dir)
      } else{
        stop('Unsupported filetype')
      }
      # If there are already pop.tif files, remove them
      if (any(grepl("pop.tif", list.files(dest_dir)))){
        file.remove(paste(dest_dir,"/pop.tif", sep=""))
      }
      # Save the new rhdx file as pop.tif
      file.rename(paste(dest_dir,"/",file_names,sep=""), paste(dest_dir,"/pop.tif",sep="") )
      # Create text file for when tif last updated
      text_directory <- paste(dest_dir, "/tif_last_updated.txt", sep="")
      fileConn<-file(text_directory)
      writeLines(c("Last Updated",paste(Sys.Date() )), fileConn)
      close(fileConn)
    }
  }
  ## End OSM Section
  
  ## Download H3 Hexgrids
  if(run_hex_grid=="True"){
    if( replace=="False" & any(grepl('H3_hex_grid.shp',list.files(dest_dir)))){
      paste("hex grid already exists for: ",country_folder, " and replace = F",sep="" )
    }  else {  
      # This buffers shapefile outline by 200 meters so that H3 method will create hexagons along the edges of the true border. H3 won't make partial hexagons. We then clip the hexagons to the true border to create partial "hexagons" ourselves
      ctry_buffer <-st_buffer(ctry_border, dist=200)
      #Split the country border boundary into 6 equal parts. 
      #This is to prevent polygon_to_cells() from crashing when running large countries
      num_splits = 6
      pol_areas <- split_poly(ctry_border, num_splits) # Equal splits
      a=  Sys.time()
      #Maintain list of hexes (prevent duplicates)
      hex_gridz ={}
      hexes =c("fsad")
      #Create H3 Hex grids for each split 
      for (i in 1:num_splits){
        #Once again buffer each split to prevent some Hexagons not generating
        pol = st_buffer(pol_areas %>% filter(id==i),500)
        #Create hexes res 8
        h3_idz <- polygon_to_cells(pol$geometry, 
                                   res = 8,  # Defines average area (around 0.887 km^2)
                                   simple = TRUE)
        #Only include hexes not already generated in a previous split (prevent duplication)
        #All hexes have a unique id based on geolocation so can use this id to prevent duplicates
        hex_gridz[[i]] <- h3_idz[ ! h3_idz %in% hexes] %>% cell_to_polygon( simple = F)
        hexes <- c(hexes, h3_idz)
        Sys.sleep(5) #helps prevent crashing
      }
      #Concat all hexes
      hex_grids = do.call(rbind, hex_gridz)
      hex_grids <- hex_grids %>% group_by(h3_address) %>% slice_sample(n = 1) # get uniqur H# ids
      #Transform to the same crs as country border
      hex_grids <- st_transform(hex_grids, st_crs(ctry_border))
      # Crop the hex file to the country border boundary file
      hex_grid <- st_crop(hex_grids, ctry_border)
      b=Sys.time()
      b-a
      #Save the buffered country file
      #system.time(st_write(ctry_buffer, dsn = paste(dest_dir, "/country_buffer.shp",sep=""),driver ="ESRI Shapefile", append=F, overwrite=T)) #62.97 
      # Save the final hex file
      st_write(hex_grid, paste0(dest_dir, "/H3_hex_grid.shp"), overwrite=T, append=F)
    }
  }
  ## End H3 Hexes Section
  gc()
} # End function





## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
options(timeout=8000) # how many seconds are you willing to let download run for. If this is not set to higher than the default of 60 the download for larger files will time out

mapply(get_inputs,
       run_osm_shp = rep(run_osm_shp_TF,length(countries)), # Set True if you want to download osm shapefiles. Even if True, Replace =T/F will supersede
       run_pop_rast = rep(run_pop_rast_TF,length(countries)),# Set True if you want to download Pop raster files. Even if True, Replace =T/F will supersede
       run_hex_grid=rep(run_hex_grid_TF, length(countries)),
       country= countries,
       # osm_path= urls_osm,
       # pop_path= urls_pop,
       replace=rep(paste(regen_inputs),length(countries))# Put "False" if you do NOT want to replace any already existing files. Otherwise downloads of already existing files will be replaced
       )

beep(1)



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# End banner when running App
if( exists("app_run")) {
removeModal()
}

