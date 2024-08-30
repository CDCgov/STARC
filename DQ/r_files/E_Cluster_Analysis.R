## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Banner when running App
if( exists("app_run")) {
   showModal(modalDialog("Generating Clusters and Maps", footer=NULL))
}


## ----setup, include=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------
time_start <- Sys.time()
if(! exists("app_run")) {
  #Establish Root Directory
  knitr::opts_knit$set(root.dir = rprojroot::find_rstudio_root_file())
  #Parameters
  countries="Burundi"
  strc_include = c("S1.1","S1.2",  "S1.3",  "S1.4",  "S2.1",  "S2.2",  "S2.3",  "S2.4",  "S3.1",  "S3.2",  "S3.3", "S3.4") # DEFAULT
  # strc_include = c("S1.1","S1.2",  "S1.3",  "S1.4",  "S2.1",  "S2.2",  "S2.3", "S2.4",  "S3.1",  "S3.2") # SMALL COUNTRY (Burundi, Haiti, Guatemala, etc)
  buffer_size=500 # 500 default, 100 small country
  
}
dogdensfx = FALSE


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

country_folder <- paste("Inputs/Country_Files/",countries,sep="")

output_folder <- paste0("Outputs/Country_Outputs/",countries )

folder <- paste0("Outputs/Country_Outputs/",countries, "/Clusters" )

dir.create(folder, recursive = TRUE)



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ctry_border = st_read(paste0(country_folder,"/borders/admin_0_rgeo.shp" ))

# best CRS for this country
country_project = suggest_crs(ctry_border, gcs = 4326)[1,]
country_crs = country_project$crs_code

# transform the country border to best crs
ctry_border =st_transform(ctry_border, crs=as.numeric(country_crs))

# Read in the regions
regions = st_read(paste0(country_folder,"/borders/admin_1_rgeo.shp" ))
# Convert regions to best crs
regions = st_transform(regions, crs=as.numeric(country_crs)) %>% dplyr::rename(shapeName=shapeNm ,shapeGroup=shapGrp, shapeType=shapTyp   ) %>% dplyr::select(shapeName, shapeGroup, shapeType ) #  




## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
starc_code_2 = st_read(paste(folder, "/starc_code_gi.shp", sep=""))
starc_code_2 =st_transform(starc_code_2, crs=as.numeric(country_crs))



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# STARC hexagons that fit the strc_cd criteria and are NOT in a hot spot
starc_code_not_Gi = starc_code_2 %>% dplyr::filter(gi_sig !="High" | is.na(gi_sig)) %>% dplyr::filter(strc_cd %in%strc_include)

# All STARC Hexagons that are in a hotspot (reglardless of starc code)
starc_code_Gi=starc_code_2 %>% dplyr::filter(gi_sig =="High")

# The below should be CLOSE to 0
nrow(starc_code_2 %>% filter(RUC %in% c("S1", "S2", "S3"))) - (nrow(starc_code_not_Gi) + nrow(starc_code_Gi)) 


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# buffer the hexagons that are significant according to Gi statistic
starc_code_Gi_buffer = st_buffer(starc_code_Gi, buffer_size)

# Union those that intersect (logic is that "hotspots" aren't really separate if they are this close by)
starc_code_Gi_intersect  <- st_union(starc_code_Gi_buffer)

# Convert above to sf object and create a unique ID for these unioned GI hexagons
starc_code_Gi_intersect <- st_cast(starc_code_Gi_intersect, "POLYGON")
starc_code_Gi_intersect <- st_as_sf (starc_code_Gi_intersect)%>% 
  dplyr::mutate (H_spot_ID=row_number()) # THIS IS AN IMPORTANT ID!!



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Buffer all the hexagons that are NOT Gi significant and are in the starc code list
starc_code_not_Gi_buffer= st_buffer(starc_code_not_Gi, buffer_size)


# Union the normal buffered hexagons. Outcome is all normal hexagons that intersect with each other 
starc_code_not_Gi_buffer = st_buffer(starc_code_not_Gi, buffer_size) 
starc_code_not_Gi_unions<- st_union(starc_code_not_Gi_buffer)
starc_code_not_Gi_unions <- st_cast(starc_code_not_Gi_unions, "POLYGON")
starc_code_not_Gi_unions <- st_as_sf (starc_code_not_Gi_unions)%>% 
  dplyr::mutate (temp_id=row_number())



# Identify which non-gi significant unioned buffered hexagons intersect with the unioned Hotspot polygons
system.time(acl <- st_intersects(starc_code_not_Gi_unions$x,
                                 starc_code_Gi_intersect$x, sparse = T))#41.63  
prim<- aggregate(col.id ~ row.id, 
                 data = data.frame(acl), 
                 FUN = paste, 
                 collapse = "; ")%>%rename("Poly_id_not_GI"="row.id", "GI_poly_id"= "col.id")
starc_code_not_Gi_unions_gi_sig <- starc_code_not_Gi_unions%>% filter(temp_id %in% prim$Poly_id_not_GI)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Rbind and then st_union() to arrive at our final CLUSTER outlines
starc_code_not_Gi_unions_gi_sig$H_spot_ID=NA # create NA column so we can rbind
starc_code_Gi_intersect$temp_id = NA # create NA column so we can rbind
rbinds= rbind(starc_code_not_Gi_unions_gi_sig,starc_code_Gi_intersect )
starc_code_unions = st_union(rbinds)


# Recast as sf and create CLUSTER ID formed from the above union
starc_code_unions <- st_cast(starc_code_unions, "POLYGON")
starc_code_unions <- st_as_sf (starc_code_unions)%>% 
  dplyr::mutate (cluster_id=row_number()) # THIS IS AN IMPORTANT ID!!




## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Create an object that contains ALL hexagons (with population) that do not fit into a hot spot
starc_code_not_Gi_all = starc_code_2 %>% dplyr::filter(gi_sig !="High" | is.na(gi_sig))  %>% dplyr::filter(RUC !="S6")
#Create a temporary ID for starc_code_not_Gi_all. 
starc_code_not_Gi_all$temp_row_id =  seq_along(data.frame(starc_code_not_Gi_all%>%st_drop_geometry())[,2])

# Identify which hexagons in starc_code_not_Gi_all are contained within the general cluster boundary (starc_code_unions)
## NOTE: we are specifying st_contains(), so ONLY hexagons that are completely contained. This can be chnanged if desired, just swap out the funciton with st_intersects
system.time(acl <-st_contains(starc_code_unions, starc_code_not_Gi_all,
                                 sparse = T) )
prim<- aggregate(col.id ~ row.id, 
                 data = data.frame(acl), 
                 FUN = paste, 
                 collapse = "; ")%>%rename("unioned_polys"="row.id", "hexes_not_gi"= "col.id")

non_gi_hexes_within_clusters = unique(unlist(strsplit(prim$hexes_not_gi, "; "))) # unlist the values into a singular list

hexes_starc_code_not_Gi<- starc_code_not_Gi_all %>% filter(temp_row_id %in% non_gi_hexes_within_clusters) # filter to match temp id to that list


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
hexes_starc_code_Gi2 <-  st_join(starc_code_Gi,starc_code_Gi_intersect) # get's the Hot spot poly id for each hexagon. 
hexes_starc_code_Gi2 <- hexes_starc_code_Gi2 %>% dplyr::select(-temp_id) %>% st_join(starc_code_unions)
hexes_starc_code_Gi2 <- hexes_starc_code_Gi2 %>% filter(!is.na(cluster_id))

hexes_starc_code_not_Gi2<- hexes_starc_code_not_Gi %>% mutate(H_spot_ID =NA) %>%st_join(starc_code_unions) 
hexes_starc_code_not_Gi2 <- hexes_starc_code_not_Gi2 %>% dplyr::select(-temp_row_id)  %>% filter(!is.na(cluster_id))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
hexes_cluster_final = rbind(hexes_starc_code_Gi2, hexes_starc_code_not_Gi2)


hexes_cluster_final <- hexes_cluster_final %>% mutate(Hot_dgp  = ifelse(!is.na(H_spot_ID),dg_pp_F, 0 )) # create a column for dog pop in hot spot



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

clusters_r0 = hexes_cluster_final %>%
  group_by(cluster_id) %>%
  summarise(dg_pp_F = sum(dg_pp_F),
            ar_sqkm = sum(ar_sqkm)) %>%
  mutate(dg_dns_F = (dg_pp_F / ar_sqkm),
         r0 = 0.1673*log(dg_dns_F)+1.0075, #.34 * log(dg_dns_F),
         risk = ifelse(r0>1 & dg_pp_F>=500,1,0))

table(clusters_r0$risk, useNA = "always")



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

clusters_separated = starc_code_unions %>% mutate(risk = NA)

for (i in 1:nrow(clusters_separated)) {
  clusters_separated$risk[i] = clusters_r0$risk[clusters_r0$cluster_id==clusters_separated$cluster_id[i]]
}

# buffer from cluster outlines 2km (assumed average FRD range)
cluster_buffer = st_buffer(clusters_separated, 2000)

# separate at risk and sus
cluster_buffer_risk = cluster_buffer %>% filter(risk==1)
cluster_buffer_sus = cluster_buffer %>% filter(risk==0)

# union intersecting at risk clusters
transmission_zones1 = st_union(cluster_buffer_risk)
transmission_zones1 = st_cast(transmission_zones1, "POLYGON") 
transmission_zones1 = st_as_sf(transmission_zones1) %>% 
  dplyr::mutate(zone_id=row_number())

# union with only overlapping susceptible
if (nrow(cluster_buffer_sus)>0){
cluster_buffer_sus2  = cluster_buffer_sus %>% mutate(intersect = NA)
if (nrow(transmission_zones1)>0){
for (i in 1:nrow(cluster_buffer_sus2)) {
  cluster_buffer_sus2$intersect[i] = ifelse(st_intersects(st_union(transmission_zones1), cluster_buffer_sus2$x[i]),1,0)
}
}

table(cluster_buffer_sus2$intersect, useNA = "always")

cluster_buffer_sus_inter = cluster_buffer_sus2 %>% filter(intersect==1 & !is.na(intersect))

transmission_zones2 = rows_append(transmission_zones1, cluster_buffer_sus_inter%>%dplyr::select(x))
transmission_zones = st_union(transmission_zones2)
transmission_zones = st_cast(transmission_zones, "POLYGON") 
transmission_zones = st_as_sf(transmission_zones) %>% 
  dplyr::mutate(zone_id=row_number())

# union non overlapping susceptible to make susceptible zones
cluster_buffer_sus_noninter = cluster_buffer_sus2 %>% filter(is.na(intersect))

susceptible_zones = st_union(cluster_buffer_sus_noninter)
susceptible_zones = st_cast(susceptible_zones, "POLYGON") 
susceptible_zones = st_as_sf(susceptible_zones) %>% 
  dplyr::mutate(zone_id=row_number()+nrow(transmission_zones))

# combine to one data frame
all_zones = rows_append(transmission_zones%>%mutate(level="Transmission"),
                        susceptible_zones%>%mutate(level="Susceptible"))
} else{
  all_zones = transmission_zones1%>%mutate(level="Transmission")
}

# just checking what things are
# tm_shape(clusters_separated) + tm_fill("MAP_COLORS")
# # tm_shape(cluster_buffer) + tm_fill("MAP_COLORS")
# tm_shape(transmission_zones1) + tm_fill("red", alpha = .5) +
#   # tm_shape(susceptible_zones) + tm_fill("orange", alpha = .5) +
#   # tm_shape(cluster_buffer_sus) + tm_fill("seagreen", alpha = .5) +
#   tm_shape(cluster_buffer_risk) + tm_fill("dodgerblue", alpha = .5)



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#identify which clusters each zone covers
inters_z <- st_intersection(all_zones,clusters_separated)
# Groupby the ID and concatenate strings of regions
inters_z <- inters_z %>% 
  group_by(zone_id) %>% 
  summarise(item = str_c(cluster_id, collapse = ", "),
                 num_clusters = length(unique(cluster_id)[!is.na(unique(cluster_id) )]),
            level = level[1])
#Create dataframe of zone:cluster pairings
inters_zones<- data.frame(inters_z) %>% dplyr::select(zone_id, Clusters=item, num_clusters, level)


#identify which zone each cluster is in
# inters_c <- st_intersection(clusters_separated,all_zones)
# # Groupby the ID and concatenate strings of regions
# inters_c <- inters_c %>% 
#   group_by(cluster_id) %>% 
#   summarise(item = str_c(zone_id, collapse = ", "))
# #Create dataframe of zone:cluster pairings
# inters_cz<- data.frame(inters_c) %>% dplyr::select(cluster_id, Zone=item )

# Get which zone each cluster is mostly in
inters_c <- st_join(clusters_separated,
                   all_zones,
                   join = st_intersects,
                   largest = T) %>% # Largest=T is key for not DUPLICATING region-cluster pairings. Otherwise all regions the cluster intersects would be included in the results, rather than just the cluster-region pairing that the cluster most intersects
  dplyr::select(colnames(clusters_separated),
                Zone=zone_id)

inters_cz <- inters_c %>% rowwise() %>%
  mutate(zone_level = inters_zones$level[inters_zones$zone_id==Zone])



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Summarise the joined file to create summary statistics for each cluster
cluster_stats <- hexes_cluster_final %>% dplyr::group_by(cluster_id) %>% dplyr::summarise(geometry=st_union(geometry),
                 population = round(sum(pop_sum, na.rm=T),digits=2),
                 area = sum(ar_sqkm, na.rm=T),
                 # Dog Population
                 D_pop_s1 = round(sum(Dgs_s1_F, na.rm=T),digits=2),
                 D_pop_s2=round(sum(Dgs_s2_F, na.rm=T),digits=2),
                 D_pop_s3=round(sum(Dgs_s3_F, na.rm=T),digits=2),
                 D_pop_s4=round(sum(Dgs_s4_F, na.rm=T),digits=2),
                 D_pop_s5=round(sum(Dgs_s5_F, na.rm=T),digits=2),
                 Dog_pop = round(sum(dg_pp_F, na.rm=T),digits=2),
                 Hot_dgp = round(sum(Hot_dgp, na.rm=T),digits=2),
                 hot_spots =paste(sort(unique(H_spot_ID)),collapse=", "),
                 num_hot_spots = length(unique(H_spot_ID)[!is.na(unique(H_spot_ID) )])
                 )

# Additional calculations 
cluster_stats <- cluster_stats %>% mutate(
                            hu_dens = round (population/area, digits=2),
                            dog_dens =round (Dog_pop/ area, digits=2),
                            Rep_O = 0.1673*log(dog_dens)+1.0075,
                            R0 = NA, risk = NA
                             )

for (i in 1:nrow(cluster_stats)) {
  cluster_stats$R0[i] = clusters_r0$r0[clusters_r0$cluster_id==cluster_stats$cluster_id[i]]
  cluster_stats$risk[i] = clusters_r0$risk[clusters_r0$cluster_id==cluster_stats$cluster_id[i]]
}



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#Summarise the joined file to create summary statistics for each hot spot
hot_spot_stats <- hexes_cluster_final %>% filter(! is.na(H_spot_ID)) %>% dplyr::group_by(H_spot_ID) %>% dplyr::summarise(geometry=st_union(geometry),
                 population = round(sum(pop_sum, na.rm=T),digits=2),
                 area = sum(ar_sqkm, na.rm=T),
                 # Dog Population
                 D_pop_s1 = round(sum(Dgs_s1_F, na.rm=T),digits=2),
                 D_pop_s2=round(sum(Dgs_s2_F, na.rm=T),digits=2),
                 D_pop_s3=round(sum(Dgs_s3_F, na.rm=T),digits=2),
                 D_pop_s4=round(sum(Dgs_s4_F, na.rm=T),digits=2),
                 D_pop_s5=round(sum(Dgs_s5_F, na.rm=T),digits=2),
                 Dog_pop = round(sum(dg_pp_F, na.rm=T),digits=2),
                 cluster =paste(sort(unique(cluster_id)),collapse=", ")

                 )
# Additional calculations 
hot_spot_stats <- hot_spot_stats %>% mutate(
                            hu_dens = round (population/area, digits=2),
                            dog_dens =round (Dog_pop/ area, digits=2),
                            Rep_O = 0.1673*log(dog_dens)+1.0075, 
                             )




## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
regions2<-  regions %>%st_transform(st_crs(cluster_stats))


#identify which regions each cluster is in
inters_ <- st_intersection(cluster_stats,regions2)
# Groupby the ID and concatenate strings of regions
inters_ <- inters_ %>% 
  group_by(cluster_id) %>% 
  summarise(item = str_c(shapeName, collapse = ", "))
#Create dataframe of id:region pairings
inters<- data.frame(inters_) %>% dplyr::select(cluster_id, Regions=item )

# Get which region each cluster is mostly in
inters2 <- st_join(cluster_stats, 
                   regions2, 
                   join = st_intersects, 
                   largest = T)%>% # Largest=T is key for not DUPLICATING region-cluster pairings. Otherwise all regions the cluster intersects would be included in the results, rather than just the cluster-region pairing that the cluster most intersects
  dplyr::select(colnames(cluster_stats),
                Region_majority=`shapeName`)

cluster_stats2 <- inters2 %>% merge(inters, by="cluster_id") %>% 
  dplyr::select(colnames(inters2), Regions_intersected=Regions)




## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
quants = quantile(cluster_stats2$Hot_dgp, probs = seq(0, 1, 0.25))

cluster_stats2 <- cluster_stats2 %>% mutate(Rep_pri= ifelse(Hot_dgp>=quants[4], "primary", # greater than or equal to  top 75%
                                                                     ifelse(Hot_dgp<quants[4] & Hot_dgp>=quants[2], "secondary", # greater than or equal to top 25% and smaller than top 75% 
                                                                     ifelse(Hot_dgp<quants[2] , "tertiary",NA))) # less than top 25% (AKA bottom 25 %)
                                                            )
table(cluster_stats2$Rep_pri, useNA="always")

cluster_stats2 <- cluster_stats2%>% 
  mutate(Cluster_Label= ifelse(Rep_pri =="primary", paste(cluster_id, " (*1)", sep=""),
                            ifelse(Rep_pri=="secondary", paste(cluster_id, " (*2)", sep=""), 
                                   ifelse(Rep_pri=="tertiary", paste(cluster_id, " (*3)", sep=""), NA)))
  )







## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# get highest cluster level
cluster_zone_stats = inters_cz %>% mutate(Rep_pri = NA)
for (i in 1:nrow(cluster_zone_stats)) {
  cluster_zone_stats$Rep_pri[i] = cluster_stats2$Rep_pri[cluster_stats2$cluster_id==cluster_zone_stats$cluster_id[i]]
}
cluster_zone_stats2 = cluster_zone_stats %>% 
  group_by(Zone) %>% 
  summarise(item = str_c(Rep_pri, collapse = ", ")) %>%
  mutate(Rep_pri = ifelse(grepl("primary",item),"primary",
                          ifelse(grepl("secondary",item),"secondary","tertiary"))) %>%
  rename(zone_id=Zone)

zone_stats = inters_zones %>% merge(cluster_zone_stats2,by="zone_id") %>%
  dplyr::select(-item)



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

zone_stats2 = cluster_stats2 %>% mutate(Zone = NA)
for (i in 1:nrow(cluster_stats2)) {
  #if(zone_stats2$cluster_id[i]%in%cluster_zone_stats$cluster_id){
  zone_stats2$Zone[i] = as.numeric(cluster_zone_stats$Zone[cluster_zone_stats$cluster_id==zone_stats2$cluster_id[i]])
  #}
}

zone_stats3 = zone_stats2 %>% filter(!is.na(Zone)) %>% group_by(Zone) %>%
  summarise(Cluster_population = sum(population),
            Cluster_area = sum(area),
            Cluster_Dog_pop = sum(Dog_pop),
            Hot_Dog_pop = sum(Hot_dgp),
            hot_spots = str_c(hot_spots, collapse = ", "),
            num_hot_spots = sum(num_hot_spots)) %>%
  mutate(Cluster_hu_dens = round(Cluster_population/Cluster_area, digits=2),
         Cluster_dog_dens = round (Cluster_Dog_pop/ Cluster_area, digits=2),
         Cluster_Rep_O = 0.1673*log(Cluster_dog_dens)+1.0075, #round(0.35 * log(Cluster_dog_dens) + 0.1852, digits=2),
         level = NA)

for (i in 1:nrow(zone_stats3)) {
  zone_stats3$level[i] = inters_zones$level[inters_zones$zone_id==zone_stats3$Zone[i]]
}



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# indentify which hexes each zone covers
hexes_zones = st_intersection(all_zones, starc_code_2) %>% mutate(checkmult = 1)

multzones = hexes_zones %>% filter(duplicated(id))

if(nrow(multzones)>0){
for (i in 1:nrow(multzones)) {
  # m = multzones$id[i]
  sub = hexes_zones %>% filter(id==multzones$id[i])
  max = max(st_area(sub))
  hexes_zones$checkmult[st_area(hexes_zones)!=max & hexes_zones$id==multzones$id[i]] = 0
}
}

hexes_zones = hexes_zones %>% filter(checkmult!=0) %>% dplyr::select(-checkmult)

# Groupby the ID and sum stats
inters_zs <- hexes_zones %>%
  group_by(zone_id) %>%
  summarise(Human_pop = sum(pop_sum),
            area = sum(ar_sqkm),
            Dog_pop_ALL = sum(dg_pp_A),
            Dog_pop_FREE = sum(dg_pp_F)) %>%
  mutate(hu_dens = round(Human_pop/area, digits=2),
         dog_dens_ALL = round (Dog_pop_ALL/ area, digits=2),
         dog_dens_FREE = round (Dog_pop_FREE/ area, digits=2)) %>%
  rename(Zone = zone_id) %>% st_drop_geometry()

zone_stats3 = left_join(zone_stats3, inters_zs, by="Zone")



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#identify which regions each zone is in
inters_zr <- st_intersection(all_zones,regions2)
# Groupby the ID and concatenate strings of regions
inters_zr <- inters_zr %>% 
  group_by(zone_id) %>% 
  summarise(item = str_c(shapeName, collapse = ", "))
#Create dataframe of id:region pairings
inters_zr2 <- data.frame(inters_zr) %>% dplyr::select(zone_id, Regions=item )

# Get which region each cluster is mostly in
inters2_zr <- st_join(all_zones, 
                   regions2, 
                   join = st_intersects, 
                   largest = T)%>% # Largest=T is key for not DUPLICATING region-cluster pairings. Otherwise all regions the cluster intersects would be included in the results, rather than just the cluster-region pairing that the cluster most intersects
  dplyr::select(colnames(all_zones),
                Region_majority=`shapeName`)

zone_stats_regions <- inters2_zr %>% merge(inters_zr2, by="zone_id") %>%
  rename(Zone=zone_id) %>% st_drop_geometry()#%>% dplyr::select(colnames(inters2_zr),
                                                                                      # Regions_intersected=Regions)

# add regions and buffer stats to other zone stats
zone_stats4 = zone_stats3 %>% merge(zone_stats_regions, by="Zone") %>% mutate(Rep_pri = NA, Zone_Label = NA,
                                                                              clusters = NA, num_clusters = NA) %>%
  dplyr::select(-level.x) %>% rename(level=level.y)

# tm_shape(zone_stats4) + tm_fill(col = "Rep_pri") +
#   tm_shape(all_zones) + tm_fill("Rep_pri", alpha = .5)

# add priority level to zone stats and zone buffer layer and add cluster list
all_zones$Rep_pri = NA
for (i in 1:nrow(zone_stats4)) {
  
  zone_stats4$Rep_pri[i] = zone_stats$Rep_pri[zone_stats$zone_id==zone_stats4$Zone[i]]
  zone_stats4$Zone_Label[i] = ifelse(zone_stats4$Rep_pri[i] =="primary", paste(zone_stats4$Zone[i], " (*1)", sep=""),
                            ifelse(zone_stats4$Rep_pri[i]=="secondary", paste(zone_stats4$Zone[i], " (*2)", sep=""), 
                                   ifelse(zone_stats4$Rep_pri[i]=="tertiary", paste(zone_stats4$Zone[i], " (*3)", sep=""), NA)))
  
  zone_stats4$clusters[i] = zone_stats$Clusters[zone_stats$zone_id==zone_stats4$Zone[i]]
  zone_stats4$num_clusters[i] = zone_stats$num_clusters[zone_stats$zone_id==zone_stats4$Zone[i]]
  
  all_zones$Rep_pri[i] = zone_stats$Rep_pri[zone_stats$zone_id==all_zones$zone_id[i]]
  
}

# reorder variables
zone_stats4 = zone_stats4 %>% 
  dplyr::select(Zone, level, Human_pop, Dog_pop_ALL, Dog_pop_FREE, area, hu_dens, dog_dens_ALL, dog_dens_FREE,
                Cluster_population,Cluster_Dog_pop, Cluster_area,
                Cluster_hu_dens,Cluster_dog_dens,Cluster_Rep_O, clusters,
                num_clusters,Hot_Dog_pop,hot_spots,num_hot_spots,
                Regions, Region_majority, Rep_pri, Zone_Label)



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# zone_stats4sep = zone_stats4 %>% rowwise() %>%
#   mutate(country = ifelse(Region_majority%in%regions4SL$shapeName, "Sierra Leone",
#                           ifelse(Region_majority%in%regions3LI$shapeName, "Liberia",
#                                  ifelse(Region_majority%in%regions2CI$shapeName, "Ivory Coast",
#                                         "Guinea"))))



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
summary_part_one = read.csv(paste0(output_folder, "/starc_beginning_stats.csv"))

total_est_dog_pop_ALL = sum(starc_code_2$dg_pp_A)
total_est_dog_pop_FREE = sum(starc_code_2$dg_pp_F)
buffer_distance_km = buffer_size/1000
clust_hexes_included =c(strc_include)
R0_formula = "0.1673*ln(dog_density)+1.0075"

sum_data = data.frame(total_est_dog_pop_ALL,total_est_dog_pop_FREE,buffer_distance_km,R0_formula) %>% mutate(clust_hexes_included=paste(clust_hexes_included,collapse=","))

summary_part_two = summary_part_one %>% cbind(sum_data) %>% rename(strc_cd = X) %>% mutate(strc_cd = "TOTAL")



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#categorize all hexes
starc_code_cat = starc_code_2 %>% 
  mutate(category = if_else(id %in% hexes_starc_code_Gi2$id, "HOTSPOT",
                          if_else(id %in% hexes_cluster_final$id, "CLUSTER",
                          if_else(id %in% hexes_zones$id[hexes_zones$level=="Transmission"], "ZONE - TRANSMISSION",
                          if_else(id %in% hexes_zones$id[hexes_zones$level=="Susceptible"], "ZONE - SUSCEPTIBLE",
                                  "none")))),
         cat_hotspot = if_else(id %in% hexes_starc_code_Gi2$id, 1, 0),
         cat_cluster = if_else(id %in% hexes_cluster_final$id, 1, 0),
         cat_zone = if_else(id %in% hexes_zones$id, 1, 0),
         cat_tz = if_else(id %in% hexes_zones$id[hexes_zones$level=="Transmission"], 1, 0),
         cat_sz = if_else(id %in% hexes_zones$id[hexes_zones$level=="Susceptible"], 1, 0))



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

summary_starc_codes_all = starc_code_cat %>% st_drop_geometry() %>% group_by(strc_cd) %>%
  summarise(human_pop = sum(pop_sum),
            dog_pop_ALL = sum(dg_pp_A),
            dog_pop_FREE = sum(dg_pp_F),
            area_km2 = sum(ar_sqkm))
            
summary_starc_codes_hot = starc_code_cat %>% filter(cat_hotspot==1) %>% st_drop_geometry() %>% group_by(strc_cd) %>%
  summarise(HOT_human_pop = sum(pop_sum),
            HOT_dog_pop_ALL = sum(dg_pp_A),
            HOT_dog_pop_FREE = sum(dg_pp_F),
            HOT_area_km2 = sum(ar_sqkm))
            
summary_starc_codes_clu = starc_code_cat %>% filter(cat_cluster==1) %>% st_drop_geometry() %>% group_by(strc_cd) %>%
  summarise(CLU_human_pop = sum(pop_sum),
            CLU_dog_pop_ALL = sum(dg_pp_A),
            CLU_dog_pop_FREE = sum(dg_pp_F),
            CLU_area_km2 = sum(ar_sqkm))
            
summary_starc_codes_zone = starc_code_cat %>% filter(cat_zone==1) %>% st_drop_geometry() %>% group_by(strc_cd) %>%
  summarise(ZONE_human_pop = sum(pop_sum),
            ZONE_dog_pop_ALL = sum(dg_pp_A),
            ZONE_dog_pop_FREE = sum(dg_pp_F),
            ZONE_area_km2 = sum(ar_sqkm))
            
summary_starc_codes_tz = starc_code_cat %>% filter(cat_tz==1) %>% st_drop_geometry() %>% group_by(strc_cd) %>%
  summarise(TZ_human_pop = sum(pop_sum),
            TZ_dog_pop_ALL = sum(dg_pp_A),
            TZ_dog_pop_FREE = sum(dg_pp_F),
            TZ_area_km2 = sum(ar_sqkm))
            
summary_starc_codes_sz = starc_code_cat %>% filter(cat_sz==1) %>% st_drop_geometry() %>% group_by(strc_cd) %>%
  summarise(SZ_human_pop = sum(pop_sum),
            SZ_dog_pop_ALL = sum(dg_pp_A),
            SZ_dog_pop_FREE = sum(dg_pp_F),
            SZ_area_km2 = sum(ar_sqkm)
            )

summary_starc_codes = full_join(summary_starc_codes_all, 
                                full_join(summary_starc_codes_hot,
                                          full_join(summary_starc_codes_clu,
                                                    full_join(summary_starc_codes_zone,
                                                              full_join(summary_starc_codes_tz, summary_starc_codes_sz,
                                                                        by = "strc_cd"),by = "strc_cd"),by = "strc_cd"),
                                                                        by = "strc_cd"),by = "strc_cd")



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# identify which region each hex is mostly in
inters_hex <- st_join(starc_code_cat, 
                   regions2, 
                   join = st_intersects, 
                   largest = T) %>% 
  dplyr::select(colnames(starc_code_cat),
                Region_majority=`shapeName`)

regions_byhex <- inters_hex %>% rename(Region = Region_majority)

# summary stats
summary_region_starc_codes_all = regions_byhex %>% st_drop_geometry() %>% group_by(Region, strc_cd) %>%
  summarise(human_pop = sum(pop_sum),
            dog_pop_ALL = sum(dg_pp_A),
            dog_pop_FREE = sum(dg_pp_F),
            area_km2 = sum(ar_sqkm))
            
summary_region_starc_codes_hot = regions_byhex %>% filter(cat_hotspot==1) %>% st_drop_geometry() %>% group_by(Region, strc_cd) %>%
  summarise(HOT_human_pop = sum(pop_sum),
            HOT_dog_pop_ALL = sum(dg_pp_A),
            HOT_dog_pop_FREE = sum(dg_pp_F),
            HOT_area_km2 = sum(ar_sqkm))
            
summary_region_starc_codes_clu = regions_byhex %>% filter(cat_cluster==1) %>% st_drop_geometry() %>% group_by(Region, strc_cd) %>%
  summarise(CLU_human_pop = sum(pop_sum),
            CLU_dog_pop_ALL = sum(dg_pp_A),
            CLU_dog_pop_FREE = sum(dg_pp_F),
            CLU_area_km2 = sum(ar_sqkm))
            
summary_region_starc_codes_zone = regions_byhex %>% filter(cat_zone==1) %>% st_drop_geometry() %>% group_by(Region, strc_cd) %>%
  summarise(ZONE_human_pop = sum(pop_sum),
            ZONE_dog_pop_ALL = sum(dg_pp_A),
            ZONE_dog_pop_FREE = sum(dg_pp_F),
            ZONE_area_km2 = sum(ar_sqkm))
            
summary_region_starc_codes_tz = regions_byhex %>% filter(cat_tz==1) %>% st_drop_geometry() %>% group_by(Region, strc_cd) %>%
  summarise(TZ_human_pop = sum(pop_sum),
            TZ_dog_pop_ALL = sum(dg_pp_A),
            TZ_dog_pop_FREE = sum(dg_pp_F),
            TZ_area_km2 = sum(ar_sqkm))
            
summary_region_starc_codes_sz = regions_byhex %>% filter(cat_sz==1) %>% st_drop_geometry() %>% group_by(Region, strc_cd) %>%
  summarise(SZ_human_pop = sum(pop_sum),
            SZ_dog_pop_ALL = sum(dg_pp_A),
            SZ_dog_pop_FREE = sum(dg_pp_F),
            SZ_area_km2 = sum(ar_sqkm)
            )

summary_region_starc_codes = full_join(summary_region_starc_codes_all, 
                                full_join(summary_region_starc_codes_hot,
                                          full_join(summary_region_starc_codes_clu,
                                                    full_join(summary_region_starc_codes_zone,
                                                              full_join(summary_region_starc_codes_tz,
                                                                        summary_region_starc_codes_sz,
                                                                        by = c("Region", "strc_cd")),
                                                              by = c("Region", "strc_cd")),by = c("Region", "strc_cd")),
                                                              by = c("Region", "strc_cd")),by = c("Region", "strc_cd"))



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# change zone and cluster if not regional
hdrs = read.csv(paste0("Inputs/HDRs/",countries,"_HDRs.csv" ))
zone_stats_df = data.frame(zone_stats4) %>% dplyr::select(-`geometry`)
clust_stats_df = data.frame(cluster_stats2) %>% dplyr::select(-`geometry`)
hot_spot_stats_df = data.frame(hot_spot_stats) %>% dplyr::select(-`geometry`)

if(dogdensfx){
  # GUATEMALA FUNCTIONS
  hdrs = data.frame(type = c("All dogs", "Free-roaming dogs"),
                    HDR_fx = c("-8.9 + 1.9*ln(h_pop_dens)",
                                       "-12 + 2.4*ln(h_pop_dens)"))
}

wb <- createWorkbook()
addWorksheet(wb, "Overview")
addWorksheet(wb, "HDRs_Used")
addWorksheet(wb, "STARC_Stats")
addWorksheet(wb, "Region_Stats")
# addWorksheet(wb, "Country_Stats")
addWorksheet(wb, "Zone_Stats")
addWorksheet(wb, "Cluster_Stats")
addWorksheet(wb, "Hot_Spot_Stats")


writeData(wb, "Overview", summary_part_two, startRow = 1, startCol = 1)
writeData(wb, "HDRs_Used", hdrs, startRow = 1, startCol = 1)
writeDataTable(wb, "STARC_Stats", summary_starc_codes, startRow = 1, startCol = 1)
writeDataTable(wb, "Region_Stats", summary_region_starc_codes, startRow = 1, startCol = 1)
# writeDataTable(wb, "Country_Stats", summary_starc_codes_sep, startRow = 1, startCol = 1)
writeDataTable(wb, "Zone_Stats", zone_stats_df, startRow = 1, startCol = 1)
writeDataTable(wb, "Cluster_Stats", clust_stats_df, startRow = 1, startCol = 1)
writeDataTable(wb, "Hot_Spot_Stats", hot_spot_stats_df, startRow = 1, startCol = 1)

saveWorkbook(wb, file =paste(output_folder, "/", "STARC_and_Zone_Statistics_", buffer_size,"_R0andPOP_",countries,Sys.Date(),".xlsx", sep=""), overwrite = TRUE)



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Cluster_outlines = starc_code_unions %>% st_join(cluster_stats2, largest=T)
st_write(Cluster_outlines, paste(folder, "/cluster_outlines_", buffer_size,".shp", sep=""), overwtite=T, append=F)

st_write(hot_spot_stats, paste(folder, "/hot_spot_outlines.shp", sep=""), overwtite=T, append=F)

st_write(zone_stats4, paste(folder, "/zones_area_",buffer_size,".shp", sep=""), overwtite=T, append=F)
st_write(all_zones, paste(folder, "/zones_buffer_",buffer_size,".shp", sep=""), overwtite=T, append=F)



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
hexes_cluster_final2 = hexes_cluster_final %>% merge(data.frame(cluster_stats2) %>% dplyr::select(cluster_id ,Rep_pri ), all.x=T)# merge the cluster_id / priority data to the actual hexagons 
st_write(hexes_cluster_final2, paste(folder, "/hexes_in_cluster_final_", buffer_size,".shp", sep=""), overwtite=T, append=F)

beep(10)



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# End banner when running App
if( exists("app_run")) {
removeModal()
}

