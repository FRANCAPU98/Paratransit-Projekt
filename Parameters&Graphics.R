library(tidyverse)
library(foreign)
library(readr)
library(utils)
library(RColorBrewer)
library(sf)
library(xtable)
library(ggspatial)
library(rnaturalearth)
library(tidygeocoder)
library(maps)
library(ggrepel)


###### Import Data #####
# GEODATA ####
#Sudamerica SHP
sud_map_sd <- read_sf(file.choose()) %>% st_transform(32721)

# Localidades SHP
localidades_sf <- read_sf(file.choose()) %>% st_transform(32721)

## Partidos SHP
partidos_sf <-read_sf(file.choose()) %>% st_transform(32721)

# Provincias SHP
Provincias_sf <- read_sf(file.choose()) %>% 
  filter(FNA == "Provincia de Buenos Aires") %>% st_transform(32721)

# BASE PLOT 
tiff("base_plot.png", units="in", width=7, height=6, res=300)
sud_map_sd %>% filter(COUNTRY == "Argentina") %>% 
  ggplot() + 
  geom_sf() +
  geom_sf(data = localidades_sf, fill = c('antiquewhite1')) +
  geom_sf(data = partidos_sf, color = "black", fill = NA) +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(313587.3, 400876), ylim = c(6125091, 6207241)) +
  theme_bw() +
  annotate(geom = "text", x = 390000, y = 612509, label = "Mar de Plata", 
           fontface = "italic", color = "grey22", size = 6) +
  labs(title = "Partidos & Localidades in BAMR", 
       color = "Oficial Public Transport Stations") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, family = 'Avenir'), 
        legend.position = "bottom", 
        legend.title = element_text(size = 12),
        legend.key.size = unit(2, 'cm'), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(color = gray(.5), linetype = "dashed",
                                        size = 0.5),
        panel.background = element_rect(fill = 'aliceblue')) +
  guides(color = guide_legend(title.position = "top", 
                              override.aes = list(size = 3)))
dev.off()

# Mobility and Infraestructure Data 
##Buses Shape SHP
buses_shape2 <- read_sf(file.choose()) %>% st_transform(32721)

#### Oficial Public Transport Station
#Colectivo Mode
parada_de_autobus_sf <- read_sf(file.choose()) %>% select(-c(2:12)) %>%
  mutate(stop_id = as.character(stop_id), 
         MODO = sample("Colectivo", 42463, replace = TRUE)) %>% 
  rename(NOMBRE = stop_id) %>% st_transform(32721)

#Ferrocarril Mode
parada_de_ferrocarril_sf <- read_sf(file.choose()) %>% select(-c(2:5)) %>% 
  mutate(ID = as.character(ID),
         MODO = sample("Ferrocarril", 47, replace = T)) %>%
  rename(NOMBRE = ID) %>% st_transform(32721)

#Subte Mode
parada_de_subte_sf <- read_sf(file.choose()) %>% select(-c(1:3,5,6)) %>% 
  mutate(MODO = sample("Subte", 93, replace = T)) %>% 
  rename(NOMBRE = estacion) %>% 
  st_transform(32721)

#Metrobus Mode
parada_de_metrobus_sf <- read_sf(file.choose()) %>% select(c(2)) %>% 
  mutate(MODO = sample("Metrobus", 231, replace = T)) %>% st_transform(32721)

### Dataset Fusion \\ all the FORMAL PUBLIC TRANSPORT MODALITIES 
Paradas_Transporte_Publico <- bind_rows(parada_de_ferrocarril_sf,
          parada_de_subte_sf,
          parada_de_autobus_sf,
          parada_de_metrobus_sf) %>% 
  st_filter(localidades_sf, .pred = st_intersects)

#PLOT BASE R ggplot2
tiff("bustations_network.png", units="in", width=7, height=6, res=300)
sud_map_sd %>% filter(COUNTRY == "Argentina") %>% 
  ggplot() + 
  geom_sf() +
  geom_sf(data = localidades_sf, fill = c('antiquewhite1')) +
  geom_sf(data = buses_shape2, size = 0.1, color = "black") +
  geom_sf(data = Paradas_Transporte_Publico, aes(color = MODO), 
          size = .2, shape = 20) + scale_color_brewer(palette = "Dark2") +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
    coord_sf(xlim = c(313587.3, 400876), ylim = c(6125091, 6207241)) +
  theme_bw() +
  labs(title = "Oficial Public Tranport Stations in BAMR", 
                      color = "Oficial Public Transport Stations") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, family = 'Avenir'), 
        legend.position = "bottom", 
        legend.title = element_text(size = 12),
        legend.key.size = unit(2, 'cm'), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(color = gray(.5), linetype = "dashed", 
                                        size = 0.5),
        panel.background = element_rect(fill = 'aliceblue')) +
  guides(color = guide_legend(title.position = "top", 
                              override.aes = list(size = 3)))
dev.off()

# COUNTINT OF PUBLIC TRANSPORT
table(Paradas_Transporte_Publico$MODO)
nrow(Paradas_Transporte_Publico)

# LATEX REPRESENTATION
print(xtable(data.frame("Colectivo" = c(42463,99.13),
                        "Metrobus" = c(231, 0.54),
                        "Subte" = c(93,0.22),
                        "Ferrocarril" = c(47, 0.11),
                        "Total" = c(42834, 100),
                        row.names = c("Station Number", "Percentage"))))

table(Paradas_Transporte_Publico$MODO)/sum(table(Paradas_Transporte_Publico$MODO))*100

# Bus Station Density
localidades_sf$Area_in_km <- as.numeric(st_area(localidades_sf)/1000)

Paradas_Transporte_Publico_density <- 
  st_join(Paradas_Transporte_Publico, localidades_sf) %>% 
  st_drop_geometry() %>% as_tibble() %>%
  group_by(LOCALIDAD, Area_in_km) %>% count(LOCALIDAD) %>% 
  mutate(density_stations_in_km_2 = log(n/Area_in_km)) %>%
  right_join(., localidades_sf, by = c("LOCALIDAD", "Area_in_km")) %>% 
  st_as_sf(crs = 32721)

tiff("bus_station_density.png", units="in", width=7, height=6, res=300)
sud_map_sd %>% filter(COUNTRY == "Argentina") %>% 
  ggplot() + 
  geom_sf(fill = c('antiquewhite1')) +
  geom_sf(data = localidades_sf, fill = c('antiquewhite1')) +
  geom_sf(data = Paradas_Transporte_Publico_density, 
          aes(fill = density_stations_in_km_2)) + 
  scale_fill_viridis_c(option = "D") +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(313587.3, 400876), ylim = c(6125091, 6207241)) + 
  theme_bw() +
  labs(title = "Oficial Public Tranport Stations log Density in BAMR", 
       fill = "Oficial Public Transport Stations log Density ") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, family = 'Avenir'), 
        legend.position = "bottom", 
        legend.title = element_text(size = 12),
        legend.key.size = unit(.5, 'cm'), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(color = gray(.5), linetype = "dashed", 
                                        size = 0.5),
        panel.background = element_rect(fill = 'aliceblue')) +
  guides(color = guide_legend(title.position = "top", 
                              override.aes = list(size = 3)))
dev.off()

#### Mobility Survey Data 
#Persons
population <- read.spss(file.choose(), to.data.frame = TRUE)

#households
hogares <- read.spss(file.choose(), to.data.frame = TRUE)

#etapas
etapas <- read.spss(file.choose(), to.data.frame = TRUE)

#trips
viajes <- read.spss(file.choose(), to.data.frame = TRUE)

###############################################################################
# INCOME DETERMINATION
# Quantile Average
qu_income_localidad <- hogares %>% 
  group_by(LOCALIDA, z_Quintil_ing_per_capita_AD_EQUIV, wt1) %>% 
  summarise (sum_z = sum(z_Quintil_ing_per_capita_AD_EQUIV)) %>% 
  summarise(n_hab = sum(wt1)) %>% 
  mutate(q_mal_n_hab = z_Quintil_ing_per_capita_AD_EQUIV*n_hab) %>% 
  group_by(LOCALIDA) %>% 
  summarize(q_mal_n_total = sum(q_mal_n_hab), n_hab = sum(n_hab)) %>% 
  mutate(average_quantile = q_mal_n_total/n_hab) %>% 
  rename(LOCALIDAD = LOCALIDA) %>% 
  full_join(., localidades_sf, by = "LOCALIDAD") %>% st_as_sf()  %>%
  st_transform(32721)

# ggplot2 Map
#tiff("Averga_income_quantile.png", units="in", width=7, height=6, res=300)
sud_map_sd %>% filter(COUNTRY == "Argentina") %>% 
  ggplot() + 
  geom_sf(fill = c('antiquewhite1')) +
  geom_sf(data = localidades_sf, fill = c('antiquewhite1')) +
  geom_sf(data = qu_income_localidad, 
          aes(fill = average_quantile)) + 
  scale_fill_viridis_c(option = "A") +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(313587.3, 400876), ylim = c(6125091, 6207241)) + 
  theme_bw() +
  labs(title = "Average Income Quintil in BARM", 
       fill = "Average Income Quintil") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, family = 'Avenir'), 
        legend.position = "bottom", 
        legend.title = element_text(size = 12),
        legend.key.size = unit(.5, 'cm'), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(color = gray(.5), linetype = "dashed", 
                                        size = 0.5),
        panel.background = element_rect(fill = 'aliceblue')) +
  guides(color = guide_legend(title.position = "top", 
                              override.aes = list(size = 3)))
#dev.off()

###ALT 2.1 FINAL= Average Income of each Locality with the average income of each quintil
#from the EMODO DATASET

#Change column quintil name and assign values of avergae income for each quintil
hogares_real_final <- hogares
colnames(hogares_real_final)[colnames(hogares_real_final) == "z_Quintil_ing_per_capita_AD_EQUIV"] <- "quintil"
hogares_real_final$quintil[hogares_real_final$quintil == 1] <- 1320.95
hogares_real_final$quintil[hogares_real_final$quintil == 2] <- 2193.23
hogares_real_final$quintil[hogares_real_final$quintil == 3] <- 2986.77
hogares_real_final$quintil[hogares_real_final$quintil == 4] <- 4116.28
hogares_real_final$quintil[hogares_real_final$quintil == 5] <- 7424.34

hogares_real_final["quintil"] <- as.numeric(hogares_real_final$quintil)

#To calculate the income for each person and not for each home
quant_final1 <- population %>% group_by(IDH) %>% 
  summarise(habitantes_hogar = n()) %>% 
  full_join(.,hogares_real_final, by = "IDH") %>%
  mutate(ingreso_promedio_hogar = quintil*habitantes_hogar) %>% 
  group_by(LOCALIDA) %>%
  summarise(n_hab=sum(habitantes_hogar*wt1), income_total = sum(quintil*wt1)) %>% 
  mutate(average_income = log(income_total/n_hab)) %>%
  rename(LOCALIDAD = LOCALIDA) %>% 
  full_join(., localidades_sf, by = "LOCALIDAD") %>% st_as_sf()
  

# ggplot2 map
tiff("Averga_income.png", units="in", width=7, height=6, res=300)
sud_map_sd %>% filter(COUNTRY == "Argentina") %>% 
  ggplot() + 
  geom_sf(fill = c('antiquewhite1')) +
  geom_sf(data = localidades_sf, fill = c('antiquewhite1')) +
  geom_sf(data = quant_final1, 
          aes(fill = average_income)) + 
  scale_fill_viridis_c(option = "A") +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(313587.3, 400876), ylim = c(6125091, 6207241)) + 
  theme_bw() +
  labs(title = "Log Average Income in BARM", 
       fill = "Log Average Income") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, family = 'Avenir'), 
        legend.position = "bottom", 
        legend.title = element_text(size = 12),
        legend.key.size = unit(1, 'cm'), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = 'aliceblue')) +
  guides(color = guide_legend(title.position = "top", 
                              override.aes = list(size = 3)))
dev.off()

################################################################################
################################################################################
# Population Density

## Population density Correction
pop_density_localidades_sf <- readRDS(file.choose()) %>% st_as_sf() %>% 
  st_transform(32721) %>%
  mutate(area_in_km2 = as.numeric(area)/1000, 
         density_hab_pro_km2_by_localidad = 
           inhabitant_2020_projected/area_in_km2)

# ggplot2 map
tiff("pop_density.png", units="in", width=7, height=6, res=300)
sud_map_sd %>% filter(COUNTRY == "Argentina") %>% 
  ggplot() + 
  geom_sf(fill = c('antiquewhite1')) +
  geom_sf(data = localidades_sf, fill = c('antiquewhite1')) +
  geom_sf(data = pop_density_localidades_sf, 
          aes(fill = density_hab_pro_km2_by_localidad)) + 
  scale_fill_viridis_c(option = "B") +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(313587.3, 400876), ylim = c(6125091, 6207241)) + 
  theme_bw() +
  labs(title = "Population Density pro KM2 in BARM", 
       fill = "Population Density pro KM2") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, family = 'Avenir'), 
        legend.position = "bottom", 
        legend.title = element_text(size = 12),
        legend.key.size = unit(1, 'cm'), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = 'aliceblue')) +
  guides(color = guide_legend(title.position = "top", 
                              override.aes = list(size = 3)))
dev.off()

##Distance to city Center
centroid <- st_centroid(localidades_sf$geometry)  %>% st_as_sf() 
localidades_new_centroid_1 <- cbind(localidades_sf, centroid)

#Calculate the distance from each centroid to Caballito (399)
mm <- st_distance(centroid)
localidades_new_centroid_1$distance_to_center <- as.numeric(mm[ ,399]/1000) 

#calculate the distance from the center of the different localidades to the centroid of Caballito

tiff("city_center_distance.png", units="in", width=7, height=6, res=300)
sud_map_sd %>% filter(COUNTRY == "Argentina") %>% 
  ggplot() + 
  geom_sf(fill = c('antiquewhite1')) +
  geom_sf(data = localidades_sf, fill = c('antiquewhite1')) +
  geom_sf(data = localidades_new_centroid_1, 
          aes(fill = distance_to_center)) + 
  scale_fill_viridis_c(option = "D") +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(313587.3, 400876), ylim = c(6125091, 6207241)) + 
  theme_bw() +
  labs(title = "Distance to city center in meter in BAMR taking as a reference Caballitos Localitiy", 
       fill = "Distance in meters") +
  theme(plot.title = element_text(hjust = 0.5, size = 10, family = 'Avenir'), 
        legend.position = "bottom", 
        legend.title = element_text(size = 12),
        legend.key.size = unit(0.5, 'cm'), 
        legend.text = element_text(size = 10),
        panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = 'aliceblue')) +
  guides(color = guide_legend(title.position = "top", 
                              override.aes = list(size = 3)))
dev.off()