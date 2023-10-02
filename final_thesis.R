#combined files 

setwd("C:/Users/Rob/OneDrive/Documents/RFiles/thesis")

library(sp)
library(sf)
library(raster)
library(rasterize)
library(rgdal)
library(rlang)
library(tidyverse)
library(ggplot2)
library(viridisLite)
library(viridis)
library(readxl)
library(dplyr)
library(purrr)
library(data.table)
library(terra)
library(tidyterra)
library(ggrepel)
library(psych)
library(knitr)
library(flextable)
library(officer)
library(stargazer)
library(cowplot)
library(smplot2)
library(lmtest)
library(sandwich)
library(jtools)

# Define the main folder containing the subfolders
main_folder2 <- "C:/Users/Rob/OneDrive/Documents/RFiles/thesis/Data/movilidad_cotidiana_enero_noviembre_2021/movilidad_cotidiana_enero_noviembre_2021"

# Define a list of the subfolders to search
subfolders2 <- c("Enero 2021", "Febrero 2021", "Marzo 2021", "Abril 2021", "Mayo 2021", "Junio 2021", "Julio 2021", "Agosto 2021", "Septiembre 2021", "Octubre 2021", "Noviembre 2021","Diciembre 2021")

# Create a temporary directory to copy the Excel files because extracting them says file path is long
temp_dir2 <- file.path(getwd(), "temp_dir2")
dir.create(temp_dir2)

# Copy the Excel files from the original directory to the temporary directory
map(subfolders2, ~ {
  files <- list.files(path = file.path(main_folder2, .), pattern = "^Tabla 1.3.*\\.xlsx$", full.names = TRUE)
  file.copy(from = files, to = temp_dir2)
})

# Use list.files() to find the Excel files in the temporary directory
file_list2 <- list.files(temp_dir2, pattern = "^Tabla 1\\.3.*\\.xlsx$", full.names = TRUE)

# Use purrr::map() and readxl::read_excel() to read each file into a data frame
df_list2 <- map(file_list2, ~ possibly(read_excel, otherwise = NULL)(.))

# Remove any NULL elements from the list
df_list2 <- compact(df_list2)

# Use dplyr::mutate_all() to convert all columns to the same data type
df_list2 <- map(df_list2, ~ mutate_all(., as.character))

# Use dplyr::bind_rows() to combine the data frames into a single data frame
df2 <- bind_rows(df_list2, .id = "file_id")


#rename residence and destination
full_year_flows<- df2 %>% rename("province_residence" = "Provincia de residencia",
                                     "province_destination" = "Provincia de destino",
                                     "residence" = "Nombre área de residencia",
                                     "destination" = "Nombre área de destino",
                                     "residence_code" = "Código área de residencia",
                                     "destination_code" = "Código área de destino",
                                     "flows" = "Flujo origen-destino (nº de personas)",
                                "region_residence" = "Comunidad Autónoma de residencia",
                                "region_destination" = "Comunidad Autónoma de destino" )
#filter barcelona province only
full_year_flows2 <- full_year_flows %>% 
  filter(grepl("^Barcelona", province_residence) & grepl("^Barcelona", province_destination))

#convert last column to numeric
full_year_flows2 <- full_year_flows2 %>%
  mutate_at(vars(10), as.numeric)

#Average all flows within Barcelona Province
full_year_flows3 <- full_year_flows2 %>%
  group_by(province_residence, residence_code, residence, province_destination, destination_code, destination) %>%
  summarise(avg_flow = mean(flows, na.rm = TRUE))

#Sum all flows by residence
residence_full_year_flows <- full_year_flows3 %>%
  filter(grepl("Barcelona", residence)) %>%
  group_by(residence_code, residence, province_residence) %>%
  summarise(total_flows = sum(avg_flow))

#Sum all flows by destination
destination_full_year_flows <- full_year_flows3 %>%
  filter(grepl("Barcelona", destination)) %>%
  group_by(destination_code,province_destination,destination) %>%
  summarise(total_flows = sum(avg_flow))

#read shape file
spain <- st_read("Data/Map/shapefiles_celdas_marzo2020/celdas_marzo_2020.shp")

#rename columns in DF
residence_full_year_flows2<- residence_full_year_flows%>% rename("ID_GRUPO" = "residence_code")
destination_full_year_flows2 <- destination_full_year_flows %>% rename ("ID_GRUPO" = "destination_code")

#merge files
residence_full_year_flows3 <- left_join(spain, residence_full_year_flows2, by ="ID_GRUPO")
destination_full_year_flows3 <- left_join(spain, destination_full_year_flows2, by ="ID_GRUPO")

#filter province only (OPTION)
#april_flow_residence3 <- filter(april_flow_residence2, province_residence == 'Barcelona')
#april_flow_destination3 <- filter(april_flow_destination2, province_destination == 'Barcelona')

#filter barcelona city only
residence_full_year_flows4 <- residence_full_year_flows3 %>% filter(grepl("^Barcelona", residence))
destination_full_year_flows4 <- destination_full_year_flows3 %>% filter (grepl ("^Barcelona", destination))


# Define the main folder containing the subfolders
main_folder <- "C:/Users/Rob/OneDrive/Documents/RFiles/thesis/Data/movilidad_cotidiana_enero_noviembre_2021/movilidad_cotidiana_enero_noviembre_2021"

# Define a list of the subfolders to search
subfolders <- c("Enero 2021", "Febrero 2021", "Marzo 2021", "Abril 2021", "Mayo 2021", "Junio 2021", "Julio 2021", "Agosto 2021", "Septiembre 2021", "Octubre 2021", "Noviembre 2021","Diciembre 2021")

# Create a temporary directory to copy the Excel files because extracting them says file path is long
temp_dir <- file.path(getwd(), "temp_dir")
dir.create(temp_dir)

# Copy the Excel files from the original directory to the temporary directory
map(subfolders, ~ {
  files <- list.files(path = file.path(main_folder, .), pattern = "^Tabla 1.2.*\\.xlsx$", full.names = TRUE)
  file.copy(from = files, to = temp_dir)
})

# Use list.files() to find the Excel files in the temporary directory
file_list <- list.files(temp_dir, pattern = "^Tabla 1\\.2.*\\.xlsx$", full.names = TRUE)

# Use purrr::map() and readxl::read_excel() to read each file into a data frame
df_list <- map(file_list, ~ possibly(read_excel, otherwise = NULL)(.))

# Remove any NULL elements from the list
df_list <- compact(df_list)

# Use dplyr::mutate_all() to convert all columns to the same data type
df_list <- map(df_list, ~ mutate_all(., as.character))

# Use dplyr::bind_rows() to combine the data frames into a single data frame
df <- bind_rows(df_list, .id = "file_id")

# Convert to numeric
df <- df %>%
  mutate_at(vars(7:19), as.numeric)

# Group by "Nombre del Área" column and calculate mean for each column
df_avg <- df %>%
  group_by(`Nombre del Área`) %>%
  summarise_all(~ ifelse(is.numeric(.), mean(., na.rm = TRUE), first(.)))

# Rename columns in DF
full_year_avg<- df_avg %>% rename("ID_GRUPO" = "Área",
                                  "distinct_destinations" = "Número de áreas de destino distintas en las que se encuentra durante el día población residente en esta área",
                                  "distinct_residencials" = "Número de áreas de residencia distintas en las que reside población que se encuentra durante el día en esta área",
                                  "total_population" = "Población residente (A)",
                                  "population_stay" = "Población residente que se localiza durante el día en su área de residencia (B)",
                                  "population_outside" = "Población residente encontrada durante el día en otra área (C)",
                                  "non_resident_arrival" = "Población no residente que se localiza durante el día en esta área (D)",
                                  "total_stayers" = "Población total que se localiza durante el día en el área (E=B+D)",
                                  "population_balance" = "Saldo población entra y sale de esta área (F=D-C)",
                                  "population_stay_ratio" = "Porcentaje de población residente que se localiza durante el día en su área de residencia (B*100/A)",
                                  "population_outside_ratio" = "Porcentaje de población residente que sale de su área (C*100/A)",
                                  "non_resident_ratio" = "Porcentaje de población no residente que se localiza durante el día en esta área (D*100/A)",
                                  "stayers_quotient" = "Cociente entre población total que se localiza durante el día y población residente (E*100/A)",
                                  "population_balance_quotient" = "Porcentaje de población que gana o pierde durante el día (F*100/A)"
)
#merge data frames
full_year_merged <- left_join(spain, full_year_avg, by ="ID_GRUPO")

#filter Barcelona only
full_year_merged2 <- filter(full_year_merged, Provincia == 'Barcelona')

#Ratio for province
full_year_outside <- mutate(full_year_merged2, population_outside_ratio2 = population_outside/total_population)
full_year_inside <- mutate(full_year_merged2, population_stay_ratio2 = population_stay/total_population)
full_year_non_resident <- mutate(full_year_merged2, non_resident_ratio2 = non_resident_arrival/total_population)
full_year_stayers_quotient <- mutate(full_year_merged2, stayers_quotient2 = total_stayers/total_population)
full_year_population_balance_quotient <- mutate(full_year_merged2, population_balance_quotient2 = population_balance/total_population)


#filter Barcelona City
full_year_outside_filtered <- full_year_outside %>% filter(grepl("^Barcelona", NOMBRE_CEL))
full_year_inside_filtered <- full_year_inside %>% filter(grepl("^Barcelona",NOMBRE_CEL ))
full_year_non_resident_filtered <- full_year_non_resident %>% filter(grepl("^Barcelona", NOMBRE_CEL))
full_year_stayers_quotient_filtered <- full_year_stayers_quotient %>% filter(grepl("^Barcelona", NOMBRE_CEL))
full_year_population_balance_quotient_filtered <- full_year_population_balance_quotient %>% filter(grepl("Barcelona", NOMBRE_CEL))
full_year_merged3 <- full_year_merged2 %>% filter(grepl("Barcelona",NOMBRE_CEL))

#or to include surroundings
full_year_outside_filtered2<- full_year_outside %>% 
  filter(grepl("^(Barcelona|Hospitalet|Santa Coloma|Sabadell|Badalona|Sant Boi|Sant Cugat|Sant Adri|Cornell|Cerdanyola|Montcada|Prat de Ll)", NOMBRE_CEL))

#Plot total resident population outside
gg1=ggplot () +
  geom_sf(data = full_year_outside_filtered, color = "transparent", size = 0.1, aes (fill = population_outside_ratio2))+
  scale_fill_viridis(discrete=FALSE,
                     name="% of resident population outside during the day.",
                     option="viridis",
                     direction=1,
                     breaks = pretty(full_year_outside$population_outside_ratio2, n = 4)) +
  theme(
    legend.position = "right",
    legend.spacing.y = unit(0.5,'cm'),
    legend.text = element_text(margin = margin(t = 30)),
    legend.margin = margin(t = 0, r = 10, b = 0, l = 0),
    plot.margin = margin(t = 0, r = 5, b = 0, l = 5),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
  )

gg1
#plot total population not moving
gg2 = ggplot() +
  geom_sf(data = full_year_inside_filtered, color = "transparent", size =0.1, aes(fill = population_stay_ratio2)) +
  scale_fill_viridis(discrete=FALSE,
                     name="% of resident population not moving out residence 
 area during the day",
                     option="viridis",
                     direction=1,
                     breaks = pretty(full_year_inside$population_stay_ratio2, n=5)) +
  
  theme(
    legend.position = "right",
    legend.spacing.y = unit(0.5,'cm'),
    legend.text = element_text(margin = margin(t = 30)),
    legend.margin = margin(t = 0, r = 10, b = 0, l = 0),
    plot.margin = margin(t = 0, r = 5, b = 0, l = 5),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
  )

#plot non resident ratio (flows)
gg3 = ggplot() +
  geom_sf(data = full_year_non_resident_filtered, color = "transparent", size =0.1, aes(fill = non_resident_ratio2)) +
  scale_fill_viridis(discrete=FALSE,
                     name="Ratio of non-residents in the area 
over total residents during the day",
                     option="viridis",
                     direction=1,
                     breaks= pretty(full_year_non_resident$non_resident_ratio2, n=3)) +
  
  theme(
    legend.position = "right",
    legend.spacing.y = unit(0.5,'cm'),
    legend.text = element_text(margin = margin(t = 30)),
    legend.margin = margin(t = 0, r = 10, b = 0, l = 0),
    plot.margin = margin(t = 0, r = 5, b = 0, l = 5),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
  )

#plot distinct destinations

gg4= ggplot() +
  geom_sf(data = full_year_merged3, color = "transparent", size = 0.1, aes(fill = distinct_destinations)) +
  scale_fill_viridis(discrete = FALSE,
                     name = "Number of distinct destinations
areas during the day",
                     option = "viridis",
                     direction = 1,
                     breaks = pretty(full_year_merged3$distinct_destinations, n = 5)) +
  theme(
    legend.position = "right",
    legend.spacing.y = unit(0.5, 'cm'),
    legend.text = element_text(margin = margin(t = 30)),
    legend.margin = margin(t = 0, r = 10, b = 0, l = 0),
    plot.margin = margin(t = 0, r = 5, b = 0, l = 5),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  )


#plot distinct residencials

gg5= ggplot() +
  geom_sf(data = full_year_merged3, color = "transparent", size = 0.1, aes(fill = distinct_residencials)) +
  scale_fill_viridis(discrete = FALSE,
                     name = "Number of distinct residencials 
areas during the day",
                     option = "viridis",
                     direction = 1,
                     breaks = pretty(full_year_merged3$distinct_residencials, n = 5)) +
  theme(
    legend.position = "right",
    legend.spacing.y = unit(0.5, 'cm'),
    legend.text = element_text(margin = margin(t = 30)),
    legend.margin = margin(t = 0, r = 10, b = 0, l = 0),
    plot.margin = margin(t = 0, r = 5, b = 0, l = 5),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA)
  )


#plot stayers quotient

gg6 = ggplot() +
  geom_sf(data = full_year_stayers_quotient_filtered, color = "transparent", size =0.1, aes(fill = stayers_quotient2)) +
  scale_fill_viridis(discrete=FALSE,
                     name="Quotient of population during the day
over total resident population",
                     option="viridis",
                     direction=1,
                     breaks = pretty(full_year_stayers_quotient$stayers_quotient2, n=4)) +
  
  theme(
    legend.position = "right",
    legend.spacing.y = unit(0.5,'cm'),
    legend.text = element_text(margin = margin(t = 30)),
    legend.margin = margin(t = 0, r = 10, b = 0, l = 0),
    plot.margin = margin(t = 0, r = 5, b = 0, l = 5),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
  )

#plot full year population balance quotient
gg7 = ggplot() +
  geom_sf(data = full_year_population_balance_quotient_filtered, color = "transparent", size =0.1, aes(fill = population_balance_quotient2)) +
  scale_fill_viridis(discrete=FALSE,
                     name="Population gains (losses)
relative to total population of area
during the day",
                     option="viridis",
                     direction=1,
                     breaks = pretty(full_year_population_balance_quotient$population_balance_quotient2, n=4)) +
  
  theme(
    legend.position = "right",
    legend.spacing.y = unit(0.5,'cm'),
    legend.text = element_text(margin = margin(t = 30)),
    legend.margin = margin(t = 0, r = 10, b = 0, l = 0),
    plot.margin = margin(t = 0, r = 5, b = 0, l = 5),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
  )


gg1
gg2
gg3
gg4
gg5
gg6
gg7

#save
dir.create("maps")
ggsave(gg1, filename = "maps/full_year_outside.png", width = 8.335, height = 6.946, dpi = 900)
ggsave(gg2, filename = "maps/full_year_inside.png", width = 8.335, height = 6.946, dpi = 900)
ggsave(gg3, filename = "maps/full_year_ratio_non_residents_over_total.png", width = 8.335, height = 6.946, dpi = 900)
ggsave(gg4, filename = "maps/full_year_distinct_destinations.png", width = 8.335, height = 6.946, dpi = 900)
ggsave(gg5, filename = "maps/full_year_distinct_residents.png", width = 8.335, height = 6.946, dpi = 900)
ggsave(gg6, filename = "maps/full_year_quotient_of_stayers.png", width = 8.335, height = 6.946, dpi = 900)
ggsave(gg7, filename = "maps/full_year_gains_losses.png", width = 8.335, height = 6.946, dpi = 900)

#merge data frame to get population from other file (first get ful year avg from combined files)
residence_full_year_flows5 <- left_join(residence_full_year_flows4, full_year_avg, by ="ID_GRUPO")
destination_full_year_flows5 <- left_join(destination_full_year_flows4, full_year_avg, by="ID_GRUPO")

#get the flow ratio over total population
residence_full_year_flows6 <- residence_full_year_flows5 %>% 
  mutate(ratio_flows = total_flows / total_population)

destination_full_year_flows6 <- destination_full_year_flows5 %>%
  mutate(ratio_flows = total_flows/total_population)

#Plot movement from residence
gg8 = ggplot() +
  geom_sf(
    data = residence_full_year_flows6,
    color = "transparent",
    size = 0.1,
    aes(fill = pmin(ratio_flows, 1))  # Cap values at 1 using pmin()
  ) +
  scale_fill_viridis(
    discrete = FALSE,
    name = "Ratio of movers from their residence 
over total population of distinct area",
    option = "viridis",
    direction = 1
  ) +
  theme(
    legend.position = "right",
    legend.spacing.y = unit(0.5,'cm'),
    legend.text = element_text(margin = margin(t = 30)),
    legend.margin = margin(t = 0, r = 10, b = 0, l = 0),
    plot.margin = margin(t = 0, r = 5, b = 0, l = 5),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
  )

#Plot movement to destination
gg9 = ggplot() +
  geom_sf(data = destination_full_year_flows6, color = "transparent", size =0.1, aes(fill = ratio_flows)) +
  scale_fill_viridis(discrete=FALSE,
                     name="Ratio of movers to destination 
over total population of distinct area",
                     option="viridis",
                     direction=1) +
  theme(
    legend.position = "right",
    legend.spacing.y = unit(0.5,'cm'),
    legend.text = element_text(margin = margin(t = 30)),
    legend.margin = margin(t = 0, r = 10, b = 0, l = 0),
    plot.margin = margin(t = 0, r = 5, b = 0, l = 5),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
  )

gg8
gg9

#save
dir.create("maps")
ggsave(gg8, filename = "maps/combined_flows_residence.png", width = 8.335, height = 6.946, dpi = 900)
ggsave(gg9, filename = "maps/combined_flows_destination.png", width = 8.335, height = 6.946, dpi = 900)

#combining flows and foreign population

#Read excel file
city <- read_excel("Data/dissimilarity calculations.xlsx", 
                   sheet = "city dissimilarity", col_types = c("numeric", 
                                                               "text", "text", "text", "numeric", 
                                                               "numeric", "numeric", "numeric", 
                                                               "text", "numeric", "numeric", "numeric", 
                                                               "numeric"))
#rename column

city2 = city%>% 
  rename("NOM" = "Nom_Barri")

#upload neighborhood map
neighborhoods <- st_read("Data/Map/0301100100_UNITATS_ADM_POLIGONS.json") %>% filter(CONJ_DESCR == "Barris")
 

#change barrio to numeric
neighborhoods$BARRI <- as.numeric(as.double(neighborhoods$NOM))

#join data
neighborhoods1 = left_join(neighborhoods, city2, by ="NOM")

#save previous maps as gpkg
st_write(full_year_outside_filtered, "maps/residents_outside.gpkg")
st_write(full_year_inside_filtered, "maps/residents_inside.gpkg")
st_write(residence_full_year_flows6, "maps/residents_flows.gpkg")
st_write(full_year_merged3, "maps/distinct_destinations.gpkg")

#read gpkg and transform to neighborhood map
full_outside = st_read("C:/Users/Rob/OneDrive/Documents/RFiles/thesis/maps/residents_outside.gpkg") %>% st_transform(st_crs(neighborhoods1))

full_inside = st_read("C:/Users/Rob/OneDrive/Documents/RFiles/thesis/maps/residents_inside.gpkg") %>% st_transform(st_crs(neighborhoods1))

resident_flows = st_read("C:/Users/Rob/OneDrive/Documents/RFiles/thesis/maps/residents_flows.gpkg") %>% st_transform(st_crs(neighborhoods1))

distinct_destinations = st_read("C:/Users/Rob/OneDrive/Documents/RFiles/thesis/maps/distinct_destinations.gpkg") %>% st_transform(st_crs(neighborhoods1))

#raster

r = rast(crs = "epsg:25831", 
         resolution= 100, 
         xmin = st_bbox(neighborhoods1)$xmin, 
         xmax = st_bbox(neighborhoods1)$xmax, 
         ymin = st_bbox(neighborhoods1)$ymin, 
         ymax = st_bbox(neighborhoods1)$ymax)

Dr = rasterize(neighborhoods1, r, field='foreignpop')

ggplot() + geom_spatraster(data = Dr, aes(fill = foreignpop))

#do test binds
test1 = extract(Dr, full_outside, fun = mean, weights = TRUE, na.rm=TRUE, bind=TRUE)
test2 = extract(Dr, full_inside, fun = mean, weights = TRUE, na.rm=TRUE, bind=TRUE)
test3 = extract(Dr, resident_flows, fun = mean, weights = TRUE, na.rm=TRUE, bind=TRUE)
test4 = extract(Dr, distinct_destinations, fun=mean, weights = TRUE, na.rm=TRUE, bind=TRUE)

full_outside <- cbind(full_outside, test1)
full_inside <- cbind(full_inside, test2)
resident_flows <- cbind(resident_flows, test3)
distinct_destinations <-cbind (distinct_destinations, test4)

#get top 10 top and bottom residential differences and sort the data by the "foreignpop" column in descending order
sorted_data_full_outside <- full_outside[order(full_outside$foreignpop, decreasing = TRUE), ]

# Get the top 5 and bottom 5 rows based on the sorted "foreignpop" column and do for every ggplot after ggplot10
top10_1<- head(sorted_data_full_outside, 10)
bottom10_1<- tail(sorted_data_full_outside, 10)

#plot foreign pop vs findings
gg10 = ggplot(full_outside) +
  geom_point(aes(x = foreignpop, y = population_outside_ratio2)) +
  scale_color_viridis(option = "viridis", guide = "legend") +
  scale_size_continuous(range = c(2, 9)) +
  labs(x = "Percentage of foreign population (in hundredths, %)",
       y = "Percentage of residents who leave during the day (in hundredths, %)",
       size = "Percentage of resident
population which leave") +
  guides(color = guide_legend(title = "Foreign Population"))

gg10

#get top 10 top and bottom residential differences and sort the data by the "foreignpop" column in descending order
sorted_data2<- full_inside[order(full_inside$foreignpop, decreasing = TRUE), ]

# Get the top 5 and bottom 5 rows based on the sorted "foreignpop" column
top10_2 <- head(sorted_data2, 10)
bottom10_2<- tail(sorted_data2, 10)

gg11=ggplot(full_inside) +
  geom_point(aes(x = foreignpop, y = population_stay_ratio2)) +
  scale_color_viridis(option = "viridis", guide = "legend") +
  scale_size_continuous(range = c(2, 9))+
  labs(x = "Percentage of foreign population (in hundredths, %)",
       y = "Percentage of residents who stay during the day (in hundredths, %)",
       size = "Percentage of resident
population which stay") +
  guides(color = guide_legend(title = "Foreign Population"))

gg11


#get top 10 top and bottom residential differences and sort the data by the "foreignpop" column in descending order
sorted_data3<- resident_flows[order(resident_flows$foreignpop, decreasing = TRUE), ]

# Get the top 5 and bottom 5 rows based on the sorted "foreignpop" column
top10_3 <- head(sorted_data3, 10)
bottom10_3<- tail(sorted_data3, 10)

gg12=ggplot(resident_flows) +
  geom_point(aes(x = foreignpop, y = ratio_flows)) +
  scale_color_viridis(option = "viridis", guide = "legend") +
  scale_size_continuous(range = c(2, 9))+
  labs(x = "Percentage of foreign population (in hundredths, %)",
       y = "Flow Ratio of leavers over resident population",
       size = "Flow Ratio") +
  guides(color = guide_legend(title = "Foreign Population"))

gg12


#get top 10 top and bottom residential differences and sort the data by the "foreignpop" column in descending order
sorted_data4<- distinct_destinations[order(distinct_destinations$foreignpop, decreasing = TRUE), ]

# Get the top 5 and bottom 5 rows based on the sorted "foreignpop" column
top10_4 <- head(sorted_data4, 10)
bottom10_4<- tail(sorted_data4, 10)

gg13=ggplot(distinct_destinations) +
  geom_point(aes(x = foreignpop, y = distinct_destinations)) +
  scale_color_viridis(option = "viridis", guide = "legend") +
  scale_size_continuous(range = c(2, 9))+
  labs(x = "Percentage of foreign population (in hundredths, %)",
       y = "Number of distinct destinations",
       size = "Distinct destinations") +
  guides(color = guide_legend(title = "Foreign Population"))

gg13

#using income

disposable_income <- read_csv("Data/2019_renda_disponible_llars.csv", 
                              col_types = cols(Euros_Any = col_number()))

#rename income

disposable_income2 = disposable_income%>% 
  rename("NOM" = "Nom_Barri")

#join

disposable_income_map = left_join(neighborhoods1, disposable_income2, by ="NOM")

#raster

r1 = rast(crs = "epsg:25831", 
         resolution= 100, 
         xmin = st_bbox(disposable_income_map)$xmin, 
         xmax = st_bbox(disposable_income_map)$xmax, 
         ymin = st_bbox(disposable_income_map)$ymin, 
         ymax = st_bbox(disposable_income_map)$ymax)

Dr1 = rasterize(disposable_income_map, r, field='Euros_Any')

test11 = extract(Dr1, full_outside, fun = mean, weights = TRUE, na.rm=TRUE, bind=TRUE)
test22 = extract(Dr1, full_inside, fun = mean, weights = TRUE, na.rm=TRUE, bind=TRUE)
test33 = extract(Dr1, resident_flows, fun = mean, weights = TRUE, na.rm=TRUE, bind=TRUE)
test44 = extract(Dr1, distinct_destinations, fun=mean, weights = TRUE, na.rm=TRUE, bind=TRUE)

full_outside1 <- cbind(full_outside, test11)
full_inside1 <- cbind(full_inside, test22)
resident_flows1 <- cbind(resident_flows, test33)
distinct_destinations1 <-cbind (distinct_destinations, test44)

#get top 10 top and bottom residential differences and sort the data by the "foreignpop" column in descending order
sorted_data5 <- full_outside1[order(full_outside1$foreignpop, decreasing = TRUE), ]

# Get the top 5 and bottom 5 rows based on the sorted "foreignpop" column
top10_5 <- head(sorted_data5, 10)
bottom10_5<- tail(sorted_data5, 10)

# Create the scatter plot with labels
gg14 = ggplot(full_outside1) +
  geom_point(aes(x = Euros_Any, y = population_outside_ratio2, color = foreignpop)) +
  scale_color_viridis(option = "viridis", guide = "legend") +
  scale_size_continuous(range = c(2, 9)) +
  labs(x = "Annual disposable household income per capita",
       y = "Percentage of residents who are outside during the day (in hundredths, %)",
       size = "Percentage of resident
population outside") +
  guides(color = guide_legend(title = "Foreign Population"))

gg14

#get top 10 top and bottom residential differences and sort the data by the "foreignpop" column in descending order
sorted_data6 <- full_inside1[order(full_inside1$foreignpop, decreasing = TRUE), ]

# Get the top 5 and bottom 5 rows based on the sorted "foreignpop" column
top10_6 <- head(sorted_data6, 10)
bottom10_6<- tail(sorted_data6, 10)

gg15=ggplot(full_inside1) +
  geom_point(aes(x = Euros_Any, y = population_stay_ratio2, color = foreignpop)) +
  scale_color_viridis(option = "viridis", guide = "legend") +
  scale_size_continuous(range = c(2, 9))+
  labs(x = "Annual disposable household income per capita",
       y = "Percentage of residents who stay during the day (in hundredths, %)",
       size = "Percentage of resident
population who stay") +
  guides(color = guide_legend(title = "Foreign Population"))

gg15


#get top 10 top and bottom residential differences and sort the data by the "foreignpop" column in descending order
sorted_data7 <- resident_flows1[order(resident_flows1$foreignpop, decreasing = TRUE), ]

# Get the top 5 and bottom 5 rows based on the sorted "foreignpop" column
top10_7<- head(sorted_data7, 10)
bottom10_7<- tail(sorted_data7, 10)

gg16=ggplot(resident_flows1) +
  geom_point(aes(x = Euros_Any, y = ratio_flows, color = foreignpop)) +
  scale_color_viridis(option = "viridis", guide = "legend") +
  scale_size_continuous(range = c(2, 9))+
  labs(x = "Annual disposable household income per capita",
       y = "Flow ratio of leavers over resident population",
       size = "Flow Ratio") +
  guides(color = guide_legend(title = "Foreign Population"))

gg16


#get top 10 top and bottom residential differences and sort the data by the "foreignpop" column in descending order
sorted_data8 <- distinct_destinations1[order(distinct_destinations1$foreignpop, decreasing = TRUE), ]

# Get the top 5 and bottom 5 rows based on the sorted "foreignpop" column
top10_8<- head(sorted_data8, 10)
bottom10_8<- tail(sorted_data8, 10)

gg17=ggplot(distinct_destinations1) +
  geom_point(aes(x = Euros_Any, y = distinct_destinations, color = foreignpop)) +
  scale_color_viridis(option = "viridis", guide = "legend") +
  scale_size_continuous(range = c(2, 9))+
  labs(x = "Annual disposable household income per capita",
       y = "Number of distinct destinations",
       size = "Distinct destinations") +
  guides(color = guide_legend(title = "Foreign Population"))

gg17


gg18=ggplot(full_outside1) +
  geom_point(aes(x = Euros_Any, y = population_outside_ratio2, color = Euros_Any, size = population_outside_ratio2)) +
  scale_color_viridis(option = "viridis", guide = "legend") +
  scale_size_continuous(range = c(2, 9))

gg18

ggsave(gg10, filename = "maps/f_residents_outside_by_foreign.png", width = 8.335, height = 6.946, dpi = 1200)
ggsave(gg11, filename = "maps/f_residents_inside_by_foreign.png", width = 8.335, height = 6.946, dpi = 1200)
ggsave(gg12, filename = "maps/f_residents_flows_by_foreign.png", width = 8.335, height = 6.946, dpi = 1200)
ggsave(gg13, filename = "maps/f_distinct_destinations_by_foreign.png", width = 8.335, height = 6.946, dpi = 1200)
ggsave(gg14, filename = "maps/f_residents_outside_by_foreign_inc.png", width = 8.335, height = 6.946, dpi = 900)
ggsave(gg15, filename = "maps/f_residents_inside_by_foreign_inc.png", width = 8.335, height = 6.946, dpi = 900)
ggsave(gg16, filename = "maps/f_residents_flows_by_foreign_inc.png", width = 8.335, height = 6.946, dpi = 900)
ggsave(gg17, filename = "maps/f_distinct_destinations_by_foreign_inc.png", width = 8.335, height = 6.946, dpi = 900)
ggsave(gg18, filename = "maps/f_residents_outside_inc.png", width = 8.335, height = 6.946, dpi = 900)


#create descriptive statistics

# Create a new non-spatial data frame with selected variables
non_spatial_data <- data.frame(
  population_outside_ratio = resident_flows1$population_outside_ratio,
  population_stay_ratio = resident_flows1$population_stay_ratio,
  distinct_destinations = resident_flows1$distinct_destinations,
  ratio_flows = resident_flows1$ratio_flows,
  foreignpop = resident_flows1$foreignpop,
  Euros_Any = resident_flows1$Euros_Any
)

# create variable to describe
stats <- describe(non_spatial_data)


# Create a new table with the descriptive statistics
table_stats <- flextable(stats)

# Rename the rows
row_names <- c(
  "Percentage of resident population outside of residential areas",
  "Percentage of population not leaving their residential areas",
  "Distinct destinations",
  "Origin flow ratio",
  "Foreign Population",
  "Annual disposable household income per capita"
)
table_stats <- set_header_labels(table_stats, row_label = row_names)

# Create a Word document
doc <- read_docx()

# Add the table to the document
doc <- body_add_flextable(doc, table_stats)

# Save the document as a Word file
output_file <- "descriptive_statistics.docx"
print(doc, target = output_file)


#create regression tables

#make regression tables for foreign pop

model1 <- lm(population_outside_ratio2 ~ foreignpop, data = full_outside1)
model2 <- lm(population_stay_ratio2~ foreignpop, data= full_inside1 )
model3 <- lm(distinct_destinations~foreignpop, data =distinct_destinations1)
model4 <- lm(ratio_flows~foreignpop, data=resident_flows1)
summary(model1)
summary(model2)
summary(model3)
summary(model4)

# Create an HTML table with the regression results
html_table <- stargazer(model1, model2, model3, model4, type = "html")

# Save the HTML table to a file
writeLines(html_table, "Regression_Results_foreigner.html")



# Model without control variable
full_model1 <- lm(population_outside_ratio2 ~ Euros_Any, data = full_outside1)
full_model3 <- lm(population_stay_ratio2 ~ Euros_Any, data=full_inside1)
full_model5 <- lm(distinct_destinations~Euros_Any, data=distinct_destinations1)
full_model7 <- lm(ratio_flows~Euros_Any, data=resident_flows1)

# Model with control variable
full_model2 <- lm(population_outside_ratio2 ~ Euros_Any + foreignpop, data = full_outside1)
full_model4 <- lm(population_stay_ratio2 ~ Euros_Any + foreignpop, data = full_inside1)
full_model6 <- lm(distinct_destinations~Euros_Any+ foreignpop, data=distinct_destinations1)
full_model8 <- lm(ratio_flows~Euros_Any + foreignpop, data=resident_flows1)

full_model_results1 <- tidy(full_model1, conf.int= TRUE) %>%
  mutate( model = "Model 1")
full_model_results2 <- tidy(full_model2, conf.int= TRUE) %>%
  mutate(model= "Model 2")

model_results <- bind_rows(full_model_results1, full_model_results2)

gg30=ggplot(data = model_results,
        aes (x = estimate, y = term, xmin=conf.low, xmax=conf.high,
             color = model, shape = model))+
  geom_pointrange() +
  labs(title = "Model Estimates of % of Resident population who leave area of residence",
       x = "Coefficient Estimate",
       y = "Predictor",
       caption = "Models fit with OLS. Error bars show the 95% confidence interval.") +
  scale_y_discrete(labels = c("Intercept", "Disposable Income", "Neighborhood Diversity")) +
  ggpubr::theme_pubclean(flip = TRUE)

full_model_results3 <- tidy(full_model3, conf.int= TRUE) %>%
  mutate( model = "Model 3")
full_model_results4 <- tidy(full_model4, conf.int= TRUE) %>%
  mutate(model= "Model 4")

model_results2 <- bind_rows(full_model_results3, full_model_results4)

gg31=ggplot(data = model_results2,
            aes (x = estimate, y = term, xmin=conf.low, xmax=conf.high,
                 color = model, shape = model))+
  geom_pointrange() +
  labs(title = "Model Estimates of % of Resident population who
do not leave area of residence",
       x = "Coefficient Estimate",
       y = "Predictor",
       caption = "Models fit with OLS. Error bars show the 95% confidence interval.") +
  scale_y_discrete(labels = c("Intercept", "Disposable Income", "Neighborhood Diversity")) +
  ggpubr::theme_pubclean(flip = TRUE)

gg31

summary(full_model1)
summary(full_model2)
summary(full_model3)
summary(full_model4)
summary(full_model5)
summary(full_model6)
summary(full_model7)
summary(full_model8)


# Create an HTML table with the regression results
html_table <- stargazer(full_model1, full_model2, type = "html")
html_table2 <- stargazer(full_model3, full_model4, type = "html")
html_table3 <- stargazer(full_model5, full_model6, type = "html")
html_table4 <- stargazer(full_model7, full_model8, type = "html")

# Save the HTML table to a file
writeLines(html_table, "Regression_Results_full.html")
writeLines(html_table2, "Regression_Results_full2.html")
writeLines(html_table3, "Regression_Results_full3.html")
writeLines(html_table4, "Regression_Results_full4.html")

# Perform the first correlation test
cor_test1 = cor.test(full_outside1$foreignpop, full_outside1$population_outside_ratio2, method = "spearman")
cor_test2 = cor.test(full_inside1$foreignpop, full_inside1$population_stay_ratio2, method = "spearman")
cor_test3 = cor.test(distinct_destinations1$foreignpop, distinct_destinations1$distinct_destinations, method="spearman")
cor_test4 = cor.test(resident_flows1$foreignpop, resident_flows1$ratio_flows, method = "spearman")

gg19=ggplot(data = full_outside1, mapping = aes(x = foreignpop, y = population_outside_ratio2)) +
  geom_point(shape = 21, fill = 'black', color = 'white', size = 3) + 
  labs(x = "Foreign population (%)",
       y = "Percentage of population outside of their residence") +
  sm_statCorr(corr_method = 'spearman',
              fit.params = list(color = 'red', 
                                linetype = 'dashed'), 
              separate_by = '\n')

gg20=ggplot(data = full_inside1, mapping = aes(x = foreignpop, y = population_stay_ratio2)) +
  geom_point(shape = 21, fill = 'black', color = 'white', size = 3) + 
  labs(x = "Foreign population (%)",
       y = "Percentage of population who do not leave their area of residence") +
  sm_statCorr(corr_method = 'spearman',
              fit.params = list(color = 'red', 
                                linetype = 'dashed'), 
              separate_by = '\n')

gg21=ggplot(data = distinct_destinations1, mapping = aes(x = foreignpop, y = distinct_destinations)) +
  geom_point(shape = 21, fill = 'black', color = 'white', size = 3) + 
  labs(x = "Foreign population (%)",
       y = "Distinct Destinations") +
  sm_statCorr(corr_method = 'spearman',
              fit.params = list(color = 'red', 
                                linetype = 'dashed'), 
              separate_by = '\n')

gg21

gg22=ggplot(data = resident_flows1, mapping = aes(x = foreignpop, y = ratio_flows)) +
  geom_point(shape = 21, fill = 'black', color = 'white', size = 3) + 
  labs(x = "Foreign population (%)",
       y = "Origin flow ratio") +
  sm_statCorr(corr_method = 'spearman',
              fit.params = list(color = 'red', 
                                linetype = 'dashed'), 
              separate_by = '\n')

gg22


ggsave(gg19, filename = "maps/f_spearman_outside.png", width = 8.335, height = 6.946, dpi = 900)
ggsave(gg20, filename = "maps/f_spearman_inside.png", width = 8.335, height = 6.946, dpi = 900)
ggsave(gg21, filename = "maps/f_spearman_destinations.png", width = 8.335, height = 6.946, dpi = 900)
ggsave(gg22, filename = "maps/f_spearman_flows.png", width = 8.335, height = 6.946, dpi = 900)


gg23=ggplot(data = full_outside1, mapping = aes(x = Euros_Any, y = population_outside_ratio2)) +
  geom_point(shape = 21, fill = 'black', color = 'white', size = 3) + 
  labs(x = "Annual disposable household income (Euros)",
       y = "Percentage of population outside of their residence") +
  sm_statCorr(corr_method = 'spearman',
              fit.params = list(color = 'red', 
                                linetype = 'dashed'), 
              separate_by = '\n')

gg23

gg24=ggplot(data = full_inside1, mapping = aes(x = Euros_Any, y = population_stay_ratio2)) +
  geom_point(shape = 21, fill = 'black', color = 'white', size = 3) + 
  labs(x = "Annual disposable household income (Euros)",
       y = "Percentage of population who do not leave their area of residence") +
  sm_statCorr(corr_method = 'spearman',
              fit.params = list(color = 'red', 
                                linetype = 'dashed'), 
              separate_by = '\n')

gg24

gg25=ggplot(data = distinct_destinations1, mapping = aes(x = Euros_Any, y = distinct_destinations)) +
  geom_point(shape = 21, fill = 'black', color = 'white', size = 3) + 
  labs(x = "Annual disposable household income (Euros)",
       y = "Distinct Destinations") +
  sm_statCorr(corr_method = 'spearman',
              fit.params = list(color = 'red', 
                                linetype = 'dashed'), 
              separate_by = '\n')

gg25

gg26=ggplot(data = resident_flows1, mapping = aes(x = Euros_Any, y = ratio_flows)) +
  geom_point(shape = 21, fill = 'black', color = 'white', size = 3) + 
  labs(x = "Annual disposable household income (Euros)",
       y = "Origin flow ratio") +
  sm_statCorr(corr_method = 'spearman',
              fit.params = list(color = 'red', 
                                linetype = 'dashed'), 
              separate_by = '\n')

gg26


ggsave(gg23, filename = "maps/f_spearman_outside_inc.png", width = 8.335, height = 6.946, dpi = 900)
ggsave(gg24, filename = "maps/f_spearman_inside_inc.png", width = 8.335, height = 6.946, dpi = 900)
ggsave(gg25, filename = "maps/f_spearman_destinations_inc.png", width = 8.335, height = 6.946, dpi = 900)
ggsave(gg26, filename = "maps/f_spearman_flows_inc.png", width = 8.335, height = 6.946, dpi = 900)


#test for heteroskedasticity

#create residual vs. fitted plot
bptest(full_model1)
bptest(full_model2)
bptest(full_model3)
bptest(full_model4)
bptest(full_model5)
bptest(full_model6)
bptest(full_model7)
bptest(full_model8)

# Do a WLS model

# Calculate the residuals
residuals = residuals(full_model1)
residuals2 = residuals(full_model2)
residuals3 = residuals(full_model3)
residuals4 = residuals(full_model4)
residuals5 = residuals(full_model5)
residuals6 = residuals(full_model6)
residuals7 = residuals(full_model7)
residuals8 = residuals(full_model8)


# Compute the squared residuals
squared_residuals <- residuals^2
squared_residuals2 <- residuals2^2
squared_residuals3 <- residuals3^2
squared_residuals4 <- residuals4^2
squared_residuals5 <- residuals5^2
squared_residuals6 <- residuals6^2
squared_residuals7 <- residuals7^2
squared_residuals8 <- residuals8^2

# Assign weights based on the inverse of squared residuals
weights <- 1/squared_residuals
weights2 <- 1/squared_residuals2
weights3 <- 1/squared_residuals3
weights4 <- 1/squared_residuals4
weights5 <- 1/squared_residuals5
weights6 <- 1/squared_residuals6
weights7 <- 1/squared_residuals7
weights8 <- 1/squared_residuals8


# Fit the weighted regression model using robust estimation
weighted_full_model1 <- lm(population_outside_ratio2 ~ Euros_Any, data = full_outside1, weights = weights)
weighted_full_model2 <- lm(population_outside_ratio2 ~ Euros_Any + foreignpop, data = full_outside1, weights = weights2)
weighted_full_model3 <- lm(population_stay_ratio2 ~ Euros_Any , data = full_inside1, weights = weights3)
weighted_full_model4 <- lm(population_stay_ratio2 ~ Euros_Any + foreignpop, data = full_inside1, weights = weights4)
weighted_full_model5 <- lm(distinct_destinations ~ Euros_Any, data = distinct_destinations1, weights = weights5)
weighted_full_model6 <- lm(distinct_destinations~Euros_Any+ foreignpop, data=distinct_destinations1, weights = weights6)
weighted_full_model7 <- lm(ratio_flows ~ Euros_Any, data = resident_flows1, weights = weights7)
weighted_full_model8 <- lm(ratio_flows~Euros_Any + foreignpop, data=resident_flows1, weights = weights8)


# Assess the model's fit and residuals
summary(weighted_full_model1)
summary(weighted_full_model2)
summary(weighted_full_model3)
summary(weighted_full_model4)
summary(weighted_full_model5)
summary(weighted_full_model6)
summary(weighted_full_model7)
summary(weighted_full_model8)


# Create an HTML table with the regression results
html_table5 <- stargazer(weighted_full_model1, weighted_full_model2, type = "html")
html_table6 <- stargazer(weighted_full_model3, weighted_full_model4, type = "html")
html_table7 <- stargazer(weighted_full_model5, weighted_full_model6, type = "html")
html_table8 <- stargazer(weighted_full_model7, weighted_full_model8, type = "html")

# Save the HTML table to a file
writeLines(html_table5, "WeightedRegression_Results_full.html")
writeLines(html_table6, "WeightedRegression_Results_full2.html")
writeLines(html_table7, "WeightedRegression_Results_full3.html")
writeLines(html_table8, "WeightedRegression_Results_full4.html")

full_model_results5 <- tidy(weighted_full_model5, conf.int= TRUE) %>%
  mutate( model = "Model 5")
full_model_results6 <- tidy(weighted_full_model6, conf.int= TRUE) %>%
  mutate(model= "Model 6")

model_results3 <- bind_rows(full_model_results5, full_model_results6)
model_results3[1, 2:7] <- model_results3[1, 2:7] * 0.1000
model_results3[3, 2:7] <- model_results3[3, 2:7] * 0.1000
model_results3[5, 2:7] <- model_results3[5, 2:7] * 0.10000
gg32=ggplot(data = model_results3,
            aes (x = estimate, y = term, xmin=conf.low, xmax=conf.high,
                 color = model, shape = model))+
  geom_pointrange() +
  labs(title = "Model Estimates of number of distinct destinations",
       x = "Coefficient Estimate",
       y = "Predictor",
       caption = "Models fit with WLS. Error bars show the 95% confidence interval.") +
  scale_y_discrete(labels = c("Intercept", "Disposable Income", "Neighborhood Diversity")) +
  ggpubr::theme_pubclean(flip = TRUE)

gg32


full_model_results7 <- tidy(weighted_full_model7, conf.int= TRUE) %>%
  mutate( model = "Model 7")
full_model_results8 <- tidy(weighted_full_model8, conf.int= TRUE) %>%
  mutate(model= "Model 8")

model_results4 <- bind_rows(full_model_results7, full_model_results8)

gg33=ggplot(data = model_results4,
            aes (x = estimate, y = term, xmin=conf.low, xmax=conf.high,
                 color = model, shape = model))+
  geom_pointrange() +
  labs(title = "Model Estimates of origin ratio flows",
       x = "Coefficient Estimate",
       y = "Predictor",
       caption = "Models fit with WLS. Error bars show the 95% confidence interval.") +
  scale_y_discrete(labels = c("Intercept", "Disposable Income", "Neighborhood Diversity")) +
  ggpubr::theme_pubclean(flip = TRUE)

gg33

ggsave(gg30, filename = "maps/presentation1.png", width = 8.335, height = 6.946, dpi = 900)
ggsave(gg31, filename = "maps/presentation2.png", width = 8.335, height = 6.946, dpi = 900)
ggsave(gg32, filename = "maps/presentation3.png", width = 8.335, height = 6.946, dpi = 900)
ggsave(gg33, filename = "maps/presentation4.png", width = 8.335, height = 6.946, dpi = 900)
