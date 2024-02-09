Sys.setenv(JAVA_HOME = "C:/Program Files/Java/jdk-11.0.13")
# give r5r more memory. 8GB is a lot and can be reduced on less powerful computers
options(java.parameters = "-Xmx24G")

library(rJava)
library(r5r) # old version is 0.7.1
library(sf)
library(tmap)
library(tmaptools)
library(units)
library(tidyverse)

path <- '/Users/your/output/directory/here/'
tmap_mode('plot')

all_origins <- tibble(id = c('1', '2', '3'), lat = c(52.09075, 53.51878, 53.44297), lon = c(5.1217, -113.49598, -113.4900), map_name = c('utrecht', 'strathcona', 'ikea')) %>% st_as_sf(coords = c('lon', 'lat'), crs = 4326)

output_isos <- vector("list", length(all_origins))

for (i in seq_along(all_origins)) {
  origin <- all_origins %>% filter(id == i)
  map_name <- origin$map_name
  aoi <- st_buffer(origin, set_units(5, km))
  
  # download OSM data from geofabrik.de
  if (origin$id == '1') {
    data_path <- 'D:/Data/Spatial/NL' # place netherlands-latest.osm.pbf here
  } else {
    data_path <- 'D:/Data/Spatial/Alberta' # place alberta-latest.osm.pbf here
  }
  
  r5r_core <- setup_r5(data_path = data_path, verbose = FALSE)
  
  isos_4 <- isochrone(r5r_core,
                      origins = origin,
                      mode = c('BICYCLE'),
                      cutoffs = 15,
                      max_lts = 4,
                      bike_speed = 20) %>% st_make_valid()
  
  isos_1 <- isochrone(r5r_core,
                      origins = origin,
                      mode = c('BICYCLE'),
                      cutoffs = 15,
                      max_lts = 1,
                      bike_speed = 20) %>% st_make_valid()
  
  isos_1$lts <- 1
  isos_4$lts <- 4
  isos <- rbind(isos_1, isos_4) %>% mutate(iso_area = set_units(st_area(.), km^2), pct_eucl = iso_area / set_units(pi * 5^2, km^2))
  output_isos[[i]] <- isos
  
  basemap <- split(read_osm(aoi, type = 'osm'))
  
  outmap <- tm_shape(basemap) + tm_rgb(tm_mv('red', 'green', 'blue')) + tm_shape(aoi) + tm_borders(lwd = 3) + tm_shape(isos %>% filter(isochrone == 15)) + tm_borders(col = 'lts', col.scale = tm_scale_discrete(ticks = c(1, 4), values = c('#33a02c', '#e31a1c')), lwd = 3)
  
  tmap_save(outmap, file.path(path, paste0('map_', map_name, '_full.png')), height = 1080, width = 1920)
  
  outmap <- tm_shape(basemap) + tm_rgb(tm_mv('red', 'green', 'blue')) + tm_shape(aoi) + tm_borders(lwd = 3)
  
  tmap_save(outmap, file.path(path, paste0('map_', map_name, '_circle.png')), height = 1080, width = 1920)
}

all_isos <- bind_rows(output_isos)
out_table <- all_isos %>% st_drop_geometry() %>% pivot_wider(names_from = lts, values_from = c(iso_area, pct_eucl)) %>% left_join(., all_origins %>% st_drop_geometry() %>% select(id, map_name), by = c('id' = 'id'))
write_csv(out_table, file.path(path, 'isos_table.csv'))

# create map with square for Utrecht
origin <- all_origins %>% filter(id == 1)
aoi <- st_buffer(origin, set_units(5, km))
aoi_bbox <- st_bbox(aoi)
xmin <- aoi_bbox$xmin
xmax <- aoi_bbox$xmax
ymin <- aoi_bbox$ymin
ymax <- aoi_bbox$ymax
xmid <- mean(c(xmin, xmax))
ymid <- mean(c(ymin, ymax))
square_range <- st_sfc(st_polygon(list(cbind(c(xmid, xmax, xmid, xmin, xmid), c(ymin, ymid, ymax, ymid, ymin)))), crs = 4326)
basemap <- split(read_osm(aoi, type = 'osm'))
outmap <- tm_shape(basemap) + tm_rgb(tm_mv('red', 'green', 'blue')) + tm_shape(aoi) + tm_borders(lwd = 3) + tm_shape(square_range) + tm_borders(lwd = 3, col = '#ff7f00')
tmap_save(outmap, file.path(path, paste0('map_', 'utrecht_square.png')), height = 1080, width = 1920)
