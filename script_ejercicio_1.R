

# Load libraries ----------------------------------------------------------
install.packages('pacman') # To install pacman
require(pacman)
pacman::p_load(readr, leaflegend, mapview, leaflet.extras, rgdal, leaflet, fs, ggspatial, ggthemes, RColorBrewer, raster, sf, spatstat, maptools, rgeos, tmap, gstat, sp, rpostgis, RPostgreSQL, srt, glue, tidyverse, sf)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
data <- read.table('./tbl/temperatura_maxima_media_agosto.csv', header = TRUE, sep = ';')
head(data)
colnames(data) <- c('indicativo', 'x', 'y', 'temperatura')

glimpse(data)

# Convert comma to point --------------------------------------------------
data <- mutate(data, x = gsub(',', '\\.', x), y = gsub(',', '\\.', y))
data <- mutate(data, x = as.numeric(x), y = as.numeric(y))

# Read the shapefile ------------------------------------------------------
poly <- raster::shapefile('./shp/provincias_extremadura.shp')
poly$nameunit <- iconv(poly$nameunit, from = 'UTF-8', to = 'latin1')

# Filtering Caceres
poly <- poly[poly@data$nameunit == 'Cáceres',]
plot(poly)
points(data$x, data$y, pch = 16, col = 'red')

# Table to shapefile ------------------------------------------------------
pnts <- data
coordinates(pnts) <- ~ x + y
raster::crs(pnts) <- raster::crs(poly)

# Dirichlet ---------------------------------------------------------------
th0 <- as(dirichlet(as.ppp(pnts)), "SpatialPolygons")
raster::crs(th0) <- raster::crs(pnts) # Coordinate system

plot(th0)
plot(pnts, add = TRUE, col = 'red', pch = 16)

# Overlay between the polygons and the points
th.df <- sp::over(th0, pnts)
th0 <- SpatialPolygonsDataFrame(th0, th.df)

# Map using tmap library

tmap_mode("plot")
tm_shape(th0) +
  tm_polygons(col = "temperatura", title="pluviometria", palette = "RdBu", border.col="red") +
  tm_legend(show=TRUE) +
  tm_shape(pnts) +
  tm_dots(col="black", size=0.1) +
  tm_legend(legend.outside=TRUE)

# Interactive map with tmap
tmap_mode("view")
tm_shape(th0) +
  tm_polygons(col = "temperatura", title="pluviometria", palette = "RdBu", border.col="red") +
  tm_legend(show=TRUE) +
  tm_shape(pnts) +
  tm_dots(col="black", size=0.1) +
  tm_legend(legend.outside=TRUE) %>% 
  mapshot()

# Interactive map with leaflet 
poly <- st_read('./output/ngb.shp')
pnts <- st_read('./output/pnt_tmp.shp')
poly <- st_transform(poly, crs = st_crs(4326))
pnts <- st_transform(pnts, crs = st_crs(4326))
mrkr <- st_coordinates(pnts) %>% as_tibble()

# Palette 
pal <- colorQuantile(
  palette = cpt(pal = "jjg_misc_temperature"),
  domain = poly$temprtr,
  n = 5)

leaflet() %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>% 
  addPolygons(data = poly) %>% 
  addMarkers(data = mrkr, lng = 'X', lat = 'Y')
  addPolygons(
    weight = 2,
    opacity = 1,
    fillOpacity = 1,
    popup = poly$temprtr 
  ) 
  
pnts %>% 
  leaflet() %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>% 
  addMarkers(popup = pnts$temprtr)
  

poly %>% 
  leaflet() %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>% 
  addPolygons(
    color = 'red',
    weight = 2,
    opacity = 1,
    fillColor = ~pal(temprtr),
    fillOpacity = 1,
    popup = poly$temprtr 
  ) 


# Map using ggplot2 library
gmap <- ggplot() + 
  geom_sf(data = st_as_sf(th0), aes(fill = temperatura)) + 
  scale_fill_gradientn(colors = brewer.pal(n = 9, name = 'RdYlBu')) +
  geom_sf(data = st_as_sf(pnts), col = 'black') +
  coord_sf() + 
  theme_bw() +
  ggtitle(label = 'Interpolación Vecino más Próximo') +
  labs(fill = 'Temperatura') +
  theme(legend.title = element_text(face = 'bold'), 
        legend.position = 'bottom',
        legend.key.width = unit(2, 'line'), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        plot.title = element_text(face = 'bold', size = 16, hjust = 0.5)) +
  annotation_scale(location =  "br", width_hint = 0.5, text_col = 'white') +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.2, "in"), 
                         style = north_arrow_fancy_orienteering())

dir_create('./png')
ggsave(plot = gmap, filename = './png/map vecino mas proximo.png', units = 'in', width = 8, height = 8, dpi = 300)

# Save the results as shapefile -------------------------------------------
raster::shapefile(th0, './output/ngb.shp')
raster::shapefile(pnts, './output/pnt_tmp.shp')
