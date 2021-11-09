# Setup ####
pacman::p_load(raster, tidyverse, here, sp, sf)

# Los datos ####
# Los datos de la zona
dem.raster <- getData("SRTM", lat = -37.94733974478734, lon = -57.77767482589422, download = TRUE, path = here("./Day9/data"))

# Armo un cuadrado de las sierras
my_bbox <- c(xmin = -57.79,
             xmax = -57.76,
             ymin = -37.97,
             ymax = -37.93)

my_bbox.m <- matrix(c(my_bbox['xmin'], my_bbox['xmin'], my_bbox['xmax'], my_bbox['xmax'], my_bbox['xmin'], 
                      my_bbox['ymax'], my_bbox['ymin'], my_bbox['ymin'], my_bbox['ymax'], my_bbox['ymax']),
         ncol = 2)
my_bbox.sf <- st_geometry(st_polygon(x = list(my_bbox.m)))
st_crs(my_bbox.sf) <- 4326

# Corto los datos de la zona
dem.raster.cropped <- crop(dem.raster, as(my_bbox.sf, 'Spatial'), snap='out')

# Los convierto en un df
dem.m  <-  rasterToPoints(dem.raster.cropped)
dem.df <-  data.frame(dem.m)
colnames(dem.df) = c("lon", "lat", "alt")

# Armo elevación para que queden separadas
dem.df$elev <- abs(dem.df$lat)*100000 + dem.df$alt * 8

# La figura ####
# Armo la figura base
p = ggplot() + 
  labs(x = 'BUENOS AIRES, ARGENTINA', 
       y = NULL, 
       title = 'SIERRA DE LOS PADRES',
       caption = "@spiousas") + 
  theme_minimal() +
  theme(axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        plot.background = element_rect(fill = "black"),
        plot.margin = margin(l = 60, r = 60, t = 20, b = 20),
        plot.title = element_text(family = "Helvetica", colour = 'white', size = 40, hjust = .5),
        axis.title.x = element_text(family = "Helvetica", colour = 'white', size = 25),
        plot.caption = element_text(hjust = .5, size = 10, family = "Helvetica", color = "white"))

# Sumo una línea por cada latitud
for(g in unique(dem.df$lat)){
  dat = subset(dem.df, lat == g)
  p = p + geom_line(data = dat, aes(lon, elev), col='white', size=.5)
}

# Guardo la figura
ggsave(here("./Day9/Unknown_Sierras.png"), width = 7, height = 10, dpi = 300)
