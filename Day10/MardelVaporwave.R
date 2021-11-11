# Setup ####
pacman::p_load(raster, tidyverse, here, sp, sf, vapoRwave, extrafont, viridis)

# Los datos ####
# Los datos de la zona
dem.raster <- getData("SRTM", lat = -37.94733974478734, lon = -57.77767482589422, download = TRUE, path = here("./Day10/data"))

# Armo el círculo para cortar
crs2 <- 6384 # https://epsg.io/6384
center = c(long = -57.5481297228625, lat = -38.02345711413089) #-57.54827
center_proj <-
  tibble(lat = center["lat"], long = center["long"]) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

dist <- 7000
circle <- tibble(lat = center["lat"], long = center["long"]) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = crs2) %>%
  st_buffer(dist = dist) %>%
  st_transform(crs = 4326)

# Corto la zona y la hago df
dem.raster_cropped <- crop(dem.raster, circle)
dem.raster_masked <- mask(dem.raster_cropped, circle)
dem.m  <-  rasterToPoints(dem.raster_masked)
dem.df <-  data.frame(dem.m)
colnames(dem.df) = c("lon", "lat", "alt")

# Leo las calles
calles <- st_read(here("./base_map/callejero_0/calles.shp")) %>% st_intersection(circle)

# La figura ####
ggplot() +
  # Círculo negro
  geom_sf(
    data = circle,
    color = NA,
    fill = "black",
    size = 1,
    alpha = 1
  ) +
  # El raster
  geom_tile(
    data = dem.df, 
    aes(x = lon, 
        y = lat, 
        fill = alt), 
    alpha = 1
    ) + 
  # Mapa de las calle
  geom_sf(data = calles,
          color = "#F8B660",
          size = .5,
          alpha = .3,
          fill = "transparent") +
  # El borde del círculo
  geom_sf(
    data = circle,
    color = "#FF819C",
    fill = NA,
    size = 2,
    alpha = 1
  ) +
  # Estilo
  labs(title = "Mar del Vaporwave",
       caption = "Fuente: Datos abiertos MGP | Viz: @spiousas",
       fill = "ALTITUD\n(m)",
       x = NULL,
       y = NULL) +
  new_retro() + 
  scale_fill_viridis_c(option = "plasma",
                       limits = c(-10, 60)) +
  coord_sf() +
  theme(legend.key.width=unit(.3, "cm"),
        legend.key.height=unit(2, "cm"),
        plot.title.position = "plot",
        plot.title = element_text(size = 40, color = "#FF4373", hjust = .5),
        plot.caption.position = "plot",
        plot.caption = element_text(family = "Blade Runner Movie Font", size = 8, color = "#FF4373", hjust = 1))

# Guardo la figura
ggsave(here("./Day10/MardelVaporwave.png"), width = 20, height = 16, units = "cm", dpi = 300)
                                    