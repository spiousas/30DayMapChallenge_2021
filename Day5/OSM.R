# Setup ####
pacman::p_load(tidyverse, sf, janitor, ggtext, extrafont, here, osmdata, Rcpp)

# Leo las fracciones censales y me quedo con las céntricas
fracciones <- st_read(here("./Day3/data/fracciones_0/fracciones.shp")) %>%
  clean_names() %>% 
  filter(gid>12) %>% 
  st_set_crs(4326) 

# Leo el mapa callejero de Mar del Plata y filtro sólo las que están en las fracciones céntricas
calles <- st_read(here("./base_map/callejero_0/calles.shp")) %>%
  clean_names() %>% st_intersection(fracciones)

# Bounding box seg'un el callejero de Mar del Plara
m <- st_bbox(calles)

# Armos los queries
q_restaurant <- m %>% 
  opq (timeout = 25*100) %>%
  add_osm_feature("amenity", "restaurant")

q_ice_cream <- m %>% 
  opq (timeout = 25*100) %>%
  add_osm_feature("amenity", "ice_cream")

q_bar <- m %>% 
  opq (timeout = 25*100) %>%
  add_osm_feature("amenity", "bar")

# Hago los queries
restaurant <- osmdata_sf(q_restaurant)
ice_cream <- osmdata_sf(q_ice_cream)
bar <- osmdata_sf(q_bar)

# La figura ####
fig_mapa <- ggplot() + 
  # Mapa de las calles
  geom_sf(data = calles,
          color = "white",
          size = .1,
          fill = "transparent",
          alpha = 0.4) +
  # Restaurantes
  geom_sf(data = restaurant$osm_points,
          colour = "#04ADC3",
          fill = "#04ADC3",
          alpha = .5,
          size = 1,
          shape = 21) +
  # Heladerías
  geom_sf(data = ice_cream$osm_points,
          colour = "#A9BE33",
          fill = "#A9BE33",
          alpha = .5,
          size = 1,
          shape = 21) +
  # Bares
  geom_sf(data = bar$osm_points,
          colour = "#FE9D02",
          fill = "#FE9D02",
          alpha = .5,
          size = 1,
          shape = 21) +
  # Título
  annotate(
    geom = "richtext", x = -57.64 , y = -38.07,
    label = "<span style='color:#04ADC3;'><b>Restaurantes</b></span>, <span style='color:#A9BE33;'><b>heladerías</b></span> y <span style='color:#FE9D02;'><b>bares</b></span> en<br>Mar del Plata según Open Street Maps.",
    family = "Roboto",
    size = 3, 
    color = "gray80",
    hjust = 0,
    vjust = 1,
    fill = NA, label.color = NA
  ) +
  # Estilo
  theme_void() +
  labs(caption = "Data: © OpenStreetMap contributors | Vis: @spiousas") +
  theme(plot.caption = element_markdown(size = 8, hjust = 1, color = "gray80"),
        plot.caption.position = "plot",
        plot.background = element_rect(fill = "black", color = "black"))
  
fig_mapa
# Guardo la figura
ggsave(here("./Day5/OSM.png"), width = 12, height = 17, units = "cm", dpi = 300)
