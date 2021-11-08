# Setup ####
pacman::p_load(tidyverse, osmdata, here, sf, ggtext, janitor, colorspace)

# Los datos ####
# Seteo Mar del Plata como origen
bbx <- getbb("Mar del Plata, Argentina")

# Calles y avenidas de OSM
highways <- bbx %>%
  opq() %>%
  add_osm_feature(
    key = "highway",
    value = c(
      "motorway",
      "trunk",
      "primary",
      "secondary",
      "tertiary",
      "motorway_link",
      "trunk_link",
      "primary_link",
      "secondary_link",
      "tertiary_link"
    )
  ) %>%
  osmdata_sf()

# Calles y avenidas de OSM
streets <- bbx %>%
  opq() %>%
  add_osm_feature(
    key = "highway",
    value = c(
      "residential",
      "living_street",
      "service",
      "unclassified",
      "pedestrian",
      "footway",
      "track",
      "path"
    )
  ) %>%
  osmdata_sf()

# Armo el círculo para cortar
crs2 <- 6384 # https://epsg.io/6384
center = c(long = -57.59, lat = -37.99772) #-57.54827
center_proj <-
  tibble(lat = center["lat"], long = center["long"]) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

dist <- 10000
circle <- tibble(lat = center["lat"], long = center["long"]) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  st_transform(crs = crs2) %>%
  st_buffer(dist = dist) %>%
  st_transform(crs = 4326)

# Interseco las calles con el círculo
streets_lines <- st_intersection(circle, streets$osm_lines)
highways_lines <- st_intersection(circle, highways$osm_lines)

# Leo los reocrridos de los colectivos
recorridos <- st_read(here("./Day6-7-8/data/recorridos/recorridos.shp")) %>%
  clean_names() %>%
  st_intersection(circle) %>%
  mutate(color = case_when(col1 %in% c("521", "522", "523", "541", "542", "543", "551", "552", "553", "554", "555") ~ "Roja",
                           col1 %in% c("591", "593") ~ "Celeste",
                           col1 %in% c("562", "563") ~ "Azul",
                           col1 %in% c("511", "512", "717") ~ "Bordó",
                           col1 %in% c("501", "571", "573") ~ "Amarilla",
                           col1 %in% c("531", "532", "533") ~ "Verde",
                           TRUE ~ "Otro")
         ) %>%
  filter(color != "Otro")

# Armo los rectángulos para meterle color
rectangles <- tibble(color = rep(unique(recorridos$color), each = 4),
  x = rep(c(st_bbox(circle)[1], center[1], st_bbox(circle)[3], st_bbox(circle)[1]), length(unique(recorridos$color))),
  y = rep(c(center[2], center[2], st_bbox(circle)[4]+.01, st_bbox(circle)[4]+.01), length(unique(recorridos$color)))
)

# La figura ####
ggplot() +
  # Rectángulos con color  
  geom_polygon(
    data = rectangles,
    aes(x = x, y = y, fill = color),
    color = NA,
    alpha = .7
  ) +
  # El círculo
  geom_sf(
    data = circle,
    color = "gray50",
    fill = lighten("#04A5CB", .9),
    size = .7,
    alpha = 1
  ) +
  # Las calles
  geom_sf(
    data = streets_lines,
    col = "gray50",
    size = .2,
    alpha = .8
  ) +
  # Avenidas
  geom_sf(
    data = highways_lines,
    col = "gray50",
    size = .2,
    alpha = .8
  ) +
  # Recorridos
  geom_sf(
    data = recorridos,
    aes(color = color),
    size = .5,
    alpha = 1
  ) +
  # El título
  geom_text(
    data = rectangles,
    aes(label = paste0("Línea ", color)),
    x =  st_bbox(circle)[1]+.005 , y = st_bbox(circle)[4]+.005,
    family = "Roboto",
    size = 5,
    hjust = 0,
    vjust = 0.5,
    color ="white"
  ) +
  # Separo por línea
  facet_wrap(.~color, ncol = 2) +
  # Estilo
  coord_sf(clip = 'off') +
  scale_color_manual(breaks = c("Roja", "Bordó", "Amarilla", "Azul", "Celeste", "Verde"),
                     values = c("#D80404", "#6B2132", "#F87303", "#253E65", "#2E89D6", "#337B45")) +
  scale_fill_manual(breaks = c("Roja", "Bordó", "Amarilla", "Azul", "Celeste", "Verde"),
                     values = c("#D80404", "#6B2132", "#F87303", "#253E65", "#2E89D6", "#337B45")) +
  labs(caption = "Fuente: Datos abiertos MGP | Visualización: @spiousas") + 
  theme_void() + 
  theme(legend.position = "none",
        plot.caption = element_markdown(hjust = .95, family = "Roboto", color = "gray20"),
        plot.background = element_rect(fill = "#F3F6F7", color = NA),
        plot.margin = margin(t = 0.5, b = 0.1, l = 0.5, r = 0.1),
        strip.background = element_blank(),
        strip.text.x = element_blank()) 

# Guardo la figura
ggsave(here("./Day6-7-8//MdP_bus.png"), width = 6, height = 10, dpi = 300)
