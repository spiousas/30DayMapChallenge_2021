# Setup ####
pacman::p_load(tidyverse, sf, janitor, ggtext, ggbump, extrafont, here, glue, 
               colorspace, viridis)

# Los datos ####
# Leo el mapa de Mar del Plata
mdp <- st_read(here("./base_map/gral_pueyrredon/gral_pueyrredon.shp")) %>%
  clean_names()

# Leo el mapa callejero de Mar del Plata
calles <- st_read(here("./base_map/callejero_0/calles.shp")) %>%
  clean_names()

# Leo los mapas de los arroyos
arroyos <- st_read(here("./Day2/data/arroyos/arroyos_2020.shp")) %>%
  clean_names() %>%
  mutate(tipo = if_else(str_detect(nombre, "AFL"), "Afluente", "Arroyo"))

# La figura ####
fig_mapa <- ggplot() + 
  # Mapa de las calles
  geom_sf(data = calles,
          color = "gray90",
          size = 1,
          fill = "transparent",
          alpha = 0.4) +
  # Mapa de la ciudad de Mar del Plata
  geom_sf(data = st_transform(mdp, "+proj=longlat +datum=WGS84"),
          size = .5,
          fill = "transparent",
          color = "black") +
  # Mapa de las arroyos
  geom_sf(data = arroyos,
          aes(color = tipo),
          size = 1,
          fill = "transparent",
          alpha = 0.7) +
  # Título
  annotate(
    geom = "richtext", x =  -58.1 , y = -37.68,
    label = "<span style='color:#04A5CB;'><b>Los arroyos del<br>Partido de<br>General<br>Pueyrredon</b></span>",
    family = "Roboto",
    size = 8, 
    hjust = 0,
    vjust = 1,
    fill = NA, label.color = NA
  ) +
  # Subítulo
  annotate(
    geom = "richtext", x =  -58.1 , y = -38.2,
    label = glue("De acuerdo a los datos abiertos<br>de la <b>Municipalidad de General<br>Pueyrredon</b>, en el partido hay<br><span style='color: #04ADC3;'><b>{length(unique(arroyos$nombre[arroyos$tipo=='Arroyo']))} arroyos</b></span> y <span style='color: #A9BE33;'><b>{length(unique(arroyos$nombre[arroyos$tipo=='Afluente']))} afluentes</b></span>."),
    family = "Roboto",
    color = "black",
    size = 4, 
    hjust = 0,
    vjust = 0,
    fill = NA, label.color = NA
  ) +
  # Caption
  annotate(
    geom = "richtext", x = -57.5, y = -38.23,
    label = "Fuente: Datos abiertos MGP<br>Visualización: @spiousas",
    family = "Roboto",
    size = 2.5, 
    hjust = 1,
    vjust = 0,
    color = "grey20",
    fill = NA, label.color = NA
  ) +
  # Estilo
  theme_void() +
  scale_color_manual(breaks = c("Arroyo", "Afluente"),
                     values = c("#04ADC3", "#A9BE33")) +
  labs(color = NULL) +
  guides(color = guide_legend(ncol=1)) +
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, "cm"),
        legend.text = element_markdown(color = "white", family = "Roboto"),
        legend.position = "none",
        plot.background = element_rect(fill = "#F6F5F5", color = "#F6F5F5")) 

# Guardo la figura
ggsave(here("./Day2/Arroyos_MGP.png"), width = 15, height = 17, units = "cm")
