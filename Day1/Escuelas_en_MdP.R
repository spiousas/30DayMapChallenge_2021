# Setup ####
pacman::p_load(tidyverse, sf, janitor, ggtext, ggbump, extrafont, here)

# Los datos ####
# Leo el mapa de Mar del Plata
mdp <- st_read(here("./base_map/gral_pueyrredon/gral_pueyrredon.shp")) %>%
  clean_names()

# Leo el mapa callejero de Mar del Plata
calles <- st_read(here("./base_map/callejero_0/calles.shp")) %>%
  clean_names()

# Leo los mapas de las escuelas y los junto con un label de acuerdo a la gestión
escuelas <- st_read(here("./Day1/data/escuelas-privadas/escuelasPrivadas.shp")) %>%
  clean_names() %>%
  mutate(tipo = "Privada") %>%
  rbind(st_read(here("./Day1/data/escuelas-publicas-municipales/escuelasPublicasMunicipales.shp")) %>%
          clean_names() %>%
          mutate(tipo = "Municipal")) %>%
  rbind(st_read(here("./Day1/data/escuelas-publicas-provinciales/escuelasPublicasProvinciales.shp")) %>%
          clean_names() %>%
          mutate(tipo = "Provincial"))

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
  # Mapa de las escuelas coloreadas por tipo
  geom_sf(data = escuelas,
          mapping = aes(color = tipo),
          size = 1,
          alpha = .5,
          fill = "transparent") +
  # Título
  annotate(
    geom = "richtext", x =  -58.15 , y = -37.68,
    label = "<span style='color:#04A5CB;'><b>Distribución de instituciones<br>educativas en el Partido<br>de General Pueyrredon</b></span>",
    family = "Roboto",
    size = 6, 
    hjust = 0,
    vjust = 1,
    fill = NA, label.color = NA
  ) +
  # Subítulo
  annotate(
    geom = "richtext", x =  -58.15 , y = -38.2,
    label = "En el mapa se pueden ver como puntos<br>la distribución de las instituciones<br>educativas coloreadas de acuerdo a su gestión.<br><br>
    La distribución no es homogénea,<br>con más concentración de inst. ed. <span style='color:#FE9D02;'><b>privadas</b></span><br>
    en la zona céntrica y más escuelas <span style='color: #A9BE33;'><b>municipales</b></span><br> y <span style='color: #04ADC3;'><b>provinciales</b></span> en la zona periférica de la ciudad<br>de <b>Mar del Plata</b>. ",
    family = "Roboto",
    size = 2.7, 
    hjust = 0,
    vjust = 0,
    fill = NA, label.color = NA
  ) +
  # Caption
  annotate(
    geom = "richtext", x = -57.5, y = -38.2456,
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
  scale_color_manual(breaks = c("Privada", "Municipal", "Provincial"),
                     values = c("#FE9D02", "#A9BE33", "#04ADC3")) +
  theme(plot.margin = margin(t = 0, r = .5, b = 0, l = .5, "cm"),
        legend.position = "none",
        plot.background = element_rect(fill = "#F6F5F5", color = "#F6F5F5")) 

# Guardo la figura
ggsave(here("./Day1/Escuelas_MGP.png"), width = 15, height = 17, units = "cm")

       