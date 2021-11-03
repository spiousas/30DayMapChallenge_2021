# Setup ####
pacman::p_load(tidyverse, sf, janitor, ggtext, ggbump, extrafont, here, rgdal, 
               glue, patchwork, colorspace)

# Los datos ####
# Datos de población del censo (lo más razonable que conseguí es separado por sexo)
poblacion <- read_csv(here("./Day3/data/POBLACION-sexo.csv")) %>%
  clean_names() %>%
  mutate(fraccion = as.numeric(fraccion),
         radio = as.numeric(radio)) %>%
  mutate(total = varon + mujer) %>%
  select(-c(varon, mujer, codigo))

# Población por fracción censal
poblacion_fraccion <-  poblacion %>%
  group_by(fraccion) %>%
  summarize(total = sum(total))

# Población por radio censal
poblacion_radio <-  poblacion %>%
  group_by(fraccion, radio) %>%
  summarize(total = sum(total))

# Mapas de radios censales (hago un join con la cantidad de habitantes)
radios <- st_read(here("./Day3/data/radios_2/radios.shp")) %>%
  clean_names()  %>%
  mutate(fraccion = as.numeric(fraccion),
         radio = as.numeric(radio)) %>%
  left_join(poblacion_radio, by = c("fraccion", "radio"))

# Mapas de fracciones censales
fracciones <- st_read(here("./Day3/data/fracciones_0/fracciones.shp")) %>%
  clean_names() %>% st_set_crs(4979)

# La figura ####
# Figura del mapa
fig_mapa <- ggplot() + 
  # Mapa de las radios censales
  geom_sf(data = st_transform(radios, "+proj=longlat +datum=WGS84"),
          aes(alpha = total/2584),
          size = .1,
          fill = "#04ADC3",
          color = "#04ADC3") +
  # Mapa de las Fracciones censales
  geom_sf(data = st_transform(fracciones, "+proj=longlat +datum=WGS84"),
          size = .3,
          fill = "transparent",
          color = "#A9BE33") +
  # Título
  annotate(
    geom = "richtext", x =  -58.1 , y = -37.68,
    label = "<span style='color:#04A5CB;'><b>Radios y fracciones<br>censales del<br>Partido de<br>General<br>Pueyrredon</b></span>",
    family = "Roboto",
    size = 8, 
    hjust = 0,
    vjust = 1,
    fill = NA, label.color = NA
  ) +
  # Subítulo
  annotate(
    geom = "richtext", x =  -58.1 , y = -38.2,
    label = glue("En el partido hay <span style='color: #A9BE33;'><b>{nrow(fracciones)} fracciones</b></span> y<br><span style='color: #04ADC3;'><b>{nrow(radios)} radios</b></span> censales.<br><br>La intensidad de color de los <span style='color: #04ADC3;'><b>radios</b></span><br>censales es proporcional a su cantidad<br>de habitantes."),
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
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, "pt"),
        legend.position = "none",
        plot.background = element_rect(fill = "#F6F5F5", color = "#F6F5F5")) 

# Figura de la población
fig_ploblacion <- poblacion_fraccion %>% 
  ggplot(aes(x = reorder(fraccion, total),
           y = total)) +
  geom_hline(yintercept = c(10000, 20000), color = "gray90") +
  geom_col(fill = "#A9BE33",
           color = "white") +
  scale_y_continuous(breaks = c(0, 10000, 20000),
                     labels = c("0", "10k", "20k")) +
  labs(x = "Fracción censal",
       title = "Habitantes por <span style='color:#A9BE33;'><b>fracción</b></span> censal") +
  coord_flip(expand = FALSE, clip = "off") +
  theme_void() +
  theme(plot.margin = margin(t = 0, r = 18, b = 0, l = 5, "pt"),
        legend.position = "none",
        plot.title = element_markdown(family = "Roboto", size = 10, hjust = 1),
        plot.background = element_rect(fill = "#F6F5F5", color = "#F6F5F5"),
        axis.text.x = element_text(family = "Roboto", size = 8, hjust = 0),
        axis.text.y = element_text(family = "Roboto", size = 6, hjust = 1, color = darken("#A9BE33")))
fig_ploblacion

# Combinación de las figuras
layout <- '
AAA#
AAAB
AAA#
'
wrap_plots(A = fig_mapa, C = fig_ploblacion, design = layout) +
  plot_annotation(theme = theme(plot.background = element_rect(color = "#F6F5F5", fill = "#F6F5F5"))) +
  plot_layout(heights = c(1, 9,1))

# Guardo la figura
ggsave(here("./Day3/Radios_censales_MGP.png"), width = 25, height = 20, units = "cm")
  