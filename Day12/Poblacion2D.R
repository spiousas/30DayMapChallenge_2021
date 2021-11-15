# Setup ####
pacman::p_load(tidyverse, sf, janitor, ggtext, extrafont, here)

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

# Mapas de fracciones censales (hago un join con la cantidad de habitantes)
fracciones <- st_read(here("./Day3/data/fracciones_0/fracciones.shp")) %>%
  clean_names() %>% st_set_crs(4979) %>%
  mutate(fraccion = as.numeric(fraccion)) %>%
  left_join(poblacion_fraccion, by = c("fraccion")) %>%
  mutate(density = (total/shape_area)*1e6) # Convierto a Km

# La figura ####
# Figura del mapa
fig_mapa <- ggplot() + 
  # Mapa de las Fracciones censales
  geom_sf(data = st_transform(fracciones, "+proj=longlat +datum=WGS84"),
          aes(fill = log10(density)),
          size = .3, 
          color = "black") +
  # Caption
  annotate(
    geom = "richtext", x = -57.51661, y =  -38.2437,
    label = "Fuente: Datos abiertos MGP<br>Visualización: @spiousas",
    family = "Roboto",
    size = 2.5, 
    hjust = 1,
    vjust = 0,
    color = "black",
    fill = NA, label.color = NA
  ) +
  # Estilo
  theme_void() +
  scale_fill_distiller(palette = "YlGnBu",
                       name = str_wrap("Habitantes por km. cuadrado",
                                                 width = 30), 
                       guide = guide_colourbar(title.position = "top"),
                       direction = -1,
                       limits = c(min(log10(fracciones$density)), max(log10(fracciones$density))),
                       breaks = seq(1, 4),
                       labels = round(10^(seq(1, 4))) ) +
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, "pt"),
        legend.position = c(.24,.15),
        plot.background = element_rect(fill = "#F6F5F5", color = "#F6F5F5"),
        legend.text = element_text(family = "Roboto", size = 10, vjust = 1),
        legend.key.width = unit(1.8,"line"),
        legend.key.height = unit(.3,"line"),
        legend.title.align = 0,
        legend.direction =  "horizontal") 

# Guardo la figura
ggsave(here("./Day12/Poblacion_2D.png"), width = 15, height = 17, units = "cm")
  