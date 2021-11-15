# Setup ####
pacman::p_load(tidyverse, sf, janitor, ggtext, extrafont, here, rgdal, rayshader)

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
  mutate(density = total/shape_area)

# La figura ####
# Figura del mapa
fig_mapa <- ggplot() + 
  # Mapa de las Fracciones censales
  geom_sf(data = st_transform(fracciones, "+proj=longlat +datum=WGS84"),
          aes(fill = log10(density)),
          size = 0.3, 
          color = "gray60") +
  # Caption
  annotate(
    geom = "richtext", x = -58, y = -38.23,
    label = "Fuente: Datos abiertos MGP<br>Visualización: @spiousas",
    family = "Roboto",
    size = 2.5, 
    hjust = 0,
    vjust = 0,
    color = "black",
    fill = NA, label.color = NA
  ) +
  # Estilo
  theme_void() +
  scale_fill_distiller(palette = "YlGnBu",
                       direction = -1) +
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, "pt"),
        legend.position = "none",
        plot.background = element_rect(fill = "#F6F5F5", color = "#F6F5F5")) 

fig_mapa

# La figura en 3D con Rayshader
plot_gg(fig_mapa,
        multicore = TRUE,
        width = 5,
        height = 5,
        scale = 300,
        zoom = 0.5,
        theta = -20,
        phi = 45)

render_snapshot(filename = "./Day11/Poblacion3D.png",
                title_text = "Densidad de población del MGP", 
                title_color = "white", 
                title_bar_color = "#A9BE33",
                vignette = TRUE, 
                title_offset=c(0,20),
                title_font = "Roboto", 
                title_position = "north")

# Guardo la figura
ggsave(here("./Day11/Poblacion3D.png"), width = 20, height = 20, units = "cm")

  