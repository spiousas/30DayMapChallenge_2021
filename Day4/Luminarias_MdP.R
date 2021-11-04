# Setup ####
pacman::p_load(tidyverse, sf, janitor, ggtext, ggbump, extrafont, here, glue, 
               colorspace, patchwork)

# Los datos ####
# Leo las fracciones censales y me quedo con las céntricas
fracciones <- st_read(here("./Day3/data/fracciones_0/fracciones.shp")) %>%
  clean_names() %>% 
  filter(gid>12) %>% 
  st_set_crs(4326) 

# Leo el mapa callejero de Mar del Plata y filtro sólo las que están en las fracciones céntricas
calles <- st_read(here("./base_map/callejero_0/calles.shp")) %>%
  clean_names() %>% st_intersection(fracciones)

# Leo las ubicaciones de las luminarias y hago un left_join() con los datos del tipo de luminaria
# Filtro sólo las que están en las fracciones céntricas
luces <- st_read(here("./Day4/data/alumbrado-publico_0/luces.shp")) %>%
  clean_names() %>%
  left_join(read_delim(here("./Day4/data/alumbrado-publico_0.csv"), delim = ";") %>% clean_names() %>% select(c(id, tipo_luminaria)),
            by = "id") %>%
  drop_na(tipo_luminaria) %>%
  st_intersection(fracciones) %>%
  mutate(tipo_luminaria = factor(tipo_luminaria, levels = c("SAP", "LED", "HQT"))) 
  
# La figura ####
fig_mapa <- ggplot() + 
  # Mapa de las calle
  geom_sf(data = calles,
          color = "black",
          size = .1,
          fill = "transparent") +
  # Mapa de las luminarias
  geom_hex(data = luces,
           mapping = aes(x = longitud, y = latitud),
           color = NA, alpha = .65, binwidth = c(1,.8)*.008) +
  # Estilo
  theme_void() +
  labs(fill = NULL) +
  scale_fill_gradient(low = lighten("#A9BE33", .6), high = darken("#A9BE33", .4)) +
  facet_grid(.~tipo_luminaria) +
  theme(plot.margin = margin(t = 0, r = .5, b = 0, l = .5, "cm"),
        legend.position = "bottom",
        plot.background = element_rect(fill = "#F6F5F5", color = "#F6F5F5"),
        strip.text.x = element_text(family = "Roboto", size = 14, face = "bold"),
        legend.key.width = unit(5,"line"),
        legend.key.height = unit(.3,"line")) 

# Tibble con el texto
text.df <- tibble(
  label = c("Las lámparas <b>SAP</b> (sodio) son las más utilizadas en el alumbrado público urbano pero están siendo reempladas por lámparas <b>LED</b> que tienen una eficiencia luminosa cerca de un 50% superior.<br><br>Por otro lado, las lámparas <b>HQT</b> pertenecen, al igual que las <b>SAP</b>, a la familia de las de descarga y son contaminantes ya que contienen mercurio.",
            glue("En <span style='color:#04A5CB;'><b>Mar del Plata</span></b> hay {nrow(luces[luces$tipo_luminaria=='SAP',])} luminarias <b>SAP</b>, {nrow(luces[luces$tipo_luminaria=='LED',])} luminarias <b>LED</b> y {nrow(luces[luces$tipo_luminaria=='HQT',])} luminarias <b>HQT</b>.<br><br>Si bien ya se ha comenzado a renovar las luminarias, la mayoría de las <b>LED</b> aún se concentran en algunas arterias principales (la costa, Av. Constitución, Av. Juan B. Justo y Av. Independencia) y el centro de la ciudad.")),
  x = c(0, 12),
  y = c(0, 0),
  hjust = c(0, 1),
  vjust = c(1, 1),
  halign = c(0, 0)
)


fig_text <- ggplot() +   
  # Título
  annotate(
    geom = "richtext", x =  0 , y = 0,
    label = "<span style='color:#04A5CB;'><b>Luminarias urbanas<br>por tipo en la ciudad de<br>Mar del Plata</b></span>",
    family = "Roboto",
    size = 8,
    hjust = 1,
    vjust = 1,
    color = NA,
    fill = NA, label.color = NA
  ) +
  # Copete
    geom_textbox(
    data = text.df,
    aes(x = x, y = y, label = label, 
        hjust = hjust, vjust = vjust,
        halign = halign),
    fill = NA,
    size = 3,
    text.colour = "black",
    width = unit(9.2, "cm"),
    color = NA,
    family = "Roboto"
  ) +
  # Estilo
  scale_y_continuous(limits = c(-2.5,0)) +
  scale_x_continuous(limits = c(-5,12)) +
  theme_void() +
  theme(plot.margin = margin(t = 0, r = .5, b = 0, l = .5, "cm"),
        legend.position = "none",
        plot.background = element_rect(fill = "#F6F5F5", color = "#F6F5F5"),
        )

# Combino las figuras
fig_text / fig_mapa  +
  plot_layout(heights = c(1, 3)) + 
  plot_annotation(caption = "Fuente: Datos abiertos MGP | Visualización: @spiousas") &
  theme(plot.background = element_rect(color = "#F6F5F5", fill = "#F6F5F5"),
        plot.caption = element_text(family = "Roboto", size = 8, color = "gray20"))

# Guardo la figura
ggsave(here("./Day4/Luminarias_MdP.png"), width = 30, height = 20, units = "cm", dpi = 300)