# Setup ####
pacman::p_load(tidyverse, sf, janitor, here, mapsf)

# Datos ####

# The scale bar won't work with EPSG:4326 or any other non projected (lon/lat) base map
# I suggest using a local CRS, this one: EPSG:22185?


# Mapas de fracciones censales
fracciones <- st_read(here("./Day3/data/fracciones_0/fracciones.shp")) %>%
  clean_names() %>%
  st_set_crs(4326) %>%
  st_transform("EPSG:22185")

# Farmacias del MGP
farmacias <- st_read(here("./Day14/data/farmacias/farmacias_2.shp")) %>%
  clean_names() %>%
  st_transform("EPSG:22185")


# Cuento las farmacias por fracción censal
farmacias_por_fraccion <- st_join(farmacias, fracciones, join = st_within)
farmacias_por_fraccion_tbl <- count(as_tibble(farmacias_por_fraccion), fraccion,
                                    name = "farmacias_por_fraccion")

# Join de geometrías y cantidad de farmacias
fracciones <- fracciones %>%
  left_join(farmacias_por_fraccion_tbl, by = "fraccion") %>%
  mutate(farmacias_por_fraccion = replace_na(farmacias_por_fraccion, 0))

# La figura ####
# El tema
mf_theme("agolalight")

# Inicio la exportación
mf_export(x = fracciones[62, ], expandBB = c(10,5,5,5),
          res = 300,  width = 1800,
          filename = here("./Day14/Farmacias.png"))

## This part is redundant with the mf_export settings, I'va transferred the
# mf_init settings to mf_export directly
# Inicio con zoom centrado en la fraccion que contiene a la Municipalidad
# mf_init(x = fracciones[62, ], expandBB = c(10,5,5,5))

# Inicio el mapa
mf_map(x = fracciones,
       border = "white", add = TRUE)

# El choroplet de las farmacias
mf_map(x = fracciones, type = "choro",
       var = "farmacias_por_fraccion",
       leg_title = "# de farmacias",
       leg_pos = "topright",
       add = TRUE,
       border = "white",
       pal = "Dark Mint")

# El mapita del mundo
mf_inset_on(x = "worldmap",
            pos = "right",
            cex = .1)
mf_worldmap(fracciones)
mf_inset_off()

# La escala
mf_scale(pos = "bottomright")

# La flecha
mf_arrow(pos = "topleft")

# El título
mf_title("Farmacias por fracción censal")

# El caption
mf_credits("@spiousas",
           pos = "bottomleft",
           col = "black")

#Cierro la exportación
dev.off()
