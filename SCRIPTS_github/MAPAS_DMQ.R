rm(list = ls(all.names = TRUE))

library(sf)
library(tidyverse)
library(ggspatial)
library(ggplot2)
library(readxl)

# Path de trabajo
ruta <- "D:/MDMQ/MDMQ_DWA/DB_onedrive/SHAPES/dissolve_dmq_adm_zonal.shp"

# Lectura del Shapefile
dmq_df <- st_read(ruta)


dissolved_dmq <- dmq_df %>%
  group_by(adm_zonal) %>%
  summarise(geometry = st_union(geometry))


# Realizar un dissolve agrupando por 'adm_zonal' y sumando 'pob_t'
dissolved_dmq <- dmq_df %>%
  group_by(adm_zonal) %>%
  summarise(pob_t = sum(pob_t, na.rm = TRUE),
            geometry = st_union(geometry))

# Verificar el resultado
print(dissolved_dmq)

# Guardar el shapefile resultante (opcional)
st_write(dissolved_dmq, "ruta/a/tu/dissolved_shapefile.shp")

# Convertir a GeoJSON y guardar (opcional)
geojson_write(dissolved_dmq, file = "ruta/a/tu/dissolved_shapefile.geojson")



# Lectura del archivo Excel y renombrar columnas a minúsculas
smr_df <- readxl::read_xlsx("D:/MDMQ/BDD/mapa_1.xlsx", sheet = "Hoja1") %>%
  rename_all(tolower)

# Unión entre tablas
dmq_df <- dmq_df %>% 
  left_join(smr_df, by = "adm_zonal") %>%
  mutate(
    costo_m2_label = case_when(
      adm_zonal == "CHOCO ANDINO" ~ "Reserva Ecológica (No aplica)",
      adm_zonal == "LA MARISCAL" ~ "Sin datos",
      TRUE ~ paste0(costo_m2, " USD")
    )
  )
