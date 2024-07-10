rm(list = ls(all.names = T))

library(tidyverse)
library(openxlsx)
library(rpivotTable)

db_turisticas <- readxl::read_excel("DB_onedrive/CIIU_ACTIVIADES_TURISTICAS.xlsx",
                                    sheet = "Hoja1")

colnames(db_turisticas) <- tolower(colnames(db_turisticas))

db_luae_2022 <- readxl::read_excel("DB_onedrive/LUAE/LUAE_2022.xlsx",
                                   skip = 1,
                                   sheet = "luae_2022")

colnames(db_luae_2022) <- tolower(colnames(db_luae_2022))

db_luae_2022 <- left_join(db_luae_2022,
                          db_turisticas,
                          by="codigo_ciiu") %>% 
  mutate(descripcion_categorica = case_when(
    categoria == "1" ~ "LUAE Simplificada",
    categoria == "2" ~ "LUAE Ordinaria",
    categoria == "3" ~ "LUAE Especial",
    TRUE ~ NA_character_  # Maneja otros casos, si existen
  )) %>% 
  filter(estado_emision %in% c("OTORGADA",
                               "OTORGADA_PARCIAL") &
           (!estado_bpm %in% c("ABORTADO",
                               "FINALIZADO CON ERROR"))) %>% 
  distinct() %>% 
  select(numero_tramite,
         actividad_economica,
         administracion_zonal,
         codigo_ciiu,
         movimiento_actual,
         actividades_turisticas,
         tipologia_turisticas,
         descripcion_ciiu_turisticas,
         descripcion_categorica)

write.xlsx(db_luae_2022, "DB_onedrive/LUAE/db_luae_turisticas.xlsx")

rpivotTable(db_luae_2022)
