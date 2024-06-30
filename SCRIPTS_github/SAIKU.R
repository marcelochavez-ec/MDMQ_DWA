rm(list = ls(all.names = T))

library(tidyverse)
library(readxl)
library(openxlsx)

ventas <- readxl::read_xlsx("D:/MDMQ/BDD/SAIKU/VENTAS.xlsx",
                             sheet = "Sheet 1")

ipc <- readxl::read_xlsx("D:/MDMQ/BDD/SAIKU/Series IPC Empalmadas/ipc_ind_nac_reg_ciud_emp_clase_05_2024.xlsx", 
                 sheet = "1. NACIONAL",
                 skip = 4,
                 n_max = 116) %>% 
  rename("seccion"="Nivel",
         "cod_ccif"="Cód. CCIF",
         "descripcion_ccif"="Descripción CCIF")

ipc_largo <- ipc %>%
  mutate(cod_ccif = as.character(cod_ccif)) %>% 
  pivot_longer(cols = matches("-"),  # Selecciona todas las columnas que contienen "-"
               names_to = "fecha",   # Nombre para la nueva columna de fechas
               values_to = "ipc") %>%  # Nombre para la nueva columna de valores
  separate(fecha, into = c("mes_abreviado", "anio_abreviado"), sep = "-") %>%  # Separar la columna 'fecha' en mes y año
  mutate(anio_fiscal = as.character(paste0("20", anio_abreviado)),  # Convertir el año abreviado a año completo
         mes_fiscal = case_when(  # Convertir el mes abreviado a número de mes
           mes_abreviado == "ene" ~ "1",
           mes_abreviado == "feb" ~ "2",
           mes_abreviado == "mar" ~ "3",
           mes_abreviado == "abr" ~ "4",
           mes_abreviado == "may" ~ "5",
           mes_abreviado == "jun" ~ "6",
           mes_abreviado == "jul" ~ "7",
           mes_abreviado == "ago" ~ "8",
           mes_abreviado == "sep" ~ "9",
           mes_abreviado == "oct" ~ "10",
           mes_abreviado == "nov" ~ "11",
           mes_abreviado == "dic" ~ "12",
           TRUE ~ NA_character_
         )) %>%
  select(-mes_abreviado,
         -anio_abreviado) %>% # Eliminar columnas auxiliares
  mutate(ipc = as.numeric(ipc)) %>% 
  filter(anio_fiscal>=2011)

# 1. Filtro del df ipc por el filtro nivel=="General":

ipc_general <- ipc_largo %>% 
  filter(seccion == "General") %>% 
  mutate(ponderador = ipc_largo$ipc[1],
         ipc_deflactado = ipc / ponderador) %>% 
  select(anio_fiscal, 
         mes_fiscal,
         ipc_deflactado,
         ipc,
         ponderador)

# 2. Realizar el merge con ventas
ventas <- merge(ventas,
                ipc_general,
                by = c("anio_fiscal", 
                       "mes_fiscal"),
                all.x = TRUE) %>% 
  mutate(ventas = (ventas_locales_12 + ventas_locales_0) / ipc_deflactado)

# Reporte Anual:

r_ventas_anual <- ventas %>% 
  group_by(canton, anio_fiscal) %>% 
  summarise(ventas_totales = sum(ventas, na.rm = TRUE) / 1000000000,
  .groups = 'drop')

openxlsx::write.xlsx(r_ventas_anual,
                     "D:/MDMQ/REPORTES/REPORTE_VENTAS_ANUAL_SAIKU.xlsx", 
                     sheetName="Ventas_Anual")

# Reporte Mensual Acumulado:

r_ventas_mensual <- ventas %>% 
  filter(mes_fiscal %in% c("1","2","3","4")) %>%  
  group_by(canton, anio_fiscal) %>% 
  summarise(ventas_totales = sum(ventas, na.rm = TRUE),
            .groups = 'drop')

openxlsx::write.xlsx(r_ventas_mensual,
                     "D:/MDMQ/REPORTES/REPORTE_VENTAS_MENSUAL_SAIKU.xlsx", 
                     sheetName="Ventas_Mensual")








