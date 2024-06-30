library(tidyverse)

datos <- read_delim("C:/Users/marcelochavez/Documents/MDMQ/MDMQ_DWA/DB/SB/inversion_societaria1719608459513.txt", 
                    delim = "\t", 
                    skip = 4) %>% 
  
library(data.table)

# Leer el archivo .txt omitiendo las primeras 4 líneas
datos <- fread("C:/Users/marcelochavez/Documents/MDMQ/MDMQ_DWA/DB/SB/inversion_societaria1719608459513.txt", 
               sep = "\t")

# Suponiendo que tu data frame se llama datos y la columna se llama FECHA VAR INGRESO JURIDICO

datos$FECHA_INGRESO_ACTO_JURIDICO <- as.Date(datos$`FECHA INGRESO ACTO JURÍDICO`, format = "%d/%m/%Y")

datos <- datos %>%
  mutate(
    MES = month(FECHA_INGRESO_ACTO_JURIDICO),
    TRIMESTRE = case_when(
      MES %in% 1:3 ~ "Q1",
      MES %in% 4:6 ~ "Q2",
      MES %in% 7:9 ~ "Q3",
      MES %in% 10:12 ~ "Q4"
    ),
    ANIO = year(FECHA_INGRESO_ACTO_JURIDICO),
    TRIMESTRE_ANIO = paste(TRIMESTRE, ANIO, sep = " ")
  )


quito <- datos %>% 
  filter(`TIPO INVERSIÓN`=="EXTRANJERA DIRECTA" & CANTÓN=="QUITO") %>%
  group_by(`AÑO`) %>%
  summarise(TOTAL_QUITO = sum(`CAPITAL POR ACTO JURÍDICO`))

nacional <- datos %>% 
  filter(`TIPO INVERSIÓN`=="NACIONAL") %>%
  group_by(`AÑO`) %>%
  summarise(TOTAL_exponencial = sum(`CAPITAL POR ACTO JURÍDICO`)) %>% 
  mutate(TOTAL_NACIONAL = format(TOTAL_exponencial, scientific = FALSE)) %>% 
  select(-TOTAL_exponencial)

reporte_unificado_anual <- full_join(quito, nacional, by = "AÑO")

write.xlsx(reporte_unificado_anual,"C:/Users/marcelochavez/Documents/MDMQ/MDMQ_DWA/REPORTES/REPORTE_UNIFICADO_ANUAL.xlsx")


# REPORTE TRIMESTRAL ------------------------------------------------------


quito_tri <- datos %>% 
  filter(`TIPO INVERSIÓN`=="EXTRANJERA DIRECTA" & CANTÓN=="QUITO") %>%
  group_by(TRIMESTRE_ANIO) %>%
  summarise(TOTAL_QUITO = sum(`CAPITAL POR ACTO JURÍDICO`))

nacional_tri <- datos %>% 
  filter(`TIPO INVERSIÓN`=="NACIONAL") %>%
  group_by(TRIMESTRE_ANIO) %>%
  summarise(TOTAL_exponencial = sum(`CAPITAL POR ACTO JURÍDICO`)) %>% 
  mutate(TOTAL_NACIONAL = format(TOTAL_exponencial, scientific = FALSE)) %>% 
  select(-TOTAL_exponencial)

reporte_unificado_trimestral <- full_join(quito_tri, nacional_tri, by = "TRIMESTRE_ANIO")

write.xlsx(reporte_unificado_trimestral,"C:/Users/marcelochavez/Documents/MDMQ/MDMQ_DWA/REPORTES/REPORTE_UNIFICADO_TRIMESTRAL.xlsx")








