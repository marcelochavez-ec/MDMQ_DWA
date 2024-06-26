rm(list = ls(all.names = T))

# Librerías y directorio:
library(tidyverse)
library(skimr)

setwd("D:/DMQ/")

# ******************************************************************************

plusvalia <- readxl::read_excel("BDD/plusvalia/mayo/RD_BDD_2024_CORTE_TRIBUTARIO_006_PLUSVALIA.xlsx",
                                sheet = "CORTE_TRIBUTARIO_006")

df_1 <- plusvalia %>%
  mutate(NUM_PREDIO = str_count(NUMERO_PREDIO, ",") + 1,
         NUM_PREDIO_PARROQUIA = str_count(PREDIO_PARROQUIA, ",") + 1,
         NUM_DYA = str_count(DYA, ",") + 1,
         NUM_VALOR_CATASTRAL = str_count(VALOR_CATASTRAL, ",") + 1,
         NUM_CEDULAS_TRADENTES = str_count(CEDULAS_TRADENTES, ",") + 1,
         NUM_CEDULAS_ADQUIRENTES = str_count(CEDULAS_ADQUIRENTES, ",") + 1,
         VALIDACION_1 = ifelse(NUM_PREDIO == NUM_PREDIO_PARROQUIA & 
                               NUM_PREDIO == NUM_DYA & 
                               NUM_PREDIO == NUM_VALOR_CATASTRAL, 1, 0)) %>% 
  select(NUMERO_TRAMITE,
         VALIDACION_1,
         NUM_PREDIO,
         NUMERO_PREDIO,
         NUM_PREDIO_PARROQUIA,
         PREDIO_PARROQUIA,
         NUM_DYA,
         DYA,
         NUM_VALOR_CATASTRAL,
         VALOR_CATASTRAL,
         NUM_CEDULAS_ADQUIRENTES,
         CEDULAS_ADQUIRENTES,
         NUM_CEDULAS_TRADENTES,
         CEDULAS_TRADENTES)

df_2 <- plusvalia %>% 
  select(NUMERO_TRAMITE,
         NUMERO_PREDIO) %>% 
  mutate(NUMERO_PREDIO = str_split(NUMERO_PREDIO, ",")) %>%
  unnest(NUMERO_PREDIO) %>% 
  complete(NUMERO_TRAMITE,
           fill = list(NUMERO_PREDIO = NA)) %>%
  arrange(NUMERO_TRAMITE)


# Realizar todas las operaciones en una sola secuencia
# df_3 <- plusvalia %>% 
#   select(NUMERO_TRAMITE, NUMERO_PREDIO, PREDIO_PARROQUIA, DYA, VALOR_CATASTRAL) %>% 
#   mutate_at(vars(NUMERO_PREDIO, PREDIO_PARROQUIA, DYA, VALOR_CATASTRAL), 
#             ~ str_split(., ",")) %>%
#   unnest(NUMERO_PREDIO, PREDIO_PARROQUIA, DYA, VALOR_CATASTRAL) %>% 
#   complete(NUMERO_TRAMITE, fill = list(NUMERO_PREDIO = NA, PREDIO_PARROQUIA = NA, DYA = NA, VALOR_CATASTRAL = NA)) %>%
#   arrange(NUMERO_TRAMITE)

# Realizar todas las operaciones en una sola secuencia
df_3 <- plusvalia %>% 
  select(NUMERO_TRAMITE, NUMERO_PREDIO, PREDIO_PARROQUIA, DYA, VALOR_CATASTRAL) %>% 
  mutate(across(c(NUMERO_PREDIO, PREDIO_PARROQUIA, DYA, VALOR_CATASTRAL), ~ str_split(., ","))) %>%
  unnest(c(NUMERO_PREDIO, PREDIO_PARROQUIA, DYA, VALOR_CATASTRAL)) %>% 
  complete(NUMERO_TRAMITE, fill = list(NUMERO_PREDIO = NA, PREDIO_PARROQUIA = NA, DYA = NA, VALOR_CATASTRAL = NA)) %>%
  arrange(NUMERO_TRAMITE) %>% 
  mutate(VALOR_CATASTRAL = as.numeric(VALOR_CATASTRAL),
         DYA = as.numeric(DYA))




# plusvalia_cast <- plusvalia %>%
#   mutate(NUMERO_TRAMITE = as.character(NUMERO_TRAMITE),
#          NUMERO_PREDIO = as.character(NUMERO_PREDIO),
#          PREDIO_PARROQUIA = as.character(PREDIO_PARROQUIA),
#          FECHA_LIQUIDACION = as.Date(FECHA_LIQUIDACION),
#          TIPO_LIQUIDACION = as.character(TIPO_LIQUIDACION),
#          ESTADO_TRAMITE = as.character(ESTADO_TRAMITE),
#          TIPO_CONTRATO = as.character(TIPO_CONTRATO),
#          TIPO_DECLARACION = as.character(TIPO_DECLARACION),
#          NOMBRES_TRADENTES = as.character(NOMBRES_TRADENTES),
#          CEDULAS_TRADENTES = as.character(CEDULAS_TRADENTES),
#          NOMBRES_ADQUIRENTES = as.character(NOMBRES_ADQUIRENTES),
#          CEDULAS_ADQUIRENTES = as.character(CEDULAS_ADQUIRENTES),
#          DYA = as.numeric(DYA),
#          VALOR_CATASTRAL = as.numeric(VALOR_CATASTRAL),
#          VALOR_CONTRACTUAL = as.numeric(VALOR_CONTRACTUAL),
#          NUMERO_OBLIGACION = as.character(NUMERO_OBLIGACION),
#          VALOR_OBLIGACION = as.numeric(VALOR_OBLIGACION),
#          FECHA_PAGO_OBLIGACION = as.Date(FECHA_PAGO_OBLIGACION))
