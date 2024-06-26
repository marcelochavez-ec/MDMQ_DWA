# EXPLORATORIO ------------------------------------------------------------

library(e1071)

exploratorio <- function(df) {
  tipos <- sapply(df, class)
  valores_min <- sapply(df, function(x) ifelse(is.numeric(x), min(x[complete.cases(x)]), NA))
  valores_max <- sapply(df, function(x) ifelse(is.numeric(x), max(x[complete.cases(x)]), NA))
  coeficientes_asimetria <- sapply(df, function(x) ifelse(is.numeric(x), round(e1071::skewness(x[complete.cases(x)]), 2), NA))
  curtosis <- sapply(df, function(x) ifelse(is.numeric(x), round(kurtosis(x[complete.cases(x)]), 2), NA))
  promedio <- sapply(df, function(x) ifelse(is.numeric(x), round(mean(x[complete.cases(x)]), 2), NA))
  medianas <- sapply(df, function(x) ifelse(is.numeric(x), round(median(x[complete.cases(x)]), 2), NA))
  modas <- sapply(df, function(x) ifelse(is.numeric(x), {
    tab <- table(x[complete.cases(x)])
    as.numeric(names(tab)[tab == max(tab)])}, NA))
  rangos <- sapply(df, function(x) ifelse(is.numeric(x), round(max(x[complete.cases(x)]) - min(x[complete.cases(x)]), 2), NA))
  varianzas <- sapply(df, function(x) ifelse(is.numeric(x), round(var(x[complete.cases(x)]), 2), NA))
  desviaciones <- sapply(df, function(x) ifelse(is.numeric(x), round(sd(x[complete.cases(x)]), 2), NA))
  coeficientes_variacion <- sapply(df, function(x) ifelse(is.numeric(x), round(sd(x[complete.cases(x)]) / mean(x[complete.cases(x)]), 2), NA))
  
  resumen <- data.frame(Variable = names(df),
                        Tipo = tipos,
                        Minimo = valores_min,
                        Maximo = valores_max,
                        Rango = rangos,
                        Promedio = promedio,
                        Mediana = medianas,
                        Desviacion_Estandar = desviaciones,
                        Coeficiente_Variacion = coeficientes_variacion,
                        Moda = modas,
                        Varianza = varianzas,
                        Coeficiente_Asimetria = coeficientes_asimetria,
                        Curtosis = curtosis,
                        stringsAsFactors = FALSE)
  
  resumen$Tipo <- ifelse(resumen$Tipo == "factor", "Categórica", resumen$Tipo)
  resumen$Tipo <- ifelse(resumen$Tipo == "POSIXct", "Fecha", resumen$Tipo)
  resumen$Tipo <- ifelse(resumen$Tipo == "logical", "Booleana", resumen$Tipo)
  resumen$Tipo <- ifelse(resumen$Tipo == "numeric", "Numérica", resumen$Tipo)
  
  # Elimina los nombres de las etiquetas de las filas
  rownames(resumen) <- NULL
  
  return(resumen)
}
