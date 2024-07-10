# Base R Shiny image
FROM rocker/shiny:4.1.2

# Instalar dependencias del sistema
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Crear un directorio en el contenedor y establecer el directorio de trabajo
RUN mkdir -p /home/app_sidep/BDD
WORKDIR /home/app_sidep

# Instalar dependencias de R
RUN R -e "install.packages(c('tidyverse','DT','shiny','shinythemes','RPostgreSQL'), dependencies=TRUE)"

# Copiar el código de la aplicación Shiny y los archivos necesarios
COPY app.R /home/app_sidep/app.R
COPY BDD/db_luae.RDS /home/app_sidep/BDD/db_luae.RDS

# Ajustar permisos para el usuario shiny
RUN chown -R shiny:shiny /home/app_sidep

# Exponer el puerto de la aplicación
EXPOSE 8180

# Run the R Shiny app
CMD Rscript /home/app_sidep/app.R