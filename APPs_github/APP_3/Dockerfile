# Base R Shiny image
FROM rocker/shiny:4.1.2

# Actualizar e instalar dependencias del sistema y PostgreSQL
RUN apt-get update \
    && apt-get install -y \
       libpq5 -y \
       libpq-dev \
       r-base \
       r-base-dev \
       libcurl4-openssl-dev \
       libssl-dev \
       libxml2-dev \
       gnupg2 \
       wget \
       && rm -rf /var/lib/apt/lists/*

# Agregar el repositorio de PostgreSQL al sistema
RUN wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - \
    && echo "deb [arch=amd64] https://apt.postgresql.org/pub/repos/apt/ $(lsb_release -cs)-pgdg main" | tee /etc/apt/sources.list.d/pgdg.list

# Instalar PostgreSQL 16 y sus componentes
RUN apt-get update \
    && apt-get install -y \
       postgresql-16 \
       postgresql-client-16 \
       postgresql-contrib-16 \
    && rm -rf /var/lib/apt/lists/*


# Configurar PostgreSQL para escuchar en todos los interfaces
USER postgres
RUN /etc/init.d/postgresql start

# Exponer el puerto de PostgreSQL (5490)
EXPOSE 5432

# Configurar PostgreSQL para escuchar en todas las interfaces
USER postgres
RUN /etc/init.d/postgresql start && \
    psql -c "ALTER SYSTEM SET listen_addresses TO '*';" && \
    psql -c "SELECT pg_reload_conf();"

# Cambiar al usuario root para crear el directorio y establecer permisos
USER root

# Crear un directorio en el contenedor y establecer el directorio de trabajo
RUN mkdir -p /home/app_3
WORKDIR /home/app_3

# Instalar dependencias de R
RUN R -e "install.packages(c('tidyverse','DT','shiny','shinythemes','RPostgreSQL'), dependencies=TRUE)"

# Copiar los archivos de la aplicación Shiny
COPY *.R /home/app_3/
COPY BDD/db_luae.RDS /home/app_3/BDD/db_luae.RDS
COPY www/header.png /home/app_3/www/header.png
COPY www/PortadaQuitoEnDatos.png /home/app_3/www/PortadaQuitoEnDatos.png
COPY www/styles.css /home/app_3/www/styles.css

# Ajustar permisos para el usuario shiny
RUN chown -R shiny:shiny /home/app_3

# Exponer el puerto de la aplicación Shiny (8180)
EXPOSE 8180

# Cambiar al usuario shiny y ejecutar la aplicación Shiny
USER shiny
CMD R -e "source('/home/app_3/verificacion_paquete.R'); shiny::runApp(host='0.0.0.0', port=8180)"