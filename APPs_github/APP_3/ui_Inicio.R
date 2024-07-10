ui_Inicio <- function() {
    tags$div(
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
        # tags$div(
        #     class = "container-box",
        #     tags$div(
        #         class = "title-inicio",
        #         h4("Maestría en Estadística Aplicada")
        #     )
        # ),
        br(),
        br(),
        tags$div(
            class = "content-container",
            tags$div(
                class = "left-pane",
                tags$img(src = "PortadaQuitoEnDatos.png", alt = "Marcelo Chávez", class = "img-responsive-inicio")
            ),
            tags$div(
                class = "right-pane",
                tags$p(
                    "Un Sistema de Información es esencial para analizar el
                    desarrollo económico y productivo en el Distrito Metropolitano
                    de Quito, ya que permite recopilar, procesar y analizar datos
                    de diversas fuentes. Proporciona una visión integral de las
                    dinámicas económicas locales, ayudando a los tomadores de
                    decisiones a identificar tendencias, evaluar políticas 
                    públicas y detectar oportunidades de crecimiento. Además,
                    facilita la planificación y gestión de recursos, promoviendo
                    la transparencia y rendición de cuentas en la administración pública."
                ),
                tags$p(
                    "Este sistema también mejora la visualización de datos
                    mediante herramientas interactivas y mapas geoespaciales,
                    haciendo la información accesible y comprensible para
                    funcionarios, empresarios y ciudadanos. Al visualizar
                    el crecimiento económico en diferentes zonas,
                    se pueden identificar áreas con alto potencial de
                    desarrollo o necesidades específicas. Un Sistema
                    de Información eficiente fomenta la participación
                    ciudadana y la colaboración entre actores económicos,
                    proporcionando acceso abierto a datos que benefician
                    a la comunidad y potencian el desarrollo económico
                    sostenible del Distrito Metropolitano de Quito."))),
        tags$div(
            class = "footer",
            "Copyright © 2024 | Dirección Metropolitana de
            Gestión Económica y Productiva |
            Todos los derechos reservados"
        )
    )
}
