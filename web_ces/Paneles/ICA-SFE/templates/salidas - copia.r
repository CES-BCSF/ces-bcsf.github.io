path_viejo <- base::getwd()

anios_historial <- 4 # AÑOS DE HISTORIA A MOSTRAR EN LOS PANELES

### CREACION Y SALIDA DE LA FECHA ####

fecha_html <- base::sprintf(base::format(base::Sys.Date(), "%d de %B de %Y"))

base::writeLines(fecha_html, "update_date.html")

#### PREPARA LOS DATOS PARA EL DATATABLE VAR_M ####
base::setwd(path_base)

base <- readxl::read_excel(DF_paneles_path, sheet = "mensuales", col_names = TRUE)

media <- base |> # CALCULO DE MEDIA
  dplyr::select(-Fecha) |>
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(.x, na.rm = TRUE)))|>
  tidyr::pivot_longer(dplyr::everything(), names_to = "Indicadores", values_to = "μ")

desvio <- base |> # CALCULO DE SD
  dplyr::select(-Fecha) |>
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ sd(.x, na.rm = TRUE)))|>
  tidyr::pivot_longer(dplyr::everything(), names_to = "Indicadores", values_to = "σ")

estadisticos <- base::data.frame(media, σ = desvio$σ) # JUNTANDO AMBOS ESTADÍSTICOS

ultimo_dato_m <- base$Fecha[max(base::which(base::rowSums(!base::is.na(base[-1])) > 0))]
base_filtrada <- base |> dplyr::filter(
  Fecha > lubridate::add_with_rollback(ultimo_dato_m, -lubridate::years(anios_historial)),
  Fecha <= ultimo_dato_m
)
base_larga <- base_filtrada |>
  tidyr::pivot_longer(
    cols = -Fecha,  # Todas las columnas excepto 'Fecha'
    names_to = "Indicadores", # Crear una columna 'Indicadores' con los nombres de las series
    values_to = "Valor" # Los valores de las series en una columna 'Valor'
  )

# base_pivoteada <- base_larga |>
#   tidyr::pivot_wider(
#     names_from = Fecha,
#     values_from = Valor
#   ) |>
#   dplyr::rename_with(
#     ~ base::ifelse(.x == "Indicadores" | base::is.na(lubridate::ymd(.x, quiet = TRUE)),
#                    .x,
#                    base::format(lubridate::ymd(.x),
#                                 "%b-%y")) # "%b-%y" EXPRESA MES-AÑO
#   )

rename_fecha_cols <- function(nombres) {
  sapply(nombres, function(x) {
    parsed <- suppressWarnings(lubridate::ymd(x))
    if (is.na(parsed)) {
      x
    } else {
      format(parsed, "%b-%y")
    }
  })
}

base_pivoteada <- base_larga |>
  tidyr::pivot_wider(
    names_from = Fecha,
    values_from = Valor
  )

names(base_pivoteada) <- rename_fecha_cols(names(base_pivoteada))


base_total <- clasif |>
  dplyr::left_join(base_pivoteada, by = "Indicadores") |>
  dplyr::select(-c(Carpeta, Descripcion, nombre)) |>
  dplyr::select(Indicadores, Sector, dplyr::everything()) |>
  dplyr::rename("Grupo" = Sector) |>
  dplyr::mutate(Grupo_orden = base::as.numeric(gsub("G", "", Grupo))) |> # Extraer la parte numérica de "Grupo"
  dplyr::arrange(Grupo_orden) |> # Ordenar por la parte numérica
  dplyr::select(-Grupo_orden) |> # Eliminar la columna auxiliar si no es necesaria
  dplyr::left_join(estadisticos, by = "Indicadores") |> # incorpora los estadísticos media y desvío calculados
  dplyr::select(Indicadores, Grupo, μ, σ, dplyr::everything()) # ordena las columnas

base_def <- base_total |>
  dplyr::mutate(Indicadores = base::paste0('<a href="', Enlace, '" target="_blank">', Indicadores, '</a>')) |>
  dplyr::select(-Enlace) |>
  dplyr::rename(Series = Indicadores)

# DETECTA CAMBIO DE FECHA Y AÑO
years <- base::sapply(base::names(base_def), function(x) base::ifelse(base::grepl("-", x), base::substr(x, base::nchar(x) - 1, base::nchar(x)), NA))

# IDENTIFICA POSICIONES DONDE CAMBIA EL BORDE
month_positions <- base::seq(3, base::length(base::names(base_def)))  # Desde la tercera columna (primer mes)
year_positions <- base::which(years[-1] != years[-base::length(years)]) + 1  # Donde cambia el año

#### GENERA UNA SALIDA PARA EL DATATABLE VAR_M ####
base::setwd(path_viejo)
mi_script <- "
(function() {
    // 1. Función que mide y envía la altura, para que el html interno (tabla) le diga como adaptarse al externo
    function sendHeight() {
        // Buscamos el widget o el contenedor principal
        const container = document.querySelector('.html-widget') || document.body;
        const height = container.scrollHeight;
        
        console.log('Hijo: Enviando altura al padre:', height); //HIJO = HTML INTERNO
        window.parent.postMessage({ 'height': height }, '*');
    }

    // 2. Eventos de disparo
    window.addEventListener('load', sendHeight);
    window.addEventListener('resize', sendHeight);

    // 3. El Observador: detecta cambios en el HTML (filtros, búsquedas)
    const observer = new MutationObserver((mutations) => {
        // Pequeño delay para dejar que el widget termine de redibujarse
        setTimeout(sendHeight, 100);
    });

    observer.observe(document.body, {
        childList: true,
        subtree: true,
        attributes: true
    });
})();
"

library(DT)
library(htmlwidgets)
dt <- DT::datatable(
  base_def,
  rownames = FALSE,
  escape = FALSE,
  extensions = c("FixedColumns", "Scroller"),
  options = list(
    pageLength = -1,
    dom = 't',
    scrollX = TRUE,
    scrollY = FALSE,
    scrollCollapse = TRUE,
    autoWidth = TRUE,
    fixedColumns = list(leftColumns = 4),
    columDefs = list(
      list(targets = 0:3, autoWidth = TRUE)
    ),
    # columnDefs = list(
    #   list(targets = 0, width = "120px"),  
    #   list(targets = 1:3, width = "60px"),                      # primeras 4 columnas
    #   list(targets = 4:(ncol(base_def)-1), width = "60px")       # meses
    # ),
    initComplete = JS(
      "function(settings, json) {
         var $scrollBody = $('.dataTables_scrollBody');
         setTimeout(function(){
            if ($scrollBody.length) {
               $scrollBody.scrollLeft($scrollBody[0].scrollWidth);
            }
         }, 200);
       }"
    ),
    
    headerCallback = JS(
      "function(thead, data, start, end, display){
         $(thead).css({'background-color': '#e5e7eb',
                       'color': '#000',
                       'font-weight': 'bold',});
       }"
    )
  ),
  class = 'stripe row-border',
  width = "100%"
) |>
formatRound(columns = names(base_def)[3:4], digits = 2) |>
formatRound(columns = names(base_def)[5:ncol(base_def)], digits = 1) |>
formatStyle(
  columns = names(base_def)[5:ncol(base_def)], 
  backgroundColor = styleInterval(0, c("#CD5555", "darkseagreen")),
  color = styleInterval(0, c("white", "black")),
  textAlign = "center",
  fontWeight = "bold"
) |>
formatStyle(
  columns = names(base_def)[1:4], 
  textAlign = "left",
  color = "#234e5f",
  `vertical-align` = "middle",
  fontWeight = "bold"
)

mi_css <- base::readLines("../../assets/css/styles-interno-datatable.css")

dt <- htmlwidgets::prependContent(dt, htmltools::tags$style(htmltools::HTML(mi_css)))
dt <- htmlwidgets::prependContent(dt, htmltools::tags$script(htmltools::HTML(mi_script)))

htmlwidgets::saveWidget(
  dt,
  file = "Tabla_ICA-SFE_M.html",
  selfcontained = TRUE,
  title = NULL
)


#### PREPARA LOS DATOS PARA EL DATATABLE VAR_IA ####
base::setwd(path_base)

base_ia <- readxl::read_excel(DF_paneles_path, sheet = "i.a.", col_names = TRUE)

media_ia <- base_ia |> # CALCULO DE MEDIA
  dplyr::select(-Fecha) |>
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(.x, na.rm = TRUE)))|>
  tidyr::pivot_longer(dplyr::everything(), names_to = "Indicadores", values_to = "μ")

desvio_ia <- base_ia |> # CALCULO DE SD
  dplyr::select(-Fecha) |>
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ sd(.x, na.rm = TRUE)))|>
  tidyr::pivot_longer(dplyr::everything(), names_to = "Indicadores", values_to = "σ")

estadisticos_ia <- base::data.frame(media_ia, σ = desvio_ia$σ) # JUNTANDO AMBOS ESTADÍSTICOS

ultimo_dato_ia <- base_ia$Fecha[max(base::which(base::rowSums(!base::is.na(base_ia[-1])) > 0))]
base_filtrada_ia <- base_ia |> dplyr::filter(
  Fecha > lubridate::add_with_rollback(ultimo_dato_ia, -lubridate::years(anios_historial)),
  Fecha <= ultimo_dato_ia
)
base_larga_ia <- base_filtrada_ia |>
  tidyr::pivot_longer(
    cols = -Fecha,  # Todas las columnas excepto 'Fecha'
    names_to = "Indicadores", # Crear una columna 'Indicadores' con los nombres de las series
    values_to = "Valor" # Los valores de las series en una columna 'Valor'
  )

# base_pivoteada <- base_larga |>
#   tidyr::pivot_wider(
#     names_from = Fecha,
#     values_from = Valor
#   ) |>
#   dplyr::rename_with(
#     ~ base::ifelse(.x == "Indicadores" | base::is.na(lubridate::ymd(.x, quiet = TRUE)),
#                    .x,
#                    base::format(lubridate::ymd(.x),
#                                 "%b-%y")) # "%b-%y" EXPRESA MES-AÑO
#   )

base_pivoteada_ia <- base_larga_ia |>
  tidyr::pivot_wider(
    names_from = Fecha,
    values_from = Valor
  )

names(base_pivoteada_ia) <- rename_fecha_cols(names(base_pivoteada_ia))

base_total_ia <- clasif |>
  dplyr::left_join(base_pivoteada_ia, by = "Indicadores") |>
  dplyr::select(-c(Carpeta, Descripcion, nombre)) |>
  dplyr::select(Indicadores, Sector, dplyr::everything()) |>
  dplyr::rename("Grupo" = Sector) |>
  dplyr::mutate(Grupo_orden = base::as.numeric(gsub("G", "", Grupo))) |> # Extraer la parte numérica de "Grupo"
  dplyr::arrange(Grupo_orden) |> # Ordenar por la parte numérica
  dplyr::select(-Grupo_orden) |> # Eliminar la columna auxiliar si no es necesaria
  dplyr::left_join(estadisticos_ia, by = "Indicadores") |> # incorpora los estadísticos media y desvío calculados
  dplyr::select(Indicadores, Grupo, μ, σ, dplyr::everything()) # ordena las columnas

base_def_ia <- base_total_ia |>
  dplyr::mutate(Indicadores = base::paste0('<a href="', Enlace, '" target="_blank">', Indicadores, '</a>')) |>
  dplyr::select(-Enlace) |>
  dplyr::rename(Series = Indicadores)  

# Detectar cambios de mes y año
years_ia <- base::sapply(base::names(base_def_ia), function(x) base::ifelse(base::grepl("-", x), base::substr(x, base::nchar(x) - 1, base::nchar(x)), NA))

# Identificar las posiciones donde cambiará el borde
month_positions_ia <- base::seq(3, base::length(base::names(base_def_ia)))  # Desde la tercera columna (primer mes)
year_positions_ia <- base::which(years_ia[-1] != years_ia[-base::length(years_ia)]) + 1  # Donde cambia el año

#### GENERA UNA SALIDA PARA EL DATATABLE VAR_IA ####
base::setwd(path_viejo)

library(DT)
library(htmlwidgets)

dt_ia <- datatable(
  base_def_ia,
  rownames = FALSE,
  escape = FALSE,
  extensions = c("FixedColumns", "Scroller"),
  options = list(
    pageLength = -1,
    dom = 't',
    scrollX = TRUE,
    scrollY = FALSE,
    scrollCollapse = TRUE,
    autoWidth = TRUE,
    fixedColumns = list(leftColumns = 4),
    columDefs = list(
      list(targets = 0:3, autoWidth = TRUE)
    ),
    initComplete = JS(
      "function(settings, json) {
         var $scrollBody = $('.dataTables_scrollBody');
         setTimeout(function(){
            if ($scrollBody.length) {
               $scrollBody.scrollLeft($scrollBody[0].scrollWidth);
            }
         }, 200);
       }"
    ),
    
    headerCallback = JS(
      "function(thead, data, start, end, display){
         $(thead).css({'background-color': '#e5e7eb',
                       'color': '#000',
                       'font-weight': 'bold',});
       }"
    )
  ),
  class = 'stripe row-border',
  width = "100%"
)|>
  formatRound(columns = names(base_def_ia)[3:4], digits = 2) |>
  formatRound(columns = names(base_def_ia)[5:ncol(base_def_ia)], digits = 1) |>
  formatStyle(
    columns = names(base_def_ia)[5:ncol(base_def_ia)], 
    backgroundColor = styleInterval(0, c("#CD5555", "darkseagreen")),
    color = styleInterval(0, c("white", "black")),
    textAlign = "center",
    fontWeight = "bold"
  ) |>
  formatStyle(
    columns = names(base_def_ia)[1:4], 
    textAlign = "left",
    color = "#234e5f",
    `vertical-align` = "middle",
    fontWeight = "bold"
  )

dt_ia <- htmlwidgets::prependContent(dt_ia, htmltools::tags$style(htmltools::HTML(mi_css)))
dt_ia <- htmlwidgets::prependContent(dt_ia, htmltools::tags$script(htmltools::HTML(mi_script)))

htmlwidgets::saveWidget(
  dt_ia,
  file = "Tabla_ICA-SFE_IA.html",
  selfcontained = TRUE,
  title = NULL
)


#### GENERA LA LISTA DE INDICADORES ####
tabla <- clasif |>  # TABLA DE CLASIFICADORES PARA LEYENDA
  dplyr::left_join(base_pivoteada, by = "Indicadores") |>
  dplyr::select(Indicadores, Sector, nombre) |>
  dplyr::mutate(Grupo_orden = as.numeric(gsub("G", "", Sector))) |> # Extraer la parte numérica de "Grupo"
  dplyr::arrange(Grupo_orden) |> # Ordenar por la parte numérica
  dplyr::select(-Grupo_orden) |> # Eliminar la columna auxiliar si no es necesaria  
  dplyr::select(-Sector)

# GENERAR TEXTO DE LOS INDICADORES EN NEGRITA
glosario_indicadores <- paste0(
  tabla$Indicadores, ": " , tabla$nombre, collapse = "<br>\n"
)

#### GENERA LA SALIDA DE LA LISTA DE INDICADORES ####
writeLines(glosario_indicadores, "glosario_indicadores.html")

#### GENERA LA SALIDA DE GRUPOS DE CLASIFICACION ####
grupos_html <- clasif |>
  dplyr::filter(!is.na(Sector)) |>
  dplyr::distinct(Sector, Descripcion) |>
  dplyr::mutate(Grupo_orden = base::as.numeric(gsub("G", "", Sector))) |>
  dplyr::arrange(Grupo_orden) |>
  dplyr::mutate(linea = base::paste0(Sector, ": ", Descripcion, "<br>")) |>
  dplyr::pull(linea) |>
  base::paste0(collapse = "\n")

base::writeLines(grupos_html, "grupos_clasificacion.html")

