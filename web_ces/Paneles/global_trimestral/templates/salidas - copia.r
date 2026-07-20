# FUNCIONES ####
convertir_a_trimestre <- function(fecha_str) {

  anio <- substr(fecha_str, 1, 4)

  anio_ult_2 <- substr(fecha_str, 3, 4)

  mes <- as.numeric(substr(fecha_str, 6, 7))

  trimestre <- ifelse(mes >= 1 & mes <= 3, "T1",
                      ifelse(mes >= 4 & mes <= 6, "T2",
                             ifelse(mes >= 7 & mes <= 9, "T3",
                                    ifelse(mes >= 10 & mes <= 12, "T4", NA))))

  resultado <- paste0(trimestre, "-" ,anio_ult_2)

  return(resultado)
}

### CREACION Y SALIDA DE LA FECHA ####

fecha_html <- base::sprintf(base::format(base::Sys.Date(), "%d de %B de %Y"))

base::writeLines(fecha_html, "update_date.html")

#### PREPARA LOS DATOS PARA EL DATATABLE VAR_VT ####
DF_paneles_path <- "../input/DF_paneles.xlsx"

nombres <- base::as.character(readxl::read_excel(DF_paneles_path, # NOMBRES DE LA SERIE
                                                 sheet = "trimestrales",
                                                 range = "a1:j1", col_names = F)) # AL CARGAR UNA NUEVA SERIE, SE DEBE MODIFICAR EL RANGO PARA INCLURLA

# CARGAR Y PREPARAR LOS DATOS
base <- readxl::read_excel(DF_paneles_path, #
                           sheet = "trimestrales",
                           range = "a202:j433", col_names = F) # 202 ES ENERO DE 2020 | AL CARGAR UNA NUEVA SERIE, SE DEBE MODIFICAR EL RANGO PARA INCLURLA
base::names(base) <- nombres

media <- readxl::read_excel(DF_paneles_path, # CALCULO DE MEDIA
                            sheet = "trimestrales", col_names = T) |>
  dplyr::select(-Fecha) |>
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(.x, na.rm = TRUE)))|>
  tidyr::pivot_longer(dplyr::everything(), names_to = "Indicadores", values_to = "μ")

desvio <- readxl::read_excel(DF_paneles_path, # CALCULO DE SD
                             sheet = "trimestrales", col_names = T) |>
  dplyr::select(-Fecha) |>
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ sd(.x, na.rm = TRUE)))|>
  tidyr::pivot_longer(dplyr::everything(), names_to = "Indicadores", values_to = "σ")

estadisticos <- base::data.frame(media, σ = desvio$σ) # JUNTANDO AMBOS ESTADÍSTICOS

base_filtrada <- utils::tail(base[1:max(base::which(base::rowSums(!base::is.na(base[-1])) > 0)),],48) # 48 TRIMESTRES

base_filtrada <- base_filtrada |>
  dplyr::mutate(Fecha = convertir_a_trimestre(Fecha))

base_larga <- base_filtrada |>
  tidyr::pivot_longer(
    cols = -Fecha,  # Todas las columnas excepto 'Fecha'
    names_to = "Indicadores", # Crear una columna 'Indicadores' con los nombres de las series
    values_to = "Valor" # Los valores de las series en una columna 'Valor'
  )

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

# Detectar cambios de trimestre y año
years <- base::sapply(base::names(base_def), function(x) base::ifelse(base::grepl("-", x), base::substr(x, base::nchar(x) - 1, base::nchar(x)), NA))

# Identificar las posiciones donde cambiará el borde
quarter_positions <- grep("^T[1-4]", names(base_def))  # Desde la tercera columna (primer trimestre)
year_positions <- grep("T4", names(base_def)) + 1  # Donde cambia el año

#### GENERA UNA SALIDA PARA EL DATATABLE VAR_VT ####

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

dt_vt <- DT::datatable(
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

dt_vt <- htmlwidgets::prependContent(dt_vt, htmltools::tags$style(htmltools::HTML(mi_css)))
dt_vt <- htmlwidgets::prependContent(dt_vt, htmltools::tags$script(htmltools::HTML(mi_script)))

htmlwidgets::saveWidget(
  dt_vt,
  file = "Tabla_global_VT.html",
  selfcontained = TRUE,
  title = NULL
)

#### PREPARA LOS DATOS PARA EL DATATABLE VAR_IA ####
nombres <- base::as.character(readxl::read_excel(DF_paneles_path, # NOMBRES DE LA SERIE
                                                 sheet = "i.a.",
                                                 range = "a1:j1", col_names = F)) # AL CARGAR UNA NUEVA SERIE, SE DEBE MODIFICAR EL RANGO PARA INCLURLA

# CARGAR Y PREPARAR LOS DATOS
base <- readxl::read_excel(DF_paneles_path, #
                           sheet = "i.a.",
                           range = "a202:j433", col_names = F) # 202 ES ENERO DE 2020 | AL CARGAR UNA NUEVA SERIE, SE DEBE MODIFICAR EL RANGO PARA INCLURLA
base::names(base) <- nombres

media <- readxl::read_excel(DF_paneles_path, # CALCULO DE MEDIA
                            sheet = "i.a.", col_names = T) |>
  dplyr::select(-Fecha) |>
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(.x, na.rm = TRUE)))|>
  tidyr::pivot_longer(dplyr::everything(), names_to = "Indicadores", values_to = "μ")

desvio <- readxl::read_excel(DF_paneles_path, # CALCULO DE SD
                             sheet = "i.a.", col_names = T) |>
  dplyr::select(-Fecha) |>
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ sd(.x, na.rm = TRUE)))|>
  tidyr::pivot_longer(dplyr::everything(), names_to = "Indicadores", values_to = "σ")

estadisticos <- base::data.frame(media, σ = desvio$σ) # JUNTANDO AMBOS ESTADÍSTICOS

base_filtrada <- utils::tail(base[1:max(base::which(base::rowSums(!base::is.na(base[-1])) > 0)),],48) # 48 TRIMESTRES

base_filtrada <- base_filtrada |>
  dplyr::mutate(Fecha = convertir_a_trimestre(Fecha))

base_larga <- base_filtrada |>
  tidyr::pivot_longer(
    cols = -Fecha,  # Todas las columnas excepto 'Fecha'
    names_to = "Indicadores", # Crear una columna 'Indicadores' con los nombres de las series
    values_to = "Valor" # Los valores de las series en una columna 'Valor'
  )

rename_fecha_cols_ia <- function(nombres) {
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

# Detectar cambios de trimestre y año
years <- base::sapply(base::names(base_def), function(x) base::ifelse(base::grepl("-", x), base::substr(x, base::nchar(x) - 1, base::nchar(x)), NA))

quarter_positions <- grep("^T[1-4]", names(base_def))  # Desde la tercera columna (primer trimestre)
year_positions <- grep("T4", names(base_def)) + 1  # Donde cambia el año

#### GENERA UNA SALIDA PARA EL DATATABLE VAR_IA ####

dt_ia <- DT::datatable(
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

dt_ia <- htmlwidgets::prependContent(dt_ia, htmltools::tags$style(htmltools::HTML(mi_css)))
dt_ia <- htmlwidgets::prependContent(dt_ia, htmltools::tags$script(htmltools::HTML(mi_script)))

htmlwidgets::saveWidget(
  dt_ia,
  file = "Tabla_global_IA.html",
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
  tabla$Indicadores, ": ", tabla$nombre, collapse = "<br>\n"
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
