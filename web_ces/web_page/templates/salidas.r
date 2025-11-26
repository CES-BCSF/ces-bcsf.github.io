#### GENERA UNA SALIDA PARA LA FECHA ####
fecha_html <- sprintf(format(Sys.Date(), "%d de %B de %Y"))

writeLines(fecha_html, "fragments/update_date.html")

#### PREPARA LOS DATOS PARA EL DATA TABLE ####
nombres <- base::as.character(readxl::read_excel("C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/Panel ICA-SFE/Archivos necesarios/DF_paneles.xlsx", # NOMBRES DE LA SERIE
                                                 sheet = "mensuales",
                                                 range = "a1:i1", col_names = F)) # AL CARGAR UNA NUEVA SERIE, SE DEBE MODIFICAR EL RANGO PARA INCLURLA

# CARGAR Y PREPARAR LOS DATOS
base <- readxl::read_excel("C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/Panel ICA-SFE/Archivos necesarios/DF_paneles.xlsx", # 
                           sheet = "mensuales",
                           range = "a362:i433", col_names = F) # 362 ES ENERO DE 2020 | AL CARGAR UNA NUEVA SERIE, SE DEBE MODIFICAR EL RANGO PARA INCLURLA
base::names(base) <- nombres

media <- readxl::read_excel("C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/Panel ICA-SFE/Archivos necesarios/DF_paneles.xlsx", # CALCULO DE MEDIA
                            sheet = "mensuales", col_names = T) |>
  dplyr::select(-Fecha) |>                
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(.x, na.rm = TRUE)))|>
  tidyr::pivot_longer(dplyr::everything(), names_to = "Indicadores", values_to = "μ")

desvio <- readxl::read_excel("C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/Panel ICA-SFE/Archivos necesarios/DF_paneles.xlsx", # CALCULO DE SD
                             sheet = "mensuales", col_names = T) |>
  dplyr::select(-Fecha) |>                
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ sd(.x, na.rm = TRUE)))|>
  tidyr::pivot_longer(dplyr::everything(), names_to = "Indicadores", values_to = "σ")

estadisticos <- base::data.frame(media, σ = desvio$σ) # JUNTANDO AMBOS ESTADÍSTICOS

base_filtrada <- utils::tail(base[1:max(base::which(base::rowSums(!base::is.na(base[-1])) > 0)),],48) # 48 MESES
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

#### GENERA UNA SALIDA PARA EL DATATABLE ####

library(DT)
library(htmlwidgets)
dt <- datatable(
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

htmlwidgets::saveWidget(
  dt,
  file = "temp_dt.html",
  libdir = "datatable_panel_files",
  selfcontained = FALSE,    # <---- IMPORTANTE PARA QUE NO ROMPA EL LAYOUT
  title = NULL
)

frag <- readLines("temp_dt.html", warn = FALSE)

frag <- frag[!grepl("^<!DOCTYPE|<html|</html>|<head>|</head>|<body>|</body>|<style>|</style>|<div id=\"htmlwidget_container\">|<meta", frag)]

# 4) GUARDADO PARA INYECCION
writeLines(frag, "datatable_panel.html")


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
  "  <strong>", tabla$Indicadores, "</strong>: ", tabla$nombre, collapse = "<br>\n"
)

#### GENERA LA SALIDA DE LA LISTA DE INDICADORES ####
writeLines(glosario_indicadores, "fragments/glosario_indicadores.html")

