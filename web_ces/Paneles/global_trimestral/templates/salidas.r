#### GENERA UNA SALIDA PARA LA FECHA ####
fecha_html <- sprintf(format(Sys.Date(), "%d de %B de %Y"))

writeLines(fecha_html, "fragments/update_date.html")

#### PREPARA LOS DATOS PARA EL DATATABLE VAR_VT ####
#     position: absolute; # esto estaba en logo, primera fila
nombres <- base::as.character(readxl::read_excel("C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/Panel global trimestral/Archivos necesarios/DF_paneles.xlsx", # NOMBRES DE LA SERIE
                                                 sheet = "trimestrales",
                                                 range = "a1:h1", col_names = F)) # AL CARGAR UNA NUEVA SERIE, SE DEBE MODIFICAR EL RANGO PARA INCLURLA

# CARGAR Y PREPARAR LOS DATOS
base <- readxl::read_excel("C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/Panel global trimestral/Archivos necesarios/DF_paneles.xlsx", # 
                           sheet = "trimestrales",
                           range = "a202:h433", col_names = F) # 202 ES ENERO DE 2020 | AL CARGAR UNA NUEVA SERIE, SE DEBE MODIFICAR EL RANGO PARA INCLURLA
base::names(base) <- nombres

media <- readxl::read_excel("C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/Panel global trimestral/Archivos necesarios/DF_paneles.xlsx", # CALCULO DE MEDIA
                            sheet = "trimestrales", col_names = T) |>
  dplyr::select(-Fecha) |>                
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(.x, na.rm = TRUE)))|>
  tidyr::pivot_longer(dplyr::everything(), names_to = "Indicadores", values_to = "μ")

desvio <- readxl::read_excel("C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/Panel global trimestral/Archivos necesarios/DF_paneles.xlsx", # CALCULO DE SD
                             sheet = "trimestrales", col_names = T) |>
  dplyr::select(-Fecha) |>                
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ sd(.x, na.rm = TRUE)))|>
  tidyr::pivot_longer(dplyr::everything(), names_to = "Indicadores", values_to = "σ")

estadisticos <- base::data.frame(media, σ = desvio$σ) # JUNTANDO AMBOS ESTADÍSTICOS

base_filtrada <- utils::tail(base[1:max(base::which(base::rowSums(!base::is.na(base[-1])) > 0)),],12) # 12 TRIMESTRES

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

library(DT)

dt_vt <- datatable(
  base_def,
  rownames = FALSE,
  escape = FALSE,
  extensions = c("FixedColumns", "Scroller"),
  options = list(
    pageLength = -1,
    dom = 't',
    scrollX = TRUE,
    scrollY = TRUE,
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

htmlwidgets::saveWidget(
  dt_vt,
  file = "../datatable_panel_vt.html",
  libdir = "datatable_panel_files_vt",
  selfcontained = FALSE,    # <---- IMPORTANTE PARA QUE NO ROMPA EL LAYOUT
  title = NULL
)

# limpiar_widget_html <- function(path_in, path_out) {
#   frag <- readLines(path_in, warn = FALSE)
#   
#   # 1) Líneas globales a eliminar siempre
#   patrones_basura <- c(
#     "<!DOCTYPE", "<html", "</html>", "<head>", "</head>",
#     "<body>", "</body>", "<meta", "<style", "</style>"
#   )
#   
#   frag <- frag[!grepl(paste(patrones_basura, collapse = "|"), frag)]
#   
#   
#   # 2) Detectar el bloque htmlwidget_container
#   idx_open <- grep('<div id="htmlwidget_container">', frag)
#   
#   if (length(idx_open) == 1) {
#     
#     # 2A) buscar el cierre real (primer </div> después de la apertura)
#     idx_after <- (idx_open + 1):length(frag)
#     idx_close_rel <- grep("^\\s*</div>\\s*$", frag[idx_after])
#     
#     if (length(idx_close_rel) > 0) {
#       idx_close <- idx_after[idx_close_rel[1]]
#       
#       # 3) Antes de borrar, rescatamos los div internos
#       bloque <- frag[(idx_open + 1):(idx_close - 1)]
#       
#       # 4) Eliminamos todo el bloque externo
#       frag <- frag[-(idx_open:idx_close)]
#       
#       # 5) Insertamos solo las líneas internas donde estaba el bloque
#       # (primero insertamos, luego hacemos limpieza de indices)
#       frag <- append(frag, bloque, after = idx_open - 1)
#     } 
#     else {
#       # Si no encuentra cierre, solo borra apertura (fallback seguro)
#       frag <- frag[-idx_open]
#     }
#   }
#   
#   
#   # 4) Borrar scripts inline basura de htmlwidgets pero NO bibliotecas
#   frag <- frag[!grepl("^<script>.*htmlwidgets.*</script>$", frag)]
#   
#   
#   # 5) Limpieza final de líneas vacías
#   frag <- frag[nchar(trimws(frag)) > 0]
#   
#   
#   # 6) Guardar
#   writeLines(frag, path_out)
#   message("[OK] Widget limpiado y guardado en: ", path_out)
# }
# 
# limpiar_widget_html("../datatable_panel_vt.html", "../datatable_panel_vt.html")


frag <- readLines("../datatable_panel_vt.html", warn = FALSE)


frag <- frag[!grepl("^<!DOCTYPE|<html|</html>|<head>|</head>|<body>|</body>|<style>|</style>|<div id=\"htmlwidget_container\">|<meta", frag)]

# 4) GUARDADO PARA INYECCION
writeLines(frag, "../datatable_panel_vt.html")

#### PREPARA LOS DATOS PARA EL DATATABLE VAR_IA ####
nombres <- base::as.character(readxl::read_excel("C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/Panel global trimestral/Archivos necesarios/DF_paneles.xlsx", # NOMBRES DE LA SERIE
                                                 sheet = "i.a.",
                                                 range = "a1:h1", col_names = F)) # AL CARGAR UNA NUEVA SERIE, SE DEBE MODIFICAR EL RANGO PARA INCLURLA

# CARGAR Y PREPARAR LOS DATOS
base <- readxl::read_excel("C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/Panel global trimestral/Archivos necesarios/DF_paneles.xlsx", # 
                           sheet = "i.a.",
                           range = "a202:h433", col_names = F) # 362 ES ENERO DE 2020 | AL CARGAR UNA NUEVA SERIE, SE DEBE MODIFICAR EL RANGO PARA INCLURLA
base::names(base) <- nombres

media <- readxl::read_excel("C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/Panel global trimestral/Archivos necesarios/DF_paneles.xlsx", # CALCULO DE MEDIA
                            sheet = "i.a.", col_names = T) |>
  dplyr::select(-Fecha) |>                
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ mean(.x, na.rm = TRUE)))|>
  tidyr::pivot_longer(dplyr::everything(), names_to = "Indicadores", values_to = "μ")

desvio <- readxl::read_excel("C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/Panel global trimestral/Archivos necesarios/DF_paneles.xlsx", # CALCULO DE SD
                             sheet = "i.a.", col_names = T) |>
  dplyr::select(-Fecha) |>                
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ sd(.x, na.rm = TRUE)))|>
  tidyr::pivot_longer(dplyr::everything(), names_to = "Indicadores", values_to = "σ")

estadisticos <- base::data.frame(media, σ = desvio$σ) # JUNTANDO AMBOS ESTADÍSTICOS

base_filtrada <- utils::tail(base[1:max(base::which(base::rowSums(!base::is.na(base[-1])) > 0)),],48) # 48 trimestres

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
library(DT)
library(htmlwidgets)

dt_ia <- datatable(
  base_def,
  rownames = FALSE,
  escape = FALSE,
  extensions = c("FixedColumns", "Scroller"),
  options = list(
    pageLength = -1,
    dom = 't',
    scrollX = TRUE,
    scrollY = TRUE,
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

htmlwidgets::saveWidget(
  dt_ia,
  file = "../datatable_panel_ia.html",
  libdir = "datatable_panel_files_ia",
  selfcontained = FALSE,    # <---- IMPORTANTE PARA QUE NO ROMPA EL LAYOUT
  title = NULL
)

frag <- readLines("../datatable_panel_ia.html", warn = FALSE)

frag <- frag[!grepl("^<!DOCTYPE|<html|</html>|<head>|</head>|<body>|</body>|<style>|</style>|<div id=\"htmlwidget_container\">|<meta", frag)]

# 4) GUARDADO PARA INYECCION
writeLines(frag, "../datatable_panel_ia.html")


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
writeLines(glosario_indicadores, "fragments/glosario_indicadores.html")

