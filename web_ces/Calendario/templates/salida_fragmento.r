### FUNCIONES ####
limpiar_widget_html <- function(path_in, path_out) {
  frag <- readLines(path_in, warn = FALSE)
  
  # 1) Líneas globales a eliminar siempre
  patrones_basura <- c(
    "<!DOCTYPE", "<html", "</html>", "<head>", "</head>",
    "<body>", "</body>", "<meta", "<style", "</style>", "<title>", "</title>"
  )
  
  frag <- frag[!grepl(paste(patrones_basura, collapse = "|"), frag)]
  
  
  # 2) Detectar el bloque htmlwidget_container
  idx_open <- grep('<div id="htmlwidget_container">', frag)
  
  if (length(idx_open) == 1) {
    
    # 2A) buscar el cierre real (primer </div> después de la apertura)
    idx_after <- (idx_open + 1):length(frag)
    idx_close_rel <- grep("^\\s*</div>\\s*$", frag[idx_after])
    
    if (length(idx_close_rel) > 0) {
      idx_close <- idx_after[idx_close_rel[1]]
      
      # 3) Antes de borrar, rescatamos los div internos
      bloque <- frag[(idx_open + 1):(idx_close - 1)]
      
      # 4) Eliminamos todo el bloque externo
      frag <- frag[-(idx_open:idx_close)]
      
      # 5) Insertamos solo las líneas internas donde estaba el bloque
      # (primero insertamos, luego hacemos limpieza de indices)
      frag <- append(frag, bloque, after = idx_open - 1)
    }
    else {
      # Si no encuentra cierre, solo borra apertura (fallback seguro)
      frag <- frag[-idx_open]
    }
  }
  
  
  # 4) Borrar scripts inline basura de htmlwidgets pero NO bibliotecas
  frag <- frag[!grepl("^<script>.*htmlwidgets.*</script>$", frag)]
  
  
  # 5) Limpieza final de líneas vacías
  frag <- frag[nchar(trimws(frag)) > 0]
  
  
  # 6) Guardar
  writeLines(frag, path_out)
  message("[OK] Widget limpiado y guardado en: ", path_out)
}
#### INPUT DATOS ####
df_calendario <- readxl::read_excel("C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/Seguimiento Manual.xlsx", range = "OUT_Calendario!B2:I300", col_names = TRUE)

#### VOLVER AL INDICE ####
volver_al_indice <- paste('<div class="back-to-index-container">
                              <a href="#ref_notas" class="back-to-index-btn">
                                <span class="btn-icon">↑</span>
                                <span class="btn-text">Volver al índice</span>
                              </a>
                            </div>')

#### LIMPIEZA DE DATOS ####
df_limpio <- df_calendario |>
  dplyr::filter(!base::is.na(`Categoría`) & `Categoría` != "") |>
  dplyr::mutate(
    `Fecha de publicación conocida` = dplyr::case_when(
      suppressWarnings(!base::is.na(base::as.numeric(`Fecha de publicación conocida`))) ~ base::as.Date(base::as.numeric(`Fecha de publicación conocida`), origin = "1899-12-30"),
      suppressWarnings(!base::is.na(lubridate::dmy(`Fecha de publicación conocida`))) ~ lubridate::dmy(`Fecha de publicación conocida`),
      TRUE ~ lubridate::NA_Date_
    ),
    `Fecha de publicación estimada` = dplyr::case_when(
      suppressWarnings(!base::is.na(base::as.numeric(`Fecha de publicación estimada`))) ~ format(base::as.Date(base::as.numeric(`Fecha de publicación estimada`), origin = "1899/12/30"), "%d/%m/%Y"),
      suppressWarnings(!base::is.na(lubridate::dmy(`Fecha de publicación estimada`))) ~ format(lubridate::dmy(`Fecha de publicación estimada`), "%d/%m/%Y"),
      TRUE ~ as.character(`Fecha de publicación estimada`)
    ),
    `Último dato` = dplyr::case_when(
      # Caso para datos mensuales
      `Frecuencia` == "Mensual" ~ {
        year <- stringr::str_extract(`Último dato`, "^\\d{4}")
        # Corrige la expresión regular para extraer los últimos dos dígitos
        month_num <- base::as.numeric(stringr::str_extract(`Último dato`, "\\d{2}$"))
        base::paste0(year, ".M", month_num)
      },
      # Caso para datos trimestrales
      `Frecuencia` == "Trimestral" ~ {
        year <- stringr::str_extract(`Último dato`, "^\\d{4}")
        # Corrige la expresión regular para extraer los últimos dos dígitos
        month_num <- base::as.numeric(stringr::str_extract(`Último dato`, "\\d$"))
        base::paste0(year, ".T", month_num)
      },
      # Caso para datos semestrales
      `Frecuencia` == "Semestral" ~ {
        year <- stringr::str_extract(`Último dato`, "^\\d{4}")
        # Corrige la expresión regular para extraer los últimos dos dígitos
        month_num <- base::as.numeric(stringr::str_extract(`Último dato`, "\\d$"))
        base::paste0(year, ".S", month_num)
      },
      # El resto de los casos (incluyendo anuales) no se modifican
      TRUE ~ `Último dato`
    )
  )

#### FILTROS ####
shared_df <- crosstalk::SharedData$new(df_limpio)

filtros <- htmltools::tagList(
  htmltools::div(class = "row",
                 htmltools::div(class = "col-sm-4",
                                crosstalk::filter_select("Categoría", "Categoría", shared_df, ~`Categoría`)
                 ),
                 htmltools::div(class = "col-sm-4",
                                crosstalk::filter_select("Fuente", "Fuente", shared_df, ~Fuente)
                 ),
                 htmltools::div(class = "col-sm-4",
                                crosstalk::filter_select("Alcance geográfico", "Alcance geográfico", shared_df, ~`Alcance geográfico`)
                 )
  )
)

htmltools::save_html(
  filtros,
  file = "../filtros.html",
  background = "transparent"
)

limpiar_widget_html("../filtros.html", "../filtros.html")

#### TABLA INTERACTIVA (DEFINICION Y EXPORTACION) ####
tabla <- DT::datatable(
  shared_df,
  rownames = FALSE,
  filter = "none",
  options = base::list(
    pageLength = 150,
    dom = "ft",
    scrollX = TRUE,
    fixedHeader = TRUE,
    autoWidth = TRUE,
    order = base::list(base::list(0, "asc"), base::list(1, "asc")),  # Ordenar por la primera columna "Indicador"
    columnDefs = base::list(
      base::list(className = "dt-center", targets = "_all"),
      base::list(
        targets = 3, # columnas de fecha
        render = DT::JS(
          "function(data, type, row, meta) {",
          "  if(type === 'sort') {",
          "    if(data === null || data === '' || data === '-') { return '9999-12-31'; }",
          "    return data;",
          "  }",
          "  if(type === 'display') {",
          "    if(data === null || data === '') { return '-'; }",
          "    var d = new Date(data);",
          "    if(isNaN(d)) { return data; }",
          "    var day = ('0' + (d.getUTCDate())).slice(-2);",
          "    var month = ('0' + (d.getUTCMonth()+1)).slice(-2);",
          "    var year = d.getFullYear();",
          "    return day + '/' + month + '/' + year;",
          "  }",
          "  return data;",
          "}"
        )
      )
    ),
    language = base::list(
      search = "Buscar",
      searchPlaceholder = "texto en tabla",
      paginate = base::list(previous = "", `next` = ""),
      info = ""
    )
  ),
  class = "compact stripe hover"
)

htmlwidgets::saveWidget(
  tabla,
  file = "../tabla.html",
  libdir = "tabla_files",
  selfcontained = FALSE,    # <---- IMPORTANTE PARA QUE NO ROMPA EL LAYOUT
  title = NULL
)

limpiar_widget_html("../tabla.html", "../tabla.html")

#### CREACION Y SALIDA DE LA FECHA ####
fecha_html <- sprintf(format(Sys.Date(), "%d de %B de %Y"))

writeLines(fecha_html, "update_date.html")
#### INYECCIÓN EN CALENDARIO ####
base::source("injects.r")
