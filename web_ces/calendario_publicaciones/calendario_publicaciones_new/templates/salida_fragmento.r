#### CONFIGURACIÓN DE PATH ####
# path_0 <- getwd()
# setwd("C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/calendario_publicaciones/calendario_publicaciones_new")

#### INPUT DATOS ####
df_calendario <- readxl::read_excel("C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/Seguimiento Manual.xlsx", range = "OUT_Calendario!B2:I300", col_names = TRUE)

#### LIMPIEZA DE DATOS ####
df_limpio <- df_calendario |>
  dplyr::filter(!base::is.na(`Categoría`) & `Categoría` != "") |>
  dplyr::mutate(
    `Último dato` = stringr::str_squish(`Último dato`)
  ) |>
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
    
    year = base::as.integer(stringr::str_extract(`Último dato`, "^\\d{4}")),
      
    segundo_str = dplyr::if_else(
        `Frecuencia` == 'Mensual',
        base::as.integer(stringr::str_extract(`Último dato`, "(?<=,)\\d+")),
        (dplyr::if_else( 
          `Frecuencia` == "Trimestral" | `Frecuencia` == "Semestral",
          base::as.integer(stringr::str_extract(`Último dato`, "(?<=,)\\d+")),
          NA_integer_)
         )
      ),
      
    month = dplyr::case_when(
      `Frecuencia` == "Censal" ~ 1L,
      `Frecuencia` == "Anual" ~ 1L,
      `Frecuencia` == "Mensual" ~ segundo_str,
      `Frecuencia` == "Trimestral" ~ segundo_str * 3L,
      `Frecuencia` == "Semestral" ~ segundo_str * 6L,
      TRUE ~ NA_integer_
      ),
    
    orden_fecha = year * 100 + month,
    
    `Último dato` = orden_fecha #UNA VEZ ESTABILIZADO EL CALENDARIO REEMPLAZAR LA LOGICA DE ORDEN DE FECHA PARA ULTIMO DATO ASI SE LIBERA DE CREAR UNA COLUMNA REDUNDANTE
    
    )    

shared_df <- crosstalk::SharedData$new(df_limpio)
#### TABLA INTERACTIVA (DEFINICION Y EXPORTACION) ####
idx_frecuencia <- which(colnames(df_limpio) == "Frecuencia") - 1

mi_script <- "
(function() {
    // 1. La función que mide y envía
    function sendHeight() {
        // Buscamos el widget o el contenedor principal
        const container = document.querySelector('.html-widget') || document.body;
        const height = container.scrollHeight;
        
        console.log('Hijo: Enviando altura al padre:', height);
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
      base::list(
        targets= which(colnames(df_limpio) %in% c("year", "segundo_str", "month", "orden_fecha")) - 1,
        visible= FALSE
                   ),
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
      ),
      base::list(
        targets = which(colnames(df_limpio) == "Último dato") - 1,
        render = DT::JS(
          "function(data, type, row, meta) {",
          "  if (data === null || data === '') { return '-'; }",
          "  if (type === 'display') {",
          "    var s = data.toString();",
          "    if (s.length === 6) {",
          "      var anio = s.substring(0, 4);",
          "      var valor = parseInt(s.substring(4, 6));",
          "      var frec = row[", idx_frecuencia, "];", # Accedemos a la columna Frecuencia
          "      if (frec === 'Mensual') return anio + '.M' + valor;",
          "      if (frec === 'Trimestral') return anio + '.T' + (parseInt(valor)/3);",
          "      if (frec === 'Semestral') return anio + '.S' + (parseInt(valor)/6);",
          "      if (frec === 'Anual' || frec === 'Censal' ) return anio;",
          "      ",
          "      return anio + '.' + valor;", # Por si hay otra frecuencia no contemplada
          "    }",
          "  }",
          "  return data;",
          "}"
        )
      )
    ),
    language = base::list(
      search = "Buscar",
      searchPlaceholder = "texto dentro de la tabla",
      paginate = base::list(previous = "", `next` = ""),
      info = ""
    )
  ),
  class = "compact stripe hover"
)

mi_css <- readLines("../assets/css/styles-interno-datatable.css")
# mi_script <- readLines("../assets/js/script-interno-datatable.js")

tabla <- htmlwidgets::prependContent(tabla, htmltools::tags$style(htmltools::HTML(mi_css)))
tabla <- htmlwidgets::prependContent(tabla, htmltools::tags$script(htmltools::HTML(mi_script)))

htmlwidgets::saveWidget(
  tabla,
  file = "tabla.html",
  selfcontained = TRUE,
  title = NULL
)

# setwd(path_0)

### CREACION Y SALIDA DE LA FECHA ####

# Generar la fecha con el formato deseado
fecha_texto <- format(Sys.Date(), "%d de %B de %Y")

# Leer la plantilla principal
template <- readLines("../Calendario.html", encoding = "UTF-8")

# Reemplazar la marca por la fecha real
html_final <- gsub("{{FECHA_ACTUAL}}", fecha_texto, template, fixed = TRUE)

# Guardar el HTML final
writeLines(html_final, "../Calendario.html", useBytes = TRUE)

# 
# setwd(path_0)
