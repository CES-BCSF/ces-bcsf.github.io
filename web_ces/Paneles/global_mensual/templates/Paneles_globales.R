# SERIES DESESTACIONALIZADAS DE LOS MAESTROS ####

clasif <- readxl::read_excel("C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/Panel global mensual/Archivos necesarios/Clasificador.xlsx",
                     sheet = "Clasificador",
                     col_names = T) |>
  dplyr::mutate(Enlace = base::paste0("https://ces-bcsf.github.io/web_ces/indicadores/", Indicadores, "_views.html"))

directorio <- "C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/Output_R" # DIRECTORIO DEL CUAL TOMA EL INPUT DE CADA INDICADOR

archivos <- base::list.files(path = directorio, pattern = "*.xlsx", full.names = TRUE) # LISTADO DE ARCHIVOS A LEER

lista_datos <- base::list() # LISTA DE DATOS DESESTACIONALIZADOS
valores_c5 <- base::list() # LISTA DE DESCRIPCIONES DE SERIE
nombre <- base::list() # LISTA DE NOMBRES DE GRUPOS
 
for (archivo in archivos) { 
  nombre_columna <- stringr::str_extract(base::basename(archivo), "^[^_]+")
  
  # LEER DATOS DE LA HOJA
  datos <- readxl::read_excel(archivo,
                              col_names = TRUE,
                              sheet = "Data",
                              range = "M1:M493") # LLEGADO EL CASO, 493 DEBE SER MODIFICADO PARA CONTEMPLAR TODAS LA COLUMNA. TENIENDO EN CUENTA LA SERIE MÁS LARGA (DATADA EN 1990.01) 493 ES 2030.12
  base::colnames(datos) <- nombre_columna
  lista_datos[[nombre_columna]] <- datos
  
  nombre[[nombre_columna]] <- readxl::read_excel(archivo, range = "C2:C2", col_names = FALSE)
  
  # LEER VALOR DE LA CELDA C5 | NOMBRE DEL SECTOR ASIGNADO EN LA FICHA DE CADA MAESTRO
  valor_c5 <- readxl::read_excel(archivo, range = "C5:C5", col_names = FALSE)
  
  # VERIFICAR QUE EXISTEN DATOS EN AVLOR _c5 ANTES DE INTENTAR ACCEDER AL PRIMER ELEMENTO
  if (nrow(valor_c5) > 0 && !is.na(valor_c5[[1, 1]])) {
    valores_c5[[nombre_columna]] <- valor_c5[[1, 1]]
  } else {
    valores_c5[[nombre_columna]] <- NA  # ASIGNAR NA SI NO HAY DATOS
  }
} # LOOP PARA LEER E INCORPORAR A LAS LISTAS ANTERIORES

lista_datos["HP"] <- NULL  # Equivalente al método anterior

df_combinado <- dplyr::bind_cols(lista_datos) # COMBINAR SERIES DESESTACIONALIZADAS

df_valores_c5 <- base::data.frame(Valores_C5 = base::unlist(valores_c5)) |> # COMBINAR DESCRIPCIONES DE LAS SERIES
  dplyr::mutate(Sector = dplyr::case_when( 
    Valores_C5 == "Producto y actividad económica" ~ "G1",
    Valores_C5 == "Mercado de capitales" ~ "G8",
    Valores_C5 == "Sector externo" ~ "G6",
    Valores_C5 == "Consumo minorista" ~ "G10",
    Valores_C5 == "Industria" ~ "G5",
    Valores_C5 == "Mercado inmobiliario y sector de la construcción" ~ "G3",
    Valores_C5 == "Recursos y gastos del sector público de la provincia de Santa Fe" ~ "G7",
    Valores_C5 == "Mercado de trabajo" ~ "G4",
    Valores_C5 == "Percepción macroeconómica" ~ "G2",
    Valores_C5 == "Patentamientos, transferencias y circulación vehicular" ~ "G9",
    Valores_C5 == "Otros indicadores" ~ "G11",
    TRUE ~ NA_character_  # ASIGNA NA A CUALQUIER OTRO CASO NO CONTEMPLADO 
  )) # ASIGNAR SECTORES A LAS DESCRIPCIONES

clasif2 <- df_valores_c5 |> # CLASIFICADOR USADO POSTERIORMENTE
  dplyr::mutate(Indicadores = base::rownames(df_valores_c5)) |>
  dplyr::select(Indicadores, dplyr::everything()) |>
  dplyr::rename("Descripcion" = Valores_C5)

df_nombre <- base::data.frame(nombre = base::unlist(nombre)) # COMBINAR TODOS LOS NOMBRES DE LAS SERIES
  
clasif3 <- df_nombre |>
  dplyr::mutate(Indicadores = base::substr(base::rownames(df_nombre), 1, base::nchar(base::rownames(df_nombre)) - 5))

clasif <- clasif |>
  dplyr::left_join(clasif2, by = "Indicadores") |>
  dplyr::left_join(clasif3, by = "Indicadores") # CLASIFICADOR FINAL

# FECHAS DE INICIO ####

lista_inicio <- base::list()

for (archivo in archivos) {
  nombre_columna <- stringr::str_extract(base::basename(archivo), "^[^_]+")
  
  datos <- readxl::read_excel(archivo, col_names = F, sheet = "Data", range = "a2:a2") # LEO LA CELDA ASOCIADA AL INICIO
  
  base::colnames(datos) <- nombre_columna
  
  lista_inicio[[nombre_columna]] <- datos
}

lista_inicio["HP"] <- NULL  # Equivalente al método anterior

df_combinado_inicio <- dplyr::bind_cols(lista_inicio) # UNIR FECHAS DE INICIO

base::length(df_combinado_inicio) # ARROJA LA CANTIDAD DE SERIES QUE CONSIDERA EL SISTEMA | TE PERMITE VERIFICAR QUE SE BARRIO TODA LISTA DE CLASIFICADOR

## LISTANDO POR CARPETA DEL SISTEMA #####

list_df <- clasif |> # LISTA DE SECTORES AGRUPADOS EN SECCIONES EN CORRESPONDECIA CON CARPETAS DEL SISTEMA
  dplyr::group_by(Carpeta) |>
  dplyr::summarise(serie = base::list(Indicadores), .groups = "drop") |>
  dplyr::mutate(data = purrr::map(serie, ~ df_combinado[.x])) |>
  dplyr::pull(data) |>
  magrittr::set_names(clasif$Carpeta[!base::duplicated(clasif$Carpeta)])  # ASIGNAR NOMBRES A CADA ELEMENTO DE LA LISTA BASADO EN CARPETA

lista_ts_desest <- base::lapply(list_df, function(df) { 
  purrr::map(df, ~stats::ts(.x, start=base::c(1990, 01), frequency=12))
}) # PASANDO A OBJETO TS A CADA UNA DE LAS SERIES

## PRODUCCION EN UN UNICO EXCEL DE LAS SERIES DESESTACIONALIZADAS  ####

df_combinado_inicio <- df_combinado_inicio |> # SE PARTE DEL EXCEL CON INICIOS DE FECHA Y SE LO PASA A DATE
    dplyr::mutate(dplyr::across(dplyr::everything(),
                  ~ base::as.Date(base::paste0(base::substr(., 1, 4),
                                   "-",
                                   base::substr(., 6, 7),
                                   "-01"))))
  
extender_serie <- function(ts_data, nombre_serie, fecha_inicio) { 
  datos <- tsibble::tsibble(
    Fecha = base::seq(fecha_inicio, by = "month", length = base::length(ts_data)),
    Valor = ts_data,
    .name_repair = "minimal"
    ) |>
    dplyr::select(Fecha, dplyr::all_of("Valor")) |>
    dplyr::rename_with(~ nombre_serie, .cols = "Valor")
  
  datos_completos <- df_fechas |>
    dplyr::left_join(datos, by = "Fecha")
  
  return(datos_completos)
 } # FUNCION PARA EXTENDER CADA SERIE
  

df_fechas <- tsibble::tibble(Fecha = base::seq(lubridate::ymd("1990-01-01"),  # CREANDO UN TIBBLE PARA RANGO COMPLETO DE FECHAS
                                               lubridate::ymd("2025-12-01"),
                                               by = "month"))

df_final <- df_fechas # CREANDO EL OBJETO FINAL
  
for (nombre_carpeta in base::names(lista_ts_desest)) { # REEMPLAZANDO CON AYUDA DE LA FUNCIÓN QUE EXTIENDE FECHAS
  for (nombre_serie in base::names(lista_ts_desest[[nombre_carpeta]])) {
    ts_data <- lista_ts_desest[[nombre_carpeta]][[nombre_serie]]
    fecha_inicio <- df_combinado_inicio[[nombre_serie]]
    df_serie_extendida <- extender_serie(ts_data, nombre_serie, fecha_inicio)
    df_final <- dplyr::left_join(df_final, df_serie_extendida, by = "Fecha")
  }
}

# NUEVAS LISTAS - CREACION DE SERIES DE VARIACIONES MENSUALES E INTERANUALES ####
  
## FUNCIONES ####
  
variacion_mensual <- function(serie, nombre) {
    serie |>
      dplyr::transmute(!!nombre := (serie[[1]] / dplyr::lag(serie[[1]]) - 1) * 100) 
  } # FUNCION DE VARIACION MENSUAL

variacion_interanual <- function(serie, nombre) {
    serie |>
    dplyr::transmute(!!nombre := (serie[[1]] / dplyr::lag(serie[[1]], 12) - 1) * 100)
  } # FUNCION DE VARIACION I.A.

## CREACION DE LISTAS ####
  
  variaciones_mensuales <- base::list()
  variaciones_interanuales <- base::list()
  
  for (nombre in base::names(lista_datos)) { # LISTA DATOS
    
    serie <- lista_datos[[nombre]]
    
    variaciones_mensuales[[nombre]] <- variacion_mensual(serie, nombre)
    
    variaciones_interanuales[[nombre]] <- variacion_interanual(serie, nombre)
  }
  
## EXTENDER VARIACIONES MENSUALES E INTERANUALES
  
  extender_variaciones <- function(lista_variaciones, lista_fechas_inicio) {
    lista_extendida <- base::list()
    for (nombre in base::names(lista_variaciones)) {
      fecha_inicio <- lista_fechas_inicio[[nombre]]
      serie <- lista_variaciones[[nombre]]
      lista_extendida[[nombre]] <- extender_serie(serie[[1]], nombre, fecha_inicio)
    }
    return(lista_extendida)
  }
  
  # EXTENDER LAS LISTAS DE VARIACIONES
  variaciones_mensuales_ext <- extender_variaciones(variaciones_mensuales, df_combinado_inicio)
  variaciones_interanuales_ext <- extender_variaciones(variaciones_interanuales, df_combinado_inicio)
  
  # COMBINAR LAS VARIACIONES EN DATAFRAME
  df_variaciones_mensuales <- purrr::reduce(variaciones_mensuales_ext, dplyr::left_join, by = "Fecha")
  df_variaciones_interanuales <- purrr::reduce(variaciones_interanuales_ext, dplyr::left_join, by = "Fecha")

# ORDEN DE LAS COLUMNAS
  
  orden_columnas <- base::c("Fecha", base::setdiff(base::names(df_final), "Fecha"))
  
  # REORDENA COLUMNAS
  df_variaciones_mensuales <- df_variaciones_mensuales |>
    dplyr::select(dplyr::all_of(orden_columnas))
  
  df_variaciones_interanuales <- df_variaciones_interanuales |>
    dplyr::select(dplyr::all_of(orden_columnas))
  
  # Escribir el archivo Excel con columnas en orden consistente
  writexl::write_xlsx(
    list(
      "nivel" = df_final,
      "mensuales" = df_variaciones_mensuales,
      "i.a." = df_variaciones_interanuales
    ),
    path = "C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/Panel global mensual/Archivos necesarios/DF_paneles.xlsx"
  )

  #
  
### GENERAR SALIDAS Y ACTUALIZAR HTML ####

base::source("salidas.r")

base::source("injects.r")

### SUBIR PANELES A GITHUB ####
base::source("subir_paneles.R")
  