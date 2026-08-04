## ACTUALIZAR SITE-CONTENT.JSON ####
#
# Funcion compartida para que CUALQUIER script de actualizacion (monitores de
# seguimiento, reportes automaticos, paneles, graficas interactivas, etc.)
# avise al portal (ces-bcsf.github.io) de que un documento se actualizo.
#
# Se llama DESPUES del commit/push propio de cada script, una vez que el
# archivo ya esta publicado (en CicSFE_GitHub, en el propio ces-bcsf.github.io,
# o donde corresponda). Esta funcion:
#   1. Sincroniza el repo del sitio (ces-bcsf.github.io)
#   2. Actualiza (o agrega) el item dentro de la categoria correspondiente
#   3. Actualiza (o agrega) la entrada en el feed "updates" de la portada
#   4. Commitea y pushea SOLO site-content.json
#
# Categorias validas (deben existir en site-content.json):
#   "informes", "paneles", "graficas", "calendario", "documentos"

# Helper: usa el valor de la izquierda si no es NULL, si no el de la derecha
`%||%` <- function(a, b) if (base::is.null(a)) b else a

actualizar_site_content <- function(categoria,
                                     titulo,
                                     item_url,
                                     descripcion = NULL,
                                     icono = NULL,
                                     repo_site = "C:/_Sitio_GitHub/Github_main/ces-bcsf.github.io",
                                     fecha = base::Sys.time(),
                                     push = TRUE) {

  json_relativo <- "assets/data/site-content.json"
  json_path <- base::file.path(repo_site, json_relativo)

  # 1. SINCRONIZA EL REPO DEL SITIO (otros scripts pueden haber pusheado antes)
  gert::git_pull(repo = repo_site, remote = "origin", refspec = "main")

  # 2. LEE EL JSON ACTUAL
  data <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)

  if (base::is.null(data$categories[[categoria]])) {
    base::stop(base::paste0(
      "Categoria '", categoria, "' no existe en site-content.json. ",
      "Categorias validas: ", base::paste(base::names(data$categories), collapse = ", ")
    ))
  }

  fecha_iso <- base::format(fecha, "%Y-%m-%dT%H:%M:%S-03:00")

  # 3. ACTUALIZA (O AGREGA) EL ITEM DENTRO DE LA CATEGORIA
  items <- data$categories[[categoria]]$items
  idx <- base::which(base::sapply(items, function(x) x$title) == titulo)

  item_existente <- if (base::length(idx) == 1) items[[idx]] else NULL

  nuevo_item <- base::list(
    title = titulo,
    description = descripcion %||% item_existente$description,
    url = item_url,
    icon = icono %||% item_existente$icon,
    updated = fecha_iso
  )
  nuevo_item <- nuevo_item[!base::sapply(nuevo_item, base::is.null)]

  if (base::length(idx) == 1) {
    items[[idx]] <- nuevo_item
  } else {
    items[[base::length(items) + 1]] <- nuevo_item
  }
  data$categories[[categoria]]$items <- items

  # 4. ACTUALIZA (O AGREGA) LA ENTRADA EN EL FEED "updates" DE LA PORTADA
  updates <- data$updates
  idx_u <- base::which(base::sapply(updates, function(x) x$title) == titulo)

  nuevo_update <- base::list(
    title = titulo,
    description = nuevo_item$description,
    url = item_url,
    updated = fecha_iso
  )
  nuevo_update <- nuevo_update[!base::sapply(nuevo_update, base::is.null)]

  if (base::length(idx_u) == 1) {
    updates[[idx_u]] <- nuevo_update
  } else {
    updates[[base::length(updates) + 1]] <- nuevo_update
  }
  data$updates <- updates

  # 5. ESCRIBE EL JSON (pretty, sin simplificar listas de un elemento a escalares)
  jsonlite::write_json(data, json_path, auto_unbox = TRUE, pretty = TRUE)

  # 6. COMMIT (Y PUSH, si push = TRUE) -- solo el JSON, para no pisar cambios de otros scripts
  gert::git_add(json_relativo, repo = repo_site)
  gert::git_commit(
    base::paste0("Actualiza site-content.json | ", titulo, " | ", base::Sys.time()),
    repo = repo_site
  )

  if (push) {
    gert::git_push(remote = "origin", repo = repo_site)
    base::message(base::paste0("\033[34msite-content.json actualizado y pusheado (", categoria, " > ", titulo, ") ✔\033[39m\n"))
  } else {
    base::message(base::paste0("\033[33msite-content.json actualizado y COMMITEADO LOCAL (sin pushear) (", categoria, " > ", titulo, ") -- revisa con git_log()/git_diff() antes de pushear a mano\033[39m\n"))
  }
}
