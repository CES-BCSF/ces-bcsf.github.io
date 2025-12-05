# CONFIGURACIÓN ####
# ---;---;---;---;---;---;---;---;---;---;---;---;---;---;---;

# Carpeta origen (carpeta Paneles actual)
origen <- "../../global_mensual"

# Carpeta destino (cambiar por la ruta final donde quieras copiar)
destino <- "C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/_Github_out/ces-bcsf.github.io/web_ces/Paneles"   # <-- EDITAR AQUÍ

# 1. COPIAR LA CARPETA COMPLETA ####
# ---;---;---;---;---;---;---;---;---;---;---;---;---;---;---;

file.copy(
  from = origen,
  to   = destino,
  recursive = TRUE
)

base::Sys.sleep(0.7)

message("Copia completa realizada en: ", destino)

# 2. DEFINIR ARCHIVOS A ELIMINAR EN LA COPIA ####
# ---;---;---;---;---;---;---;---;---;---;---;---;---;---;---;

archivos_a_borrar <- c(
  file.path(destino, "global_mensual" , "correr_y_actualizar.lnk"),
  file.path(destino, "global_mensual" , "templates", "Paneles_globales.R"),
  file.path(destino, "global_mensual" , "templates", "salidas.r"),
  file.path(destino, "global_mensual" , "templates", "injects.r"),
  file.path(destino, "global_mensual" , "templates", "subir_paneles.R")
)

# 3. ELIMINAR ARCHIVOS SI EXISTEN ####
# ---;---;---;---;---;---;---;---;---;---;---;---;---;---;---;

message("Eliminando archivos innecesarios...")

for (archivo in archivos_a_borrar) {
  if (file.exists(archivo)) {
    file.remove(archivo)
    message("Eliminado: ", archivo)
  } else {
    message("No encontrado: ", archivo)
  }
  base::Sys.sleep(0.5)
}

message("Limpieza completada.")

# 4. SCRIPT PARA SUBIR EL ARCHIVO A GITHUB EN LA NUBE ####
# ---;---;---;---;---;---;---;---;---;---;---;---;---;---;---;

## CAMBIO LA UBICACION BASE DE R PARA EJECUTAR DESDE LA CARPETA DE GITHUB 
path_viejo <- getwd()
base::setwd("C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/_Github_out/ces-bcsf.github.io")

# PIDE AL USUARIO UN MENSAJE DE COMMIT PARA SUBIR A GITHUB
commit_message <- base::paste0("Auto-update: PANELES GLOBALES MENSUALES actualizado al ", base::Sys.time())

# EJECUTA EL COMANDO PARA SUBIR A GITHUB (EL TRYCATCH ES PARA QUE NO SE CIERRE R STUDIO SI HAY ERROR)
base::tryCatch({ 
  gert::git_fetch(remote = "origin")
  gert::git_pull(repo = ".", remote = "origin", refspec = "main")
  gert::git_status() 
  gert::git_add("/web_ces/Paneles/global_mensual")
  gert::git_commit(commit_message)
  #gert::git_branch_list()
  gert::git_push(remote = "origin") # LA NUBE
  base::message("Cambios en PANELES GLOBALES MENSUALES subidos con Exito: ", commit_message)
}, error = function(e) {
  base::message("Error al subir: ", e$message)
})

base::setwd(path_viejo)