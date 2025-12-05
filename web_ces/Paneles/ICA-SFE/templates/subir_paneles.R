# CONFIGURACIÓN ####
# ---;---;---;---;---;---;---;---;---;---;---;---;---;---;---;

# Carpeta origen (carpeta Paneles actual)
origen <- "../../ICA-SFE"

# Carpeta destino (cambiar por la ruta final donde quieras copiar)
destino <- "C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/_Github_out/ces-bcsf.github.io/web_ces/Paneles"

# 1. COPIAR LA CARPETA COMPLETA ####
# ---;---;---;---;---;---;---;---;---;---;---;---;---;---;---;

# Crear carpeta destino si no existe
if (!base::dir.exists(dirname(destino))) {
  base::dir.create(dirname(destino), recursive = TRUE)
}

base::file.copy(
  from = origen,
  to   = destino,
  recursive = TRUE
)

base::Sys.sleep(0.7)

message("Copia completa realizada en: ", destino)

# 2. DEFINIR ARCHIVOS A ELIMINAR EN LA COPIA ####
# ---;---;---;---;---;---;---;---;---;---;---;---;---;---;---;

archivos_a_borrar <- c(
  base::file.path(destino, "ICA-SFE" , "correr_y_actualizar.lnk"),
  base::file.path(destino, "ICA-SFE" , "templates", "Paneles_icasfe.R"),
  base::file.path(destino, "ICA-SFE" , "templates", "salidas.r"),
  base::file.path(destino, "ICA-SFE" , "templates", "injects.r"),
  base::file.path(destino, "ICA-SFE" , "templates", "subir_paneles.R")
)

carpetas_a_borrar <- c(
  base::file.path(destino, "ICA-SFE" , "input")
)

# 3. ELIMINAR ARCHIVOS SI EXISTEN ####
# ---;---;---;---;---;---;---;---;---;---;---;---;---;---;---;

message("Eliminando archivos innecesarios...")

for (archivo in archivos_a_borrar) {
  if (base::file.exists(archivo)) {
    base::file.remove(archivo)
    message("Eliminado: ", archivo)
  } else {
    message("No encontrado: ", archivo)
  }
  base::Sys.sleep(0.5)
}

for (carpeta in carpetas_a_borrar) {
  if (base::file.exists(carpeta)) { 
    base::unlink(carpeta, recursive = TRUE, force = TRUE) #FORZA PORQUE SINO QUEDA LA CARPETA VACIA
    message("Eliminado: ", carpeta)
  } else {
    message("No encontrado: ", carpeta)
  }
  base::Sys.sleep(0.5)
}

message("Limpieza completada.")

# 4. SCRIPT PARA SUBIR EL ARCHIVO A GITHUB EN LA NUBE ####
# ---;---;---;---;---;---;---;---;---;---;---;---;---;---;---;

## CAMBIO LA UBICACION BASE DE R PARA EJECUTAR DESDE LA CARPETA DE GITHUB 
path_viejo <- base::getwd()
base::setwd("C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/_Github_out/ces-bcsf.github.io")

# PIDE AL USUARIO UN MENSAJE DE COMMIT PARA SUBIR A GITHUB
commit_message <- base::paste0("Auto-update: PANELES ICA-SFE actualizados al ", base::Sys.time())

# EJECUTA EL COMANDO PARA SUBIR A GITHUB (EL TRYCATCH ES PARA QUE NO SE CIERRE R STUDIO SI HAY ERROR)
base::tryCatch({ 
  gert::git_fetch(remote = "origin")
  gert::git_pull(repo = ".", remote = "origin", refspec = "main")
  gert::git_status() 
  gert::git_add("/web_ces/Paneles/ICA-SFE")
  gert::git_commit(commit_message)
  #gert::git_branch_list()
  gert::git_push(remote = "origin") # LA NUBE
  base::message("Cambios en PANELES ICA-SFE subidos con Exito: ", commit_message)
}, error = function(e) {
  base::message("Error al subir: ", e$message)
})

base::setwd(path_viejo)