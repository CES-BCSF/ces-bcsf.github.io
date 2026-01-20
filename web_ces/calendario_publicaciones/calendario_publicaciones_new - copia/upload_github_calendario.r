### CALENDARIO ## CERRAR EL EXCEL MANUAL PARA QUE PUEDA CORRERSE ESTE ARCHIVO ##

# ESTABLECER LA RAIZ DE TRABAJO ####
path_1 <- base::getwd()
base::setwd("C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/calendario_publicaciones/calendario_publicaciones_new")

# ACTUALIZAR LA TABLA Y EL HTML ####
base::source("templates/salida_fragmento.r") #USAR SIEMPRE PATH RELATIVOS AL PATH RAIZ

# VARIABLES ####
carpeta_origen <- "C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/calendario_publicaciones/calendario_publicaciones_new"
carpeta_destino <- "C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/_Github_out/ces-bcsf.github.io/web_ces"

# COPIA EL HTML DESDE _Reportes rmd A _Github_out
base::file.copy(
  from = carpeta_origen,
  to   = carpeta_destino,
  recursive = TRUE,
  overwrite = TRUE
)

# ESPERA 1 SEGUNDO PARA QUE TERMINE DE GUARDAR EL ARCHIVO
base::Sys.sleep(0.7)

message("Copia completa realizada en: ", destino)

# ELIMINAMOS ARCHIVOS INNECESARIOS DE LA CARPETA DE GITHUB ANTES DE SUBIR ####

archivos_a_borrar <- c(
  base::file.path(carpeta_destino, "upload_github_calendario.r"),
  base::file.path(carpeta_destino, "Ejecución de Calendario.txt"),
  base::file.path(carpeta_destino, "ejecutar_Actualizar-calendario.bat"),
  base::file.path(base::paste0(carpeta_destino,"/templates"), "salida_fragmento.r"),
)

base::for (archivo in archivos_a_borrar) {
  if(base::file.exists(archivo)){
    base::unlink(archivo, recursive = TRUE, force = TRUE)
    message("Eliminado: ", archivo)
  }else{
    message("No encontrado: ", archivo)
  }
  base::Sys.sleep(0.5)
}

message("Limpieza completada.")

# SCRIPT PARA SUBIR EL ARCHIVO A GITHUB EN LA NUBE ####
## CAMBIO LA UBICACION BASE DE R PARA EJECUTAR DESDE LA CARPETA DE GITHUB 
# path_2 <- base::getwd()
base::setwd("C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/_Github_out/ces-bcsf.github.io")

# PIDE AL USUARIO UN MENSAJE DE COMMIT PARA SUBIR A GITHUB
commit_message <- svDialogs::dlg_input(
  "Ingresa tu mensaje de commit:",
  base::paste0("Auto-update: CALENDARIO con datos actualizados al ", base::Sys.time())
)$res

#" SI EL USUARIO NO INGRESA NADA O CANCELA, SE USA UN MENSAJE POR DEFECTO
if (base::is.null(commit_message) || base::nchar(commit_message) == 0) {
  commit_message <- base::paste("Auto-update:", base::Sys.time())
  base::message("No se ingresC3 un mensaje. Usando el mensaje predeterminado: ", commit_message)
}

# EJECUTA EL COMANDO PARA SUBIR A GITHUB (EL TRYCATCH ES PARA QUE NO SE CIERRE R STUDIO SI HAY ERROR)
base::tryCatch({ 
  #gert::git_fetch(remote = "origin")
  gert::git_pull(repo = ".", remote = "origin", refspec = "main")
  gert::git_status() 
  gert::git_add("/web_ces/calendario_publicaciones")
  gert::git_commit(commit_message)
  #gert::git_branch_list()
  gert::git_push(remote = "origin") # LA NUBE
  base::message("Cambios en calendario subidos con Exito: ", commit_message)
}, error = function(e) {
  base::message("Error al subir: ", e$message)
})

# VUELVO A LA CARPETA ORIGINAL
base::setwd(path_1)
