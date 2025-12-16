### CALENDARIO
### CERRAR EL EXCEL MANUAL PARA QUE PUEDA CORRERSE ESTE ARCHIVO ###

#RENDER DEL HTML ####
rmarkdown::render("C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/Calendario/calendario_1.1.1.Rmd",
                  output_file = "C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/Calendario/Calendario.html",
                  output_dir = "C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/Calendario/")

# VARIABLES ####
archivo <- "Calendario.html"
path_actual <- getwd()
carpeta_origen <- "C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/Calendario"
carpeta_destino <- "C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/_Github_out/ces-bcsf.github.io/web_ces/calendario_publicaciones"

# PREPARA DIRECCIONES | ARCHIVOS DE LA CARPETA LOCAL (_Reportes rmd) Y CARPETA LOCAL GITHUB (_Github_out) ####
origen <- base::file.path(carpeta_origen, archivo)
destino <- base::file.path(carpeta_destino, archivo)

# COPIA EL HTML DESDE _Reportes rmd A _Github_out
control_copia=base::file.copy(from = origen, to = destino, overwrite = TRUE)

# ESPERA 1 SEGUNDO PARA QUE TERMINE DE GUARDAR EL ARCHIVO
base::Sys.sleep(0.7)

# SCRIPT PARA SUBIR EL ARCHIVO A GITHUB EN LA NUBE ####
## CAMBIO LA UBICACION BASE DE R PARA EJECUTAR DESDE LA CARPETA DE GITHUB 
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
  gert::git_add("/web_ces/calendario_publicaciones/Calendario.html")
  gert::git_commit(commit_message)
  #gert::git_branch_list()
  gert::git_push(remote = "origin") # LA NUBE
  base::message("Cambios en calendario subidos con Exito: ", commit_message)
}, error = function(e) {
  base::message("Error al subir: ", e$message)
})

