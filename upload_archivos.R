
# ESTE SCRIPT SINCRONIZA TODOS LOS CAMBIOS INTRODUCIDOS EN: 
# C:\mysyncfolders\bcsf.com.ar\BCSF - Grupo CES - Documentos\CicSFE_sp\_Reportes rmd\_Github_out\ces-bcsf.github.io\
# CON SU ESPEJO EN LA NUBE EN EL GITHUG DEL CES

# PARA PODER CORRER ESTE SCIPT HAY QUE TENER INSTALADO GIT

# ESTABLECE EL DIRECTORIO DE TRABAJO
# ESTO FUNCIONA YA QUE ESTE SCRIPT SERA EJECUTADO CON "SOURCE" DESDE LA CARPETA DONDE ESTAN LOS ARCHIVOS A SUBIR
base::setwd("C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/_Github_out/ces-bcsf.github.io")

commit_message <- svDialogs::dlg_input(
  "Ingresa tu mensaje de commit:",
  "Auto-update: Datos actualizados"
)$res

# SI EL USUARIO NO INGRESA NADA O CANCELA, SE USA UN MENSAJE POR DEFECTO
if (base::is.null(commit_message) || base::nchar(commit_message) == 0) {
  commit_message <- base::paste("Auto-update:", base::Sys.time())
  base::message("No se ingresó un mensaje. Usando el mensaje predeterminado: ", commit_message)
}

base::tryCatch({ 
   gert::git_status() 
   gert::git_add(".")
   gert::git_commit(commit_message)
   gert::git_branch_list()
   gert::git_push(remote = "origin") # LA NUBE
   base::message("Cambios subidos con Exito: ", commit_message)
  }, error = function(e) {
  base::message("Error al subir: ", e$message)
})
