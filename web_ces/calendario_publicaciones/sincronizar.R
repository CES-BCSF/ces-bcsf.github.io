path_1 <- base::getwd() 

base::setwd("C:/mysyncfolders/bcsf.com.ar/BCSF - Grupo CES - Documentos/CicSFE_sp/_Reportes rmd/_Github_out/ces-bcsf.github.io")

# PIDE AL USUARIO UN MENSAJE DE COMMIT PARA SUBIR A GITHUB
commit_message <- base::paste0("Sincronizacion de repositorio ", base::Sys.time())

# EJECUTA EL COMANDO PARA SUBIR A GITHUB (EL TRYCATCH ES PARA QUE NO SE CIERRE R STUDIO SI HAY ERROR)
base::tryCatch({ 
  gert::git_status() 
  gert::git_add(".")
  gert::git_commit(commit_message)
  gert::git_push(
    remote = "origin", 
    refspec = "refs/heads/main", 
    force = TRUE
  ) # LA NUBE
  base::message("Sincronizacion exitosa: ", commit_message)
}, error = function(e) {
  base::message("Error al subir: ", e$message)
})

# VUELVO A LA CARPETA ORIGINAL
base::setwd(path_1)