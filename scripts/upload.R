
# ESTE SCRIPT SINCRONIZA TODOS LOS CAMBIOS INTRODUCIDOS EN: 
# C:\mysyncfolders\bcsf.com.ar\BCSF - Grupo CES - Documentos\CicSFE_sp\_Reportes rmd\_Github_out\ces-bcsf.github.io\
# CON SU ESPEJO EN LA NUBE EN EL GITHUG DEL CES

commit_message <- base::paste("Auto-update:", base::Sys.time()) # PARA NOMBRAR EL CAMBIO

base::tryCatch({ 
   gert::git_status() 
   gert::git_add(".")
   gert::git_commit(commit_message)
   gert::git_branch_list()
   gert::git_push(remote = "origin") # LA NUBE
   base::message("✅ Cambios subidos con éxito: ", commit_message)
  }, error = function(e) {
  base::message("❌ Error al subir: ", e$message)
})