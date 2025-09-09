# Instalar {gert} si no está disponible
if (!requireNamespace("gert", quietly = TRUE)) {
  install.packages("gert")
}

library(gert)

# Mensaje dinámico de commit (incluye fecha y hora)
commit_message <- paste("Auto-update:", Sys.time())

tryCatch({
  
  # 0. muestra en consola el estado del repositorio
  git_status()
  
  # 1. Agregar todos los cambios
  git_add(".")
  
  # 2. Hacer commit
  git_commit(commit_message)
  
  # 3. Subir cambios al remoto (rama main)
  git_push(remote = "origin", refspec = "main")
  
  message("✅ Cambios subidos con éxito: ", commit_message)
  
}, error = function(e) {
  message("❌ Error al subir: ", e$message)
})
