inject_fragment <- function(template_path, fragment_path, placeholder, output_path) {
  
  html <- readLines(template_path, warn = FALSE)
  frag <- readLines(fragment_path, warn = FALSE)
  
  html <- gsub(
    placeholder,
    paste(frag, collapse = "\n"),
    html,
    fixed = TRUE
  )
  
  writeLines(html, output_path)
}

inject_fragment(
  template_path = "paneles_template.html",
  fragment_path = "fragments/update_date.html",
  placeholder = "<!-- inject:update_date -->",
  output_path = "../paneles.html"
)

inject_fragment(
  template_path = "../paneles.html", #A PARTIR DEL SEGUNDO LLAMADO YA TIENE LOS PLACEHOLDERS ASI QUE PARA NO SOBREESCRIBIR LOS QUE ESTAN REEMPLAZADOS, SE LLAMA AL TEMPLATE FINAL.
  fragment_path = "datatable_panel.html",
  placeholder = "<!-- inject:datatable_panel -->",
  output_path = "../paneles.html"
)

inject_fragment(
  template_path = "../paneles.html", #A PARTIR DEL SEGUNDO LLAMADO YA TIENE LOS PLACEHOLDERS ASI QUE PARA NO SOBREESCRIBIR LOS QUE ESTAN REEMPLAZADOS, SE LLAMA AL TEMPLATE FINAL.
  fragment_path = "datatable_panel_ia.html",
  placeholder = "<!-- inject:datatable_panel_ia -->",
  output_path = "../paneles.html"
)

inject_fragment(
  template_path = "../paneles.html",
  fragment_path = "fragments/glosario_indicadores.html",
  placeholder = "<!-- inject:glosario_indicadores -->",
  output_path = "../paneles.html"
)
# inject_fragment_dt <- function(template_path, fragment_path, placeholder, output_path) {
#   
#   html <- readLines(template_path, warn = FALSE)
#   frag <- readLines(fragment_path, warn = FALSE)
#   
#   html <- gsub(
#     placeholder,
#     paste(frag, collapse = "\n"),
#     html,
#     fixed = TRUE
#   )
#   
#   writeLines(html, output_path)
# }


