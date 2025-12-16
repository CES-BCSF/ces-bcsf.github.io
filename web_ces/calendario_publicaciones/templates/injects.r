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
  template_path = "calendario_template.html",
  fragment_path = "../tabla.html",
  placeholder = "<!-- inject:tabla -->",
  output_path = "../calendario.html"
)

inject_fragment(
  template_path = "../calendario.html",
  fragment_path = "update_date.html",
  placeholder = "<!-- inject:update_date -->",
  output_path = "../calendario.html"
)

inject_fragment(
  template_path = "../calendario.html",
  fragment_path = "../filtros.html",
  placeholder = "<!-- inject:filtros -->",
  output_path = "../calendario.html"
)


