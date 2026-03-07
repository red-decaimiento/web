
#### Funcion para extraer 
extract_info_users <- function(file_path) {
  content <- readLines(file_path, warn = FALSE)
  
  # title → nombre
  name_line <- grep("^title:", content, value = TRUE)
  name <- if (length(name_line) > 0)
    gsub('^title:\\s*"(.*)"$', "\\1", name_line)
  else NA
  
  # email
  email_line <- grep("^email:", content, value = TRUE)
  email <- if (length(email_line) > 0)
    gsub('^email:\\s*"(.*)"$', "\\1", email_line)
  else NA
  
  # Encontrar la línea después de "## Grupo de Investigación"
  group_index <- grep("## Grupo de Investigación", content)
  department <- if (length(group_index) > 0 && length(content) > group_index) {
    gsub("^\\s*-\\s*", "", content[group_index + 1])  # Limpia el guion y espacios
  } else {
    NA
  }
  
  # Encontrar la línea después de "## Institución"
  institution_index <- grep("## Institución", content)
  institution <- if (length(institution_index) > 0 && length(content) > institution_index) {
    gsub("^\\s*-\\s*", "", content[institution_index + 1])  # Limpia el guion y espacios
  } else {
    NA
  }
  
  # person_type
  person_type_line <- grep("^person_type:", content, value = TRUE)
  person_type <- if (length(person_type_line) > 0)
    gsub("^person_type:\\s*'?(.+?)'?$", "\\1", person_type_line)
  else NA
  
  
  # categories: [A, B, C]
  cat_line <- grep("^categories:", content, value = TRUE)
  categories <- if (length(cat_line) > 0) {
    x <- sub("^categories:\\s*\\[", "", cat_line)
    x <- sub("\\]\\s*$", "", x)
    paste(trimws(strsplit(x, ",")[[1]]), collapse = "; ")
  } else NA
  
  # ORCID (cualquier línea con orcid.org)
  orcid_line <- grep("orcid\\.org/", content, value = TRUE)
  orcid <- if (length(orcid_line) > 0)
    sub(".*(https?://orcid\\.org/[^ ]+).*", "\\1", orcid_line[1])
  else NA
  
  
  data.frame(
    nombre = name,
    email = email,
    orcid = orcid,
    categories = categories,
    type = person_type,
    grupo = department,
    institucion = institution,
    stringsAsFactors = FALSE
  )
}

# Función para procesar todos los archivos .qmd en una carpeta
extract_from_folder <- function(folder_path) {
  # Buscar todos los archivos .qmd
  files <- list.files(folder_path, pattern = "\\.qmd$", recursive = TRUE, full.names = TRUE)
  
  # Aplicar extract_info a cada archivo
  results <- lapply(files, extract_info_users)
  
  # Combinar los resultados en un único data frame
  do.call(rbind, results)
}

# --- EJECUCIÓN ---

# Ruta de la carpeta con los archivos .qmd
folder_path <- "people/"

# Extraer la información
d <- extract_from_folder(folder_path)


# Limpiar los datos
# eliminar " y la palabra email
d_clean <- d |> 
  mutate(email = gsub('^email:\\s*"', '', email)) |> 
  mutate(email = gsub('"', '', email)) |> 
  mutate(grupo = gsub('\\[- ', '', grupo)) |> 
  mutate(institucion = gsub("\\s*\\(.*?\\)", "", institucion)) |> 
  mutate(institucion = str_remove(institucion, "\\[")) |>
  mutate(institucion = str_remove(institucion, "\\]")) |> 
  mutate(grupo = gsub("\\s*\\(https[^)]*\\)", "", grupo)) |> 
  mutate(grupo = str_remove(grupo, "\\[")) |>
  mutate(grupo = str_remove(grupo, "\\]")) |> 
  mutate(
    FirstName = if_else(
      is.na(nombre),
      NA_character_,
      word(nombre, 1)
    ),
    LastName = if_else(
      is.na(nombre),
      NA_character_,
      word(nombre, 2, -1)
    )
  )

write_csv(d_clean, "/Users/ajpelu/Desktop/people.csv")
