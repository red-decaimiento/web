library(tidyverse)

# Función para extraer información de un archivo .qmd
extract_info <- function(file_path) {
  # Leer el archivo completo como texto
  content <- readLines(file_path, warn = FALSE)
  
  # Extraer el título (nombre completo) y limpiar
  name_line <- grep("^title:", content, value = TRUE)
  name <- if (length(name_line) > 0) gsub('^title:\\s*"(.*)"$', "\\1", name_line) else NA
  
  # Extraer el email y limpiar
  email_line <- grep("^email:", content, value = TRUE)
  email <- if (length(email_line) > 0) gsub('^email:\\s*"(.*)"$', "\\1", email_line) else NA
  
  
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
  
  # Separar nombre y apellidos si el nombre existe
  if (!is.na(name)) {
    name_parts <- unlist(strsplit(name, " "))
    first_name <- name_parts[1]
    last_name <- if (length(name_parts) > 1) paste(name_parts[-1], collapse = " ") else NA
  } else {
    first_name <- NA
    last_name <- NA
  }
  
  # Crear y devolver un data frame con los resultados
  data.frame(
    FirstName = first_name,
    LastName = last_name,
    Email = email,
    Department = department,
    Institution = institution,
    stringsAsFactors = FALSE
  )
}

# Función para procesar todos los archivos .qmd en una carpeta
extract_from_folder <- function(folder_path) {
  # Buscar todos los archivos .qmd
  files <- list.files(folder_path, pattern = "\\.qmd$", recursive = TRUE, full.names = TRUE)
  
  # Aplicar extract_info a cada archivo
  results <- lapply(files, extract_info)
  
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
  mutate(Email = gsub('^email:\\s*"', '', Email)) |> 
  mutate(Email = gsub('"', '', Email)) |> 
  separate(Department, into = c("Department", "ResearchGroup"), sep = "\\]\\(") |> 
  mutate(ResearchGroup = str_remove_all(ResearchGroup, "\\)")) |> 
  mutate(ResearchGroup = str_remove_all(ResearchGroup, "\\[")) |> 
  mutate(Department = str_trim(Department)) |> 
  dplyr::select(-ResearchGroup) |> 
  # eliminar "[- " en Department
  mutate(Department = str_remove(Department, "\\[- ")) |> 
  # eliminar "[" en Department
  mutate(Department = str_remove(Department, "\\[")) |> 
  # lo mismo en institution
  separate(Institution, into = c("Institution", "Institutionurl"), sep = "\\]\\(") |> 
  dplyr::select(-Institutionurl) |> 
  mutate(Institution = str_remove(Institution, "\\[- ")) |>
  mutate(Institution = str_remove(Institution, "\\[")) |> 
  mutate(FirstName = paste0(str_sub(FirstName, 1, 1), ".")) |> 
  dplyr::select(-Department) |> 
  mutate(Institution = str_replace(Institution, "Consejo Superior de Investigaciones Científicas", "CSIC")) |> 
  mutate(Institution = str_replace(Institution, "Instituto Nacional de Investigación y Tecnología Agraria y Alimentaria", "INIA")) |> 
  mutate(Institution = str_replace(Institution, "Centro de Investigación Ecológica y Aplicaciones Forestales", "CREAF")) 


write_csv(d_clean, "/Users/ajpelu/Desktop/people.csv")
  
  





  

p <- read_csv("/Users/ajpelu/Desktop/Desktop/people.csv")
g <- p |> 
  unite("aut", c("FirstName", "LastName"), sep = " ", remove = FALSE) |> 
  unite("autn", c("aut", "Number"), sep = " ", remove = FALSE) |> 
  unite("instn", c("Number","Institution", ), sep = " ", remove = FALSE) |> 
  arrange(LastName) 



### Institution
d |> 
  dplyr::select(Institution) |> 
  distinct()



instituciones_limpias <- d |> 
  mutate(
    inst = Institution |> 
      str_remove_all("\\[|\\]|\\(.*?\\)") |>  # quitar links y paréntesis
      str_trim() |> 
      str_to_lower()
  ) |> 
  distinct(inst)

nrow(instituciones_limpias)






  
