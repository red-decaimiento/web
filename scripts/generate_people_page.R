## Funciones para generar una página de los nuevos integrantes
library(tidyverse)
library(here)
library(glue)
library(stringi)
library(janitor)


### Flujo de trabajo 
## 1. Descargar los datos de Fillout 
## 2. Cargar las funciones 
## 3. Establecer el directorio de trabajo (de la web)
## 4. Aplica la función a cada respuesta del fillout


################################################################################
## 1. Descargar los datos de Fillout -------------------------------------------
file <- "/Users/ajpelu/Downloads/Fillout Solicitud Inscripción ReDeC results (2).csv"
respuestas_fillout <- read_csv(file = file) |> janitor::clean_names()

## filter by date 
respuestas_fillout<- respuestas_fillout |>
  mutate(last_updated2 = as.POSIXct(strptime(last_updated, "%a %b %d %Y %H:%M:%S", tz = "GMT"))) |> 
  filter(last_updated2 >= as.POSIXct("2025-10-04", tz = "GMT"))

respuestas_fillout |> unique()


## 2 Cargar las funciones ------------------------------------------------------
generate_links <- function(x) { 
  
  link_text <- glue::glue(
    '
    links:
      - icon: envelope
        text: Email
        href: mailto:{x$email}
  '
  )
  
  # ORCID 
  if (!is.na(x$identificador_orcid)) { 
    output <- glue::glue(
      '{link_text}
          - text: "{glue::glue("{{< ai orcid >}} ORCID", .open = "{{{{", .close = "}}}}")}"
            href: {paste0("https://orcid.org/", x$identificador_orcid)}
      ')
  } else {
    output <- link_text
  }
  
  # URL 
  if (!is.na(x$pagina_web_personal)) { 
    output <- glue::glue(
      '{output}
          - text: "{glue::glue("{{< fa globe >}} Personal web", .open = "{{{{", .close = "}}}}")}"
            href: {x$pagina_web_personal}
      ')
  }
  return(output)
}

get_area_tematica <- function(data){
  area_tematica_vars <- grep("*tematicos", names(data), value = TRUE)
  # Extract values for variables starting with "area-tematica" if they contain data
  values <- unlist(data[area_tematica_vars])
  values <- values[!is.na(values)]
  output <- glue_collapse(values, sep = ", ")
  output <- gsub(";", ",", output) # change ; by , 
  output <- gsub(",(\\S)", ", \\1", output)  # Asegurar un espacio después de cada coma
  return(output)
}

# Function to get name for directory 
name_dir <- function(name_last, name_first){
  
  # remove acent & convert tolower & remove "-" in the last name 
  aux_last <- name_last |> 
    stringi::stri_trans_general("latin-ascii") |> 
    tolower() |> 
    gsub(pattern = "-", replacement = " ", .data)
  
  n_words_last <- str_count(aux_last, "\\S+")
  
  if (n_words_last > 1) {
    output <- gsub("\\s+", "_", aux_last)
  } else {
    aux_first <- name_first |> 
      stringi::stri_trans_general("latin-ascii") |> 
      tolower() |> 
      str_extract("\\S+") # ensure the first word
    
    output <- paste(aux_last, aux_first, sep = "_")
  }
  return(output)
}

get_research_group <- function(data){
  research_group_vars <- grep("^departamento_grupo_s_de_investigacion_a_los_que_pertenece", names(data), value = TRUE)
  # Extract values for variables starting with "research_group" if they contain data
  values <- unlist(data[research_group_vars])
  values <- values[!is.na(values)]
  if (length(values) > 0) { 
    values <- glue("- {values}")
    output <- glue_collapse(values, sep = "\n")
  } else {
    output <- ""
  }
  return(output)
}

# function to create personal page
generate_personal_page <- function(x){
  
  # genera target path
  target_path <- paste0(mypath, "/", name_dir(x$apellidos, x$nombre))
  
  # create dir     
  if ( !(dir.exists(paths = target_path))) {
    dir.create(path = target_path)
  }
  
  # To create index file the first time
  file.create(paste0(target_path, "/index.qmd"), overwrite = TRUE)
  
  
  texto_yml <- glue::glue(
    '
  ---
  title: \"{paste(x$nombre,x$apellidos, sep=" ")}\"
  email: \"{x$email}\"  
  namesort: \"{name_dir(x$apellidos, x$nombre)}\"
  image: {paste0("avatar_", name_dir(x$apellidos, x$nombre), ".jpg")}
  page-layout: full
  about: 
    id: person-profile
    template: trestles
    image-shape: round
    image: {paste0("avatar_", name_dir(x$apellidos, x$nombre), ".jpg")}
  {generate_links(x)}
  categories: [{get_area_tematica(x)}]
  person_type: \'investigadores\'
  ---
 
  <hr>

  :::{glue::glue("{#person-profile}", .open = "{{", .close = "}}")}

  ## Grupo de Investigación
  [{get_research_group(x)}]({x$pagina_web_del_grupo_de_investigacion})
  
  ## Institución
  - {x$institucion_a_la_que_pertenece}
  
  :::
  ' 
  )
  
  write(texto_yml, 
        paste0(target_path, "/index.qmd"),
        append = TRUE)
  
  # si hay foto      
  if (!is.na(x$foto)) { 
    download.file(x$foto, destfile = paste0(target_path, "/avatar_", name_dir(x$apellidos, x$nombre), ".jpg"))
  } else { 
    file.copy(from = paste0(mypath, "/avatar.jpg"), 
              to = paste0(target_path, "/avatar_", name_dir(x$apellidos, x$nombre), ".jpg"),
              overwrite = TRUE)
      }
  
  cat(paste0("Yeahhhhh. Página web creada en /people/", 
               name_dir(x$apellidos, x$nombre), 
               "\n", "Por favor revisa los datos de gruopo de investigación, y de institución"))
}

## 3 Establecer el directorio de trabajo (de la web) ---------------------------
mypath <- here::here("people")

## 4. Aplica la función a cada respuesta del fillout ---------------------------
# se puede aplicar un purr pero prefiero hacerlo uno a uno para revisar 
generate_personal_page(respuestas_fillout[1,])


## 5. Siguientes pasos ---------------------------------------------------------
# - Revisar institucion (nombre, url)
# - Revisar grupo de investigación (hacer un listado de grupos)
################################################################################