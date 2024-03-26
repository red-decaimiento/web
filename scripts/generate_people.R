library(tidyverse)
library(here)


# Read files 
person <- read_csv("/Users/ajpelu/Downloads/db_redec_v1_dicc_person.csv")
institution <- read_csv("/Users/ajpelu/Downloads/db_redec_v1_dicc_institution.csv")

person <- person |> inner_join(institution, by="institution_code")


# Auxiliary functions 
generate_links <- function(x) { 
  
  link_text <- glue::glue(
    '
    links:
      - icon: envelope
        text: Email
        href: {x$person_mail}
  '
  )
  
  # ORCID 
  if (!is.na(x$orcid)) { 
    output <- glue::glue(
      '{link_text}
          - text: "{glue::glue("{{< ai orcid >}} ORCID", .open = "{{{{", .close = "}}}}")}"
            href: {paste0("https://orcid.org/", x$orcid)}
      ')
  } else {
    output <- link_text
  }
  
  # URL 
  if (!is.na(x$person_url)) { 
    output <- glue::glue(
      '{output}
          - text: "{glue::glue("{{< fa globe >}} Personal web", .open = "{{{{", .close = "}}}}")}"
            href: {x$person_url}
      ')
  }
  return(output)
}

get_area_tematica <- function(data){
  area_tematica_vars <- grep("^area_tematica", names(data), value = TRUE)
  # Extract values for variables starting with "area-tematica" if they contain data
  values <- unlist(data[area_tematica_vars])
  values <- values[!is.na(values)]
  output <- glue_collapse(values, sep = ", ")
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
  research_group_vars <- grep("^research_group", names(data), value = TRUE)
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


### Lop to create QMDs 
mypath <- here::here("people")

for (i in 1:nrow(person)) {
  
  x <- person[i,]
  
  target_path <- paste0(mypath, "/", name_dir(x$name_last, x$name_first))
                   
  # create dir     
  if ( !(dir.exists(paths = target_path))) {
  dir.create(path = target_path)
  }
  
  # To create log file the first time
  file.create(paste0(target_path, "/index.qmd"), overwrite = TRUE)
  
  texto_yml <- glue::glue(
    '
  ---
  title: \"{paste(x$name_first,x$name_last, sep=" ")}\"
  email: \"{x$person_mail}\"  
  image: {paste0("avatar_", name_dir(x$name_last, x$name_first), ".jpg")}
  page-layout: full
  about: 
    id: person-profile
    template: trestles
    image-shape: round
    image: {paste0("avatar_", name_dir(x$name_last, x$name_first), ".jpg")}
  {generate_links(x)}
  categories: [{get_area_tematica(x)}]
  ---
 
  <hr>

  :::{glue::glue("{#person-profile}", .open = "{{", .close = "}}")}

  ## Grupo de Investigación
  {get_research_group(x)}
  
  ## Institución
  - [{x$institution_name}]({x$institution_url})
  
  :::
  ' 
  )

  write(texto_yml, 
        paste0(target_path, "/index.qmd"),
        append = TRUE)
  
}





## Custom loop to add images 
for (i in 1:nrow(person)) {
  
  x <- person[i,] 
  
  target_path <- paste0(mypath, "/", name_dir(x$name_last, x$name_first)) 
  
  if (length(list.files(path = target_path, pattern = ".jpg")) < 1) {
  
  file.copy(from = paste0(mypath, "/avatar.jpg"), 
            to = paste0(target_path, "/avatar_", name_dir(x$name_last, x$name_first), ".jpg"),
            overwrite = TRUE)
  }
}






  
  
fff <- list.files(here::here("people"), 
           pattern = "*.qmd", recursive = TRUE, full.names = TRUE)

lapply(fff, unlink)







