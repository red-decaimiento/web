#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| echo: false
#| message: false
#| warning: false
library(tidyverse)
library(knitr)
library(gt)


raw <- readxl::read_excel("horario.xlsx")


d  <- raw |> 
    rename(
        Hora = time, 
        Mesa = mesa,
        Coordina = coordinacion) |>
    mutate(
        Coordina = str_replace_all(Coordina,
    c("Francisco Lloret" = "[Francisco Lloret](../../people/lloret_francisco)", 
    "Paloma Ruiz-Benito" = "[Paloma Ruiz-Benito](../../people/ruiz_benito)",
    "Enrique Andivia" = "[Enrique Andivia](../../people/andivia_enrique)", 
    "Raúl Sánchez-Salguero" = "[Raúl Sánchez-Salguero](../../people/sanchez_salguero)",
    "Antonio J. Pérez-Luque" = "[Antonio J. Pérez-Luque](../../people/perez_luque)",
     "Antonio Gazol" = "[Antonio Gazol](../../people/gazol_burgos)",
     "Lorena Gómez-Aparicio" = "[Lorena Gómez-Aparicio](../../people/gomez_aparicio)",
     "Jorge Curiel" = "[Jorge Curiel](../../people/curiel_jorge)",
     "Jordi Martínez-Vilalta" = "[Jordi Martínez-Vilalta](../../people/martinez_vilalta)",
     "Rafael M. Navarro Cerrillo" = "[Rafael M. Navarro Cerrillo](../../people/navarro_cerrillo)", 
     "Miguel A. de Zavala" = "[Miguel A. de Zavala](../../people/zavala_miguel)" 
    ))
    ) 
#
#
#
#| echo: false

d17 <- d |> 
    filter(day == 17) |> 
    dplyr::select(-jornada, -day) 

gt::gt(d17) |> 
  fmt_markdown(colum = Coordina) |>
  tab_style(
    style = cell_fill(color = "#dfece9"),
    locations = cells_body(
      rows = type == "break")
  ) |> 
  cols_hide(columns = type) |> 
  cols_width(
    "Hora" ~ pct(10),
    "Mesa" ~ pct(40),
    "Coordina" ~ pct(50)
  ) |> 
  sub_missing(
    columns = 3,
    missing_text = ""
  ) |> 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) |> 
  as_raw_html()
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| echo: false

d18 <- d |> 
    filter(day == 18) |> 
    dplyr::select(-jornada, -day) 

gt::gt(d18) |> 
  fmt_markdown(colum = Coordina) |>
  tab_style(
    style = cell_fill(color = "#dfece9"),
    locations = cells_body(
      rows = type == "break")
  ) |> 
  cols_hide(columns = type) |> 
  cols_width(
    "Hora" ~ pct(10),
    "Mesa" ~ pct(40),
    "Coordina" ~ pct(50)
  ) |> 
  sub_missing(
    columns = 3,
    missing_text = ""
  ) |> 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) |> 
  as_raw_html()
#
#
#
#
#
#
#
#
#
#| echo: false
lat_jardin <- 36.7611 
long_jardin <- -3.8450


content <- paste(sep = "<br/>",
  "<b><a href='https://visita.nerja.es/es/descubre/puntos-de-interes/jardin-botanico-detunda-cueva-de-nerja-p107361
'>Jardín Botánico Detunda - Cueva de Nerja</a></b>",
  "Recinto de la Cueva de Nerja",
  "29787. Maro. Nerja. Málaga"
)  

library(leaflet)
m <- leaflet() |>
  addTiles() |>
  setView(lng = long_jardin, lat = lat_jardin, zoom = 16) |> 
  addMarkers(lng = long_jardin, lat = lat_jardin, 
  popup = content)

m
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| echo: false

d19 <- d |> 
    filter(day == 19) |> 
    dplyr::select(-jornada, -day) 

gt::gt(d19) |> 
  fmt_markdown(colum = Coordina) |>
  tab_style(
    style = cell_fill(color = "#dfece9"),
    locations = cells_body(
      rows = type == "break")
  ) |> 
  cols_hide(columns = type) |> 
  cols_width(
    "Hora" ~ pct(10),
    "Mesa" ~ pct(40),
    "Coordina" ~ pct(50)
  ) |> 
  sub_missing(
    columns = 1:3,
    missing_text = ""
  ) |> 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_column_labels(everything())
  ) |> 
  as_raw_html()
#
#
#
#
