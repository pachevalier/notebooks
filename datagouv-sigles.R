library("tidyverse")
library("httr")
get_sigle <- function(id) {
  tibble(
    id = id, 
    term = paste0("https://www.data.gouv.fr/api/1/datasets/", id, "/") %>%
    GET() %>% 
    content() %>%
    pluck("acronym") %||% ""
  )
}
get_sigle(id = "5c34c4d1634f4173183a64f1") 
get_sigle(id = "5e870c8219cd4e437b686bac")

table_sigles <- read_csv2("data-raw/export-dataset-20200711-074504.csv") %>%
  filter(featured == TRUE) %>%
  select(id) %>%
  pluck("id") %>%
  map_df(.f = get_sigle)

read_csv2("data-raw/export-dataset-20200711-074504.csv") %>%
  filter(featured == TRUE) %>%
  select(id, title) %>%
  left_join(
    y = table_sigles, 
    by = "id"
    ) %>%
  filter(term != "") %>%
  mutate(
    source = "Datagouv", 
    url_source = paste0("https://www.data.gouv.fr/fr/datasets/", id, "/")
    ) %>%
  select(term, definition = title, source, url_source) %>%
  write_csv(path = "datagouv-datasets-sigles.csv")


