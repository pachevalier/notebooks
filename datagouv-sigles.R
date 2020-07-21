library("tidyverse")
library("httr")

get_sigle_organization <- function(id) {
  tibble(
    id = id, 
    term = paste0("https://www.data.gouv.fr/api/1/organizations/", id, "/") %>%
      GET() %>% 
      content() %>%
      pluck("acronym") %||% ""
  )
}
get_sigle_organization(id = "5ee8ce78f450050a4fa6a1d3") 

table_organizations <- "https://www.data.gouv.fr/fr/datasets/r/b7bbfedc-2448-4135-a6c7-104548d396e7" %>%
  read_csv2()

table_sigles_organizations <- table_organizations %>%
  filter(badges != "[]" ) %>%
  select(id) %>% 
  pluck("id") %>%
  map_df(.f = get_sigle_organization) %>%
  filter(term != "") 

table_organizations %>%
  inner_join(
    by = "id", 
    y = table_sigles_organizations
    ) %>%
  mutate(
    source = "Datagouv", 
    url_source = paste0("https://www.data.gouv.fr/api/1/organizations/", id, "/")
  ) %>%
  select(term, definition = name, source, url_source) %>%
  arrange(term) %>%
  write_csv(path = "datagouv-organizations-sigles.csv")

read_csv("datagouv-organizations-sigles.csv") %>%
  glimpse()

get_sigle_dataset <- function(id) {
  tibble(
    id = id, 
    term = paste0("https://www.data.gouv.fr/api/1/datasets/", id, "/") %>%
    GET() %>% 
    content() %>%
    pluck("acronym") %||% ""
  )
}
get_sigle_dataset(id = "5c34c4d1634f4173183a64f1") 
get_sigle_dataset(id = "5e870c8219cd4e437b686bac")

table_datasets <- read_csv2("https://www.data.gouv.fr/fr/datasets/r/f868cca6-8da1-4369-a78d-47463f19a9a3")

table_sigles_dataset <- table_datasets %>%
  inner_join(
    y = select(filter(table_organizations, badges != "[]"), id), 
    by = c("organization_id" = "id")
    ) %>%
  pluck("id") %>%
  map_df(.f = get_sigle_dataset)

table_datasets %>%
  inner_join(
    y = select(filter(table_organizations, badges != "[]"), id), 
    by = c("organization_id" = "id")
  ) %>%
  left_join(
    y = table_sigles_dataset, 
    by = "id"
    ) %>%
  filter(term != "") %>%
  mutate(
    source = "Datagouv", 
    url_source = paste0("https://www.data.gouv.fr/api/1/datasets/", id, "/")
    ) %>%
  select(term, definition = title, source, url_source) %>%
  write_csv(path = "datagouv-datasets-sigles.csv")
