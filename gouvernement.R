library("httr")
library("tidyverse")
library("rvest")
library("lubridate")
liste_ministres <- "https://www.gouvernement.fr/composition-du-gouvernement" %>%
  GET() %>%
  content() %>%
  html_nodes(css = ".ministre-nom")

tibble(
  ministre  = liste_ministres %>% html_text(), 
  slug = liste_ministres %>% html_attr(name = "href")
  ) %>% 
  write_csv(path = paste0("liste_ministres_", today(), ".csv"))

get_collaborateurs <- function(slug) {
  page_ministre <- paste0("https://www.gouvernement.fr/", slug) %>%
    GET() %>%
    content()
  tibble(
    slug = slug, 
    collaborateur_nom = page_ministre %>%  
      html_nodes(css = ".collaborateur-nom") %>%
      html_text(), 
    collaborateur_fonction =  page_ministre %>% 
      html_nodes(css = ".collaborateur-fonction") %>%
      html_text(), 
    collaborateur_jo = page_ministre %>% html_nodes(css = ".collaborateur-journal-officiel") %>%
      html_text()
    )
}
get_collaborateurs(slug = "ministre/jean-castex")

liste_ministres %>%
  html_attr(name = "href") %>%
  map_df(.f = get_collaborateurs) %>%
  write_csv(path = paste0("liste_collaborateurs_", today(), ".csv"))



