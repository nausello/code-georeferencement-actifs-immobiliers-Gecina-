####### Téléchargement et extraction depuis Zenodo #######

url_zip <- "https://zenodo.org/records/23034128/files/actifs_immobiliers_2007-2019-2024.zip?download=1"
dest_zip <- tempfile(fileext = ".zip")
dest_dir <- tempfile()

download.file(url_zip, destfile = dest_zip, mode = "wb")
unzip(dest_zip, exdir = dest_dir)

# Vérification que les 6 fichiers sont bien présents
list.files(dest_dir, recursive = TRUE)

# Lecture des 6 bases

lire_csv <- function(nom_fichier) {
  read.csv(file.path(dest_dir, nom_fichier), sep = ";", header = TRUE,
           fileEncoding = "Windows-1252")
}

df_bur07 <- lire_csv("bur07.csv")
df_res07 <- lire_csv("res07.csv")
df_bur19 <- lire_csv("bur19.csv")
df_res19 <- lire_csv("res19.csv")
df_bur24 <- lire_csv("bur24.csv")
df_res24 <- lire_csv("res24.csv")

# Vérification rapide
purrr::map(list(df_bur07, df_res07, df_bur19, df_res19, df_bur24, df_res24), dim)



######### Exemple pour les actifs bureaux 2019 #########

extract_clean_addresses <- function(text, dep) {
  if (is.na(text) || text == "" || text == "NA") {
    return(tibble())
  }
  if (grepl("Total actifs|Parking|^,|^$", text, ignore.case = TRUE)) {
    return(tibble())
  }

  # 0. Supprimer un suffixe ", <commune>" déjà présent dans le texte
  text <- sub(",\\s*(Paris|Hauts-de-Seine|Seine-Saint-Denis|Val-d'Oise|\\d{2,3})\\s*$", "", text)

  # 1. Séparer les adresses multiples sur séparateurs explicites
  address_parts <- unlist(str_split(text, " et | – |\\s+à\\s+|\\s+–\\s+"))
  address_parts <- address_parts[address_parts != ""]

  # 1b. Fallback: découper avant un numéro (avec range éventuel) qui démarre
  #     une nouvelle adresse au milieu du texte (jamais en tout début de part)
  split_multi_numbers <- function(part) {
    pattern <- "(?<=[a-zéèêàâôûüïçA-Z])\\s+(?=\\d+(?:[/-]\\d+)?\\s)"
    m <- gregexpr(pattern, part, perl = TRUE)[[1]]
    if (m[1] == -1) return(part)
    gsub(pattern, "\n", part, perl = TRUE)
  }
  address_parts <- unlist(lapply(address_parts, split_multi_numbers))
  address_parts <- unlist(str_split(address_parts, "\n"))
  address_parts <- address_parts[address_parts != ""]

  result_list <- list()
  for (addr in address_parts) {
    # 2. Nettoyer chaque adresse (supprimer préfixes comme "Le Building – ")
    addr <- sub("^[^0-9]*", "", addr) %>% str_trim()
    if (addr == "") next
    # 3. Extraire le numéro (peut contenir "/", "-", ou "bis")
    num_pattern <- "\\d+(?:[/-]\\d+)?\\s*(?:bis)?"
    num_match <- str_match(addr, num_pattern)
    if (is.na(num_match[1])) next
    num <- num_match[1]
    voie <- str_trim(sub(num, "", addr, fixed = TRUE))
    voie <- str_replace_all(voie, "^[,\\s-]+", "") %>% str_trim()
    voie <- str_replace_all(voie, "[,\\s-]+$", "")
    if (voie == "") next
    # 4. Déduire la commune
    commune <- case_when(
    dep == 75 ~ "Paris",
      dep == 92 ~ "Hauts-de-Seine",
      dep == 77 ~ "Seine-et-Marne",
      dep == 91 ~ "Essonne",
      dep == 93 ~ "Seine-Saint-Denis",
      dep == 95 ~ "Val-d'Oise",
      dep == 78 ~ "Yvelines",
      dep == 94 ~ "Val-de-Marne",
      TRUE ~ as.character(dep)
    )
    # 5. Garder le numéro tel quel (ex: "31/35" reste "31/35" = une seule adresse)
    result_list <- c(result_list, list(tibble(
      numero = num,
      voie = voie,
      commune = commune
    )))
  }
  if (length(result_list) == 0) return(tibble())
  bind_rows(result_list)
}

other_cols <- setdiff(names(bur19), c("Adresse", "adresse_complete"))
result24 <- bur19 %>%
  filter(!is.na(Adresse) & Adresse != "" & Adresse != "NA") %>%  # Filtrer les lignes vides
  mutate(clean_addresses = map2(Adresse, Dept, extract_clean_addresses)) %>%
  unnest(clean_addresses) %>%
dplyr::  select(all_of(other_cols), numero, voie, commune, dep = Dept) %>%
  distinct()  

result24 <- result24 %>%
  mutate(
    adresse_structuree = paste(numero, voie, sep = " "),
    adresse_structuree = paste0(adresse_structuree, ", ", commune)
  ) %>%
 dplyr:: select(-numero, -voie, -commune)  # Supprime les colonnes individuelles

df_geo24 <- result24 %>%
  mutate(adresse_structuree = as.character(adresse_structuree)) %>%
  geocode(address = adresse_structuree, method = 'arcgis', lat = latitude, long = longitude)

df_bur19 <- df_geo24 %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%  # enlever les adresses non géocodées
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
