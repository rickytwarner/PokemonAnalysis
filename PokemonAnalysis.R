# Create a list of all pokemon from all generations
library(tidyverse)
library(rvest)
library(janitor)
library(reshape)
library(stringr)
Wikipedia <- read_html('https://en.wikipedia.org/wiki/List_of_Pok%C3%A9mon')

xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[3]"

PokemonList.v1 <- Wikipedia %>%
                    html_nodes(xpath = xpath) %>%
                    html_table(fill=TRUE)

PokemonList.v2 <- PokemonList.v1[[1]]

PokemonList.v3 <- t(PokemonList.v2)

PokemonList.v4 <- PokemonList.v3 %>%
                    as.data.frame() %>%
                    rownames_to_column("Gen")

PokemonList.v4$Gen <- gsub(".1", "", PokemonList.v4$Gen)

PokemonList.v5 <- PokemonList.v4 %>%
                    group_by(Gen) %>%
                    summarise_all(funs(paste(., collapse = "-")))

PokemonList.v6 <- t(PokemonList.v5)

PokemonList.v7 <- PokemonList.v6 %>%
                    row_to_names(row_number = 1)
PokemonList.v8 <- PokemonList.v7[-1, ]

GenArray <- colnames(PokemonList.v8)

PokemonList.v9 <- PokemonList.v8

for (i in seq_along(GenArray)) {
  PokemonList.v9[, i] <- paste(GenArray[i], PokemonList.v9[, i], sep = "-")
}

PokemonList.v10 <- as_tibble(PokemonList.v9)

PokemonList.v11 <- PokemonList.v10$Generation.I

PokemonList.v12 <- PokemonList.v10 %>%
  select(-Generation.I)

PokemonList.v13 <- as.data.frame(PokemonList.v11)

GenArray <- colnames(PokemonList.v12)



for (i in seq_along(GenArray)) {
  print(GenArray[i])
  ListLoad <- as.data.frame(PokemonList.v12[, i])
  colnames(ListLoad) <- "PokemonList.v11"
  PokemonList.v13 <- rbind(PokemonList.v13, ListLoad)
}

colnames(PokemonList.v13) <- "value"

PokemonList.v14 <- PokemonList.v13 %>%
  filter(!str_detect(value, "No additional Pokémon"))

PokemonList.v15 <- PokemonList.v14 %>%
  separate(value, c("Generation", "ID", "Pokemon"), "-")

str_replace(PokemonList.v15$ID, "^0+", "")
  