library(rvest)
library(janitor)
library(reshape)
library(stringr)
library(httr)
library(jsonlite)
library(tidyverse)
library(rstudioapi)



Wikipedia <- read_html('https://en.wikipedia.org/wiki/List_of_Pok%C3%A9mon')

xpath = "/html/body/div[3]/div[3]/div[5]/div[1]/table[3]"

PokemonList.v1 <- Wikipedia %>%
                    html_nodes(xpath = xpath) %>%
                    html_table(fill=TRUE)

PokemonList.v2 <- PokemonList.v1[[1]]

PokemonList.v3 <- t(PokemonList.v2)

PokemonList.v4 <- PokemonList.v3 %>%
                    as.data.frame() %>%
                    tibble::rownames_to_column("Gen")

PokemonList.v4$Gen <- gsub(".1", "", PokemonList.v4$Gen)

PokemonList.v5 <- PokemonList.v4 %>%
                    group_by(Gen) %>%
                    summarise_all(funs(paste(., collapse = "!")))

PokemonList.v6 <- t(PokemonList.v5)

PokemonList.v7 <- PokemonList.v6 %>%
                    row_to_names(row_number = 1)
PokemonList.v8 <- PokemonList.v7[-1, ]

GenArray <- colnames(PokemonList.v8)

PokemonList.v9 <- PokemonList.v8

for (i in seq_along(GenArray)) {
  PokemonList.v9[, i] <- paste(GenArray[i], PokemonList.v9[, i], sep = "!")
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
  separate(value, c("Generation", "ID", "Pokemon"), "!")

PokemonList.v15$ID <- str_replace(PokemonList.v15$ID, "^0+", "")

req <- "https://pokeapi.co/api/v2/pokemon/"

len <- nrow(PokemonList.v15)

#i <- 3
PokemonList.v16 <- PokemonList.v15

PokeApiMaster <- read.table(text = "", 
                            col.names = c("ID", "Pokemon", "Abilities", "BaseExperience", "Height", "Moves", "HP", "Attack", "Defense", "SpecialAttack", "SpecialDefense", "Speed", "Type1", "Type2", "Weight"))
  


for (i in 1:len) {
  print(i)
  url <- paste(req, i, sep="")
  res <- GET(url)
  res.json <- rawToChar(res$content)
  data <- fromJSON(res.json)
  
  abilities.v1 <- data$abilities
  
  abilities.v2 <- abilities.v1 %>%
      mutate(
        combined = case_when(
        is_hidden == TRUE ~ " (Hidden)",
        is_hidden == FALSE ~ ""
      )
    ) 
  
  #ability name and url are nested, you're going to have to unnest them     
  abilities.v3 <- do.call(data.frame, abilities.v2)
  
  abilities.v4 <- abilities.v3 %>% # Final Product
                    mutate(fullname = paste(ability.name, combined, sep="")) %>%
                    summarize(abilities = paste0(fullname, collapse = ", "))
  
  BaseExperience <- data$base_experience # Final Product
  
  Height <- data$height # Final Product
  
  moves.v1 <- data$moves
  
  moves.v2 <- do.call(data.frame, moves.v1$move)
  
  moves.v3 <- moves.v2 %>%
                summarize(name = paste0(name, collapse = ", ")) # Final Product
  
  stats <- do.call(data.frame, data$stats)
  
  hp.v1 <- stats %>%
    filter(stat.name == "hp")
    
  hp.v2 <- hp.v1 %>%
    select(base_stat) # Final Product
  
  attack.v1 <- stats %>%
    filter(stat.name == "attack")
  
  attack.v2 <- attack.v1 %>%
    select(base_stat) # Final Product
  
  defense.v1 <- stats %>%
    filter(stat.name == "defense")
  
  defense.v2 <- defense.v1 %>%
    select(base_stat) # Final Product
  
  SpecialAttack.v1 <- stats %>%
    filter(stat.name == "special-attack")
  
  SpecialAttack.v2 <- SpecialAttack.v1 %>%
    select(base_stat) # Final Product
  
  SpecialDefense.v1 <- stats %>%
    filter(stat.name == "special-defense")
  
  SpecialDefense.v2 <- SpecialDefense.v1 %>%
    select(base_stat) # Final Product
  
  Speed.v1 <- stats %>%
    filter(stat.name == "speed") 
  
  Speed.v2 <- stats %>%
    select(base_stat) # Final Product
  
  
  types <- do.call(data.frame, data$types)
  
  Type1.v1 <- types %>%
                filter(slot == 1) %>%
                select(type.name) # Final Product 
  
  Type2.v1 <- types %>%
                filter(slot == 2) %>%
                select(type.name) # Final Product 
  
  if (lengths(Type2.v1, use.names = TRUE) == 0){
    Type2.v1 <- "None"
  }
  
  
  Weight <- data$weight
  
  PokeApiData.v1 <- c(i, abilities.v4, BaseExperience, Height, moves.v3, hp.v2, attack.v2, defense.v2, SpecialAttack.v2, SpecialDefense.v2, Speed.v2, Type1.v1, Type2.v1, Weight)
  
  PokeApiData.v2 <- as.data.frame(PokeApiData.v1)
  
  PokeApiData.v3 <- PokeApiData.v2 %>%
    transmute(
      ID = toString(PokeApiData.v2[, 1]),
      Abilities = PokeApiData.v2[, 2],
      BaseExperience = PokeApiData.v2[, 3],
      Height = PokeApiData.v2[, 4],
      Moves = PokeApiData.v2[, 5],
      HP = PokeApiData.v2[, 6],
      Attack = PokeApiData.v2[, 7],
      Defense = PokeApiData.v2[, 8],
      SpecialAttack = PokeApiData.v2[, 9],
      SpecialDefense = PokeApiData.v2[, 10],
      Speed = PokeApiData.v2[, 11],
      Type1 = PokeApiData.v2[, 12],
      Type2 = PokeApiData.v2[, 13],
      Weight = PokeApiData.v2[, 14]
    ) %>% head(1)
  
  PokeApiData.v4 <- PokeApiData.v3 %>%
    mutate(ID = gsub(",.*", "", ID))
  
  PokeApiMaster <- rbind(PokeApiMaster, PokeApiData.v4)
  Sys.sleep(10)
}

PokemonList.v17 <- PokemonList.v16 %>%
  left_join(PokeApiMaster, by = c("ID"))

SourcePath <- str_remove(getActiveDocumentContext()$path, "PokemonAnalysis.R")

FilePath <- paste(SourcePath, "PokemonMasterList.csv", sep = "")

if (file.exists(FilePath)) {
  file.remove(FilePath)
}

write_csv(PokemonList.v17, path = FilePath)





  
