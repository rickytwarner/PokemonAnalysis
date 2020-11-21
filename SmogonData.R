library(tidyverse)
library(rvest)

Smogon <- read_html('https://www.smogon.com/dex/ss/pokemon/') 


 test <-      Smogon %>%
  html_nodes(xpath = "/html/body/div[1]/div/main") 

xpath <- '//*[@id="container"]/div/main/div/div'



Abomasnow.Name <- Smogon %>%
  html_nodes(xpath = '/html/body/div[1]/div/main/div/div/div[1]/div[1]/a/span/span[2]')

Abra.Name <- Smogon %>%
  html_nodes(xpath = '/html/body/div[1]/div/main/div/div/div[2]/div[1]/a/span/span[2]')

Corsola.Name <- Smogon %>%
  html_nodes(xpath = '/html/body/div[1]/div/main/div/div/div[21]/div[1]/a/span/span[2]')


Abomasnow.Rank <- Smogon %>%
  html_nodes(xpath = '/html/body/div[1]/div/main/div/div/div[1]/div[5]/ul/li/a')

Abra.Rank <- Smogon %>%
  html_nodes(xpath = '/html/body/div[1]/div/main/div/div/div[2]/div[5]/ul/li/a')



SmogonRank <- data.frame(
  Name = character(),
  Rank = character()
)


i <- 1

Loop <- FALSE

while (Loop == FALSE) {
  
  Name <- Smogon %>%
    html_nodes(xpath = paste('/html/body/div[1]/div/main/div/div/div[', i, ']/div[1]/a/span/span[2]', sep = "")) %>%
    html_text()
  
  Rank <- Smogon %>%
    html_nodes(xpath = paste('/html/body/div[1]/div/main/div/div/div[', i,']/div[5]/ul/li/a')) %>%
    html_text()
  
  if (toString(Name) == "") {
    Loop = TRUE
    print(Loop)
  }

  
  SmogonRankLoad <- data.frame(Name, Rank)
  
  SmogonRank <- rbind(SmogonRank, SmogonRankLoad)
  
  i <- i + 1
}
