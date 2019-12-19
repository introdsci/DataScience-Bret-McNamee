## ------------------------------------------------------------------------
suppressMessages(library("tidyverse"))
suppressMessages(library("dplyr"))
suppressMessages(library("tidyr"))
suppressMessages(library("rvest"))
suppressMessages(library("knitr"))

#purl("deliverable1.Rmd", output="part1.r")
#source("part1.r")


## ------------------------------------------------------------------------
url <- read_html("https://oracleselixir.com/statistics/champions/2019-summer-champion-statistics/")

tmp <- url %>% html_nodes("tr")

champ <- tmp %>% html_nodes("td.column-1") %>% html_text()

role <- tmp %>% html_nodes("td.column-2") %>% html_text()

gamesPlayed <- tmp %>% html_nodes("td.column-3") %>% html_text() %>% as.integer()

pickRate <- tmp %>% html_nodes("td.column-4") %>% html_text()

banRate <- tmp %>% html_nodes("td.column-5") %>% html_text()

winPct <- tmp %>% html_nodes("td.column-7") %>% html_text()

KDA <- tmp %>% html_nodes("td.column-12") %>% html_text() %>% as.double()

gd10 <- tmp %>% html_nodes("td.column-18") %>% html_text() %>% as.double()

xpd10 <- tmp %>% html_nodes("td.column-19") %>% html_text() %>% as.double()

csd10 <- tmp %>% html_nodes("td.column-20") %>% html_text() %>% as.double()

summer2019 <- tibble(champ = champ, role = role, gamesPlayed = gamesPlayed, pickRate= pickRate, banRate = banRate, winpct = winPct, KDA = KDA,
    gd10 = gd10, xpd10 = xpd10, csd10 = csd10)


## ------------------------------------------------------------------------
summer2019$pickRate <- str_remove(summer2019$pickRate, '%') %>% as.double()

summer2019$banRate <- str_remove(summer2019$banRate, '%') %>% as.double()

summer2019$winpct <- str_remove(summer2019$winpct, '%') %>% as.double()


## ------------------------------------------------------------------------
summer2019$gamesWon <- as.integer(summer2019$gamesPlayed * ( summer2019$winpct/100))

totGames <- tibble(champ = unique(summer2019$champ),
                   gamesPlayed2019 = 0,
                   gamesWon2019 = 0,
                   gamesPlayedP1 = 0,
                   gamesWonP1 = 0,
                   winRate2019 = 0,
                   winRateP1 = 0)

for(i in 1:nrow(totGames)){
  totGames$gamesPlayed2019[i] <- sum(summer2019$gamesPlayed[summer2019$champ==totGames$champ[i]])
  
  totGames$gamesWon2019[i] <- sum(summer2019$gamesWon[summer2019$champ==totGames$champ[i]])
  
  totGames$winRate2019[i] <-  (totGames$gamesWon2019[i] / totGames$gamesPlayed2019[i]) * 100

  if(any(totGames$champ[i] == Champs)){
  totGames$gamesPlayedP1[i] <- Champs$games[Champs$Champ==totGames$champ[i]]
  
  totGames$gamesWonP1[i] <- Champs$wins[Champs$Champ==totGames$champ[i]]
  
  totGames$winRateP1[i] <- Champs$winPct[Champs$Champ==totGames$champ[i]] * 100
  } 
}
## Here is how the table turned out.
head(totGames)


## ------------------------------------------------------------------------

ggplot(totGames, aes(x=champ, y=(winRate2019-winRateP1))) + 
  theme(axis.text.x=element_text(angle=45,
  size = rel(0.75), margin = 
  margin(0.5, unit = "cm"))) + geom_col()



## ------------------------------------------------------------------------
games100 <- filter(totGames, totGames$gamesPlayed2019 > 100)

ggplot(games100, aes(x=champ, y=(winRate2019-winRateP1))) + 
  theme(axis.text.x=element_text(angle=45,
  size = rel(0.75), margin = 
  margin(0.5, unit = "cm"))) + geom_col()

