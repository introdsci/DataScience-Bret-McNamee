## ------------------------------------------------------------------------
suppressMessages(library(tidyverse))
suppressMessages(library(tidyr))

data <- read_csv("LeagueofLegends.csv")
tData <- dplyr::tibble(region=data$League,
    year=data$Year,winner = data$bResult,
    blueTeam=data$blueTeamTag,
    redTeam=data$redTeamTag, gameTime = data$gamelength,
    bTC=data$blueTopChamp, bJC=data$blueJungleChamp,
    bMC=data$blueMiddleChamp, bADC=data$blueADCChamp,
    bSC=data$blueSupportChamp,
    bT=data$blueTop, bJ=data$blueJungle, bM=data$blueMiddle,
    bAD=data$blueADC, bS=data$blueSupport,
    rTC=data$redTopChamp, rJC=data$redJungleChamp,
    rMC=data$redMiddleChamp, rADC=data$redADCChamp,
    rSC=data$redSupportChamp,
    rT=data$redTop, rJ=data$redJungle, rM=data$redMiddle,
    rAD=data$redADC, rS=data$redSupport)


## ------------------------------------------------------------------------

tData["redPlayers"] <- NA
tData["bluePlayers"] <- NA
tData["redChamps"] <- NA
tData["blueChamps"] <- NA

for(i in 1:nrow(tData)){
  tData$redPlayers[i] <- paste(tData$rT[i],
      tData$rJ[i], tData$rM[i], tData$rAD[i],
      tData$rS[i] ,sep=", ")
  
  tData$bluePlayers[i] <- paste(tData$bT[i],
      tData$bJ[i], tData$bM[i], tData$bAD[i],
      tData$bS[i] ,sep=", ")
  
  tData$redChamps[i] <- paste(tData$rTC[i],
      tData$rJC[i], tData$rMC[i], tData$rADC[i],
      tData$rSC[i] ,sep=", ")
  
   tData$blueChamps[i] <- paste(tData$bTC[i],
      tData$bJC[i], tData$bMC[i], tData$bADC[i],
      tData$bSC[i] ,sep=", ")
}
##> str_detect(df$redPlayers[23], RedPlayers[1])




## ------------------------------------------------------------------------
BluePlayers <- unique(c(unique(tData$bT), unique(tData$bJ), unique(tData$bM), unique(tData$bAD), unique(tData$bS)))
                     
RedPlayers <- unique(c(unique(tData$rT), unique(tData$rJ), unique(tData$rM), unique(tData$rAD), unique(tData$rS)))

BlueChamps <- unique(c(unique(tData$bTC), unique(tData$bJC), unique(tData$bMC), unique(tData$bADC), unique(tData$bSC)))
                     
RedChamps <- unique(c(unique(tData$rTC), unique(tData$rJC), unique(tData$rMC), unique(tData$rADC), unique(tData$rSC)))

Players <- data.frame(Player=c(RedPlayers,BluePlayers),wins=0, games=0, winPct = 0)

Champs <- data.frame(Champ= c(RedChamps,BlueChamps), wins=0, games=0, winPct = 0)

Players <- unique(Players)
Champs <- unique(Champs)



## ------------------------------------------------------------------------

for (i in 1:nrow(tData)){
  for(j in 1:length(RedPlayers)){
  if(str_detect(tData$redPlayers[i], RedPlayers[j])){
    if(tData$winner[i] == 0){
    Players$wins[Players$Player==RedPlayers[j]]<- Players$wins[Players$Player==RedPlayers[j]] + 1
  }
    Players$games[Players$Player==RedPlayers[j]]<- Players$games[Players$Player==RedPlayers[j]] + 1
  }
}   
}

for (i in 1:nrow(tData)){
  for(j in 1:length(BluePlayers)){
  if(str_detect(tData$bluePlayers[i], BluePlayers[j])){
    if(tData$winner[i] == 1){
    Players$wins[Players$Player==BluePlayers[j]]<- Players$wins[Players$Player==BluePlayers[j]] + 1
  }
    Players$games[Players$Player==BluePlayers[j]]<- Players$games[Players$Player==BluePlayers[j]] + 1
  }
}   
}

for (i in 1:nrow(tData)){
  for(j in 1:length(RedChamps)){
  if(str_detect(tData$redChamps[i], RedChamps[j])){
    if(tData$winner[i] == 0){
    Champs$wins[Champs$Champ==RedChamps[j]]<- Champs$wins[Champs$Champ==RedChamps[j]] + 1
  }
    Champs$games[Champs$Champ==RedChamps[j]]<- Champs$games[Champs$Champ==RedChamps[j]] + 1
  }
}   
}

for (i in 1:nrow(tData)){
  for(j in 1:length(BlueChamps)){
  if(str_detect(tData$blueChamps[i], BlueChamps[j])){
    if(tData$winner[i] == 1){
    Champs$wins[Champs$Champ==BlueChamps[j]]<- Champs$wins[Champs$Champ==BlueChamps[j]] + 1
  }
    Champs$games[Champs$Champ==BlueChamps[j]]<- Champs$games[Champs$Champ==BlueChamps[j]] + 1
  }
}   
}


## ------------------------------------------------------------------------

Players$winPct <- Players$wins / Players$games
Champs$winPct <- Champs$wins / Champs$games


