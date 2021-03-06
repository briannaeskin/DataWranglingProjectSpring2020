library(tidyverse)
library(rvest)
library(stringr)
library(purrr)
options(timeout = 400000)
options(scipen = 999999)

getGameIdsForUrl <- function(player_id) {
  #Function to scrape the player site with a given player id, and pulls the list of game ids that will be used to scrape further sites
  player_id_url <- paste0("http://www.j-archive.com/showplayer.php?player_id=", player_id)
  
  player_id_url <- read_html(url(player_id_url)) %>%
    html_nodes("a") %>%
    map(xml_attrs) %>%
    map_df(~as.list(.))
  
  game_id_list <-player_id_url %>%
    select(href) %>%
    mutate(InScope = str_detect(.$href, "game_id")) %>%
    filter(InScope == TRUE) %>%
    mutate(GameIdForUrl = str_extract(.$href, "\\d+")) %>%
    select(GameIdForUrl)
  return(game_id_list)
}

getRowForGameLevelTable <- function(contestant, gameIdForUrl, allStarTeam) {
  #Function to pull all variables needed for main table
  
  #Contestant first name
  contestant_first_name <- contestant %>%
    str_extract("^\\w+")
  
  game_url <- paste0("http://www.j-archive.com/showgame.php?game_id=", gameIdForUrl)
  game_url <-  read_html(url(game_url))
  scores_url <- paste0("http://www.j-archive.com/showscores.php?game_id=", gameIdForUrl)
  scores_url <-  read_html(url(scores_url))
  
  #Game comments, used to find tournament information
  game_comments <- game_url %>%
    html_nodes("#game_comments") %>% .[1] %>%
    toString()
  
  if(str_detect(game_comments, "Jeopardy! Round only")) {
    return("Don't include")
  }
  if(str_detect(game_comments, "Tournament|Masters|Decade|Star")) {
    format <- "Tournament"
  } else if(str_detect(game_comments, "IBM")) {
    format <- "Exhibition"
  } else {
    format <- "Regular Season"
  }
  if(str_detect(game_comments, "Ultimate Tournament of Champions")) {
    tournament_name <- "Ultimate Tournament of Champions"
  } else if(str_detect(game_comments, "Million Dollar Masters")) {
    tournament_name <- "Million Dollar Masters"
  } else if(str_detect(game_comments, "Tournament of Champions")) {
    tournament_name <- "Tournament of Champions"
  } else if(str_detect(game_comments, "IBM")) {
    tournament_name <- "IBM Challenge"
  } else if(str_detect(game_comments, "Decades")) {
    tournament_name <- "Battle of the Decades"
  } else if(str_detect(game_comments, "Star")) {
    tournament_name <- "All-Star Games"
  } else if(str_detect(game_comments, "Greatest")) {
    tournament_name <- "Greatest of All Time"
  } else if(str_detect(game_comments, "Teacher")) {
    tournament_name <- "Teachers Tournament"
  } else {
    tournament_name <- "Regular Season"
  }
  
  #Get game id and date episode aired
  game_id_and_date <- game_url %>%
    html_nodes("h1")
  game_id <- game_id_and_date %>%
    str_extract("(?<=\\#)\\d+") %>%
    as.numeric()
  game_date <- game_id_and_date %>%
    str_extract("(?<=-\\s).*(?=\\<)")
  
  #Final Jeopardy table 
  final_jeopardy_table <- scores_url %>%
    html_nodes("table") %>% .[4] %>%
    html_table(fill = TRUE) %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame()
  if(ncol(final_jeopardy_table) == 2) {
    colnames(final_jeopardy_table) <- c("Contestant", "Score")
    final_jeopardy_table <- final_jeopardy_table %>%
      mutate(Outcome = "Accumulated Game")
  } else {
    colnames(final_jeopardy_table) <- c("Contestant", "Score", "Outcome")
  }
  
  #Score at the end of the game (not necessarily winnings)
  final_score <- final_jeopardy_table %>%
    filter(Contestant %in% c(contestant_first_name, allStarTeam))
  if(nrow(final_score) == 0){
    return("Don't include")
  }
  final_score <- final_score$Score %>%
    str_extract("\\d+,\\d+|\\d+") %>%
    str_replace_all(",", "") %>%
    as.numeric()
  
  #Outcome
  outcome <- final_jeopardy_table %>%
    filter(Contestant %in% c(contestant_first_name, allStarTeam)) %>%
    .$Outcome
  if(is.na(outcome)) {
    outcome <- "Accumulated Game"
  }
  if(outcome == "") {
    outcome <- "Accumulated Game"
  }
  
  #Create table with Coryat scores
  coryat_table <- scores_url %>%
    html_nodes("table") %>% last() %>%
    html_table(fill = TRUE) %>%
    as.data.frame() %>%
    t() %>%
    as.data.frame()
  coryat_table <- coryat_table %>%
    separate(col = "V3", into = c("V4", "V5"), sep = ",", remove = FALSE) %>%
    separate(col = "V4", into = c("V6", "V7"), sep = "\\sR\\(including\\s|\\sDD|\\sDDs\\)|\\sR") %>%
    separate(col = "V5", into = c("V8", "V9"), sep = "\\sW\\(including\\s|\\sDD|\\sDDs\\)|\\sW") %>%
    select("V1", "V2", "V6", "V7", "V8", "V9")
  coryat_table$V7 <- sub("^$", 0, coryat_table$V7)
  coryat_table$V9 <- sub("^$", 0, coryat_table$V9)
  colnames(coryat_table) <- c("Contestant", "Score", "QuestionsRight", "DailyDoublesRight", "QuestionsWrong", "DailyDoublesWrong")
  coryat_table <- coryat_table %>%
    filter(Contestant %in% c(contestant_first_name, allStarTeam))
  if(nrow(coryat_table) == 0){
    return("Don't include")
  }
  
  #Get Coryat score
  coryat_score <- coryat_table$Score %>%
    str_extract("\\d+,\\d+|\\d+") %>%
    str_replace_all(",", "") %>%
    as.numeric()
  
  rowForTable <- data.frame(
    Contestant = contestant,
    GameIdForUrl = gameIdForUrl,
    GameId = game_id,
    Date = game_date,
    FinalScore = final_score,
    Outcome = outcome,
    GameFormat = format,
    TournamentName = tournament_name,
    CoryatScore = coryat_score,
    QuestionsRight = as.numeric(coryat_table[1,3]),
    DailyDoublesRight = as.numeric(coryat_table[1,4]),
    QuestionsWrong = as.numeric(coryat_table[1,5]),
    DailyDoublesWrong = as.numeric(coryat_table[1,6]),
    stringsAsFactors = FALSE
  )
  return(rowForTable)
}

getRowForTournamentWinningsTable <- function(contestant, tournament) {
  #Basic contestant info
  contestant_tournament_table <- game_level_df %>%
    filter(Contestant == contestant, TournamentName == tournament) %>%
    filter(GameId == max(GameId))
  contestants_filtered <- contestants %>%
    filter(Contestants == contestant) %>%
    select(Contestants, AllStarsTeam) %>%
    distinct()
  
  #Contestant first name
  contestant_first_name <- contestant %>%
    str_extract("^\\w+")
  
  allStarTeam <- contestants_filtered[1,2]
  gameIdForUrl <- contestant_tournament_table[1,2]
  outcome <- contestant_tournament_table[1,6]
  
  #Special condition for games that are part of accumulated totals, such as tournament finals
  if(outcome == "Accumulated Game") {
    tournament_scores <- paste0("http://www.j-archive.com/showscores.php?game_id=", gameIdForUrl) %>%
      read_html() %>%
      html_nodes("table") %>% .[5] %>%
      html_table(fill = TRUE) %>%
      as.data.frame(stringsAsFactors = FALSE) %>%
      t() %>%
      as.data.frame(stringsAsFactors = FALSE)
    colnames(tournament_scores) <- c("Contestant", "Score", "Outcome")
    tournament_scores <- tournament_scores %>%
      filter(Contestant %in% c(contestant_first_name, allStarTeam))
    result <- str_extract(tournament_scores$Outcome,".+?(?=:)")
    winnings <- str_extract(tournament_scores$Outcome,"(?<=:\\s)[\\$\\d+,]+") %>%
      str_replace_all("\\$|,", "") %>%
      as.numeric()
  } else{
    #Regular game (i.e. not tournament final)
    result <- str_extract(outcome,".+?(?=:)")
    winnings <- str_extract(outcome,"(?<=:\\s)[\\$\\d+,]+") %>%
      str_replace_all("\\$|,", "") %>%
      as.numeric()
  }
  
  #Split winnings by 3 for All-Star tournament
  if (tournament == "All-Star Games") {
    winnings <- round(winnings/3)
  }
  
  rowForTournamentWinningsTable <- data.frame(
    Contestant = contestant,
    TournamentName = tournament,
    Outcome = result,
    Winnings = winnings,
    stringsAsFactors = FALSE
  )
  return(rowForTournamentWinningsTable)
}
