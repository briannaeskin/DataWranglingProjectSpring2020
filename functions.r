library(tidyverse)
library(rvest)
library(stringr)
library(purrr)

getGameIdsForUrl <- function(player_id) {
  player_id_url <- paste0("http://www.j-archive.com/showplayer.php?player_id=", player_id) %>%
    read_html() %>%
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
  contestant_first_name <- contestant %>%
    str_extract("^\\w+")
  game_url <- paste0("http://www.j-archive.com/showgame.php?game_id=", gameIdForUrl) %>%
    read_html()
  scores_url <- paste0("http://www.j-archive.com/showscores.php?game_id=", gameIdForUrl) %>%
    read_html()
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
  } else if(str_detect(game_comments, "Stars")) {
    tournament_name <- "All Stars Tournament"
  } else if(str_detect(game_comments, "Greatest")) {
    tournament_name <- "Greatest of All Time"
  } else {
    tournament_name <- "Regular Season"
  }
  game_id_and_date <- game_url %>%
    html_nodes("h1")
  game_id <- game_id_and_date %>%
    str_extract("(?<=\\#)\\d+") %>%
    as.numeric()
  game_date <- game_id_and_date %>%
    str_extract("(?<=-\\s).*(?=\\<)")
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
  final_score <- final_jeopardy_table %>%
    filter(Contestant %in% c(contestant_first_name, allStarTeam))
  if(nrow(final_score) == 0){
    return("Don't include")
  }
  final_score <- final_score$Score %>%
    str_extract("\\d+,\\d+|\\d+") %>%
    str_replace_all(",", "") %>%
    as.numeric()
  outcome <- final_jeopardy_table %>%
    filter(Contestant %in% c(contestant_first_name, allStarTeam)) %>%
    .$Outcome
  if(is.na(outcome)) {
    outcome <- "Accumulated Game"
  }
  if(outcome == "") {
    outcome <- "Accumulated Game"
  }
  rowForTable <- data.frame(
    Contestant = contestant,
    GameIdForUrl = gameIdForUrl,
    GameId = game_id,
    Date = game_date,
    FinalScore = final_score,
    Outcome = outcome,
    GameFormat = format,
    TournamentName = tournament_name
  )
  return(rowForTable)
}

getRowForTournamentWinningsTable <- function(contestant, tournament) {
  contestant_tournament_table <- game_level_df %>%
    filter(Contestant == contestant, TournamentName == tournament) %>%
    filter(GameId == max(GameId))
  contestants_filtered <- contestants %>%
    filter(Contestants == contestant) %>%
    select(Contestants, AllStarsTeam) %>%
    distinct()
  contestant_first_name <- contestant %>%
    str_extract("^\\w+")
  allStarTeam <- contestants_filtered[1,2]
  gameIdForUrl <- contestant_tournament_table[1,2]
  outcome <- contestant_tournament_table[1,6]
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
    winnings <- str_extract(tournament_scores$Outcome,"(?<=:\\s).*") %>%
      str_replace_all("\\$|,", "") %>%
      as.numeric()
  } else{
    result <- str_extract(tournament_scores$Outcome,".+?(?=:)")
    winnings <- str_extract(tournament_scores$Outcome,"(?<=:\\s).*") %>%
      str_replace_all("\\$|,", "") %>%
      as.numeric()
  }
  rowForTournamentWinningsTable <- data.frame(
    Contestant = contestant,
    Tournament = tournament,
    Outcome = result,
    Winnings = winnings,
    stringsAsFactors = FALSE
  )
  return(rowForTournamentWinningsTable)
}


