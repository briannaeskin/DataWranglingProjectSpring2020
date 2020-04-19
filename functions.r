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
    GameId = game_id,
    Date = game_date,
    FinalScore = final_score,
    Outcome = outcome,
    GameFormat = format
  )
  return(rowForTable)
}

