pacman::p_load(tidyverse, data.table, rio, rvest)
Elo =
  "http://tennisabstract.com/reports/atp_elo_ratings.html" %>%
  read_html() %>%
  html_table(fill = T) %>% .[[5]] %>%
  discard(.p = ~ all(is.na(.)))

Tableau =
  "http://tennisabstract.com/current/2020AustralianOpenMenForecast.html" %>%
  read_html() %>%
  html_table(fill = T) %>% .[[4]] %>% filter(str_detect(string = X3, pattern = "%")) %>%
  select(Player = X1) %>% rowid_to_column() %>%
  tidyr::extract(col = Player,
                 into = c("Player1", "Country"),
                 regex = "(.*) \\((.*)\\)") %>%
  tidyr::extract(
    col = Player1,
    into = c("Seed", "Player"),
    regex = "\\((.*)\\)(.*)",
    remove = F
  ) %>%
  mutate(Player = if_else(
    condition = is.na(Player),
    true = Player1,
    false = Player
  )) %>%
  select(-Player1)

Play_ROUND <- function(Tableau) {
  input =
    left_join(
      x = Tableau %>%
        mutate(
          Code = Player %>%
            str_to_lower() %>% stringi::stri_trans_general(id = "latin-ascii") %>%
            str_remove_all(pattern = "[^[:alpha:]]")
        ),
      y = Elo %>%
        mutate(
          Code = Player %>%
            str_to_lower() %>% stringi::stri_trans_general(id = "latin-ascii") %>%
            str_remove_all(pattern = "[^[:alpha:]]")
        ),
      by = "Code"
    )
  output =
    input %>%
    select(rowid, Player = Player.x, hElo) %>%
    mutate(hElo = if_else(
      condition = is.na(hElo),
      true = 1400,
      false = hElo
    ))
  
  pacman::p_load(elo)
  N = nrow(output)
  Probas =
    list(output %>% slice(seq(1, N, 2)),
         output %>% slice(seq(2, N, 2))) %>%
    reduce(.f = cbind.data.frame) %>% repair_names() %>%
    mutate(Proba = elo::elo.prob(hElo, hElo1))
  Probas$toss = runif(n = N / 2)
  Probas =
    Probas %>%
    mutate(Winner = if_else(
      condition = Proba > toss,
      true = Player,
      false = Player1
    ))
  
  Probas %>% select(Player = Winner) %>%
    semi_join(x = Tableau, by = "Player") %>%
    return()
}

Play_Tournament <- function(Tableau, N_sim) {
  R_128 = Tableau %>% select(rowid, Player)
  R_64 = R_128 %>% Play_ROUND()
  R_32 = R_64 %>% Play_ROUND()
  R_16 = R_32 %>% Play_ROUND()
  QF = R_16 %>% Play_ROUND()
  SF = QF %>% Play_ROUND()
  FINAL = SF %>% Play_ROUND()
  Winner = FINAL %>% Play_ROUND()
  
  list(
    R_128 = R_128,
    R_64 = R_64,
    R_32 = R_32,
    R_16 = R_16,
    QF = QF,
    SF = SF,
    FINAL = FINAL,
    Winner = Winner
  ) %>%
    return()
}

Sim_MC <- function(N_Sim) {
  Monte_Carlo =
    1:N_Sim %>% set_names() %>%
    map(.f = Play_Tournament, Tableau = Tableau)
  
  
  Monte_Carlo =
    Monte_Carlo %>% purrr::transpose() %>%
    map(bind_rows, .id = "N_sim") %>%
    map(.f = count, rowid, Player) %>%
    bind_rows(.id = "Round") %>%
    mutate(n = n / max(n))
  
  Monte_Carlo$Round =
    factor(
      x = Monte_Carlo$Round,
      levels = c("R_128",
                 "R_64",
                 "R_32",
                 "R_16",
                 "QF",
                 "SF",
                 "FINAL",
                 "Winner")
    )
  Monte_Carlo %>% spread(key = Round,
                         value = n,
                         fill = 0) %>%
    data.table() %>%
    return()
}

require(foreach)
Probas = list()
foreach::foreach(N = 1:1e5) %do% {
  print(N)
  Probas[[length(Probas) + 1]] = Play_Tournament(Tableau = Tableau)
  Monte_Carlo =
    Probas %>% set_names(nm = 1:N) %>%
    purrr::transpose() %>%
    map(bind_rows, .id = "N_sim") %>%
    map(.f = count, rowid, Player) %>%
    bind_rows(.id = "Round") %>%
    mutate(n = n / max(n))
  
  Monte_Carlo$Round =
    factor(
      x = Monte_Carlo$Round,
      levels = c("R_128",
                 "R_64",
                 "R_32",
                 "R_16",
                 "QF",
                 "SF",
                 "FINAL",
                 "Winner")
    )
  
  plot =
    Monte_Carlo %>%
    ggplot(mapping = aes(x = Round, y = n)) +
    geom_text(mapping = aes(label = Player)) +
    ylim(1 / 2, 1)
  
  print(plot)
  Sys.sleep(time = 1)
}

Play_Tournament(Tableau = Tableau)