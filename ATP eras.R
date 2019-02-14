pacman::p_load(tidyverse, data.table, rvest)
url = "https://en.wikipedia.org/wiki/List_of_Open_Era_Grand_Slam_men%27s_singles_finals"
input =
  url %>% read_html() %>%
  html_table(fill = T)
input =
  input[[3]]
names(input)=input[1,] %>% flatten_chr()

input =
  input %>%
  select(Year, Championship, Champion, `Runner-up`) %>%
  filter(!is.na(as.numeric(Year))) %>%
  mutate(SLAM = row_number())  %>%
  mutate_at(
    .vars = c("Champion", "Runner-up"),
    .funs = ~ str_remove_all(
      string = stringi::stri_trans_general(str = ., "Latin-ASCII"),
      pattern = "[^[A-Z a-z]]"
    )
  ) %>%
  mutate_at(
    .vars = c("Champion", "Runner-up"),
    .funs = ~ str_remove_all(
      string = stringi::stri_trans_general(str = ., "Latin-ASCII"),
      pattern = " WC"
    )
  ) %>%
  mutate_all(.funs = str_trim)

REZO <- function(input, N) {
  pacman::p_load(igraph, ggraph)
  toto = input %>% slice(N:(N+3))
  Annee = toto$Year %>% unique() %>% paste(collapse = "-")
  jpeg(filename = str_c(N, ".jpeg"), )
  plott =
    toto %>%
    select(Champion, `Runner-up`) %>%
    as.matrix() %>%
    graph_from_edgelist(directed = T) %>%
    simplify(remove.multiple = T, remove.loops = T) %>%
    ggraph::ggraph(layout = "tree") +
    geom_edge_link() +
    geom_node_text(mapping = aes(label = name), repel = TRUE)+
    ggforce::theme_no_axes() +
    ggtitle(label = Annee)
    print(plott)
  dev.off()
}

1:(nrow(input)-3) %>% map(.f = REZO, input = input)
