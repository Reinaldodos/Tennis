pacman::p_load(pacman, data.table, rvest, tidyverse, rio, lubridate)
"~/R/Anime/" %>% list.files(ignore.case = T, pattern = "fonction", full.names = T, recursive = T) %>%
  map(source)

url = "https://www.atptour.com/en/rankings/singles"

Start = ("2019-02-11" %>% ymd)
N = 0:2500

Liste = Start - N*ddays(7)
Liste = Liste[Liste >= ymd("1973-08-23")]
Liste =
  data.table(Week = Liste) %>%
  mutate(url = str_c(url, "?rankDate=", Week, "&rankRange=0-100"))

FAITCHE = compose(flatten_df, html_table, read_html)
safe_FAITCHE = safely(FAITCHE)

input =
  Liste$url %>% set_names() %>%
  map(safe_FAITCHE)

output =
  input %>%
  map(compact) %>%
  transpose()


test =
  output$result %>%
  map(mutate_all, as.character) %>%
  bind_rows(.id = "url")

output =
  test %>% inner_join(Liste) %>%
  mutate(
    Points = Points %>% str_remove_all(pattern = "[^[0-9]]") %>% as.numeric(),
    Ranking = Ranking %>% str_remove_all(pattern = "[^[0-9]]") %>% as.numeric()
  ) %>% data.table()

Elite = output %>% filter(Ranking == 1)  %>% pull(Player) %>% unique()
GOD_start =
  output %>%
  filter(str_detect(string = Player, pattern = "Federer")) %>%
  group_by(Player) %>% filter(Week == min(Week)) %>% pull(Week)

output %>%
  filter(Player %in% Elite) %>%
  select(Week, Ranking, Player) %>%
  ggplot(mapping = aes(x = Week, y = Ranking, colour = Player)) +
  geom_smooth(se = F)+
  scale_y_reverse()+
  ylim(100,1)+
  # xlim(GOD_start, NA) +
  theme(legend.position = "bottom")
