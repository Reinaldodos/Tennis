pacman::p_load(pacman, data.table, rvest, tidyverse, rio, lubridate)

url = "https://www.atptour.com/en/rankings/singles"

IMA = now() %>% as_date()
IMA = IMA - ddays(0:7)
Start = IMA[lubridate::wday(IMA) == 2]
N = 0:2500

Liste = Start - N*ddays(7)
Liste = Liste[Liste >= ymd("1973-08-23")]
Liste =
  data.table(Week = Liste) %>%
  mutate(url = str_c(url, "?rankDate=", Week, "&rankRange=0-100"))

safe_FAITCHE = safely(read_html)

input =
  Liste$url %>% set_names() %>% 
  map(safe_FAITCHE)

output =
  input %>%
  map(compact) %>%
  purrr::transpose()


test =
  output$result %>%
  compact %>%
  map(html_table) %>%
  map(flatten_df) %>%
  map(mutate_all, as.character) %>%
  bind_rows(.id = "url")

output =
  test %>% inner_join(Liste) %>%
  mutate(
    Points = Points %>% str_remove_all(pattern = "[^[0-9]]") %>% as.numeric(),
    Ranking = Ranking %>% str_remove_all(pattern = "[^[0-9]]") %>% as.numeric()
  ) %>% data.table()

Elite = output %>% filter(Ranking <= 1)  %>% pull(Player) %>% unique()
GOD_start =
  output %>%
  group_by(Player) %>% filter(Week == min(Week)) %>% 
  filter(str_detect(string = Player, pattern = "Federer")) %>%
  pull(Week)

output %>%
  filter(Player %in% Elite) %>%
  select(Week, Ranking, Player) %>%
  ggplot(mapping = aes(x = Week, y = Ranking, colour = Player)) +
  geom_smooth(se = F) +
  scale_y_reverse() +
  ylim(100, 1) +
  geom_vline(xintercept = GOD_start, colour = "red") +
  geom_hline(yintercept = 2) +
  theme(legend.position = "bottom")
