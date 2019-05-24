require(rvest)
Table = "http://tennisabstract.com/reports/atp_elo_ratings.html" %>%
  read_html() %>%
  html_table(fill = TRUE)

require(data.table)
Table = Table[[5]] %>%
  data.table() %>% setkey()

Table$Old = Table$Age - Table$`Peak Age`

is_faded = Table$Old > median(Table$Old, na.rm = T)
is_old = Table$Age > median(Table$Age, na.rm = T)

require(ggplot2)
require(ggrepel)
Table[is_faded & !is_old] %>%
  ggplot(mapping = aes(x = Elo, y = Age)) +
  geom_point() +
  geom_text_repel(mapping = aes(label = Player))

Table[!is_faded & is_old] %>%
  ggplot(mapping = aes(x = Elo, y = Age)) +
  geom_point() +
  geom_text_repel(mapping = aes(label = Player))

Table %>%
  ggplot(mapping = aes(x = `Peak Elo`, y = Elo, colour = Age)) +
  geom_point() +
  geom_abline() +
  geom_text_repel(mapping = aes(label = Player))

require(tidyverse)
Surfaces =
Table %>% select(Player, Age, Hard, Clay, Grass) %>%
  mutate_at(.vars = c("Age", "Hard", "Clay", "Grass"), .funs = as.numeric) %>%
  gather(key = Surface, value = Elo, Hard, Clay, Grass, na.rm = T)

Surfaces %>%
  ggplot(mapping = aes(y = Elo, x = Player, colour = Surface)) +
  geom_point() +
  coord_flip()

Surfaces %>%
  group_by(Player) %>% filter(Elo == max(Elo)) %>% ungroup %>%
  split(x = .$Player, f = .$Surface)

Surfaces %>%
  group_by(Player) %>% summarise(M = mean(Elo),
                                 SD = sd(Elo)) %>% view()
