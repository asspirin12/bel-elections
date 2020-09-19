library(tidyverse)
library(readxl)
library(bbplot2)

grodno <- read_csv("raw_data/grodno_obl.csv")

grodno_long <-
  grodno %>%
  select(luka_perc, tikha_perc, station_code, district) %>%
  gather(key = "candidate", value = "perc", -station_code, -district)

grodno_long$candidate <- str_replace(grodno_long$candidate, "luka_perc", 
                                      "Александр Лукашенко")

grodno_long$candidate <- str_replace(grodno_long$candidate, "tikha_perc", 
                                      "Светлана Тихановская")

votes <- grodno_long %>%
  ggplot +
  geom_point(aes(x = station_code, y = perc, color = candidate)) +
  geom_vline(xintercept = "04-022-0135", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "04-110-0001", size = 1, color = "#dddddd")+
  geom_hline(yintercept = 0, color = "#333333", size = 1) +
  bbc_style() +
  theme(axis.text.x = element_blank()) +
  labs(title = "Распределение голосов в Гродненской\nобласти по участкам",
       subtitle = "Доля голосов в %") +
  scale_color_manual(
    values = c('#1380a1', '#ffa31e')
  ) +
  scale_y_continuous(limits = c(0, 110),
                     breaks = seq(0, 100, 25)) +
  geom_label(
    aes(x = '04-022-0077', y = 95, label = "Гродно"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 6
  ) +
  geom_label(
    aes(x = '04-112-0004', y = 97, label = "Волковысский\nи Дятловский\nрайоны"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 6
  ) +
  geom_segment(
    aes(
      x = '04-112-0003',
      y = 90,
      xend = '04-107-0001',
      yend = 75
    ),
    colour = "#333333",
    size = 0.5
  )

cairo_pdf("pdf_charts/votes_grodno.pdf", family = "Arial", width = 640/72, height = 450/72)
print(votes)
dev.off()
