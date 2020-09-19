library(tidyverse)
library(readxl)
library(bbplot2)

vitebsk <- read_csv("raw_data/viteb_obl.csv")

vitebsk_long <-
  vitebsk %>%
  select(luka_perc, tikha_perc, station_code, district) %>%
  gather(key = "candidate", value = "perc", -station_code, -district)

vitebsk_long$candidate <- str_replace(vitebsk_long$candidate, "luka_perc", 
                                    "Александр Лукашенко")

vitebsk_long$candidate <- str_replace(vitebsk_long$candidate, "tikha_perc", 
                                    "Светлана Тихановская")

votes <- vitebsk_long %>%
  ggplot +
  geom_point(aes(x = station_code, y = perc, color = candidate)) +
  geom_vline(xintercept = "02-016-0071", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "02-075-0001", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "02-075-0075", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "02-076-0040", size = 1, color = "#dddddd")+
  geom_hline(yintercept = 0, color = "#333333", size = 1) +
  bbc_style() +
  theme(axis.text.x = element_blank()) +
  labs(title = "Распределение голосов в Витебской\nобласти по участкам",
       subtitle = "Доля голосов в %") +
  scale_color_manual(
    values = c('#1380a1', '#ffa31e')
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  geom_label(
    aes(x = '02-015-0031', y = 95, label = "Витебск"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 6
  ) +
  geom_label(
    aes(x = '02-075-0010', y = 95, label = "Орша"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 6
  ) +
  geom_label(
    aes(x = '02-077-0001', y = 100, label = "Полоцк"),
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
      x = '02-081-0001',
      y = 95,
      xend = '02-076-0020',
      yend = 90
    ),
    colour = "#333333",
    size = 0.5
  )

cairo_pdf("pdf_charts/votes_vitebsk.pdf", family = "Arial", width = 640/72, height = 450/72)
print(votes)
dev.off()
