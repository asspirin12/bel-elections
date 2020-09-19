library(tidyverse)
library(readxl)
library(bbplot2)

minobl <- read_csv("raw_data/min_obl.csv")

minobl_long <-
  minobl %>%
  select(luka_perc, tikha_perc, station_code, district) %>%
  gather(key = "candidate", value = "perc", -station_code, -district)

minobl_long$candidate <- str_replace(minobl_long$candidate, "luka_perc", 
                                     "Александр Лукашенко")

minobl_long$candidate <- str_replace(minobl_long$candidate, "tikha_perc", 
                                     "Светлана Тихановская")

votes <- minobl_long %>%
  ggplot +
  geom_point(aes(x = station_code, y = perc, color = candidate)) +
  geom_vline(xintercept = "05-029-0034", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "05-032-0020", size = 1, color = "#dddddd")+
  #geom_vline(xintercept = "05-039-0050", size = 1, color = "#dddddd")+
  #geom_vline(xintercept = "05-040-0063", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "05-041-0035", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "05-042-0033", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "", size = 1, color = "#dddddd")+
  geom_hline(yintercept = 0, color = "#333333", size = 1) +
  bbc_style() +
  theme(axis.text.x = element_blank()) +
  labs(title = "Распределение голосов в Минской\nобласти по участкам",
       subtitle = "Доля голосов в %") +
  scale_color_manual(
    values = c('#1380a1', '#ffa31e')
  ) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, 25)) +
  geom_label(
    aes(x = '05-027-0023', y = 95, label = "Жодино"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 6
  ) +
  geom_label(
    aes(x = '05-041-0012', y = 95, label = "Солигорск"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 6
  )

cairo_pdf("pdf_charts/votes_minobl.pdf", family = "Arial", width = 640/72, height = 450/72)
print(votes)
dev.off() 
