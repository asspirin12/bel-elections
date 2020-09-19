library(tidyverse)
library(readxl)
library(bbplot2)

mogilev <- read_csv("raw_data/mogil_obl.csv")

mogilev_long <-
  mogilev %>%
  select(luka_perc, tikha_perc, station_code, district) %>%
  gather(key = "candidate", value = "perc", -station_code, -district)

mogilev_long$candidate <- str_replace(mogilev_long$candidate, "luka_perc", 
                                      "Александр Лукашенко")

mogilev_long$candidate <- str_replace(mogilev_long$candidate, "tikha_perc", 
                                      "Светлана Тихановская")

votes <- mogilev_long %>%
  ggplot +
  geom_point(aes(x = station_code, y = perc, color = candidate)) +
  geom_vline(xintercept = "06-023-0088", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "06-024-0084", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "02-075-0075", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "02-076-0040", size = 1, color = "#dddddd")+
  geom_hline(yintercept = 0, color = "#333333", size = 1) +
  bbc_style() +
  theme(axis.text.x = element_blank()) +
  labs(title = "Распределение голосов в Могилевской\nобласти по участкам",
       subtitle = "Доля голосов в %") +
  scale_color_manual(
    values = c('#1380a1', '#ffa31e')
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  geom_label(
    aes(x = '06-023-0135', y = 95, label = "Могилев"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 6
  ) 

cairo_pdf("pdf_charts/votes_mogilev.pdf", family = "Arial", width = 640/72, height = 450/72)
print(votes)
dev.off()
