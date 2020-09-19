library(tidyverse)
library(readxl)
library(bbplot2)
library(grDevices)

brest <- read_csv("raw_data/brest_obl.csv")

brest_long <-
  brest %>%
  select(luka_perc, tikha_perc, station_code, district) %>%
  gather(key = "candidate", value = "perc", -station_code, -district)

brest_long$candidate <- str_replace(brest_long$candidate, "luka_perc", 
                                    "Александр Лукашенко")

brest_long$candidate <- str_replace(brest_long$candidate, "tikha_perc", 
                                    "Светлана Тихановская")

votes <- brest_long %>%
  ggplot +
  geom_point(aes(x = station_code, y = perc, color = candidate)) +
  #geom_vline(xintercept = "01-010-0001", size = 1, color = "#dddddd")+
  #geom_vline(xintercept = "01-011-0003", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "01-047-0004", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "01-049-0004", size = 1, color = "#dddddd")+
  # geom_vline(xintercept = "01-050-0001", size = 1, color = "#dddddd")+
  # geom_vline(xintercept = "01-051-0003", size = 1, color = "#dddddd")+
  # geom_vline(xintercept = "01-052-0007", size = 1, color = "#dddddd")+
  # geom_vline(xintercept = "01-053-0021", size = 1, color = "#dddddd")+
  # geom_vline(xintercept = "01-054-0001", size = 1, color = "#dddddd")+
  # geom_vline(xintercept = "01-055-0002", size = 1, color = "#dddddd")+
  # geom_vline(xintercept = "01-056-0005", size = 1, color = "#dddddd")+
  # geom_vline(xintercept = "01-057-0002", size = 1, color = "#dddddd")+
  # geom_vline(xintercept = "01-058-0008", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "01-060-0010", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "01-062-0009", size = 1, color = "#dddddd")+
  # geom_vline(xintercept = "01-063-0010", size = 1, color = "#dddddd")+
  # geom_vline(xintercept = "01-142-0002", size = 1, color = "#dddddd")+
  geom_hline(yintercept = 0, color = "#333333", size = 1) +
  bbc_style() +
  theme(axis.text.x = element_blank()) +
  labs(title = "Распределение голосов в Брестской\nобласти по участкам",
       subtitle = "Доля голосов в %") +
  scale_color_manual(
    values = c('#1380a1', '#ffa31e')
  ) +
  scale_y_continuous(limits = c(0, 100)) +
  geom_label(
    aes(x = '01-011-0003', y = 95, label = "Брест"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 6
  ) +
  geom_label(
    aes(x = '01-011-0089', y = 95, label = "Барановичи"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 6
  ) +
  geom_label(
    aes(x = '01-058-0008', y = 95, label = "Пинск"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 6
  ) 
  
cairo_pdf("pdf_charts/votes_brest.pdf", family = "Arial", width = 640/72, height = 450/72)
print(votes)
dev.off()
