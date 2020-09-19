library(tidyverse)
library(readxl)
library(bbplot2)

gomel <- read_csv("raw_data/gomel_obl.csv")


gomel_long <-
  gomel %>%
  select(luka_perc, tikha_perc, station_code, district) %>%
  gather(key = "candidate", value = "perc", -station_code, -district)

gomel_long$candidate <- str_replace(gomel_long$candidate, "luka_perc", 
                                    "Александр Лукашенко")

gomel_long$candidate <- str_replace(gomel_long$candidate, "tikha_perc", 
                                    "Светлана Тихановская")

votes <- gomel_long %>%
  ggplot +
  geom_point(aes(x = station_code, y = perc, color = candidate)) +
  #geom_vline(xintercept = "01-010-0001", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "03-020-0039", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "03-102-0002", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "03-100-0004", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "03-100-0028", size = 1, color = "#dddddd")+
  geom_hline(yintercept = 0, color = "#333333", size = 1) +
  bbc_style() +
  theme(axis.text.x = element_blank()) +
  labs(title = "Распределение голосов в Гомельской\nобласти по участкам",
       subtitle = "Доля голосов в %") +
  scale_color_manual(
    values = c('#1380a1', '#ffa31e')
  ) +
  scale_y_continuous(limits = c(0, 110),
                     breaks = seq(0, 100, 25)) +
  geom_label(aes(x = '03-018-0028', y = 95, label = "Гомель"), 
             hjust = 0, 
             vjust = 0.5, 
             colour = "#333333", 
             fill = "white", 
             label.size = NA, 
             family="Arial", 
             size = 6) +
  geom_label(aes(x = '03-096-0045', y = 100, label = "Речица"), 
             hjust = 0, 
             vjust = 0.5, 
             colour = "#333333", 
             fill = "white", 
             label.size = NA, 
             family="Arial", 
             size = 6) +
  geom_label(aes(x = '03-100-0010', y = 110, label = "Светлогорск"), 
           hjust = 0, 
           vjust = 0.5, 
           colour = "#333333", 
           fill = "white", 
           label.size = NA, 
           family="Arial", 
           size = 6) +
  geom_segment(
    aes(
      x = '03-101-0015',
      y = 105,
      xend = '03-102-0015',
      yend = 90
    ),
    colour = "#333333",
    size = 0.5
  )
  

cairo_pdf("pdf_charts/votes_gomel.pdf", family = "Arial", width = 640/72, height = 450/72)
print(votes)
dev.off()
