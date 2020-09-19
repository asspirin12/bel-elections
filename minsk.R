library(tidyverse)
library(readxl)
library(bbplot2)
library(grDevices)

minsk <- read_csv("raw_data/minsk.csv")

#### ЯВКА МИНСК ####

early <- minsk %>%
  ggplot +
  geom_point(aes(x = station_code, y = early_perc), color = '#990000') +
  geom_vline(xintercept = "07-002-0001", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "07-003-0002", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "07-004-0002", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "07-005-0002", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "07-006-0001", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "07-007-0001", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "07-008-0006", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "07-009-0001", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "07-102-0075", size = 1, color = "#dddddd")+
  geom_hline(yintercept = 0, color = "#333333", size = 1) +
  bbc_style() +
  scale_y_continuous(limits = c(0, 100)) +
  theme(axis.text.x = element_blank(),
        legend.position = "none") +
  labs(title = "Досрочная явка в Минске и за рубежом\nпо участкам",
       subtitle = "Доля проголосовавших досрочно в %") + 
  geom_label(
    aes(x = '07-001-0011', y = 65, label = "Заводской\nрайон"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 4
  ) +
  geom_label(
    aes(x = '07-002-0007', y = 85, label = "Ленинский\nрайон"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 4
  ) +
  geom_label(
    aes(x = '07-008-0099', y = 95, label = "Зарубежные\nучастки"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 4
  ) +
  geom_segment(
    aes(
      x = '07-102-0078',
      y = 90,
      xend = '07-102-0094',
      yend = 75
    ),
    colour = "#333333",
    size = 0.2
  ) +
  geom_label(
    aes(x = '07-003-0002', y = 65, label = "Московский\nрайон"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 4
  ) +
  geom_label(
    aes(x = '07-003-0095', y = 50, label = "Октябрьский\nрайон"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 4
  ) +
  geom_label(
    aes(x = '07-004-0044', y = 95, label = "Партизанский\nрайон"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 4
  )  +
  geom_segment(
    aes(
      x = '07-005-0020',
      y = 87,
      xend = '07-005-0020',
      yend = 75
    ),
    colour = "#333333",
    size = 0.2
  ) +
  geom_label(
    aes(x = '07-006-0002', y = 75, label = "Первомайский\nрайон"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 4
  ) +
  geom_label(
    aes(x = '07-007-0004', y = 65, label = "Советский\nрайон"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 4
  ) +
  geom_label(
    aes(x = '07-008-0014', y = 55, label = "Фрунзенский\nрайон"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 4
  ) +
  geom_segment(
    aes(
      x = '07-008-0110',
      y = 75,
      xend = '07-009-0023',
      yend = 65
    ),
    colour = "#333333",
    size = 0.2
  ) +
  geom_label(
    aes(x = '07-008-0014', y = 75, label = "Центральный\nрайон"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 4
  ) 

cairo_pdf("pdf_charts/early_minsk.pdf", family = "Arial", width = 640/72, height = 450/72)
print(early)
dev.off()

### ЯВКА КРОМЕ МИНСКА ###

all <- read_csv("clean_data/all_regions_clean.csv")
not_minsk <- all %>% filter(region != "Минск")
early_not_minsk <- not_minsk %>%
  ggplot +
  geom_point(aes(x = station_code, y = early_perc), color = '#990000') +
  geom_hline(yintercept = 0, color = "#333333", size = 1) +
  bbc_style() +
  scale_y_continuous(limits = c(0, 100)) +
  theme(axis.text.x = element_blank(),
        legend.position = "none") +
  labs(title = "Досрочная явка в Беларуси \nза исключением Минска",
       subtitle = "Доля проголосовавших досрочно в %") +
  geom_vline(xintercept = "01-011-0089", size = 1, color = "#dddddd") +
  geom_vline(xintercept = "02-014-0003", size = 1, color = "#dddddd") +
  geom_vline(xintercept = "02-016-0071", size = 1, color = "#dddddd") +
  geom_vline(xintercept = "03-017-0002", size = 1, color = "#dddddd") +
  geom_vline(xintercept = "03-020-0039", size = 1, color = "#dddddd") +
  geom_vline(xintercept = "04-021-0002", size = 1, color = "#dddddd") +
  geom_vline(xintercept = "04-022-0135", size = 1, color = "#dddddd") +
  geom_vline(xintercept = "06-023-0088", size = 1, color = "#dddddd") +
  geom_vline(xintercept = "06-024-0084", size = 1, color = "#dddddd") +
  geom_label(
    aes(x = '01-010-0004', y = 80, label = "Брест"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 4
  ) +
  geom_label(
    aes(x = '01-060-0056', y = 80, label = "Витебск"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 4
  ) +
  geom_label(
    aes(x = '02-077-0007', y = 80, label = "Гомель"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 4
  ) +
  geom_label(
    aes(x = '04-021-0002', y = 80, label = "Гродно"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 4
  ) +
  geom_label(
    aes(x = '06-012-0077', y = 80, label = "Могилев"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 4
  ) 
  

cairo_pdf("pdf_charts/early_not_minsk.pdf", family = "Arial", width = 640/72, height = 450/72)
print(early_not_minsk)
dev.off()
  
### ГОЛОСОВАНИЕ ГРАФИК ###

minsk_long <-
  minsk %>%
  select(luka_perc, tikha_perc, station_code, district) %>%
  gather(key = "candidate", value = "perc", -station_code, -district)

minsk_long$candidate <- str_replace(minsk_long$candidate, "luka_perc", 
                                    "Александр Лукашенко")

minsk_long$candidate <- str_replace(minsk_long$candidate, "tikha_perc", 
                                    "Светлана Тихановская")

votes <- minsk_long %>%
  ggplot +
  geom_point(aes(x = station_code, y = perc, color = candidate)) +
  geom_vline(xintercept = "07-002-0001", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "07-003-0002", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "07-004-0002", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "07-005-0002", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "07-006-0001", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "07-007-0001", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "07-008-0006", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "07-009-0001", size = 1, color = "#dddddd")+
  geom_vline(xintercept = "07-102-0075", size = 1, color = "#dddddd")+
  geom_hline(yintercept = 0, color = "#333333", size = 1) +
  bbc_style() +
  theme(axis.text.x = element_blank()) +
  labs(title = "Распределение голосов в Минске\nи за рубежом по участкам",
       subtitle = "Доля голосов в %") +
  scale_color_manual(
    values = c('#1380a1', '#ffa31e')
  ) +
  scale_y_continuous(limits = c(0, 150),breaks = seq(0, 100, 25)) +
  geom_label(
    aes(x = '07-001-0011', y = 95, label = "Заводской\nрайон"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 4
  ) +
  geom_label(
    aes(x = '07-002-0007', y = 115, label = "Ленинский\nрайон"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 4
  ) +
  geom_label(
    aes(x = '07-008-0099', y = 145, label = "Зарубежные\nучастки"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 4
  ) +
  geom_segment(
    aes(
      x = '07-102-0078',
      y = 130,
      xend = '07-102-0094',
      yend = 115
    ),
    colour = "#333333",
    size = 0.2
  ) +
  geom_label(
    aes(x = '07-003-0002', y = 100, label = "Московский\nрайон"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 4
  ) +
  geom_label(
    aes(x = '07-003-0095', y = 85, label = "Октябрьский\nрайон"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 4
  ) +
  geom_label(
    aes(x = '07-004-0044', y = 125, label = "Партизанский\nрайон"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 4
  )  +
  geom_segment(
    aes(
      x = '07-005-0020',
      y = 113,
      xend = '07-005-0020',
      yend = 100
    ),
    colour = "#333333",
    size = 0.2
  ) +
  geom_label(
    aes(x = '07-006-0002', y = 105, label = "Первомайский\nрайон"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 4
  ) +
  geom_label(
    aes(x = '07-007-0004', y = 95, label = "Советский\nрайон"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 4
  ) +
  geom_label(
    aes(x = '07-008-0014', y = 95, label = "Фрунзенский\nрайон"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 4
  ) +
  geom_segment(
    aes(
      x = '07-008-0110',
      y = 125,
      xend = '07-009-0023',
      yend = 115
    ),
    colour = "#333333",
    size = 0.2
  ) +
  geom_label(
    aes(x = '07-008-0014', y = 125, label = "Центральный\nрайон"),
    hjust = 0,
    vjust = 0.5,
    colour = "#333333",
    fill = "white",
    label.size = NA,
    family = "Arial",
    size = 4
  ) 


cairo_pdf("pdf_charts/votes_minsk.pdf", family = "Arial", width = 640/72, height = 450/72)
print(votes)
dev.off()
