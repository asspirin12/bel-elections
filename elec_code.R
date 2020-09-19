library(tidyverse)
library(readxl)


elec0 <- read_xlsx("raw_data/Honest People Protocols Data.xlsx") 
elec1 <- elec0[1:27]
elec2 <- elec1[-c(7, 22)]

names(elec2) <-
  c(
    'region',
    'district',
    'town',
    'address',
    'station_num',
    'station_code',
    'total_const',
    'received_ballot',
    'voted_total',
    'voted_early',
    'early_perc',
    'voted_home',
    'voted_at_station',
    'dmitr',
    'kanop',
    'luka',
    'luka_perc',
    'tikha',
    'tikha_perc',
    'chere',
    'against_all',
    'invalid_ballots',
    'received_by_comission',
    'damaged_ballots',
    'unused_ballots'
  )

write_csv(elec2, "clean_data/all_regions_clean.csv")

brest_obl <- elec2 %>% filter(region == "Брестская")
viteb_obl <- elec2 %>% filter(region == "Витебская")
gomel_obl <- elec2 %>% filter(region == "Гомельская")
grodno_obl <- elec2 %>% filter(region == "Гродненская")
mogil_obl <- elec2 %>% filter(region == "Могилевская")
min_obl <- elec2 %>% filter(region == "Минская")
minsk <- elec2 %>% filter(region == "Минск")

write_csv(brest_obl, 'raw_data/brest_obl.csv')
write_csv(viteb_obl, 'raw_data/viteb_obl.csv')
write_csv(gomel_obl, 'raw_data/gomel_obl.csv')
write_csv(grodno_obl, 'raw_data/grodno_obl.csv')
write_csv(mogil_obl, 'raw_data/mogil_obl.csv')
write_csv(min_obl, 'raw_data/min_obl.csv')
write_csv(minsk, 'raw_data/minsk.csv')

### COUNT 100% ###
elec2 <- elec2 %>% replace(is.na(.), 0)
test <- elec2 %>%
  mutate(hundred_perc = (chere / voted_total * 100) +
           (dmitr / voted_total * 100) +
           (kanop / voted_total * 100) +
           (against_all / voted_total * 100) +
           (invalid_ballots / voted_total * 100) +
           luka_perc + tikha_perc) %>% drop_na()

test <- test %>% arrange(hundred_perc)


# делаем колонку с разностью между голосами 
test <-
  test %>% mutate(
    diff = test$dmitr + test$kanop + test$luka + test$tikha + test$against_all +
      test$invalid_ballots + test$chere - test$voted_total
  )
### CHART ###

elec_long <-
  elec2 %>%
  select(luka_perc, tikha_perc, station_code, district) %>%
  gather(key = "candidate", value = "perc", -station_code, -district)

elec_long$candidate <- str_replace(elec_long$candidate, "luka_perc", 
                                   "Александр Лукашенко")

elec_long$candidate <- str_replace(elec_long$candidate, "tikha_perc", 
                                   "Светлана Тихановская")

votes <- elec_long %>%
  ggplot +
  geom_point(aes(x = station_code, y = perc, color = candidate)) +
  geom_hline(yintercept = 0, color = "#333333", size = 1) +
  bbc_style() +
  theme(axis.text.x = element_blank()) +
  labs(title = "Распределение голосов в Беларуси по участкам",
       subtitle = "Доля голосов в %") +
  scale_color_manual(
    values = c('#1380a1', '#ffa31e')
  )

votes

cairo_pdf("pdf_charts/votes_belarus.pdf", family = "Arial", width = 640/72, height = 450/72)
print(votes)
dev.off()