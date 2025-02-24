library(tidyverse)

options(scipen = 999)

lol <- marco_upm_10 %>%
  filter(pro == "16", area == "1", enighur_orden%%19==1) %>% 
  select(id_upm, nap, enighur_pareto, enighur_sel) %>% 
  arrange(enighur_pareto)

write.table(lol, "clipboard", sep = "\t", dec = ",", row.names = F)
