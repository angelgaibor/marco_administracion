#
rm(list=ls())
#
library(tidyverse)
#
load("D:/Disco_F/ENDI2/insumos/03_muestra_usm/dpa_2022.RData")
rm(parroquia, canton)

nombre <- last(list.files("productos/01_general/", pattern = ".rds"))

marco <- readRDS(paste0("productos/01_general/",nombre))

tab01 <- marco %>% 
  mutate(cant = substr(id_upm, 1, 4),
         parr = substr(id_upm, 1, 6)) %>% 
  group_by(provin = pro) %>% 
  summarise(n_cant = n_distinct(cant),
            n_parr = n_distinct(parr)) %>% 
  ungroup()  %>% 
  left_join(provincia, by = "provin") %>% 
  rbind(data.frame(nprovin = "Total Nacional",
                   provin = "-",
                   n_cant = sum(.$n_cant),
                   n_parr = sum(.$n_parr))) %>%
  mutate(nprovin = str_to_title(nprovin)) %>% 
  select(nprovin, provin, n_cant, n_parr)

tab01 <- tab01 %>%
  mutate(colm = paste(nprovin, " & ", provin, " & ", n_cant, " & ", n_parr, "\\\\" ),
         colm = gsub("\\.", ",", colm)) %>%
  select(colm)

write.table(tab01, "clipboard", row.names = F, col.names = F, quote = F)
