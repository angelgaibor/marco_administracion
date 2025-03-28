rm(list = ls())

library(tidyverse)
library(rio)
library(janitor)

marco22 <- readRDS("productos/01_general/marco_upm_12.rds") %>% 
  select(id_upm, estrato, area, viv = Mi)

marco10 <- readRDS("../ENDI/insumos/02_muestra_upm/20210617_marco_upm.rds") %>% 
  mutate(id_conglomerado = case_when(substr(id_conglomerado, 1, 2) == "20" ~ 
                                       paste0(substr(id_upm, 1, 7), substr(id_upm, 10, 12)),
                                     T ~ id_conglomerado)) %>% 
  group_by(id_upm = id_conglomerado, estrato, area) %>% 
  summarise(viv = sum(viv))

tabla01 <- marco10 %>% 
  group_by(area) %>% 
  summarise(viv = sum(viv),
            upm = n()) %>% 
  mutate(porviv = round(100 * viv / sum(viv), 2),
         porupm = round(100 * upm / sum(upm), 2)) %>% 
  adorn_totals() %>% 
  mutate(marco = "2010") %>% 
  select(marco, area, viv, porviv, upm, porupm) %>% 
  rbind(marco22 %>% 
          group_by(area) %>% 
          summarise(viv = sum(viv),
                    upm = n()) %>% 
          mutate(porviv = round(100 * viv / sum(viv), 2),
                 porupm = round(100 * upm / sum(upm), 2)) %>% 
          adorn_totals() %>% 
          mutate(marco = "2022") %>% 
          select(marco, area, viv, porviv, upm, porupm))

tabla02 <- marco10 %>% 
  group_by(pro = substr(id_upm, 1, 2)) %>% 
  summarise(est = n_distinct(estrato),
            viv = sum(viv),
            upm = n()) %>% 
  mutate(porviv = round(100 * viv / sum(viv), 2),
         porupm = round(100 * upm / sum(upm), 2)) %>% 
  adorn_totals() %>% 
  mutate(marco = "2010") %>% 
  select(marco, pro, est, viv, porviv, upm, porupm) %>% 
  rbind(marco22 %>% 
          group_by(pro = substr(id_upm, 1, 2)) %>% 
          summarise(est = n_distinct(estrato),
                    viv = sum(viv),
                    upm = n()) %>% 
          mutate(porviv = round(100 * viv / sum(viv), 2),
                 porupm = round(100 * upm / sum(upm), 2)) %>% 
          adorn_totals() %>% 
          mutate(marco = "2022") %>% 
          select(marco, pro, est, viv, porviv, upm, porupm))

export(list(tabla01, tabla02), "pedidos/014/tablas_marco_10_22.xlsx")  
  
