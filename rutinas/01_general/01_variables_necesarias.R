rm(list = ls())

library(tidyverse)

marco_upm <- readRDS("insumos/01_general/marco_upm.rds")

set.seed(20240618)

marco_01 <- marco_upm %>% 
  group_by(estrato) %>% 
  mutate(Mh = sum(Mi)) %>% 
  ungroup() %>% 
  mutate(nap = sample(1:10000000, 56239)/10000000,
         domgeo = substr(domest, 1, 2)) |> 
  select(pro, area, domgeo, domest, estrato, Mi, nap)

saveRDS(marco_01, "productos/01_general/marco_upm_01.rds")

