rm(list = ls())

library(tidyverse)
library(rio)
library(openxlsx)


# abrir la base del marco 
marco_upm <- readRDS("productos/01_general/marco_upm_30.rds")

# revision data
names(marco_upm)
names(marco_upm)[endsWith(names(marco_upm), "_sel")]
class(marco_upm$id_upm)

set.seed(20251105)


tamanio_piloto <- marco_upm %>%
  mutate(
    prov = substr(id_upm, 1, 2),
    cant = substr(id_upm, 1, 4),
    parr = substr(id_upm, 1, 6),
    veri_amanz = substr(id_upm, 7, 7),
    id_seg_upm = substr(id_upm, 1, 10)) %>%
 
 
  filter(cant %in% c("0307", "0901", "1806",  "1807", "0608") | parr %in% c("240154", "030363", "160150")) %>%
  mutate(aux = ifelse(veri_amanz == 9, "disper", "aman"), 
         n =  ifelse(aux == "disper", 1, 2),
         id_dom_piloto = case_when(cant %in% c("0307", "0901", "1806",  "1807", "0608") ~ paste0(cant, "00"),
                                   T ~ parr), 
         id_dom_piloto = paste0(id_dom_piloto, "_", aux))


tam_piloto_envigmu <- tamanio_piloto %>% 
  filter(!duplicated(id_dom_piloto)) %>% 
  select(estrato = id_dom_piloto, nh = n)

saveRDS(tam_piloto_envigmu, "insumos/02_seleccion/11_envigmu_piloto/tam_piloto_envigmu.rds")



