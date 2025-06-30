rm(list = ls())

library(tidyverse)
library(rio)
library(openxlsx)


# abrir la base del marco 
marco_upm <- readRDS("productos/01_general/marco_upm_19.rds")

# revision data
names(marco_upm)
names(marco_upm)[endsWith(names(marco_upm), "_sel")]
class(marco_upm$id_upm)

set.seed(20250620)

# obtencion de información
marco_upm_1 <- marco_upm %>%
  mutate(
    prov = substr(id_upm, 1, 2),
    cant = substr(id_upm, 1, 4),
    parr = substr(id_upm, 1, 6),
    veri_amanz = substr(id_upm, 7, 7),
    id_seg_upm = substr(id_upm, 1, 10)
  ) %>%
  # Filtrar solo cantones Gualaceo, Sigsig o parroquias Macas y Sucúa
  filter(
    cant %in% c("0103", "0109") | parr %in% c("140150", "140650")
  ) %>%
  # 0103 Gualaceo
  # 0109 Sigsig
  # 140650 Sucua
  # 140150 Macas
  # En Macas y Sucúa solo manzanas (veri_amanz distinto de 9)
  # En Gualaceo y Sigsig solo no manzanados (veri_amanz igual a 9)
  filter(
    (parr %in% c("140150", "140650") & veri_amanz != "9") |
      (cant %in% c("0103", "0109") & veri_amanz == "9")
  ) %>%
  # Filtrar áreas donde no haya muestra de otras operaciones estadísticas
  filter(
    enighur_sel == 0,
    enciet_202411_sel == 0,
    enciet_202412_sel == 0,
    enciet_202501_sel == 0,
    enciet_202502_sel == 0,
    enciet_202503_sel == 0,
    enciet_202504_sel == 0,
    enciet_202505_sel == 0,
    endi3_sel == 0,
    enciet_202506_sel == 0,
    enciet_202507_sel == 0, 
    steps_202506_sel == 0
  ) %>% 
  group_by(cant) %>% 
  sample_n(1)



man_sec_upm <- readRDS("insumos/01_general/man_sec_upm.rds") %>% 
  rename(id_seg_upm = id_upm)

man_sec_upm_1 = man_sec_upm %>% 
  inner_join(marco_upm_1, by = c("id_seg_upm")) %>% 
  select(id_upm, man_sec)

#write.xlsx(man_sec_upm_1, "./pedidos/019_piloto_envigmu/data.xlsx")


#tabulados
table(marco_upm_1$cant, useNA = "ifany")
table(marco_upm_1$veri_amanz, useNA = "ifany")
table(marco_upm_1$parr, marco_upm_1$veri_amanz, useNA = "ifany")
table(marco_upm_1$enighur_sel, useNA = "ifany")
table(marco_upm_1$enciet_202412_sel, useNA = "ifany")
