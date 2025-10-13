
rm(list = ls())

library(tidyverse)
library(janitor)
library(rio)

# -----------------------------------------------------------------------------
# Lectura de la última actualziación del marco
# -----------------------------------------------------------------------------

marco_upm <- readRDS("productos/01_general/marco_upm_27.rds")

# -----------------------------------------------------------------------------
# Identificamos el número de orden enighur que está libre para el reemplazo
# La UPM  reemplazar: Cotacachi :100350902601 y Otavalo: 100450016401 - 100450002601
# -----------------------------------------------------------------------------

marco_upm %>% 
  filter(id_upm %in%  c("100350902601","100450016401", "100450002601")) %>% 
  select(id_upm, estrato)

estrato_cambio = c("1012","1013","1021")

aux_1 <- marco_upm %>% 
  filter(estrato %in% estrato_cambio) %>% 
  select(id_upm, estrato, enighur_orden ,
         grep(names(marco_upm), pattern = "_sel$")) %>% 
  adorn_totals(where = "col", cols = contains("_sel")) %>% 
  filter(Total == 0) %>% 
  group_by(estrato) %>% 
  summarise(selec_nueva = min(enighur_orden)) %>% 
  ungroup()

# -----------------------------------------------------------------------------
# Guardamos las modificaciones en el marco_upm_21
# -----------------------------------------------------------------------------

marco_upm_nuevo <- marco_upm %>% 
  mutate(enighur_sel = ifelse(enighur_estrato == aux_1$estrato[1] & enighur_orden == aux_1$selec_nueva[1], 
                              1, enighur_sel), 
         enighur_sel = ifelse(enighur_estrato == aux_1$estrato[2] & enighur_orden == aux_1$selec_nueva[2], 
                              1, enighur_sel), 
         enighur_sel = ifelse(enighur_estrato == aux_1$estrato[3] & enighur_orden == aux_1$selec_nueva[3], 
                              1, enighur_sel)) 

dim(marco_upm_nuevo %>% filter(enighur_sel == 1) ) #3438 ok

# -----------------------------------------------------------------------------
# La UPM que será el reemplazo
# -----------------------------------------------------------------------------

upm_de_reemplazo <- marco_upm_nuevo %>% 
  filter(enighur_estrato == aux_1$estrato[1] & enighur_orden == aux_1$selec_nueva[1] | 
         enighur_estrato == aux_1$estrato[2] & enighur_orden == aux_1$selec_nueva[2] | 
         enighur_estrato == aux_1$estrato[3] & enighur_orden == aux_1$selec_nueva[3]) %>% 
  select(c("id_upm", "pro", "area", "estrato", "Mi", "enighur_pii" ))
  
upm_de_reemplazo$id_upm

# -----------------------------------------------------------------------------
# Exportar - seleccion y upm de reemplazo
# -----------------------------------------------------------------------------

ruta <- "productos/02_seleccion/09_enighur_reemplazo_004/"

muestra_enighur_reemplazo <- marco_upm_nuevo %>% 
  filter(enighur_sel == 1) %>% 
  select(c("id_upm", "pro", "area", "estrato", "Mi", "enighur_pii" ))


export(muestra_enighur_reemplazo, paste0(ruta,"muestra_enighur_reemplazo.rds"))
export(upm_de_reemplazo, paste0(ruta,"upm_de_reemplazo.xlsx"))

# -----------------------------------------------------------------------------
# Exportar - marco
# -----------------------------------------------------------------------------

export(marco_upm_nuevo, "productos/01_general/marco_upm_28.rds")
