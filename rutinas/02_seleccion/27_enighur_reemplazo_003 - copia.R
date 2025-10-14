
rm(list = ls())

library(tidyverse)
library(janitor)
library(rio)

# -----------------------------------------------------------------------------
# Lectura de la última actualziación del marco
# -----------------------------------------------------------------------------

marco_upm <- readRDS("productos/01_general/marco_upm_26.rds")

# -----------------------------------------------------------------------------
# Identificamos el número de orden enighur que está libre para el reemplazo
# La UPM  reemplazar : 010450900201
# Estrato : 0121
# -----------------------------------------------------------------------------

marco_upm %>% filter(id_upm == "090150150201") %>% select(estrato)

estrato_cambio = "4213"

aux_1 <- marco_upm %>% 
  filter(estrato == estrato_cambio) %>% 
  select(id_upm, estrato, enighur_orden ,
         grep(names(marco_upm), pattern = "_sel$")) %>% 
  adorn_totals(where = "col", cols = contains("_sel")) %>% 
  filter(Total == 0) %>% 
  mutate(selec_nueva = min(enighur_orden))

# -----------------------------------------------------------------------------
# Guardamos las modificaciones en el marco_upm_21
# -----------------------------------------------------------------------------

orden_cambio = unique(aux_1$selec_nueva) 

marco_upm_nuevo <- marco_upm %>% 
  mutate(enighur_sel = ifelse(estrato == estrato_cambio & enighur_orden == orden_cambio, 
                              1, enighur_sel)) 

dim(marco_upm_nuevo %>%  filter(enighur_sel == 1) ) #3435 ok

# -----------------------------------------------------------------------------
# La UPM que será el reemplazo
# -----------------------------------------------------------------------------

upm_de_reemplazo <- marco_upm_nuevo %>% 
  filter(estrato == estrato_cambio & enighur_orden == orden_cambio) %>% 
  select(c("id_upm", "pro", "area", "estrato", "Mi", "enighur_pii" ))
  
upm_de_reemplazo$id_upm

# -----------------------------------------------------------------------------
# Exportar - seleccion y upm de reemplazo
# -----------------------------------------------------------------------------

ruta <- "productos/02_seleccion/09_enighur_reemplazo_003/"

muestra_enighur_reemplazo <- marco_upm_nuevo %>% 
  filter(enighur_sel == 1) %>% 
  select(c("id_upm", "pro", "area", "estrato", "Mi", "enighur_pii" ))


export(muestra_enighur_reemplazo, paste0(ruta,"muestra_enighur_reemplazo.rds"))
export(upm_de_reemplazo, paste0(ruta,"upm_de_reemplazo.xlsx"))

# -----------------------------------------------------------------------------
# Exportar - marco
# -----------------------------------------------------------------------------

export(marco_upm_nuevo, "productos/01_general/marco_upm_27.rds")
