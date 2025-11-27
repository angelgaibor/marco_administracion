rm(list = ls())

library(tidyverse)

nombre <- last(list.files("productos/01_general/", pattern = ".rds"))

marco <- readRDS(paste0("productos/01_general/",nombre))

distribucion_paquete_enciet <- marco |> 
  group_by(estrato) |> 
  summarise(n_upm_paquete = sum(enciet_202508_sel),
            n_upm_total = n(),
            n_upm_acutalizado = sum(enighur_sel == 1 | endi3_sel == 1),
            n_upm_desactualizado = sum(!(enighur_sel == 1 | endi3_sel == 1)),
            n_upm_propuesto1 = ceiling(7000 *sum(enciet_202508_sel)/319)) |> 
  ungroup() |> 
  mutate(control = ifelse(n_upm_propuesto1 > n_upm_desactualizado, 1, 0),
         n_upm_propuesto2 = case_when(control == 1 ~ n_upm_desactualizado,
                                      T ~ n_upm_propuesto1))

table(distribucion_paquete_enciet$control)  

sum(distribucion_paquete_enciet$n_upm_propuesto1)
sum(distribucion_paquete_enciet$n_upm_propuesto2)
