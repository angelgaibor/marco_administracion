rm(list = ls())

library(tidyverse)
library(arrow)
source("rutinas/funciones/save_marco.R")
source("rutinas/funciones/open_marco.R")

precenso <- readRDS("insumos/00_viviendas/20230316/marco_viv_ocu_nap.rds")

precenso01 <- precenso |> 
  filter(c_ocup == "Particular - Ocupada") |>
  mutate(n_edif_umce = n_edif,
         manloc = case_when(zon == "999" ~ n_loc,
                            T ~ man),
         jefe_hogar = paste(primernjh, segundonjh, primerajh, segudonjh)) |> 
  select(pro, can, par, zon, sec, manloc, n_edif_umce, piso_n, n_viv, n_hbt,
         jefe_hogar, man_sec, calle, pluscodes,
         link, id_upm, nap, n)

# select(pro, can, par, zon, sec, man, n_edif, loc, n_loc, nom_loc_a, direc, 
#        acceso_dis, t_via_p, n_via_p, t_via_s, n_via_s, n_pm, cod_dire, n_dcv, 
#        piso_n, patio_n, n_viv, c_ocup, c_v_col, otros, cod_otr, n_hbt, cuen, 
#        acceso_mz, geocod, t_iden, ident, primernjh, segundonjh, primerajh, 
#        segudonjh, jefe_hogar, man_sec, manz_loc, id_par, id_edif, id_vivienda, 
#        calle, edif_cod, pluscodes, link, id_conglomerado, grupo, id_upm, nap, 
#        n, id_canton, id_parroquia, area, estrato, nprovincia, ncanton, 
#        nparroq, regional, nregional, dominio, n_dominio)


save_marco("insumos/00_viviendas/marco_viviendas", precenso01)


