# Esto se corre el 19/05/2025
#
rm(list = ls())

library(tidyverse)

# Abrimos el último marco

nombre <- last(list.files("productos/01_general/", pattern = ".rds"))

marco <- readRDS(paste0("productos/01_general/",nombre))

uno <- marco %>% 
  select(id_upm, pro, area, domgeo, domest, estrato, Mi, nap,
         enighur_estrato, enighur_seleccionable, enighur_nh, enighur_pii,
         enighur_pareto, enighur_orden, enighur_sel,
         enciet_202411_estrato, enciet_202411_seleccionable, enciet_202411_nh,
         enciet_202411_pii, enciet_202411_pareto, enciet_202411_orden, enciet_202411_sel,
         enciet_202412_estrato, enciet_202412_seleccionable, enciet_202412_nh,
         enciet_202412_pii, enciet_202412_pareto, enciet_202412_orden, enciet_202412_sel,
         enciet_202501_estrato = enciet_202410_estrato,
         enciet_202501_seleccionable = enciet_202410_seleccionable,
         enciet_202501_nh = enciet_202410_nh,
         enciet_202501_pii = enciet_202410_pii,
         enciet_202501_pareto = enciet_202410_pareto,
         enciet_202501_orden = enciet_202410_orden,
         enciet_202501_sel = enciet_202410_sel,
         enciet_202502_estrato, enciet_202502_seleccionable, enciet_202502_nh,
         enciet_202502_pii, enciet_202502_pareto, enciet_202502_orden, enciet_202502_sel,
         enciet_202503_estrato, enciet_202503_seleccionable, enciet_202503_nh,
         enciet_202503_pii, enciet_202503_pareto, enciet_202503_orden, enciet_202503_sel,
         enciet_202504_estrato, enciet_202504_seleccionable, enciet_202504_nh,
         enciet_202504_pii, enciet_202504_pareto, enciet_202504_orden, enciet_202504_sel,
         enciet_202505_estrato, enciet_202505_seleccionable, enciet_202505_nh,
         enciet_202505_pii, enciet_202505_pareto, enciet_202505_orden, enciet_202505_sel,
         endi3_estrato, endi3_seleccionable, endi3_nh,
         endi3_pii, endi3_pareto, endi3_orden, endi3_sel,
         enciet_202506_estrato, enciet_202506_seleccionable, enciet_202506_nh,
         enciet_202506_pii, enciet_202506_pareto, enciet_202506_orden, enciet_202506_sel)

apoyo <- uno %>% 
  filter(enciet_202506_sel ==1, enighur_sel == 1) %>% 
  select(id_upm, pro, area, domgeo, domest, estrato)

saveRDS(uno, "productos/01_general/marco_upm_17.rds")
saveRDS(apoyo, "productos/02_seleccion/03_enciet/202506/solapamiento enighur.rds")
    
    
# apoyo <- uno %>% 
#   select(id_upm, pro, area, domgeo, domest, estrato, ends_with("_sel")) %>% 

