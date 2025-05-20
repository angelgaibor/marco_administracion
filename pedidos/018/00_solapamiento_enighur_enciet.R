rm(list = ls())

library(tidyverse)

# Abrimos el último marco

nombre <- last(list.files("productos/01_general/", pattern = ".rds"))

marco <- readRDS(paste0("productos/01_general/",nombre))

table(marco$enighur_sel, marco$enciet_202410_sel)
table(marco$enighur_sel, marco$enciet_202411_sel)
table(marco$enighur_sel, marco$enciet_202412_sel)
table(marco$enighur_sel, marco$enciet_202502_sel)
table(marco$enighur_sel, marco$enciet_202503_sel)
table(marco$enighur_sel, marco$enciet_202504_sel)
table(marco$enighur_sel, marco$enciet_202505_sel)
table(marco$enighur_sel, marco$enciet_202506_sel)

lol <- marco |> 
  filter(enighur_sel == enciet_202506_sel, enighur_sel == 1)
