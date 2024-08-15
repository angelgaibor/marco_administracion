rm(list = ls())

library(tidyverse)

pluscode <- read_csv("~/pluscode.csv")

saveRDS(pluscode |> 
          select(edif_cod, pluscodes), "pedidos/005/edif_pluscode.rds")
