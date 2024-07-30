rm(list = ls())

library(tidyverse)

source("rutinas/funciones/KerLin/ker_lin.R")
ker_lin()

marco <- readRDS("pedidos/002/marco_viv_galap.rds")

# Introducir el número del mes a conglomerar la muestra
mes <- 1

muestra <- marco |> 
  filter(.data[[paste0("sel", mes)]] == 1)

