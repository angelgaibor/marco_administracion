rm(list = ls())

library(devtools)
library(tidyverse)
library(psuR)
# devtools::install_github("matjaviernunez/psuR")

# Ejecutar la función   polygo_ext

psuR::poly

plot(poly)

psuR::boundary

plot(psuR::boundary)

psuR::gap

plot(gap)

manzanas_extendidas <- polygon_ext(poly, boundary, "id", gap, buff = 5, density = 0.1)

plot(manzanas_extendidas)

# Ejecutar la función inc_matrix

matriz_incidencia <- inc_matrix(manzanas_extendidas)

# Ejecutar la función psu_clustering

pesos <- weights

H <- psu_clustering(matriz_incidencia, W = pesos, lowerLimit = 60, idp = "idp", weight = "weight")

conglomerados <- manzanas_extendidas |> 
  left_join(H, by = c("id" = "idp")) |> 
  group_by(psuf) |> 
  summarise(weight = sum(weight)) |> 
  ungroup() |> 
  mutate(id = row_number())

# Ejecutar la función pol_enumerating

conglomerados_enumerados <- polygon_enumerating(conglomerados, "id")

