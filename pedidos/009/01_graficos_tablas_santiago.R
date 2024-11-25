rm(list = ls())

library(tidyverse)

man_sec_upm <- readRDS("insumos/01_general/man_sec_upm.rds")

sectores <- man_sec_upm |> 
  group_by(id_sector = substr(man_sec, 1, 12)) |> 
  summarise(viv = sum(viv_ocu))

g1 <- sectores %>% 
  mutate(Tipo = case_when(substr(id_sector, 7, 9) == "999" ~ "Disperso",
                          T ~ "Amanzanado")) %>% 
  ggplot(aes(x = viv)) + 
  geom_histogram(aes(color = Tipo, fill = Tipo),  alpha = 0.25, position = "identity", binwidth = 5, bins = 20) +
  #geom_histogram(alpha = 0.25, position = "identity", binwidth = 5, bins = 20) +
  geom_vline(xintercept = 60, linetype = "dashed") +
  geom_vline(xintercept = 120, linetype = "dashed") +
  labs(x = "Número de viviendas",
       y = "Número de sectores") +
  theme(panel.background = element_blank())

g1


upm <- man_sec_upm |> 
  group_by(id_upm) |> 
  summarise(viv = sum(viv_ocu))

g2 <- upm %>% 
  mutate(Tipo = case_when(substr(id_upm, 7, 7) == "9" ~ "Disperso",
                          T ~ "Amanzanado")) %>% 
  ggplot(aes(x = viv)) + 
  geom_vline(xintercept = 60, linetype = "dashed") +
  geom_vline(xintercept = 120, linetype = "dashed") +
  geom_histogram(aes(color = Tipo, fill = Tipo),  alpha = 0.25, position = "identity", binwidth = 5, bins = 20) +
  #geom_histogram(alpha = 0.25, position = "identity", binwidth = 5, bins = 20) +
  labs(x = "Número de viviendas",
       y = "Número de UPM") +
  theme(panel.background = element_blank())

g2


marco_upm_08 <- readRDS("D:/MAG/marco_administracion/productos/01_general/marco_upm_08.rds")

g3 <- marco_upm_08 %>% 
  mutate(Tipo = case_when(substr(id_upm, 7, 7) == "9" ~ "Disperso",
                          T ~ "Amanzanado")) %>% 
  ggplot(aes(x = Mi)) + 
  geom_histogram(aes(color = Tipo, fill = Tipo),  alpha = 0.25, position = "identity", binwidth = 5, bins = 20) +
  #geom_histogram(alpha = 0.25, position = "identity", binwidth = 5, bins = 20) +
  geom_vline(xintercept = 60, linetype = "dashed") +
  geom_vline(xintercept = 120, linetype = "dashed") +
  labs(x = "Número de viviendas",
       y = "Número de UPM") +
  theme(panel.background = element_blank())

g3

g4 <- sectores %>% 
  filter(viv <= 255) |> 
  mutate(Tipo = case_when(substr(id_sector, 7, 9) == "999" ~ "Disperso",
                          T ~ "Amanzanado")) %>% 
  ggplot(aes(x = viv)) + 
  geom_histogram(aes(color = Tipo, fill = Tipo),  alpha = 0.25, position = "identity", binwidth = 5, bins = 20) +
  #geom_histogram(alpha = 0.25, position = "identity", binwidth = 5, bins = 20) +
  geom_vline(xintercept = 60, linetype = "dashed") +
  geom_vline(xintercept = 120, linetype = "dashed") +
  labs(x = "Número de viviendas",
       y = "Número de sectores") +
  theme(panel.background = element_blank())

g4



a = 200
h = 100

ggsave(file ="img01.png",
       plot = g1,
       #device = cairo_ps,
       device = "png",
       path = paste0("pedidos/009/"),
       scale = 1, width = a, height = h, units = "mm",
       dpi = 300,
       limitsize = F)

ggsave(file ="img02.png",
       plot = g2,
       #device = cairo_ps,
       device = "png",
       path = paste0("pedidos/009/"),
       scale = 1, width = a, height = h, units = "mm",
       dpi = 300,
       limitsize = F)

ggsave(file ="img03.png",
       plot = g3,
       #device = cairo_ps,
       device = "png",
       path = paste0("pedidos/009/"),
       scale = 1, width = a, height = h, units = "mm",
       dpi = 300,
       limitsize = F)

ggsave(file ="img04.png",
       plot = g4,
       #device = cairo_ps,
       device = "png",
       path = paste0("pedidos/009/"),
       scale = 1, width = a, height = h, units = "mm",
       dpi = 300,
       limitsize = F)

cat(" ",sum(sectores$viv < 60)/dim(sectores)[1], "\n",
    sum(sectores$viv >= 60 & sectores$viv <= 120)/dim(sectores)[1], "\n",
    sum(sectores$viv > 120)/dim(sectores)[1], "\n",
    dim(sectores)[1])

cat(" ",sum(marco_upm_08$Mi < 60)/dim(marco_upm_08)[1], "\n",
    sum(marco_upm_08$Mi >= 60 & marco_upm_08$Mi <= 120)/dim(marco_upm_08)[1], "\n",
    sum(marco_upm_08$Mi > 120)/dim(marco_upm_08)[1], "\n",
    dim(marco_upm_08)[1])
