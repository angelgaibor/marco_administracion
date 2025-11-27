rm(list = ls())

library(tidyverse)
library(rio)
library(sf)

# COMPARACIÓN COBERTURA ENEMDU ENCIET TNE

# ENEMDU

load("../ENDI3/insumos/03_muestra_usm/dpa_2022.RData")

indexenemdu <- list.files("pedidos/029_informe_cambio_marco/enemdu/cobertura", full.names = T)

aux_enemdu <-vector("list", 0)

marco_upm_enemdu <- readRDS("pedidos/029_informe_cambio_marco/enemdu/20251024_marco_upm.rds")

aux_marco_enemdu <- marco_upm_enemdu |> 
  select(id_upm, starts_with("enemdu_24")) |> 
  select(id_upm, ends_with("_f")) |> 
  pivot_longer(cols = starts_with("enemdu"), names_to = "mes", values_to = "panel") |> 
  filter(!is.na(panel)) |> 
  group_by(id_upm, panel) |> 
  summarise()

for(i in 1:12){
  aux_enemdu[[i]] <- readRDS(indexenemdu[i]) |> 
    mutate(mes = substr(indexenemdu[i], 55, 60))
}

cobertura_enemdu <- do.call("rbind", aux_enemdu) |> 
  left_join(aux_marco_enemdu,
            by = "id_upm") |> 
  filter((panel == "c3" & mes == "202401") |
           (panel == "g3" & mes == "202402") |
           (panel == "k3" & mes == "202403") |
           (panel == "d3" & mes == "202404") |
           (panel == "h3" & mes == "202405") |
           (panel == "i3" & mes == "202406") |
           (panel == "b3" & mes == "202410") |
           (panel == "f3" & mes == "202411") |
           (panel == "j3" & mes == "202412")) |> 
  mutate(TRE = tcomp,
         TNR = trecha,
         TED = tnadie,
         TNE = totviv - (TRE + TNR + TED),
         total = totviv,
         pro = substr(id_upm, 1, 2)) |>
  select(pro, TRE, TNR, TED, TNE, total) |> 
  group_by(pro) |> 
  summarise_all(sum) |> 
  mutate(prop_ne_2010 = round(100 * TNE/total, 1)) |> 
  left_join(provincia |> 
              select(pro = provin, nprovin),
            by = "pro") |> 
  mutate(Provincia = str_to_title(nprovin)) |> 
  arrange(desc(prop_ne_2010))
  
# ENCIET

cobertura_enciet <- readRDS("pedidos/029_informe_cambio_marco/enciet/cobertura_acumulada.rds") |> 
  mutate(provin = substr(id_upm, 1, 2)) |> 
  filter(nro_visita == 1,
         !periodo %in% c("a24m11", "a24m12", "a25m01")) |> 
  mutate(tcomp = ifelse(resultado_entrevista==1, 1, 0),
         trecha = ifelse(resultado_entrevista==2, 1, 0),
         tnadie = ifelse(resultado_entrevista==3, 1, 0),
         ttemp = ifelse(resultado_entrevista==4, 1, 0),
         tdeso = ifelse(resultado_entrevista==5, 1, 0),
         tcons = ifelse(resultado_entrevista==6, 1, 0),
         tdest = ifelse(resultado_entrevista==7, 1, 0),
         tnego = ifelse(resultado_entrevista==8, 1, 0),
         totras = ifelse(resultado_entrevista==9, 1, 0)) |> 
  mutate(re = ifelse(tcomp==1, "TRE",
                     ifelse(trecha==1 | res_ent_otros %in% c(6,10), "TNR",
                            ifelse(tnadie==1 | res_ent_otros %in% c(7, 8, 9), "TED", "TNE"))),
         `Tasas de conformidad` = factor(re, levels = c("TNR", "TED", "TNE", "TRE"),
                                         labels = c("TNR", "TED", "TNE", "TRE"))) |> 
  group_by(pro = provin, re) |> 
  summarise(n = n()) |> 
  pivot_wider(names_from = re, values_from = n) |> 
  mutate(total = TED + TNE + TNR + TRE,
         prop_ne_2022 = round(TNE/total*100, 1))

apoyo_grafico_cobertura <- cobertura_enemdu |> 
  select(Provincia, pro, ENEMDU = prop_ne_2010) |> 
  left_join(cobertura_enciet |> 
              select(pro, ENCIET = prop_ne_2022),
            by = "pro") |> 
  pivot_longer(cols = c("ENEMDU", "ENCIET"),
               names_to = "Encuesta",
               values_to = "Tasa de no elegibilidad") |> 
  mutate(Provincia = factor(Provincia, cobertura_enemdu$Provincia, cobertura_enemdu$Provincia),
         `Tasa de no elegibilidad` = `Tasa de no elegibilidad` - 10) |> 
  ggplot(aes(x = Provincia, y = `Tasa de no elegibilidad`, fill = Encuesta)) +
  geom_bar(stat = "identity", position = "dodge", color = "white") +
  labs(title = "Tasa de no elegibilidad primera visita ENEMDU versus ENCIET") + 
  coord_flip() + 
  scale_y_continuous(breaks = c(0, 10, 20 ,30),
                     labels = c("0" = "10", "10" = "20", "20" = "30" ,"30"="40")) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.direction = "horizontal",
        legend.position = "bottom",
        panel.background = element_blank(), 
        panel.grid = element_blank())


ggsave(file ="05_comparacion_tne_enemdu_enciet.png",
       plot = apoyo_grafico_cobertura,
       device = "png",
       path = paste0("pedidos/029_informe_cambio_marco/"),
       scale = 2, width = 84, height = 60, units = "mm",
       dpi = 300,
       limitsize = F)

# Sectores censales con manzanas

disperso_2010 <- read_sf("../cartografia/insumos/precenso_2010/SHAPEFILE2014-AJUSTADA-V3/GEO_SECDIS2014.shp")

manzanas_2022 <- read_sf("../cartografia/insumos/precenso_2022/BNCPV22.gpkg", layer = "man_a")

inter_dis_aman <- st_intersection(disperso_2010, manzanas_2022)

resumen_interseccion <- disperso_2010 |> 
  as.data.frame() |> 
  group_by(pro = substr(DPA_SECDIS, 1, 2)) |> 
  summarise(n_sec = n()) |> 
  left_join(inter_dis_aman |> 
              as.data.frame() |> 
              group_by(pro = substr(DPA_SECDIS, 1, 2)) |> 
              summarise(n_sec_man = n_distinct(DPA_SECDIS),
                        n_man_int =  n_distinct(man)),
            by = "pro") |> 
  mutate(prop = round(100 * n_sec_man/n_sec, 1)) |> 
  left_join(provincia |> 
              select(pro = provin, nprovin),
            by = "pro") |> 
  mutate(Provincia = str_to_title(nprovin))

# tamaño muestral 
 tamanio <- aux_enemdu[[2]] |> 
   group_by(pro =substr(id_upm, 1, 2)) |> 
   summarise(n_upm_enemdu = n()) |> 
   left_join(tam_enciet <- readRDS("pedidos/029_informe_cambio_marco/enciet/cobertura_acumulada.rds") |> 
               filter(periodo == "a24m11") |> 
               group_by(pro = substr(id_upm, 1, 2)) |>
               summarise(n_upm_enciet = n_distinct(id_upm)),
             by = "pro") |> 
   left_join(provincia |> 
               select(pro = provin, nprovin),
             by = "pro") |> 
   mutate(Provincia = str_to_title(nprovin))

 write.table(tamanio, "clipboard", row.names = F, sep = "\t", dec = ",")
 
 


   