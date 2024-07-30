matinc_dif <- function(pol1, pol2, tol=10, id1 = NULL, id2 = NULL){
  aux1 <- pol1 %>% 
    rename(idx = {{id1}})
  aux2 <- pol2 %>% 
    rename(idy = {{id2}})
  aux3 <- st_intersection(aux1,aux2) %>%
    ungroup() %>% 
    mutate(largo=as.numeric(st_length(.)),
           frecuencia = 1) %>% 
    filter(largo > tol) %>%
    as.data.frame() %>% 
    select(idx, idy, frecuencia)
  
  o <- aux3 %>% 
    arrange(idy) %>% 
    pivot_wider(names_from = idy, values_from = frecuencia) %>% 
    arrange(idx)
  
  q <- data.matrix(select(o, -idx)) %>% 
    replace(is.na(.), 0)
  
  rownames(q) <- o$idx
  
  return(q)
}