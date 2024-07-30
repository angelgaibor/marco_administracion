matinc <- function(pol, tol=10, id = NULL){
  aux <- pol %>% 
    rename(id = {{id}})
  aux1 <- st_intersection(aux,aux) %>%
    mutate(largo=as.numeric(st_length(.)),
           frecuencia = 1) %>% 
    filter(largo > tol | largo == 0) %>%
    group_by(id) %>% 
    mutate(n = n()) %>% 
    ungroup() %>% 
    filter(!(id==id.1 & n > 1)) %>% 
    as.data.frame() %>% 
    select(id, id.1, frecuencia)
  
  o <- aux1 %>% 
    arrange(id.1) %>% 
    pivot_wider(names_from = id.1, values_from = frecuencia) %>% 
    arrange(id) %>% 
    select(id, .$id) %>% 
    as.data.frame()
  
  q <- data.matrix(select(o, -id)) %>% 
    replace(is.na(.), 0)
  
  rownames(q) <- colnames(q)
  
  diag(q) <- 0
  
  return(q)
}