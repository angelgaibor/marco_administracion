zonificar <- function(muestra,l){
  # Número de veces a aplicar kerlin
  m1=100
  ##########################################
  
  x <- muestra$x
  y <- muestra$y
  
  auxiliar <- muestra
  
  bdd <- as.data.frame(cbind(x,y)) %>% 
    mutate(x=as.numeric(x),
           y=as.numeric(y))
  
  k <- ceiling(dim(bdd)[1]/l)
  
  C <- matdis(bdd$x,bdd$y)
  n <- dim(bdd)[1]
  
  li <- rep(list(n),m1)
  sa <- lapply(li, solale,k)
  sr <- lapply(sa, ref,C)
  o <- sapply(sr,funobj,C)
  m <- min((1:m1)[o==min(o)])
  sr <- sr[[m]]
  apoyo <- data.frame(man=as.vector(t(sr)),
                      equipo=as.character(sort(rep(1:k,dim(sr)[2])))) %>% 
    filter(man!=0) %>% 
    arrange(man) %>% 
    select(-man) %>% 
    cbind(auxiliar)
}