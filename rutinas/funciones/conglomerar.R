conglomerar <- function(A,peso,sl,idp=NULL){
  #Renombramos en peso al identificador de conglomerado
  peso <- peso %>% 
    rename(id = {{idp}})
  
  #Guardamos la informacion de la matriz de incidencia en B para no alterar la
  #original
  B <- A
  s <- peso
  #Calculamos el numero de nodos
  n <- dim(s)[1]
  #Inicializamos el vector H de resultados
  H <- rep(0,n)
  #Paso 1. Identificamos los nodos que son cluster por si solo
  a <- (1:n)[s$viv>=sl]
  H[a] <- a
  #Aislamos los nodos que son cluster
  B[a,] <- 0
  B[,a] <- 0
  
  while(sum(B)>=1){
    #Paso 2. Identificamos los nodos que tienen menor numero de incidencias
    #y no son aislados
    d <- rowSums(B)
    d[d==0] <- n
    b <- (1:n)[d==min(d)]
    #Identificamos los nodos con los que se pueden juntar los nodos idenficados
    #anteriormente
    e <- B[,b]*(1:n)
    e <- e[e>0]
    unir <- cbind(nodo1=rep(b,each=min(d)),nodo2=e) %>% 
      as.data.frame() %>% 
      mutate(peso1=s[nodo1,2],
             peso2=s[nodo2,2],
             ninc1=d[nodo1]+peso1/1e4+sample(1:1e6,length(e))/1e11,
             ninc2=d[nodo2]+peso2/1e4+sample(1:1e6,length(e))/1e11) %>% 
      group_by(nodo1) %>% 
      filter(ninc2==min(ninc2)) %>% 
      ungroup() %>% 
      group_by(nodo2) %>% 
      filter(ninc1==min(ninc1)) %>% 
      ungroup() %>% 
      mutate(sumninc=ninc1+ninc2,
             eliminar="no",
             peso=peso1+peso2)
    
    repetidos <- unir$nodo1[unir$nodo1 %in% unir$nodo2]
    
    if(length(repetidos!=0)){
      unir_01 <- unir %>%
        filter(nodo1 %in% repetidos | nodo2 %in% repetidos)
      
      unir_02 <- unir %>%
        filter(!(nodo1 %in% repetidos | nodo2 %in% repetidos))
      
      for (i in 1:length(repetidos)){
        apoyo <- unir_01 %>% 
          filter(nodo1==repetidos[i] | nodo2==repetidos[i]) %>% 
          filter(sumninc==min(sumninc))
        if(i==1){
          unir_03 <- apoyo
        }
        else{
          unir_03 <- rbind(unir_03,apoyo)
        }
      }
      unir_03 <- unir_03 %>% 
        filter(!duplicated(.))
      
      unir <- rbind(unir_02,unir_03)
    }
    
    unir <- unir %>% 
      mutate(nodo=ifelse(nodo1 %in% H,nodo1,nodo2),
             nodob=ifelse(nodo1==nodo,nodo2,nodo1),
             previo=ifelse(nodob %in% H,1,0)) 
    
    H[unir$nodo]=unir$nodo
    H[unir$nodob]=unir$nodo
    
    if(sum(unir$previo)>0){
      index1 <- unir$nodob[unir$previo==1]
      for(i in 1:length(index1)){
        H[H==index1[i]] <- unir$nodo[unir$nodob==index1[i]]
      }
    }
    
    
    
    
    
    
    
    B[unir$nodo,] <- B[unir$nodo1,]+B[unir$nodo2,]
    B[,unir$nodo] <- B[,unir$nodo1]+B[,unir$nodo2]
    B[unir$nodob,] <- 0
    B[,unir$nodob] <- 0
    
    s[unir$nodo,2] <- s[unir$nodo1,2]+s[unir$nodo2,2]
    s[unir$nodob,2] <- 0
    
    cong <- unir$nodo[s[unir$nodo,2]>=sl]
    B[cong,] <- 0
    B[,cong] <- 0
    B[B>=2] <- 1
    diag(B) <- 0
    print(sum(B))
    
  }
  
  r1 <- peso %>% 
    cbind(cong=H) %>% 
    group_by(cong) %>% 
    summarise(vivcong=sum(viv),
              nma=n()) 
  
  aislado <- peso %>% 
    cbind(cong=H) %>% 
    group_by(cong) %>% 
    mutate(vivcong=sum(viv),
           nma=n()) %>% 
    ungroup() %>% 
    mutate(congf=ifelse(vivcong<sl,0,cong),
           npol=1:n)
  
  B <- A
  
  B[aislado$congf==0,aislado$congf==0] <- 0
  
  index <- unique(aislado$congf[aislado$congf>0])
  
  for(i in 1:length(index)){
    if(!is.na(sum(aislado$congf==index[i]))){
      if(sum(aislado$congf==index[i])>1){
        B[index[i],] <- colSums(B[aislado$congf==index[i],])
        B[,index[i]] <- rowSums(B[,aislado$congf==index[i]])
        B[aislado$congf==index[i] & aislado$npol!=index[i],] <- 0
        B[,aislado$congf==index[i] & aislado$npol!=index[i]] <- 0
      } 
    }
    
  }
  B[B>1] <- 1
  diag(B) <- 0
  
  C <- A
  
  C[aislado$congf==0,aislado$congf!=0] <- 0
  C[aislado$congf!=0,aislado$congf==0] <- 0
  C[aislado$congf!=0,aislado$congf!=0] <- 0
  
  
  while(min(aislado$congf) == 0 & sum(rowSums(rbind(B[aislado$npol[aislado$congf == 0],], B[aislado$npol[aislado$congf == 0],]))) > 0){
    if(sum(aislado$congf == 0) > 1){
      v <- rowSums(B[aislado$npol[aislado$congf == 0],])
      v <- names(v[v == min(v[v>0])])
    }else{
      v <- aislado$id[aislado$congf == 0]
    }
    
    v <- aislado$npol[aislado$id %in% v]
    
    w <- aislado$viv[aislado$npol %in% v]
    
    ais <- min(v[w == min(w)])
    candidatos <- (1:n)[B[ais,]>0]
    
    p_can <- min(aislado$vivcong[candidatos])
    ncon <- candidatos[aislado$vivcong[candidatos] == p_can]
    
    aislado <- aislado %>% 
      mutate(congf = ifelse(npol == ais, ncon, congf )) %>% 
      group_by(congf) %>% 
      mutate(vivcong = sum(viv)) %>% 
      ungroup()
    
    B[ncon, ] <- B[ncon, ] + C[ais, ]
    B[, ncon] <- B[, ncon] + C[, ais]
    C[ais, ] <- 0
    C[, ais] <- 0
    B[B>0] <- 1
    diag(B) <- 0
    
    print(sum(aislado$congf == 0))
  }
  if(min(aislado$congf) > 0){
    cong <- aislado %>% 
      select(id, viv, congf) %>% 
      rename({{idp}} := id)
  }else{
    control <- aislado %>% 
      filter(congf == 0) %>% 
      mutate(zona = ifelse(nchar(id) == 15, substr(id, 7, 9),
                           substr(id, 10, 12))) %>% 
      group_by(zona) %>% 
      summarise() %>% 
      ungroup() %>% 
      mutate(cong1 = 999999 - row_number() + 1) %>% 
      select(zona, cong1)
    
    cong <- aislado %>% 
      mutate(zona = ifelse(nchar(id) == 15, substr(id, 7, 9),
                           substr(id, 10, 12))) %>% 
      left_join(control, by = "zona") %>% 
      mutate(congf = ifelse(congf == 0 & !is.na(cong1), cong1, congf)) %>% 
      select(id, viv, congf) %>% 
      rename({{idp}} := id)
  }

  return(cong)
}