extpol <- function(pol, perfil, id = NULL, lindero = NULL, buf = 5, densidad = 0.1){
  source("rutinas/funciones/ExtPol/caja.R")
  print(paste0("La función ha iniciado", Sys.time()))
  pol <- pol %>% 
    rename(id = {{ id }})
  
  pol1 <- st_buffer(pol, -buf) %>%
    summarise()

  pol2 <- st_difference(pol, pol1)

  quntos1 <- pol2 %>%
    st_simplify(dTolerance = 0.9) %>%
    mutate(area = as.numeric(st_area(.)),
           npuntos = ceiling(area/(1/densidad))) %>%
    st_cast("MULTIPOLYGON") %>%
    st_sample(size = sum(.$npuntos), type = "hexagonal") %>%
    st_as_sf() %>%
    st_cast("POINT") %>%
    st_coordinates() %>%
    as.data.frame() %>%
    st_as_sf(coords = c("X","Y"))

  st_crs(quntos1) <- st_crs(pol)

  quntos_iden <- quntos1 %>%
    st_join(pol %>% select(id), join = st_within) %>%
    filter(!is.na(id))
  
  names(quntos_iden)[names(quntos_iden) == attr(quntos_iden, "sf_column")] = attr(pol, "sf_column")
  st_geometry(quntos_iden) <- attr(pol, "sf_column")
  
  quntos <- pol %>% 
    st_buffer(-0.2) %>%
    st_simplify(dTolerance = 0.9) %>% 
    st_as_sf() %>% 
    st_cast("MULTIPOINT") %>% 
    st_cast("POINT") %>% 
    filter(!is.na(id)) %>%
    select(id) %>% 
    rbind(quntos_iden)
  
  puntos <- st_union(quntos)
  
  st_crs(puntos) <- st_crs(pol)
  st_crs(quntos) <- st_crs(pol)
  print(paste0("los puntos han sido creados: ", Sys.time()))
  #Calculamos los polígonos de voronoi
  caj <- caja(puntos)
  voronoi <- st_voronoi(puntos,caj) %>% 
    st_cast() %>% 
    st_as_sf()
  voronoi <- voronoi %>% 
    # data.frame(geometry = .) %>% 
    # st_sf(.) %>% 
    st_join(.,quntos, join = st_contains) %>% 
    #calculo de área
    mutate(area = as.numeric(st_area(.))) 
  #Corrección voronoi
  # for (i in 1:(dim(voronoi)[1])){
  #   if(is.na(voronoi$man[i])){
  #     voronoi$man[i] <- voronoi$man[i-1]
  #   }
  # }
  print(paste0("los polígonos de voronoi han sido creados: ", Sys.time()))
  vorpun <- voronoi
  # disolver por manzana
  disolver <- voronoi %>%
    st_make_valid() %>%
    group_by(id) %>% 
    summarise(np=n()) %>% 
    st_cast() %>% 
    filter(!is.na(id))
  st_crs(disolver) <- st_crs(pol)
  print(paste0("los polígonos de voronoi ha sido disueltos: ", Sys.time()))
  # se intersecta con el perfil
  polext <- st_intersection(disolver,perfil) %>% 
    group_by(id) %>% 
    summarise() %>% 
    st_cast()
  print(paste0("los polígonos han sido cortados por el perfil: ", Sys.time()))
  # polext <- polext %>% 
  #   mutate(loli = st_is_valid(.))
    
    
  if(!is.null(lindero)){
    polext <- polext %>% 
      st_difference(lindero) %>% 
      st_buffer(0) %>% 
      #Validación de geometría
      st_make_valid() %>% 
      #Trasnformación a multipoligono
      st_cast("MULTIPOLYGON") %>% 
      #Transformación a poligono
      st_cast("POLYGON") %>% 
      #Emparejamiento geográfico para identicar pedazos de manzanas a utilizar
      st_join(pol) %>% 
      filter(id.x == id.y) %>% 
      #Disolver a nivel de manzana
      group_by(id = id.x) %>% 
      summarise() %>% 
      st_make_valid() %>% 
      st_cast("MULTIPOLYGON")
  }
  
  polext <- polext %>% 
    rename({{ id }} := id) %>% 
    st_as_sf()
  
  names(polext)[names(polext)==attr(polext, "sf_column")] = "geom"
  st_geometry(polext)="geom"
  print(paste0("las bases intermedias han sido generadas: ", Sys.time()))
  
  lista <- list(quntos, polext)
  
  return(lista)
}