int <- function(s,C,i,j){
  #Extraemos de la solución ingresada los clúster a comparar
  ci=s[i,][s[i,]>0]
  cj=s[j,][s[j,]>0]
  #Con un lazo "while" se realizarán los intercambios posibles  entre los dos clúster
  #mientras estos generen un beneficio a la función objetivo
  w=-1
  while(w<0){
    #Se calculan los costos externos de cada nodo en el clúster i e j
    E=C[ci,cj]
    Ei=rowSums(E)
    Ej=colSums(E)
    #Se calculan los costos internos de cada nodo en el clúster i e j
    Ii=rowSums(C[ci,ci])
    Ij=rowSums(C[cj,cj])
    #Se calcula la diferencia entre el costo externo e interno de cada nodo
    Di=Ei-Ii
    Dj=Ej-Ij
    #Se calcula el beneficio de intercambiar cada nodo del clúster i con cada nodo del
    #clúster j
    u=length(Di)
    v=length(Dj)
    A=matrix(Di,u,v,byrow=F)
    B=matrix(Dj,u,v,byrow=T)
    M=A+B-2*E
    #Determinamos cual es el intercambio que genera mayor beneficio al momento de realizar
    #el intercambio
    aux=which(M==min(M),arr.ind=T)
    l=dim(aux)[1]
    aux=aux[sample(1:l, 1),]
    w=min(M)
    #Si el beneficio es negativo se realiza el intercambio
    if(w<0){
      a=ci[aux[1]]
      ci[aux[1]]=cj[aux[2]]
      cj[aux[2]]=a
    }
  }
  #Dado que los clúster no necesariamente tienen el mismo tamaño, completamos con cero si 
  #faltan elementos
  r=s
  if(length(ci)<dim(s)[2]){
    ci=c(ci,0)
  }
  if(length(cj)<dim(s)[2]){
    cj=c(cj,0)
  }
  r[i,]<-ci
  r[j,]<-cj
  return(r)
}