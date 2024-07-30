matdisdif<-function(x,y,x1,y1){
  n=length(x)
  m=length(x1)
  C=matrix(0,n,m)
  for (i in 1:n){
    C[i,]=sqrt((x[i]-x1)^2+(y[i]-y1)^2)
  }
  return(C)
}