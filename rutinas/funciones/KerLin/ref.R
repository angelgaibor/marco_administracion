ref<-function(s,C){
  k=dim(s)[1]
  w=-1
  t=s
  while(w<0){
    r=t
    m=sample(1:k,k)
    for (i in 1:k){
      for (j in 1:k){
        if(i!=j){
          r=int(r,C,m[i],m[j])
        }
      }
     
    }
    w=funobj(r,C)-funobj(t,C)
    t=r
  }
  #print(funobj(t,C))
  return(t)
}