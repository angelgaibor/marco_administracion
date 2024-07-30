funobj<-function(s,C){
  k=dim(s)[1]
  o=0
  for (i in 1:k){
    o=o+sum(C[s[i,][s[i,]>0],s[i,][s[i,]>0]])
  }
  return(o)
}