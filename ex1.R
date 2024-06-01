b=scan(n=7)
calcul_statistici = function(b) {
  maxim=max(b)
  minim=min(b)
  suma=sum(b)
  medie=mean(b)
  raport=minim/maxim
  sum_mare=sum(b>=40)
  medie_mica=mean(b<40)*100
  
  rezultat=c(maxim,minim,suma,medie,raport,sum_mare,medie_mica)
  
  return(rezultat)
}

rezultate=calcul_statistici(b)
