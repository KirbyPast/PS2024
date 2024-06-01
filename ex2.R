b=scan(n=7)
k=3
# A)
procent_fiecare = function(x){
  suma=sum(x)
  c=0
  vec=vector()
  for(i in x){
    vec[c]=i/suma
    c=c+1
  }
  return(vec)
}


rezultat=procent_fiecare(b)

# B)
punct_b=function(x){
  maxim=max(x)
  minim=min(x)
  c=0
  vec=vector()
  for(i in x){
    vec[c]=(i-minim)/maxim
    c=c+1
  }
  return(vec)
}

rezultatb=punct_b(b)

# C)

punct_c=function(x,k){
  c=1
  suma_sus=0;
  suma_jos=0;
  for(i in x){
    if(c<=k){
      suma_sus=suma_sus+i;
    }
    else{
      suma_jos=suma_jos+i;
    }
    c=c+1
  }
  medie=suma_sus/suma_jos;
  return(medie);
}

rezultatc=punct_c(b,k);

# D)

