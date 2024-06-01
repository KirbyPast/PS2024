lambda = 10
p=0.5
n=40
k=7
m=30


#1.a)

calculeza_prob_binomial = function(n,p,k,m) {
  x=k:m
  probabilitati = dbinom(x,n,p)
  
  return(probabilitati);
}


calculeaza_prob_geometric = function(p, n,k,m) {
  x=k:m
  probabilitati = dgeom(x, p)
  
  return(probabilitati);
}

calculeaza_prob_poisson=function(lambda,n,k,m){
  probabilitati=dpois(k:m,lambda)
  
  return(probabilitati);
}

par(mfrow=c(3,2))

b=vector();
b=calculeza_prob_binomial(n,p,k,m);
print(b)

g=vector();
g=calculeaza_prob_geometric(p,n,k,m);
print(g)

pois = vector();
pois=calculeaza_prob_poisson(lambda,n,k,m)
print(pois);
#1.b)
reprezinta_poisson=function(lambda,n,k,m){
  probabilitati=dpois(k:m,lambda)
  
  print(barplot(probabilitati,
                names.arg=k:m,
                xlab="k",
                ylab="Probabilitate",
                main="Repartitie Poisson"))
  
}

reprezinta_geometric = function(p, n,k,m) {
  x=k:m
  probabilitati = dgeom(x, p)
  print(barplot(probabilitati,
                names.arg = k:m,
                xlab = "k",
                ylab = "Probabilitate", 
                main = "Repartitia Geometrica"))
}


reprezinta_binomial = function(n,p,k,m) {
  x=k:m
  probabilitati = dbinom(x,n,p)
  
  print(barplot(probabilitati,
                names.arg=k:m,
                xlab="k",
                ylab="Probabilitate",
                main="Repartitie Binomiala"))
}

reprezinta_poisson(lambda,n,k,m);

reprezinta_geometric(p,n,k,m);

reprezinta_binomial(n,p,k,m)

#1.c)

calculeaza_k0 = function(lambda){
  j=0;
  z=dpois(j,lambda);
  while(z<=1-10^-6){
    j=j+1;
    z=z+dpois(j,lambda);
  }
  return(j);
}

rez=calculeaza_k0(lambda);
print(rez);

#2.a)

medii_esantioane = function(filename){
  note=read.csv(filename,header=T,sep=',')
  x=note[['P']];
  y=note[['S']];
  
  frecvente_abs_P = as.vector(table(x));
  frecvente_rel_P = frecvente_abs_P / sum(frecvente_abs_P);
  
  frecvente_abs_S = as.vector(table(y));
  frecvente_rel_S = frecvente_abs_S / sum(frecvente_abs_S);
  
  medie_S = mean(x)
  medie_P = mean(y)
  print("Frecvente absoluteP:")
  print(frecvente_abs_P)
  print("Frecvente relativeP:")
  print(frecvente_rel_P)
  print("Frecvente absoluteS:")
  print(frecvente_abs_S)
  print("Frecvente relativeS:")
  print(frecvente_rel_S)
  print("medieP:")
  print(medie_P)
  print("medieS:")
  print(medie_S)
}

medii_esantioane("note_PS.csv")

#2.b)

remove_abberant = function(filename, name){
  note=read.csv(filename,header=T,sep=',')
  x=note[[name]]
  m=mean(x)
  s=sd(x)
  y=vector()
  j=0
  for(i in x){
    j=j+1
    if(i<m-2*s || i > m+2*s){
      x=x[-c(j)];
      j=j-1;
    }
  }

  interval=seq(0,10,1)
  hist(x,breaks=interval,right=T,freq=T)
  print(x)
  return(x)
}

remove_abberant("note_PS.csv",'P')

remove_abberant("note_PS.csv",'S')
