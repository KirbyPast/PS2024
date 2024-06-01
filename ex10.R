reprezinta_poisson=function(lambda,n){
  probabilitati=dpois(1:n,lambda)
  
  print(barplot(probabilitati,
                names.arg=1:n,
                xlab="k",
                ylab="Probabilitate",
                main="Repartitie Poisson"))
  
}

lambda=10;
n=100

reprezinta_poisson(lambda,n)
