#I.1
volum_tor = function(N,R,r) {
  nc=0;
  for(i in 1:N){
    x=runif(1,-R-r,R+r);
    y=runif(1,-R-r,R+r);
    z=runif(1,-r,r);
    if(z*z+(sqrt(x*x+y*y)-R)*(sqrt(x*x+y*y)-R)<=r*r){
      nc=nc+1;
    }
  }
  volum_actual=2*pi*pi*R*r*r;
  volum_spatiu=(2*(R+r))*(2*(R+r))*(2*r);
  print("volum tor real:")
  print(volum_actual);
  print("volum tor calculat:")
  print((nc/N)*volum_spatiu);
}
print("Calcule volum tor pt 10000, 20000 si 50000 de incercari")
volum_tor(10000,10,3);
volum_tor(20000,10,3);
volum_tor(50000,10,3);

#II.1

a = 0;
b = 2;
c = 0;
d = 3;

arie_triunghi = function(N,a,b,c,d){
  nc=0;
  for(i in 1:N){
    x=runif(1,a,b);
    y=runif(1,c,d);
    if(y>=0 && y<=2*x && y<=6-3*x){
      nc=nc+1;
    }
  }
  arie_spatiu=(b-a)*(d-c);
  print("arie triunghi: ")
  print((nc/N)*arie_spatiu);
}
print("Arie triunghi dupa 10000 de incercari:")
arie_triunghi(10000,a,b,c,d);

#III.1

calcul_integrala_a = function(N){
  sum=0;
  for(i in 1:N){
    x=runif(1,-1,1);
    sum=sum+(2*x-1)/(x*x-x-6);
  }
  print("Valoare reala integrala:")
  print(log(3)-log(2));
  print("Valoare calculata integrala: ")
  print(2*sum/N);
}

calcul_integrala_a(10000)

#III.2

calcul_integrala_b = function(N){
  sum=0;
  for(i in 1:N){
    x=runif(1,3+10^(-100),11);
    sum=sum+ ( (x+4) / ((x-3)^(1/3)) );
  }
  print("Valoare reala integrala: ")
  print(61.2);
  print("Valoare calculata integrala: ")
  print((8-10^(-100))*sum/N);
}

calcul_integrala_b(10000);

#III.3

calcul_integrala_c= function(N,lambda){
  sum = 0;
  for(i in 1:N) {
    u = rexp(1, lambda);
    sum = sum + (u*exp(-u*u))/(lambda*exp(-lambda*u));
  }
  print("valoare reala integrala: ")
  print(1/2)
  print("valoare calculata integrala: ")
  print(sum/N);
  print("eroare integrala:")
  print(abs(sum/N-1/2));
  return(sum/N); 

}

calcul_integrala_c(10000,1);

#IV.1

n=1000;
p=0.25;
q=0.01;

simuleaza_ani = function(numar_initial, numar_target,p,q,n){
  rezultate = vector();
  for(i in 1:n){
    numar_actual = numar_initial;
    ani = 1;
    while(numar_actual < numar_target){
      noi = rbinom(1,numar_actual,p);
      plecati = rbinom(1,numar_actual,q);
      numar_actual = numar_actual + noi - plecati;
      ani = ani + 1;
    }
    rezultate[i]=ani;
  }
  ani_average = mean(rezultate);
  print("ani average:")
  print(ani_average)
  return(ani_average);
}

simuleaza_ani(10000,15000,p,q,n);

simuleaza_ani_noprint = function(numar_initial, numar_target,p,q,n){
  rezultate = vector();
  for(i in 1:n){
    numar_actual = numar_initial;
    ani = 1;
    while(numar_actual < numar_target){
      noi = rbinom(1,numar_actual,p);
      plecati = rbinom(1,numar_actual,q);
      numar_actual = numar_actual + noi - plecati;
      ani = ani + 1;
    }
    rezultate[i]=ani;
  }
  ani_average = mean(rezultate);
  return(ani_average);
}

sansa_atingere_target = function(numar_target,numar_initial,numar_ani,numar_luni,p,q,k, N){
  numar_ani = numar_ani + numar_luni/12;
  rezultate = vector();
  for(i in 1:k){
    if(simuleaza_ani_noprint(numar_initial,numar_target,p,q,N)>numar_ani){
      rezultate[i]=0;
    }
    else {
      rezultate[i]=1;
    }
  }
  print("Sansa de atingere: ")
  print(mean(rezultate));
}

sansa_atingere_target(15000,10000,40,10,p,q,10,1000)

