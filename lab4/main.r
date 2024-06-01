#I.1
disc_area = function(N){
  N_C=0
  for(i in 1:N){
    x=runif(1,-1,1);
    y=runif(1,-1,1);
    if(x*x+y*y<=1){
      N_C=N_C+1
    }
  }
  aria = 4*N_C/N;
  print(abs(pi-aria));
  print(abs(pi-aria)/abs(pi))
  return(4*N_C/N);
}

volum_sfera = function(N) {
  nc=0
  for(i in 1:N){
    x=runif(1,-1,1);
    y=runif(1,-1,1);
    z=runif(1,-1,1);
    if(x*x+y*y+z*z<=1){
      nc=nc+1;
    }
  }
  aria=8*nc/N;
  print(4*pi/3)
  return(8*nc/N)
}
#I.2 ALBASTRA
aria_parabola = function(N){
  nc=0;
  xmin=1/2
  xmax=2
  ymin=0
  for(i in 1:N){
    x=runif(1,0,2);
    y=runif(1,0,2);
    ymax=-2*x*x+5*x-2;
    if(x>=xmin&&x<=xmax&&y>=ymin&&y<=ymax){
      nc=nc+1;
    }
  }
  integrala=9/8
  print("I. 2 PROBLEMA ALBASTRA: ")
  print(integrala)
  print(abs(4*nc/N-integrala))
  print(abs(4*nc/N-integrala)/abs(integrala))
  return(4*nc/N)
}

aria_parabola(30000);

#ex II

mc_integration = function(N){
  sum=0;
  for(i in 1:N){
    u=runif(1,0,10);
    sum=sum+exp(-u*u/2);
  }
  
  return(10*sum/N)
}

mc_integr_avg = function(k,N){
  estimates=vector()
  for(i in 1:k){
    estimates[i]=mc_integration(N)
  }
  print(mean(estimates));
  print(sd(estimates));
}

MC_improved_integration = function(N) {
  sum = 0;
  for(i in 1:N) {
    u = rexp(1, 1);
    sum = sum + exp(-u*u)/exp(-u);
  }
  return(sum/N);
}


MC_imprvd_integr_average= function(k, N) {
  estimates = 0;
  for(i in 1:k)
    estimates[i] = MC_improved_integration(N);
  print(mean(estimates));
  print(sd(estimates));
}

#II.1 a)

prob_a_int = function(N){
  sum=0
  for(i in 1:N){
    x=runif(1,0,pi);
    sum=sum+sin(x)*sin(x)
  }
  print(pi/2)
  print(abs(sum/N-pi/2));
  print(abs(sum/N-pi/2)/(pi/2));
  print(sum/N);
}

#II.1 b) ALBASTRA
prob_b_int = function(N){
  sum=0;
  for(i in 1:N){
    u=runif(1,1,4);
    sum=sum+exp(u);
  }
  print("II. 1 B) PROBLEMA ALBASTRA")
  print(abs(3*sum/N-51.87987))
  print(abs(3*sum/N-51.87987)/abs(51.87987))
  return(3*sum/N);
}

prob_b_int(10000);

prob_b_int_noprint = function(N){
  sum=0;
  for(i in 1:N){
    u=runif(1,1,4);
    sum=sum+exp(u);
  }
  return(3*sum/N);
}

prob_b_avg = function(N,k){
  estimates=vector();
  for(i in 1:k){
    estimates[i]=prob_b_int_noprint(N);
  }
  print(mean(estimates));
  print(sd(estimates));
}

#II.1 d) ALBASTRA


prob_d_int = function(N){
  sum=0;
  for(i in 1:N){
    u=runif(1,1,10000)
    sum=sum+1/(4*u*u-1);
  }
  print("II.1 D) PROBLEMA ALBASTRA");
  print(abs(log(3/4)));
  print((10000-1)*sum/N)
}

prob_d_int(50000)


#II.2 


MC_improved_integration_2 = function(N,lambda) {
  sum = 0;
  for(i in 1:N) {
    u = rexp(1, lambda);
    sum = sum + exp(-2*u*u)/(lambda*exp(-lambda*u));
  }
  print(sqrt(pi/8))
  print(abs(sum/N-sqrt(pi/8)))
  print(abs(sum/N-sqrt(pi/8))/abs(sqrt(pi/8)))
  return(sum/N);
}

MC_improved_integration_2_noprint = function(N,lambda) {
  sum = 0;
  for(i in 1:N) {
    u = rexp(1, lambda);
    sum = sum + exp(-2*u*u)/(lambda*exp(-lambda*u));
  }
  return(sum/N);
}


MC_average = function(N,lambda,k){
  estimates=vector();
  for(i in 1:k){
    estimates[i]=MC_improved_integration_2_noprint(N,lambda);
  }
  print(mean(estimates))
  print(sd(estimates))
}

#III.1
Nr_days = function() {
  nr_days = 1;
  last_errors = c(27, 31);
  nr_errors = 27;
  while(nr_errors > 0) {
    lambda = (min(last_errors));
    nr_errors = rpois(1, lambda);
    last_errors = c(nr_errors, last_errors[1]) ;
    nr_days = nr_days + 1;
  }
  return(nr_days);
}

MC_nr_days=function(N){
  s=0;
  for(i in 1:N){
    s=s+Nr_days();
  }
  print(s/N);
    
}

Nr_days2 = function() {
  nr_days = 2;
  last_errors = c(13,15,9);
  nr_errors = 9;
  while(nr_errors > 0) {
    lambda = mean(last_errors);
    nr_errors = rpois(1, lambda);
    last_errors = c(nr_errors, last_errors[1], last_errors[2]) ;
    nr_days = nr_days + 1;
  }
  return(nr_days);
}

MC_nr_days2=function(N){
  s=0;
  for(i in 1:N){
    s=s+Nr_days2();
  }
  print(s/N);
  
}

#III.2 ALBASTRA 

ex_III_2 = function(){
  x=runif(1,0,1);
  if(x<0.75){
    return(rexp(1,4));
  }
  else{
    return(rexp(1,12));
  }
}

MC_ex_III_2 = function(k){
  s=0;
  for(i in 1:k){
    s=s+ex_III_2();
  }
  print("III.2 PROBLEMA ALBASTRA:")
  print(s/k);
}

MC_ex_III_2(10000)

#IV

Nr_days3 = function() {
  nr_days = 2;
  last_errors = c(18,22,28);
  nr_errors = 27;
  while(nr_errors > 0) {
    lambda = (min(last_errors));
    nr_errors = rpois(1, lambda);
    last_errors = c(nr_errors, last_errors[1],last_errors[2]) ;
    nr_days = nr_days + 1;
  }
  return(nr_days);
}

MC_Nr_days3 = function(N){
  s=0;
  for(i in 1:N){
    if(Nr_days3()>21) s=s+1;
  }
  print(s/N);
}

#IV 2

calculatoare = function(nr_calc,p_infectare,zile){
  nr_infectate = vector();
  for(zi in 1:zile){
    nr_infectate=1;
  }
  
}

