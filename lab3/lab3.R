density_exp = function(lambda,a,n){
  x=seq(0,a,length=n);
  y=dexp(x,lambda);
  plot(x,y,type='l')
}

density_gauss = function(m,sigma,a,n){
  x=seq(m-a,m+a,length=n);
  y=dnorm(x,m,sigma);
  plot(x,y,type='l')
}

density_gamma = function(shape, rate, a, n){
  x=seq(0,a,length=n);
  y=dgamma(x,shape, rate);
  plot(x,y,type='l');
}

density_student = function(r,a,n){
  x=seq(-a,a,length=n);
  y=dt(x,r)
  plot(x,y,type='l');
}

lnm_poisson = function(lambda,n){
  sum=0;
  for(i in 1:n){
    sum=sum+rpois(1,lambda);
  }
  print(sum/n)
}

lnm_poisson2 = function(lambda, n){
  print(mean(rpois(n,lambda)))
}

lnm_gamma = function(shape, rate, n){
  print(shape/rate)
  print(mean(rgamma(n,shape,rate)))
}

lnm_student = function(r,n){
  print(mean(rt(n,r)))
}

lnm_exp = function(lambda,n){
  m=1/lambda;
  xn=mean(rexp(n,lambda));
  print(abs(m-xn));
  print(abs(m-xn)/abs(m))
  print(xn);
}

tlc_poisson = function(lambda, n, N, z) {
  expectation = lambda;
  st_dev = sqrt(lambda);
  upper_bound = z * st_dev/sqrt(n) + expectation;
  sum = 0;
  for(i in 1:N) {
    x_n = mean(rpois(n, lambda));
    if(x_n <= upper_bound) {
      sum = sum + 1;
    }
  }
  print(pnorm(z));
  return(sum/N);
}


tlc_exp = function(lambda,n,N,z){
  expectation=1/lambda;
  st_dev=1/lambda;
  upper_bound = z * st_dev/sqrt(n) + expectation;
  sum = 0;
  for(i in 1:N) {
    x_n = mean(rexp(n, lambda));
    if(x_n <= upper_bound) {
      sum = sum + 1;
    }
  }
  print(pexp(z));
  return(sum/N);
}

tlc_gamma = function(shape,rate,n,N,z){
  expectation=shape/rate;
  st_dev=sqrt(shape)/rate;
  upper_bound = z * st_dev/sqrt(n) + expectation;
  sum = 0;
  for(i in 1:N) {
    x_n = mean(rgamma(n, shape,rate));
    if(x_n <= upper_bound) {
      sum = sum + 1;
    }
  }
  print(pgamma(z,shape,rate));
  return(sum/N);
}

dml = function(n,p,k){
  expectation=n*p;
  st_dev=sqrt(n*p*(1-p));
  xd=(k+0.5-expectation)/st_dev;
  xs=(k-0.5-expectation)/st_dev;
  print(pnorm(xd)-pnorm(xs));
  print(dbinom(k,n,p));
}

dml_1 = function(n,p,k1,k2){
  expectation=n*p;
  st_dev=sqrt(n*p*(1-p));
  xd=(k2+0.5-expectation)/st_dev;
  xs=(k1-0.5-expectation)/st_dev;
  print(pnorm(xd)-pnorm(xs));
  print(sum(dbinom(k1:k2,n,p)));
}

