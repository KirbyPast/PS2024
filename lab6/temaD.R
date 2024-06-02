

#I.a)

interval_incredere = function(alfa, filename) {
  y = read.csv(filename,header=TRUE);
  x= y[["probabilitati"]]
  n = length(x);
  sigma = sqrt(92.16);
  sample_mean = mean(x);
  critical_z = -qnorm(alfa / 2);
  left = sample_mean - critical_z * sigma/sqrt(n);
  right = sample_mean + critical_z * sigma/sqrt(n);
  print(left)
  print(right)
}

print("Intervalul de incredere 95%:")
interval_incredere(0.05,"probabilitati.csv")
print("Intervalul de incredere 99%:")
interval_incredere(0.01,"probabilitati.csv")

interval_incredere_fara_sigma = function(alfa, filename) {
  y=read.csv(filename,header=TRUE);
  x=y[["statistica"]];

  sample_mean = mean(x);
  s = sd(x);
  n = length(x);
  
  critical_t = -qt(alfa / 2, n - 1);
  left = sample_mean - critical_t * s/sqrt(n);
  right = sample_mean + critical_t * s/sqrt(n);
  print(left)
  print(right)
}

print("Intervalul de incredere 95%:")
interval_incredere_fara_sigma(0.05,"statistica.csv")
print("Intervalul de incredere 99%:")
interval_incredere_fara_sigma(0.01,"statistica.csv")


proportion_test = function(n, p_prim, p_0, alfa, type){#type = 'l', 'r', 's'
  z_score = (p_prim - p_0) / (sqrt(p_0 * (1 - p_0) / n))
  print(z_score)
  if (type == 'l'){
    critical_z = qnorm(alfa);
    print(critical_z)
    if(z_score<critical_z){
      print("H0  se respinge si se accepta Ha")
    }
    else{
      print("H0 nu se poate respinge")
    }
  }
  if (type == 'r'){
    critical_z = qnorm(1 - alfa);
    print(critical_z)
    if(z_score>critical_z){
      print("H0  se respinge si se accepta Ha")
    }
    else{
      print("H0 nu se poate respinge")
    }
  }
  if (type == 's'){
    critical_z = -qnorm(alfa/2);
    print(critical_z)
    if(abs(z_score)>abs(critical_z)){
      print("H0  se respinge si se accepta Ha")
    }
    else{
      print("H0 nu se poate respinge")
    }
  }
  
}

print("Nivel de semnificatie 5%")
proportion_test(100,0.86,0.85,0.05,'r')
print("Nivel de semnificatie 1%")
proportion_test(100,0.86,0.85,0.01,'r')

