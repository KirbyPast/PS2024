# III.4
#confidence_intervalT_file = function(file_name, alfa){
#  critical_t = -qt(alfa/2,n-1);
#  left=sample_mean - critical_t*s/sqrt(n);
#  right=sample_mean + critical_t*s/sqrt(n);
#  print(left);
#  print(right);
#}

ex_III_4 = function(filename, alfa) {
  x = scan(filename);
  critical_z = -qnorm(alfa / 2);
  sample_mean = mean(x);
  sigma = sd(x);
  n = length(x);
  left = sample_mean - critical_z * sigma/sqrt(n);
  right = sample_mean + critical_z * sigma/sqrt(n);
  
  
  print(left)
  print(right)
}

#EX_IV_1

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

# h0: p = 0.1
# ha: p > 0.1

proportion_test(150,0.13,0.13,0.05,'r')