
# I.a)

generate_permutation = function(n){
  U = runif(n,0,1);
  permutation = sort(U);
  for(i in 1:n){
    for(j in 1:n){
      if(permutation[i]==U[j]){
        permutation[i]=j;
      }
    }
  }
  return(permutation);
}

generate_bit_string = function(n,k){
  x=sample(c(0,1),n*k,replace = TRUE);
  
  z=matrix(x,nrow = n, ncol = k);

  return(z);
}


# I.b)

compare_strings = function(L1,L2) {
  l = min(length(L1), length(L2));
  for( i in 1:l ){
    if(L1[i]<L2[i]){
      return(TRUE)
    }
    else if(L1[i]>L2[i]){
      return(FALSE)
    }
  }
  
  while(L1[length(L1)]==L2[length(L2)]){
    L1=append(L1,sample(c(1,0)))
    L2=append(L2,sample(c(1,0)))
  }
  
  
  if(L1[length(L1)]<L2[length(L2)]){
    return(TRUE)
  }
  else if(L1[length(L1)]>L2[length(L2)]){
    return(FALSE)
  }
}

z=generate_bit_string(5,10);

# I.c)

mareste_matrice = function(matrice,n,k,val){
  y=matrix(NA, nrow=n+1, ncol=k);
  count = dim(y);
  
  y[1:n , ] = matrice;
  y[n+1, ] = val;
  return(y)
}

randomized_quick_sort = function(bit_string){
  
  count = dim(bit_string);
  if(bit_string[1,1]==-1) return(matrix(NA,nrow=0,ncol=0));
  if(count[1] <= 1) return(bit_string);
  
  n=count[1];
  k=count[2];
  
  pivot_index = sample(1:count[1],1);
  pivot = bit_string[pivot_index, ]
  
  less=matrix(-1,nrow=1,ncol=1);
  greater=matrix(-1,nrow=1,ncol=1);
  
  
  for(i in 1:n){
    if(i == pivot_index ){
      next;
    }
    str = bit_string[i, ];
    
    cmp = compare_strings(str,pivot);
    
    if(cmp == TRUE){
      if(less[1,1]==-1){
        less = matrix(str,nrow=1,ncol=k,byrow=TRUE);
      }
      else{
        marimi_less = dim(less);
        less=mareste_matrice(less,marimi_less[1],marimi_less[2],str);
      }
    }
    else{
      if(greater[1,1]==-1){
        greater = matrix(str,nrow=1,ncol=k,byrow=TRUE);
      }
      else{
        marimi_greater = dim(greater);
        greater=mareste_matrice(greater,marimi_greater[1],marimi_greater[2],str);
      }
    }
    
  }
  #Transpunem mai intai matricea rezultat, apoi o facem vector iar apoi o concatenam cu celelalte rezultate,
  #urmand apoi sa o facem o matrice mare.
  return(matrix(c(as.vector(t(randomized_quick_sort(less))), bit_string[pivot_index, ], as.vector(t(randomized_quick_sort(greater)))),nrow=n,ncol=k,byrow=TRUE));
}

z=randomized_quick_sort(z);

# I.d)

generate_permutation_bitstring = function(n,k){
  z=generate_bit_string(n,k);
  
  x=randomized_quick_sort(z);
  perm = vector();
  for(i in 1:n){
    for(j in 1:n){
      if(identical(z[i, ], x[j, ])){
        x[j,1]=-1;
        perm[i]=j;
        break;
      }
    }
  }
  return(perm)
}

# II.a)

mat_adiac = matrix(c(0,0,0,0,0,0,1,1,0,1,0,0,0,1,0,0),nrow=4,ncol=4,byrow=TRUE)

calculeaza_taietura_maxima_aleator = function(mat,n){
  A=sample(1:n,n/2);
  noduri = c(1:n);
  B=vector();
  for(i in 1:n){
    ok=1;
    for(j in 1:(n/2)){
      if(A[j] == noduri[i]){
        ok=0;
      }
    }
    if(ok){
      B=c(B,i)
    }
  }
  
  taieturi = 0;
  for(i in 1:length(A)){
    for(j in 1:length(B)){
      if(mat[A[i],B[j]]==1){
        taieturi = taieturi + 1;
      }
    }
  }

  return(taieturi);
}

# II.b)

calculeaza_taietura_maxima_aleator_repetat=function(mat,n,k){
  rezultate = vector();
  for(i in 1:k){
    rezultate = c(rezultate,calculeaza_taietura_maxima_aleator(mat,n));
  }
  return(max(rezultate));
}

#APELURI FUNCTII:

#I.A
print("permutare subpct 1:")
print(generate_permutation(10));

print("generare siruri random ( 10 siruri de lungime 5 ): ")
print(generate_bit_string(10,5));

#I.B
print("sirul 1 mai mic decat sirul 2")
print(compare_strings(c(0,0),c(1,0)))
print("sirul 1 mai mare decat sirul 2")
print(compare_strings(c(1,0),c(0,1)))
print("Comparare siruri egale (nu va da mereu aceeasi valoare)")
print(compare_strings(c(1,0,0,1),c(1,0,0,1)));

#I.C
print("Sortarea unei matrice cu siruri random:")
print(randomized_quick_sort(generate_bit_string(10,5)));

#I.D
print("Generarea unei permutari utilizand stringurile de mai inainte")
print(generate_permutation_bitstring(10,5))

#II.A
print("Calcularea numarului 'maxim' de taieturi dintr-o singura incercare" )
print(calculeaza_taietura_maxima_aleator(mat_adiac,4))

#II.B
print("Calcularea numarului 'maxim' de taieturi din mai multe incercari (sansa de succes mult mai mare)")
print(calculeaza_taietura_maxima_aleator_repetat(mat_adiac,4,1000))


