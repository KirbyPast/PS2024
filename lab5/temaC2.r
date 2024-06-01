
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