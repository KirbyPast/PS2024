a=c(0,1,1,0,1,0,1,1,0)
A=matrix(a,3,3);
b=c(1,1,0,0,1,1,0,1,0)
B=matrix(b,3,3);
c=c(1,0,0,1,1,0,0,1,0)
C=matrix(c,3,3);

Matrix_produs = function(A,B,C){
  n=nrow(A);
  r = matrix( , nrow = n, ncol = 1);
  x = matrix( , nrow = n, ncol = 1);
  y = matrix( , nrow = n, ncol = 1);
  r = sample(0:1, n, replace = TRUE);
  
  for( i in 1:n ){ #x=Br
    x[i] = 0;
    for(j in 1:nrow(B)){
      x[i] = ( x[i] + B[i,j] * r[j] ) %% 2;
    }
  }
  
  for( i in 1:n ){ #y=Ax = ABr
    y[i] = 0;
    for(j in 1:nrow(B)){
      y[i] = ( y[i] + A[i,j] * x[j] ) %% 2;
    }
  }
  
  for( i in 1:n ){ # x = Cr
    x[i] = 0;
    for(j in 1:nrow(B)){
      x[i] = ( x[i] + C[i,j] * r[j] ) %% 2;
    }
  }
  
  for( i in 1:n ){
    if(y[i] != x[i])
      return(FALSE);
  }
  return(TRUE);
}

Matrix_produs_reduce= function(A,B,C,k){
  for(i in 1:k){
    if(!Matrix_produs(A,B,C)){
      return(FALSE);
    }
  }
  return(TRUE);
  
}

tree_eval = function(i, leaves) {
  a = runif(1);
  len = length(leaves);
  if(log(i,2) >= log(len,2) - 1) { # copiii nodului i sunt frunze
    if(a <= 0.5) {
      if(leaves[2*i - len + 1] == 0){
        return(leaves[2*i +1 -len + 1]);
      }         
      return(1);
    }
    else {
      if(leaves[2*i + 1 -len + 1] == 0)
        return(leaves[2*i -len + 1]);
      }
      return(1);
    }
  if((floor(log(i,2))%% 2 == 0)){ # nodul i este de tip MIN
    if(a <= 0.5) {
      if(tree_eval (2*i, leaves) == 1){
        return(tree_eval (2*i + 1, leaves));
      }
     return(0);
    }
      else {
        if(tree_eval (2*i +1, leaves) == 1)
          return(tree_eval (2*i, leaves));
        }
        return(0);
      }
  if((floor(log(i,2))%% 2 == 1)){ # nodul i este de tip MAX
    if(a <= 0.5) {
      if(tree_eval (2*i, leaves) == 0){
        return(tree_eval (2*i + 1, leaves));
      }
      return(1);
    }
    else {
      if(tree_eval (2*i +1, leaves) == 0)
        return(tree_eval (2*i, leaves));
    }
    return(1);
  }      
}
  
