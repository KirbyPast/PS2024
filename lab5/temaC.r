# ex albastru de data trecuta

ex_1 = function(n, k) {
  vals = 1:k;
  probabilitati = runif(k);
  probabilitati = probabilitati / sum(probabilitati);
  print(probabilitati);

  x=sample(vals, 1, TRUE, probabilitati);
  print(x)
}



# 1. a) prima parte

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

generate_permutation(10);

permutation = vector();
permutation = generate_permutation(10);
print(permutation);

generate_bit_string = function(n){
  x=c(0,1);
  bit_string = sample(x,n,replace = TRUE);
  print(bit_string);
}

print(generate_bit_string(10))


generate_bit_strings <- function(n, k) {
  #bit_strings <- replicate(n, paste(sample(c(0, 1), k, replace = TRUE), collapse = ""))
  x=character();
  for(i in 1:n){
    bit_string = sample(c(0,1),k,replace = TRUE)
    x=bit_string;
  }
  print(x)
  #print(bit_strings)
}

print(generate_bit_strings(5,6))

generate_bit = function(){
  return(sample(c(0,1),1))
}

lexicographic_compare <- function(Wi, Wj) {
  Lij <- min(nchar(Wi), nchar(Wj))

  #Comparam pana la lungimea sirului mai scurt
  
  for (l in 1:Lij) {
    if (substr(Wi, l, l) < substr(Wj, l, l)) {
      return(TRUE)
    } else if (substr(Wi, l, l) > substr(Wj, l, l)) {
      return(FALSE)
    }
  }
  
  #Dupa, incepem sa adaugam caractere la sfarsitul sirurilor pana nu sunt egale
  
  while(TRUE){
    if(nchar(Wi)<nchar(Wj)){
      Wi= paste(Wi, sep = "",generate_bit())
    } else if(nchar(Wj)<nchar(Wi)){
      Wj= paste(Wj, sep = "",generate_bit())
    } else{
      Wi = paste(Wi, sep = "", generate_bit())
      Wj = paste(Wj, sep = "", generate_bit())
    }
    
    if(substr(Wi,nchar(Wi),nchar(Wi)) < substr(Wj,nchar(Wj),nchar(Wj))){
      return(TRUE);
    } else if(substr(Wi,nchar(Wi),nchar(Wi)) > substr(Wj,nchar(Wj),nchar(Wj))){
      return(FALSE);
    }
  }
  #True daca primul sir e mai "mic" decat al doilea
}

randomized_quick_sort <- function(bit_strings) {
  if (length(bit_strings) <= 1) return(bit_strings)
  
  pivot_index <- sample(length(bit_strings), 1)
  pivot <- bit_strings[pivot_index]
  less <- c()
  equal <- c()
  greater <- c()
  
  for (str in bit_strings) {
    cmp <- lexicographic_compare(str, pivot)
    if (cmp == TRUE) {
      less <- c(less, str)
    } else if (cmp == FALSE) {
      greater <- c(greater, str)
    } else {
      equal <- c(equal, str)
    }
  }
  
  return(c(randomized_quick_sort(less), equal, randomized_quick_sort(greater)))
}

randomized_quick_sort(generate_bit_strings(5,6));


generate_sorted_permutation <- function(n, k) {
  bit_strings <- generate_bit_strings(n, k)
  sorted_strings <- randomized_quick_sort(bit_strings)

  
  permutation <- vector()
  for(i in 1:n){
    for(j in 1:n){
      if(bit_strings[i]==sorted_strings[j]){
        permutation[i] = j;
      }
    }
  }
  print(permutation)
  return(permutation)
}

generate_sorted_permutation(10,6)







#monte_carlo_max_cut <- function(G, n) {
#  vertices <- V(G)
#  A <- sample(vertices, n)
#  B <- setdiff(vertices, A)
#  cut_edges <- E(G)[(inc(A) & inc(B))]
#  return(length(cut_edges))
#}


#monte_carlo_max_cut_optimized <- function(G, n, iterations) {
#  max_cut_size <- 0
#  for (i in 1:iterations) {
#    cut_size <- monte_carlo_max_cut(G, n)
#    if (cut_size > max_cut_size) {
#      max_cut_size <- cut_size
#    }
#  }
#  return(max_cut_size)
#}
