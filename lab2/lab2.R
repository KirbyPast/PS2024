ex_II_1 = function(filename){
  date=scan(filename)
  x=mean(date)
  y=median(date)
  print(x)
  print(y)
}

ex_II_2 = function(filename){
  life = read.csv(filename,header=T,sep=',')
  x=life[['male']]
  y=life[['female']]
  print(mean(x))
  print(median(x))
  print(mean(y))
  print(median(y))
}

outliers_mean = function(x){
  m=mean(x)
  s=sd(x)
  y=vector()
  j=0
  for(i in x){
    if(i<m+2*s && i>m-2*s){
      j=j+1
      y[j]=i
    }
    else print(i)
  }
  print(y)
}

outliers_quartile = function(x){
  y=as.vector(quantile(x))
  rez=vector()
  j=0
  iqr=y[3]-y[1]
  for(i in x){
    if(i>y[1]-1.5*iqr && i <y[3]+1.5*iqr){
      j=j+1;
      rez[j]=i;
    }
    else print(i)
  }
  print(rez)
}

outliers_mean_file = function(filename){
  x=scan(filename)
  j=0
  m=mean(x)
  y=vector()
  s=sd(x)
  for(i in x){
    if(i<m+2*s && i>m-2*s){
      j=j+1
      y[j]=i
    }
    else print(i)
  }
  print(y)
}

outliers_quartile_file = function(filename){
  x=scan(filename)
  y=as.vector(quantile(x))
  rez=vector()
  j=0
  iqr=y[3]-y[1]
  for(i in x){
    if(i>y[1]-1.5*iqr && i <y[3]+1.5*iqr){
      j=j+1;
      rez[j]=i;
    }
    else print(i)
  }
  print(rez)
}
