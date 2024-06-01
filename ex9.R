reprezinta_geometric = function(p, n) {
    x=1:n
    probabilitati = dgeom(x, p)
    print(barplot(probabilitati,
                  names.arg = 1:n,
                  xlab = "k",
                  ylab = "Probabilitate", 
                  main = "Repartitia Geometrica"))
}

reprezinta_geometric(0.5,50)
