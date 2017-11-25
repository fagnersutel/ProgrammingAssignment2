#primeira tarefa da segunda subimiss??o onde criamos a fun????o makeCacheMatrix
#A ideia ?? criar um array que ir?? armazenar os dados computados das medias de tempo de consumo precedentes
# Para este fim ser?? utilizado o operador de atribui????o <<-que serve para atribuir valores a um objeto em ambientes diferente do atual.
#O Exemplo do exerc??cio demonstra a fun????o makeVector que cria o vetor 
#Como no exerc??cio de exemplo a fun????o se chamava 'makeVector' e agora chamaremos de 'makeCacheMatrix'

makeCacheMatrix <- function(x = matrix()) {
    ma <- NULL # permanece como no exemplo original
    set <- function(y) { #repretimso a fun;??o do exemplo original
        x <<- y #mesma variavel do exemplo original
        ma <<- NULL #mesma variavel do exemplo original
    }
    get <- function() x  #mesma variavel e fun????o do exemplo original
    setmean <- function(mean) ma <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}
cacheSolve <- function(x = matrix()) {
    ma <- x$getmean()
    if(!is.null(ma)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    ma <- mean(data, ...)
    x$setmean(ma)
    ma

    }