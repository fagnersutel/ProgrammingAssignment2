#primeira tarefa da segunda subimiss??o onde criamos a fun????oo makeCacheMatrix
#A ideia ?? criar um array que ir?? armazenar os dados computados das medias de tempo de consumo precedentes
# Para este fim ser?? utilizado o operador de atribui;??o <<-que serve para atribuir valores a um objeto em ambientes diferente do atual.
#O Exemplo do exerc??cio demonstra a fun;??omakeVector que cria o vetor 
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmean <- function(mean) m <<- mean
    getmean <- function() m
    list(set = set, get = get,
         setmean = setmean,
         getmean = getmean)
}
cacheSolve <- function(x = matrix()) {
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m

    }