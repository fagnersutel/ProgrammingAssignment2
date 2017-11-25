#primeira tarefa da segunda subimisao onde criamos a funcao makeCacheMatrix
#A ideia ?? criar um array que ir?? armazenar os dados computados das medias de tempo de consumo precedentes
# Para este fim ser?? utilizado o operador de atribui????o <<-que serve para atribuir valores a um objeto em ambientes diferente do atual.
#O Exemplo do exerc??cio demonstra a fun????o makeVector que cria o vetor 
#Como no exerc??cio de exemplo a fun????o se chamava 'makeVector' e agora chamaremos de 'makeCacheMatrix'
#inicio
makeCacheMatrix <- function(x = matrix()) {
    ma <- NULL # permanece como no exemplo original
    set <- function(y) { #repretimso a fun????o do exemplo original
        x <<- y #mesma variavel do exemplo original
        ma <<- NULL #mesma variavel do exemplo original
    }
    get <- function() x  #mesma variavel e fun????o do exemplo original
    setInv <- function(inverse) ma <<- inverse
    getInv <- function() ma
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}
cacheSolve <- function(x, ...) {
    ma <- x$getInv()
    if(!is.null(ma)) {
        message("getting cached data")
        return(ma)
    }
    data <- x$get()
    ma <- solve(data, ...)
    x$setInv(ma)
    ma

}

my_matrix <- matrix(c(4, 6, 8, 10), 2, 2)
my_matrix
run<- makeCacheMatrix(my_matrix)
run
run$get()
run$getInv()
cacheSolve(teste)
teste$getInv()
