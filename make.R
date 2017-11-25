#primeira tarefa da segunda subimisao onde criamos a funcao makeCacheMatrix
#A ideia sera criar um array que ira armazenar os dados computados das medias de tempo de consumo precedentes
# Para este fim sera utilizado o operador de atribuicao <<-que serve para atribuir valores a um objeto em ambientes diferente do atual.
#O Exemplo do exercicio demonstra a funcao makeVector que cria o vetor 
#Como no exercicio de exemplo a fun????o se chamava 'makeVector' e agora chamaremos de 'makeCacheMatrix'
#inicio
makeCacheMatrix <- function(x = matrix()) {
    ma <- NULL # permanece como no exemplo original
    set <- function(y) { #repretimos a funcao do exemplo original
        x <<- y #mesma variavel do exemplo original
        ma <<- NULL #mesma variavel do exemplo original
    }
    get <- function() x  #mesma variavel e funcao do exemplo original
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
#criamos uma matriz 2x2
my_matrix <- matrix(c(4, 6, 8, 10), 2, 2)
#exibimos a mariz
my_matrix
#executamos a funcao makeCacheMatrix e atribuimos a variavel run
run<- makeCacheMatrix(my_matrix)
#exibimos o conteudo de run
run
#como run e agora um objeto makeCacheMatrix acessamos o metodo get()
run$get()
#como run e agora um objeto makeCacheMatrix acessamos o metodo getInv()
run$getInv()
#passamos o objeto run como parametro para a funcao cacheSolve
cacheSolve(run)
#como cacheSolve executou o restante das tarefas podemos executar o objeti run e acessar o metodo getInv
run$getInv()
