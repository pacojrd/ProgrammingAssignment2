## Asigna un objeto una Matrix
##
## Objeto que obtienen la inversa de una matriz para lo cual se han definido los metodos: set, get, setsolve y getsolve

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x ## metodo para obtener la matriz almacenada
  setinversa <- function(solve) m <<- solve ##  asigna la matriz inversa definda
  getinversa <- function() m ## obtiene la matriz inversa
  list(set = set, get = get,
       setinversa = setinversa,
       getinversa = getinversa) 
}

## Esta función calcula la matriz inversa (x) pasando por el cache de la memoria para saber si existe dicha matriz (x),
## de ser así, recupera de la memoria cache la inversa de (x)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinversa()
  if(!is.null(m)) {
    message("Datos apartir de la memoria cache")
    return(m)
  }
  data <- x$get()
  if(det(data)!=0) {
    m <- solve(data, ...)
    x$setinversa(m)
    m
  } else {
    message("Esta matriz no tiene inversa")
  }
}
## definir una matriz con cual trabajar
mi_matriz <- matrix(c(5, 7, 13, 2, 4, 1, 7, 7, 14, 3, 11, 3, 8, 5,7, 5), nrow=4, ncol=4, byrow=T, 
                    dimnames=list(c("Blanco", "Negro", "Rojo", "Azul"), c("Toyota", "Audi", "Nissan","Hyundai")))
