## THis file contains  two functions that are used to create a special object  
## that stores a square matrixr and its inverse

## This function creates a special "matrix", which is really a list containing 
## a function to
## · set the value of the matrix
## · get the value of the matrix
## · set the value of the inverse matrix
## · set the value of the inverse matrix
makeCacheMatrix <- function(Mtrx = matrix()) {
    
    inverseMtrx <- NULL
    
    set <- function(y) {
        Mtrx <<- y
        inverseMtrx <<- NULL
    }
    
    get <- function() Mtrx
    
    setInverse <- function(z) inverseMtrx <<- z
  
    getInverse <- function() inverseMtrx
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function gets special square matrix and returns a matrix that 
## is the inverse of 'x' calling the function solve;
## if the inverse has already been calculated, it gets the inverse 
## from the cache and skips the computation. 
cacheSolve <- function(x, ...) {
  
    inverse <- x$getInverse()
  
    ##if cachedMatrix == x
    if (!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }
            
    ##calculate inverse
    message("calculating inverse")
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setInverse(inverse)
      
    ##return inverse
    inverse
}
