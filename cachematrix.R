## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function( dataMatrix = matrix() ) {
    inverseMatrix <- NULL
    
    set <- function( newDataMatrix ) {
        dataMatrix <<- newDataMatrix
        inverseMatrix <<- NULL
    }
    
    get <- function() dataMatrix
    
    setInverse <- function( inverse ) inverseMatrix <<- inverse
    
    getInverse <- function() inverseMatrix
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if ( !is.null( inverse ) ) {
        message( "Getting cached data..." )
        return( inverse )
    }
    data <- x$get()
    inverse <- solve( data, ... )
    x$setInverse( inverse )
    inverse
}
