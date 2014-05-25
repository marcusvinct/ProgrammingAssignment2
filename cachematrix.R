## This program calculates the inverse of a Matrix. For efficiency' sake, it also leverages
## a cache memory in order to avoid recalculating an inverse matrix previously done.
##
## Usage:
##      1. Assign the makeCacheMatrix' returned Matrix to a variable
##      2. Calculate its inverse Matrix using cacheSolve
##      3. Call cacheSolve one more time to verify it is using a cache
##      4. Clear cache redoing step 1 and repeat step 2 to check the cache is not used
##
## Example:
##      1. matrixTest = makeCacheMatrix( matrix( c(2,0,1,3,0,0,5,1,1), nrow=3, ncol=3, byrow=TRUE ) )
##      2. cacheSolve( matrixTest )
##      3. cacheSolve( matrixTest )
##      4. matrixTest = makeCacheMatrix( matrix( c(2,0,1,3,0,0,5,1,1), nrow=3, ncol=3, byrow=TRUE ) )
##         cacheSolve( matrixTest )
##

## makeCacheMatrix sets up an special Matrix object, which contains 2 variables
## getters and setters functions for each one of them.
## By default, the Matrix is initialized empty. Another matrix can be passed as parameter.
## The inverse of a Matrix can only be calculated for a squared matrix, which means it has to have the same number of rows and columns.
##

makeCacheMatrix <- function( dataMatrix = matrix() ) {
    inverseMatrix <- NULL
    
    ## Set the matrix
    set <- function( newDataMatrix ) {
        dataMatrix    <<- newDataMatrix
        inverseMatrix <<- NULL
    }
    
    ## Get the matrix
    get <- function() dataMatrix
    
    ## Set the inverse of the matrix
    setInverse <- function( inverse ) inverseMatrix <<- inverse
    
    ## Get the inverse of the matrix
    getInverse <- function() inverseMatrix
    
    ## Return a list with all setter and getter functions
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve returns the inverse of a makeCacheMatrix object passed as parameter.
## In case the inverse is stored in a cache, it returns the value stored there,
## otherwise the inverse is calculated.
## 

cacheSolve <- function( matrix, ... ) {
    ## Verify the inverse matrix is cached
    inverse <- matrix$getInverse()
    
    if ( !is.null( inverse ) ) {
        message( "Getting cached data..." )
        return( inverse )
    }
    
    ## In case it is not cached, calculate it
    data    <- matrix$get()
    inverse <- solve( data, ... )
    matrix$setInverse( inverse )
    
    inverse
}