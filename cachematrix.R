## Calculate and cache the inverse of a matrix


## makeCacheMatrix takes a matrix as an argument and
## creates a pseudo-matrix  which is really a list 
## containing functions to set and get the matrix 
## and set and get the inverse.
## Note that the original matrix is hidden away
## in the get function 
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(newMatrix){
        ## set the values of the actual matrix and reset the cache
        ## we do this in the parent environment
        x <<- newMatrix
        inverse <<- NULL
    }
    get <- function(){
        x
    }
    setInverse <- function(inv){
        inverse <<- inv
    }
    getInverse <- function(){
        inverse
    }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve calculates and caches the inverse 
## of the pseudo-matrix created above.
## Subsequent calls with the same "matrix" as argument
## will bypass the calculation and retrieve the result
## from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## first, look in the cache:
        inv <- x$getInverse()
        if(!is.null(inv)){
            ## we got lucky: 
            message("getting cached data")
            return(inv)
        }
        ## otherwise, we have to calculate it:
        myMatrix <- x$get()
        inv <- solve(myMatrix, ...)
        ## and put it in the cache for next time:
        x$setInverse(inv)
        inv
}
