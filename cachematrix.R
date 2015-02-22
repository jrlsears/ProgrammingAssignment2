## Two functions that work together to cache the inverse of a matrix

## makeCacheMatrix is the function that returns a small set of functions 
## in a list by storing a matrix and a cached value of the inverse of  
## the matrix. makeCacheMatrix includes the four sub-functions
## setMatrix      sets the value of a matrix
## getMatrix      gets the value of a matrix
## cacheInverse   sets the cached value of the inverse of the matrix
## getInverse     gets the cached value of the inverse of the matrix

makeCacheMatrix <- function(x = numeric()) {
        
        # holds cached value or, if nothing there yet
        # setting initial cache value to NULL
        cache <- NULL
        
        # storing matrix
        setMatrix <- function(newValue) {
            x <<- newValue
            # flush cache because  matrix is assigned new value
            cache <<- NULL
        }
        
        # returning stored matrix
        getMatrix <- function() {
            x
        }
        
        # caching the given argument 
        cacheInverse <- function(solve) {
            cache <<- solve
        }
        
        # getting the cached value
        getInverse <- function() {
            cache
        }
        
        # return list with each named element of the list as a function
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
    
}


## The cacheSolve function below calculates the inverse of a matrix 
## created with makeCacheMatrix as above

cacheSolve <- function(y, ...) {
        # getting cached value
        inverse <- y$getInverse()
        # if cached value exists return it
        if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
        }
        # else get the matrix, generate its inverse and store in cache
        data <- y$getMatrix()
        inverse <- solve(data)
        y$cacheInverse(inverse)
        
        # returning the inverse matrix
        inverse   

}
