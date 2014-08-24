## Put comments here that give an overall description of what your
## functions do

## creates a list of operations to get and set a matrix and its inverse from
## a cache mechanism

makeCacheMatrix <- function(x = matrix()) 
{
    privateInverse <- NULL
    
    set <- function(y)
    {
        x <<- y
        privateInverse <<- NULL
    }
    
    get <- function()
    {
        x
    }
    
    setInverse <- function(inverse)
    {
        privateInverse <<- inverse
    }
    
    getInverse <- function()
    {
        privateInverse
    }
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## returns the inverse of a matrix. If that inverse was previously calculated
## this method will just read it from a cache

cacheSolve <- function(x, ...) 
{
    ## try to get the inverse from the cache
    inverse <- x$getInverse()
    
    if(!is.null(inverse))
    {
        message("inverse found in the cache. Reading from there")
        return(inverse)
    }
    
    ## no luck, we have to compute the inverse
    theMatrix <- x$get()
    
    inverse <- solve(theMatrix)
    
    ## store the inverse in the cache
    x$setInverse(inverse)
    
    inverse
}
