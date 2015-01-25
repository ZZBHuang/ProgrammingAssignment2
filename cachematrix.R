## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix have 4 sub-funcs that handle inverse matrix from input maxtrix
makeCacheMatrix <- function(x = matrix()) {
    ## use M to been the inverse matrix from input
    M <- NULL
    
    ## Set input maxtrix function
    set <- function(y) {
        x <<- y
        M <<- NULL
    }
    
    ## Get input maxtrix function
    get <- function() x
    
    ## Set inverse matrix function
    setinverse <- function(inverse) M <<- inverse
    
    ## Get inverse maxtrix function
    getinverse <- function() M
    
    ## List of all sub-functions
    list(set = set, get = get, setinverse = setinverse
         , getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve return the catched inverse matrix from input matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    ## Get inverse from makeCacheMatrix$getinverse function
    M <- x$getinverse()
    
    if(!is.null(M)) { ## If there has the catched inverse matrix, return it
        message("getting cached data")
        return(M)
    }
    
    ## If there has no catched inverse matrix, calculate it and save to catche
    maxtrix <- x$get()
    M <- solve(maxtrix, ...)
    x$setinverse(M)
    ## return inverse matrix
    M
}
