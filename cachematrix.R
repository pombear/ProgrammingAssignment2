## Matrix inversion is usually a costly computation and 
## there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }
  
        get <- function() x                   ## returns the input matrix
        setInv <- function(inv) xinv <<- inv  ## sets the inversed matrix
        getInv <- function() xinv             ## returns the inversed matrix

        list(set = set, get = get,
                setInv = setInv,
                getInv = getInv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        mtx <- x$getInv()                       
        if(!is.null(mtx)) {                     
                message("getting cached data")
                return(mtx)                           
        }
        data <- x$get()                       
        mtx <- solve(data)              
        x$setInv(mtx)                          
        mtx
}
