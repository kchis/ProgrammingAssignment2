##
## The functions 'makeCacheMatrix' and 'cacheSolve' can be used together 
## to calculate the inverse of a square invertible matrix and store the result
## in the cache. If the matrix inverse has previously been stored in the cache, 
## the result is retrieved from the cache and is not recalculated.
##
## Example:  invert a 3x3 diagonal matrix
##    X <- matrix(c(1,0,0,0,2,0,0,0,10), nrow=3, ncol=3)
##    f <- makeCacheMatrix()   #  assign the special "matrix" to name 'f'
##    f$set(X)                 #  input the matrix to be inverted (must be invertible)
##    f$get()                  #  check the matrix stored in cache
##    cacheSolve(f)            #  calculate the matrix inverse
##    f$getinv()               #  check the value of the inverse stored in cache
##    cacheSolve(f)            #  retrieve matrix inverse from cache
##

##
## The first function 'makeCacheMatrix' takes as input a matrix (may be NULL)
## and returns a special "matrix" object which is actually a list of four 
## functions used to set/get the matrix or its inverse to/from the cache
##    'set' assigns a value to the matrix to be inverted
##    'get' retrieves the value of the matrix
##    'setinv' assigns the value of the matrix inverse
##    'getinv' retrieves the value of the matrix inverse
## 

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) xinv <<- inv
    getinv <- function() xinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)  
}


##
## The second function 'cacheSolve' takes as input the special "matrix" object
## created by the first function 'makeCacheMatrix'. The function checks to see 
## if the matrix inverse already exists in the cache. If it does it returns the 
## value; otherwise the inverse is calculated using the solve() function.
## 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    xinv <- x$getinv()
    if(!is.null(xinv)) {
        message("getting inverse from cache...")
        return(xinv)
    }
    mat <- x$get()
    xinv <- solve(mat, ...)
    x$setinv(xinv)
    xinv
}
