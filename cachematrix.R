## The following functions are for creating a matrix and
## calculating its inverse efficiently.

## This function creates a special "matrix" object 
## that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
    invmtrx <- NULL
    updated <<- NULL
    
    set <- function(y) {
        x <<- y
        invmtrx <<- NULL
        updated <<- TRUE
    }
    get <- function() x
    setinv <- function(inv) {
        invmtrx <<- inv
        updated <<- FALSE
    }
    getinv <- function() invmtrx
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.If the inverse has already
## been calculated (and the matrix has not changed),then cacheSolve 
## will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invmtrx <- x$getinv()
    
    if(!is.null(invmtrx) & !updated) {
        message("getting cached data")
        return(invmtrx)
    }
    data <- x$get()        
    invmtrx <- solve(data)
    x$setinv(invmtrx)
    invmtrx
}
