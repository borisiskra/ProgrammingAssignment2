## These two functions create a "cached matrix inverse",
## initialy the value of the nverse is set to NULL. The first time
## the inverse is need is calculated and stored in the field "x_inv"
## after this first calculation, any attempt to get the inverse again
## will result in returning the "cached" value, so sparing a second
## calculation of the same inverse.

## makeCacheMatrix creates an "object" with two fields, the matrix x,
## an the inverse x_inv, and four methods "get" and "set" to get and  
## set the value of the matrix, and "getinv" and "setinv" to get and 
## the value of the inverse. The initial value of "x_inv" is NULL.
##

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
            set <- function(y) {
                    x <<- y
                    x_inv <<- NULL
            }
            get <- function() x
            setinv <- function(inv) x_inv <<- inv
            getinv <- function() x_inv
            list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
}


## cacheSolve tries to get the inverse of the matrix x, from
## its cached value. If it is NULL, then is calculated,
## if it is not NULL is returned from its cached value via
## the "getinv" method.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x_inv <- x$getinv()
        if(!is.null(x_inv)) {
                message("getting cached inverse")
                return(x_inv)
        }
        x_matrix <- x$get()
        x_inv <- solve(x_matrix, ...)
        x$setinv(x_inv)
        x_inv
}
