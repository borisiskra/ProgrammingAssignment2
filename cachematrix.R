## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
