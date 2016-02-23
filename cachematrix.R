## two functions that cache the inverse of a matrix to save computing time
## for R Programming Assignment 2
## execute the functions with something like
## matx <- makeCacheMatrix(matrix(data=rnorm(100), nrow=10, ncol=10))
## cacheSolve(matx)

## makeCacheMatrix creates a list of functions to set a matrix, get the matrix,
## set the inverse, and get the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <-NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve determines whether the inverse has been cached, if so, it gets the value
## if not, it calculates the inverse and caches the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
