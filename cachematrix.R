# The following two functions are used to cache the inverse of a matrix which can be 
# beneficial in avoiding repeated computation in performing inversion of a matrix.

# makeCacheMatrix creates a special "matrix" object that can cache its inverse. 
# It contains the list of fucntions to set the matrix and its inverse, also to get that matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated then the it should retrieve the inverse from the cache. 
# If not, it computes the inverse, sets the value in the cache via setinverse function.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
