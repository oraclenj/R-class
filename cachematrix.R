## add the matrix to makeCacheMatrix first
## cacheSolve will get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

## pass in a square matrix x
## set changes the matrix x
## get returns the matrix x 
## setinv creates the inverse of the matrix x
## getinv returns the inverse of the matrix x
## 

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {

## use try to get the cached inverse of matrix x and return
## if no cached version of x exists, cacheSolve will cause one to be created
##    and return it

    m <- x$getinv()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinv(m)
    m
}
