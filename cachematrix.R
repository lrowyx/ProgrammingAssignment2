## The first function contains four functions which can set, get, setsolve and getsolve
## the matrix x. It will then store the inverse result. The second function will check
## the cache and get the result if the inverse already been calculted, otherwise it will
## calculate the inverse and store it


## This function will cache the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)

}


## The solve function returns an inverse matrix of x, which is m here
## So when m is not null, the cacheSolve function will retrieve it

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
