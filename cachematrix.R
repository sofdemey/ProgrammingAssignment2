#Assignment 2: Cache invert matrix - Sofdemey
# Certain calculations  take a lot of time and computational power, however if you 
# cache them you can recycle the result for further calculations.

# First you make an object containing the following:
# 1. a numeric vector x containing an element m without any defined form, hence the 
# NULL
# 2. several subfunctions (set, get, setsolve, getsolve), which include: adding the 
# inverse of the matrix to m and storing it in the cache.

makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
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


# The second part is investigating first if the revert matrix is already in the 
# cache and if not then it calculates the invert matrix and stores it in the cache.
# when the invert matrix is already in the cache then you get the message 
# "getting previous cached data"
# if this is not the case then you will be notified of calculation and storage in cache
cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting previous cached data")
                return(m)
        }
        message("calculating invert matrix and storing in cache")
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
