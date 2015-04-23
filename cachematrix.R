## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
                
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
         m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m

        ## Return a matrix that is the inverse of 'x'
}

#Just adding script to check that the function is working
n <- 64
    mat <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n)
    matCached <- makeMatrix(mat)
    matSolved1 <- cacheSolve(matCached)
    matSolved2 <- cacheSolve(matCached)
    if (!identical(matSolved1, matSolved2))
        message("Cached version does not match solved version")











n <- 1024
    mat <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n)
    matCached <- makeMatrix(mat)
    time1 <- system.time(matSolved1 <- cacheSolve(matCached))
    time2 <- system.time(matSolved2 <- cacheSolve(matCached))
    print(time1["user.self"])
    print(time2["user.self"])
if (!identical(matSolved1, matSolved2))
        message("Cached version does not match solved version")
