## Cache the inverse of a matrix to improve execution time if it is repeated


## First part - function makeCacheMatrix stores a list of functions and caches the input in the main function
#set m to null to reset previous stored values
#create a list of 4 functions - set, get, setinv, getinv that recalls m
#set function
        #substitute x with y x in main function using "<<" 
        #reset m to null
#get function
        #recall x value stored in main function
#setinv function
        #m as input for solve function (inverse matrix)
#getinv function
        #retrieve inverse of x (stored as m) from cache

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

## Second part - check matrix inverse - recall from cache if already computed (m), calculate if not


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

n <- 1024
    mat <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n)
    matCached <- makeCacheMatrix(mat)
    time_uncached <- system.time(matSolved1 <- cacheSolve(matCached))
    time_cached <- system.time(matSolved2 <- cacheSolve(matCached))
        #Compare time difference
    print(time_uncached["user.self"])
    print(time_cached["user.self"])
        #Check value
if (!identical(matSolved1, matSolved2))
        message("Cached version does not match solved version")
