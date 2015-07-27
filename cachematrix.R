#a caching function for inversing a matrix


makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## actual service function, applies the caching function.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}


## Usage
## A = matrix(rexp(1000000, rate=.1), ncol=1000)

## M = makeCacheMatrix(A)

## system.time(cacheSolve(M))

## system.time(cacheSolve(M))