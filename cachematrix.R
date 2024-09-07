## Cache matrix inverse result

## Provides caching helpers for matrix
makeCacheMatrix <- function(x = matrix()) {
    inversed <- NULL

    set <- function(y) {
        x <<- y
        inversed <<- NULL
    }
    get <- function() x
    setInversed <- function(i) inversed <<- i
    getInversed <- function() inversed

    list(
        set = set, get = get,
        setInversed = setInversed,
        getInversed = getInversed
    )
}


## Returns matrix inverse from cache if exists
## Otherwise calculates inverse and stores it in the cache
cacheSolve <- function(x, ...) {
    inversed <- x$getInversed()
    if (!is.null(inversed)) {
        message("getting cached data")
        return(inversed)
    }

    message("calculating inverse")
    data <- x$get()
    inversed <- solve(data, ...)
    x$setInversed(inversed)
    inversed
}

# Test
m <- matrix(c(
    5, 1, 0,
    3, -1, 2,
    4, 0, -1
), nrow = 3, byrow = TRUE)

x <- m
c <- makeCacheMatrix(x)
i <- cacheSolve(c)
cacheSolve(c)
res <- i %*% m
print(res)

# calculating inverse
# getting cached data
#      [,1] [,2] [,3]
# [1,]    1    0    0
# [2,]    0    1    0
# [3,]    0    0    1
