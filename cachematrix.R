## these functions will cache the inverse of a matrix by 1) storing the
## value of the inverse and 2) calculates the inverse of the matrix using the first function


## this function creates a special matrix object and stores 1) the matrix ad 2) the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse,
    getInverse = getInverse)
}

## this function calculates the inverse using the above function - first it checks to see 
## if the mean has already been calculated and if not completes the calculation

cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i ## return a matrix that is the inverse of x
}
