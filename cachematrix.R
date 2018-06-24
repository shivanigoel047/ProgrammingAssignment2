## Cache Inverse of matrix

## set the matrix,  <<- operator which is used to assign a value to an object in an environment that is
## different from the current environment

makeCacheMatrix <- function(x = matrix()) {
        m <- Null
        set <- function (y) {
                x <<- y
                m <<- Null
        }
        get <- function (x)
        setInverse <- function(Inverse) m <<- Inverse
        getInverse <- function() m
        list(set= set, getmatrix = getmatrix, setInverse = setInverse, getInverse = getInverse)
}


## Calculates the inverse of matrix, t first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.Null(m)){
                message("getting Cache Data")
                return(m)
        }
        data <- x$get()
        Invmatrix <- Solve(data, ...)
        x$setInverse(Invmatrix)
        m
}
