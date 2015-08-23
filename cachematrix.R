## The two functions together provide a method to cache a
## matrix and its inverse. After the first computation 
## subsequent computations retrieve the inverse from the cache
## Example to use the functions
## e.g. 
##    x <- matrix(c(1:4),2,2)
##    xcache <- makeCacheMatrix(x)
##    ...
##    invx <- cacheSolve(xcache)

## makeCacheMatrix function returns a list of 4 functions
## set and get the matrix
## set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    nv <- NULL
    set <- function(y) {
        x <<- y
        nv <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) nv <<- inv
    getInverse <- function() nv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve returns a matrix that is the inverse of the matrix stored in x
## Here x is the list object returned by makeCacheMatrix
## cacheSolve wil check if the inverse for the matrix has been computed
## if yes, it will return it from the cache
## if not, it will compute it will return the result from the solve command

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of matrix stored in x
    nv <- x$getInverse()
    if(!is.null(nv)) {
        #message("getting cached data")
        return(nv)
    }
    data <- x$get()
    nv <- solve(data, ...)
    x$setInverse(nv)
    nv
}
