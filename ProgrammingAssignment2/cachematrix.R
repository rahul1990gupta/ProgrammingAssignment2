## Matrix Inversion require a lot of computation. For this purpose, it makes sense to store the 
## value of already computed calculation and then returning the same. the functio makeCacheMatrix does the 
## following things

##  set the value of the matrix
##  get the value of the matrix
##  set the value of inverse of the matrix
##  get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function, given a matrix, first checks via getinverse function if inverse is already computed
## if that is the case, it then skips the computation and return the result. Otherwise, it compute the inverse with the help of solve function
## and then set the inverse in inv variable



cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }

    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

## Sample run:
## > x <- rbind(c(1,2), c(2,1))
## > m <- makeCacheMatrix(x)
## > m$get()
##     [,1] [,2]
##     [1,]    1    2
##     [2,]    2    1


## No cache in the first run
## > cacheSolve(m)
##         [,1]       [,2]
##[1,] -0.3333333  0.6666667
##[2,]  0.6666667 -0.3333333


## Retrieving from the cache in the second run
## > cacheSolve(m)
## getting cached data.
##         [,1]       [,2]
##[1,] -0.3333333  0.6666667
##[2,]  0.6666667 -0.3333333
## > 
