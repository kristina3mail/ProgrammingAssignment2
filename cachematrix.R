# The two functions below are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to 
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of the matrix
# get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


# The following function calculates the inverse of the matrix. It first checks to see if 
# the inverse has already been calculated. If so, it gets the invers from the cache and 
# skips the computation. Otherwise, it calculates the inverse of the data and sets the value 
# in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

# Sample
# > m <- makeCacheMatrix(matrix(3:6, 2, 2))
# > m$get()
#       [,1] [,2]
# [1,]    3    5
# [2,]    4    6
# > m$getinverse()
# NULL
# > cacheSolve(m)
#       [,1] [,2]
# [1,]   -3  2.5
# [2,]    2 -1.5
# > cacheSolve(m)
# getting cached data
#       [,1] [,2]
# [1,]   -3  2.5
# [2,]    2 -1.5
