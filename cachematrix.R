# Two functions, makeCacheMatrix and cacheSolve, will be produced in order to
# take a matrix as input, create a special matrix object that can cache the 
# inverse matrix and compute the inverse matrix.


# makeCacheMatrix: This function takes a matrix as input 
# and creates a special matrix object that can cache the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL  # i = inverse matrix
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve  # setinverse = set inverse matrix
    getinverse <- function() i   # getinverse = get inverse matrix
    list(set = set, get = get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}


# cacheSolve: This function takes the output of the makeCacheMatrix function 
# above (a special matrix object) as its input and computes the inverse matrix.
# However, if the inverse has already been calculated and the matrix hasn't  
# changed, the cacheSolve should retrieve the inverse matrix from the cache 
# (created earlier by the makeCacheMatrix function) and skip the computation. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
