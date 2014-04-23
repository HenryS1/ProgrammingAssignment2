## makeCacheMatrix creates a special matrix which stores a cached inverse to avoid recomputing the inverse
## cacheSolve looks inside a cacheMatrix for a precomputed inverse
## if it's there cacheSolve returns the cached
## inverse, otherwise cacheSolve computes the inverse of the cacheMatrix and stores it inside the cacheMatrix

## i is the matrix inverse, which is NULL when the cacheMatrix is created
## set is used to change the value of the matrix and deletes the cached inverse i by setting it to NULL
## setinverse assigns a value to the cached inverse variable i
## getinverse returns the value of the cached inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve checks whether x has a precomputed inverse using getinverse
## if the inverse exists (isn't NULL) then the cached inverse is returned
## otherwise data is assigned the matrix value stored in x 
## the inverse of data is computed with solve and
## this inverse is stored inside x using setinverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
