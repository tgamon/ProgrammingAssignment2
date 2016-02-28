## The functions will help reduce costly computations of the inverse of a matrix
## if the computation was performed on the matrix the result is retrieved from 
## the cache instead of repeating the computation.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
                  
                  m <- NULL
                  set <- function(y){
                         x <<- y
                         m <<- NULL
                  }
                  get <- function() x
                  setinverse <- function(solve) m <<- solve
                  getinverse <- function() m
                  list(set = set, get = get,
                       setinverse = setinverse,
                       getinverse = getinverse)

}


## This function computes the inverse of the "matrix" returned by makeCacheMatrix
## if the inverse has been calculated (and the matrix unchanged) cachesolve retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        ##Check if inverse of 'x' is computed and return if it exists
        if(!is.null(m)){
              message("getting cached data")
              return(m)
        }
        ##If inverse is NULL then compute inverse, cache result, and return
        ##a matrix that is the inverse of 'x'
        data <- x$get()
        m <- solve(data,...)
        x$setinverse(m)
        m
}
