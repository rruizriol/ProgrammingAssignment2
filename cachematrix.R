## Put comments here that give an overall description of what your
## functions do

## Create a cacheable vector

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      
      list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)

}


## Return the inverse of a matrix, first try to return from the cacha if exists

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      
      if(!is.null(m)){
        message("getting cached matrix")
        return(m)
      }
      
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}
