

## Stores a matrix and caches its inverse calculated with cacheSolve

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      
      set <- function(y) {
              x <<- y
              m <<- NULL
              
      }
      get <- function() x
      getInverse <- function() m 
      setInverse <- function(solve) m <<- solve 
      list(set = set, get = get,setInverse = setInverse, getInverse = getInverse)
            
}


## Calculates the inverse of the matrix stored

cacheSolve <- function(x, ...) {        
        m <- x$getInverse() ## gets the inverse matrix stored in makeCacheMatrix function
        if(!is.null(m)) {   ## if the inverse is there return it
              message("getting cached data")
              return(m)
        }
        ## Otherwise calculate the inverse and store it back in makeCacheMatrix
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
        
        
}
