## These two functions - makeCacheMatrix and CacheSolve - work together
## to speed the repeated use of an inverted matrix.  They do so by performing the inversion
## once then saving the result to the variable 'm'.  If the result 'm' is needed again, it is
## simply returned from the saved value instead of repeating the calculation.

## Note: the input matrix must be an invertible matrix

## Acknowledgement: based on sample functions from the course R Programming - Roger D. Peng - 
## Dept. of Biostatistics - Johns Hopkins Bloomberg School of Public Health



## This function creates its own list of functions for accessing or manipulating the input matrix 'x'.  
## They are: set, get, setinverse, and getinverse.
makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
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


## This function returns the inverse of a matrix 'x'.  It does so by checking if the
## inverse already exists from a previous call to the function 'getInverse' embedded in
## makeCacheMatrix function above.  If the result exists, it is simply returned 
## which avoids recalculating an existing result.
cacheSolve <- function(x, ...) {
   
   m <- x$getinverse()
   if(!is.null(m)) {
  
      message("getting cached data")
      return(m)
   }
   
   data <- x$get()
   m <- solve(data, ...)
   x$setinverse(m)
   m
   
}
