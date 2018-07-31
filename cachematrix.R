## R-programming Week3 

## This function creats a special "Matrix" object that cache its inverse.

makeCacheMatrix <- function(x = matrix()) { i<- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function calculate the inverse of the matrix depending on the out put from the function above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv() 
  if(!is.null(i)) { 
    message("getting cached data") 
    return(i) 
    }
  data <- x$get() 
  i <- solve(data, ...) 
  x$setinv(i) 
  i 
}
