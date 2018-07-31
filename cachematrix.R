## R-programming Week3 

## This function creats a special "Matrix" object that cache its inverse.

makeCacheMatrix <- function(x = matrix()) { i<- NULL    ## define i as a matrix
  set <- function(y) {                                  ## the set function
    x <<- y                                             ## value of matrix in parent environment
    i <<- NULL                                          ## a nex value of x will reset i to be NULL
  }
  get <- function() x                                   ## the get function
  setinv <- function(inverse) i <<- inverse             ## value of i in parent environment 
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)                                 ## paste the fuctions above together for next step
}


## This function calculate the inverse of the matrix depending on the out put from the function above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()                                     ## check if the inverse has already been calculated
  if(!is.null(i)) { 
    message("getting cached data") 
    return(i)                                           ## if it has, return i directly
    }
  data <- x$get() 
  i <- solve(data, ...)                                 ## otherwise calculate the inverse of the matrix
  x$setinv(i)                                           ## and set the value i in the cache 
  i 
}
