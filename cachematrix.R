## To test the functionaliety of this script load script and run the following
## commands in R console

##> x <- makeCacheMatrix(matrix(1:4, nrow=2))
##> cacheSolve(x)
##> cacheSolve(x)

## Script should return the inverted matrix of x. In second run the message that
## cached values are used should be displayed.



## Function that returns a vector (in this case a list) of functions.
## As an argument the function takes a matrix that can be inversed.
## The returned list contains function setting the value of the input matrix,
## setting the value of the input matrix, getting the value of the inverted matrix and
## setting the value of the inverted matrix.
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL #set inverted matrix to NULL initialy
  
  # when value of matrix changes, set value of input-matrix and mark
  # inverted matrix to be recalculated (NULL)
  set <- function(y)
  {
    x <<- y
    s <<- NULL 
  }
  
  # return value of input-matrix
  get <- function() x
  
  # set the cache-value to value of invered matrix
  setsolve <- function(solve) s <<- solve
  
  # return cached value
  getsolve <- function() s
  
  list (set = set, get=get,
        setsolve = setsolve,
        getsolve = getsolve)
}


## Return a matrix that is the inverse of 'x'
## Use value in cache if matrix has already been inverted
cacheSolve <- function(x, ...) {
        
  # check, if matrix has already been solved and result is cached. Otherwise, NULL will be returned
  s <- x$getsolve()
  if(!is.null(s))
  {
    message("getting cached data")
    return(s)
  }
  # get value of matrix that should be inverted
  data <- x$get()
  # invert the matrix
  s <- solve(data, ...)
  # set cached value to value of inverted matrix
  x$setsolve(s)
  s
}

