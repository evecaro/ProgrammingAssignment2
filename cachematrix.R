
## Description of function makeCacheMatrix:
##This function creates a special "matrix" object that can cache its inverse.
##This function allows you to store 4 functions (set, get, setmatrixinv, getmatrixinv) 
##in the same list. 
##The functions of the list are used to:
  ##1)set: set the value of the matrix
  ##2)get: get the value of the matrix
  ##3)setmatrixinv: set the value of  the inverse matrix 
  ##4)getmatrixinv: get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrixinv <- function(solve) m <<- solve
  getmatrixinv <- function() m
  list(set = set, get = get,
       setmatrixinv = setmatrixinv,
       getmatrixinv = getmatrixinv)
}


## Description of function cacheSolve:
##The following function calculates the inverse of the special “matrix” created 
##with the makeCacheMatrix function. It first checks to see if the inverse 
##has already been calculated and the matrix has not changed. If so, it get the 
##inverse from cache and skips the computation.
##Otherwise, it calculates the inverse of the data and sets the value of the 
##inverse in the cache via the setmatrixinv function. 

cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getmatrixinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrixinv(m)
  m
}
