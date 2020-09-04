
## author : Mathias
## ProgrammingAssignement2 Coursera
## Compute a 'special' matrix solving (inversion)
## Avoid to compute it again if the same matrix
## has been already just computed



## Function creating a list containing functions to
## 1.  set the matrix
## 2.  get the matrix
## 3.  set the solved matrix
## 4.  get the solved matrix

makeCacheMatrix <- function(x = matrix()) {
  
  invmat <- NULL
  
  ## set the matrix
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() {x}
  
  ## set the solved matrix
  setInverse <- function(inverse) {invmat <<- inverse}
  
  ## get the solved matrix
  getInverse <- function() {invmat}
  
  ## list the functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  

  get <- function() x
  
  ## set the solved matrix
  setInverse <- function(solvemat) invmat <<- solvemat
  
  ## get the solved matrix
  getInverse <- function() invmat
  
  ## list the functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
}

## Function computing the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  invmat <- x$getInverse()
  invmat <- x$getsolve()
  
  ## check if the matrix is already in cache
  ## return it
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  ## Else invert the new Matrix
  data <- x$get()
  invmat <- solve(data, ...)

  ## Set the new inverted matrix in the cache
  x$setInverse(invmat)
  
  invmat
}

