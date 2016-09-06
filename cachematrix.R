## Functions that create a matrix and invert it using caching.

## The function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  nullVar<-NULL
  setInversable <- function(c){
    y<<-c
    nullVar<-NULL
  }
  get <- function()y
  setInverse<-function(inverted) nullVar<<-inverted
  getInverse<-function() nullVar
  list(set=set, get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}


## This function computers the inverse of the special "matrix"
## returned by makeCacheMatrix above.  If the inverse has already 
## been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  matrixInversed <- x$getInverse()
  if(!is.null(matrixInversed)){
    message("working on it!")
    return(matrixInversed)
  }
    theData<-x$get()
    matrixInversed<-solve(theData)
    x$setInverse(matrixInversed)
    matrixInversed
}
