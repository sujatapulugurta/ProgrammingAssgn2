##makeCacheMatrix is a function for storing the matrix values 
##and cached values of inverse of matrix. some of the functions are
##setMatrix - to set the value of matrix
##getMatrix - to get values of matrix
##setInverse - to set the values of inverse of the matrix
##getInverse - to get the values of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  cache = NULL
  setMatrix <- function(y)
  {
    x <<- y
    cache <<- NULL
  }
  getMatrix <- function(){x}
  setInverse <- function(solve){
    cache <<- solve
  }
  getInverse <- function() cache
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse= getInverse)
  
}

##calculates inverse of the matrix created in makeCacheMatrix
cacheSolve <- function(x, ...) {
  
  inverse <- x$getInverse()
  if(!is.null(inverse))
  {
    message("getting cached data")
    return(inverse)
  }
  values <- x$getMatrix()
  inverse <- solve(values)
  x$setInverse(inverse)
  
  #return inverse
  inverse
}
