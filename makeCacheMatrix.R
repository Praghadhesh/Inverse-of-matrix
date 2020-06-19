##Set of functions created to obtain and store values of the matrix in various elements.
##Using get inverse and set inverse variables, we produce the inverse matrix. 
## x is a square inversible matrix
makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j ##returns the inverse matrix
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## Returns the inverse of the matrix
cacheSolve <- function(x, ...) {
  #Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  #if the inverse has been calculated
  if(!is.null(j)){
    #get it from the cache and return it or otherwise calculate it
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...) #setting the inverse to the cache
  x$setInverse(j)#returns the calculated value
  j
}

