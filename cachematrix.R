## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()) {
  
  ## Initialize the inverse property
  z <- NULL
 
   ## Method to set the matrix
  set <- function(y){
    m <<- y
    z <<- NULL
  }
  ## Method the get the matrix
  get <- function() m
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) z <<- inverse
  
  ## Method to get the inverse of the matrix
  getInverse <- function() z
  
  ## Return a list of the methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  z <- x$getInverse()
  
  ## Return the inverse if is  set
  if(!is.null(z)){
    message("getting cached data")
    return(z)
  }
  
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse
  z <- solve(data)
  
  ## Set the inverse
  x$setInverse(z)
  
  ## Return matrix
  z      
}