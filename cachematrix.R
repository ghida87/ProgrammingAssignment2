
# the makeCacheMatrix function creates a special Matrix object which is a list of 4 functions  
makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  # set a matrix  
  set <- function(y){
      x <<- y
      I <<- NULL
  }
  # get a matrix  
  get <- function() x
  # set the Inverse of a matrix  
  setInverse <- function(Inverse)  I <<- Inverse
  # get the Inverse of a matrix  
  getInverse <- function() I
    list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve function allows obtaining the inverse of a matrix from cache when available and re-computing the inverse when not available in cache

cacheSolve <- function(x, ...) {
  I <- x$getInverse()
  if (!is.null(I))
  {
    print("getting cached data")
    return(I)
    
  }
  data <- x$get()
  I <- solve(data)
  x$setInverse(I)
  I
  
}
