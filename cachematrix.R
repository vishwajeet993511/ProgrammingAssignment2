## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {  #function for creating cachematrix
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
         #setting the variable y in form of cache
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m    #setting m as cache in other environment
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
                          #solve function actually implementing inverse
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data, ...)  #getting inverse using Solve function 
  x$setInverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
