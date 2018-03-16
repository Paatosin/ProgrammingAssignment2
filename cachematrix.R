#These two functions in this code creates first, creates an matrix whose inverse can be cached
#This works for only square matrices since only square matrces have determinants
# First Function: set the value of the matrix, get the value of the matrix..., 
# set the value of the inverse, gets the value of the inverse 

makeCacheMatrix <- function(x = matrix()) {
  p <- NULL
  set<- function(y){
    x<<-y
    p <<-NULL
  }
  get <- function() x
  setinverse <- function(inverse) p <<- inverse
  getinverse <- function() p
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# Write a short comment describing this function
# For an already computed matrix inverse which remains constant, 
#cacheSolve function will get the already computed value 
cacheSolve <- function(x, ...) {
  p <- x$getinverse()
  if(!is.null(p)){
    message("getting cached data")
    return(p)
  }
  data <- x$get()
  p <- solve(data, ...)
  x$setinverse(p)
  p
}