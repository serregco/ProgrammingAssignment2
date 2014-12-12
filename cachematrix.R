## This function cached the inverse matrix for a better performance

##makeCacheMatrix function does the following:
##set the value of the vector
##get the value of the vector
##set the value of the mean
##get the value of the mean
############################################

makeCacheMatrix <- function(x=matrix()) {
  inversa <- NULL
  set <- function(y) {
    x <<- y
    inversa <<- NULL
  }
  get <- function() x
  
  setinversa <- function(inv) inversa <<- inv
  getinversa <- function() inversa
  
  list(set = set, get = get, setinversa = setinversa, getinversa = getinversa)
}


## cacheSolve function does the inverse. if matrix isn't cached, function sets the value in cache

cacheSolve <- function(x,...) {
  inversa <- x$getinversa()
  if(!is.null(inversa)) {
    message("getting cached data")
    return(inversa)
  }
  data <- x$get()
  inversa <- solve(data)
  x$setinversa(inversa)
  inversa
}
