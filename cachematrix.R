
## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  #set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #get the value of the matrix
  get <- function() x
  #set the value of the inverse matrix
  setinv <- function(inv) m <<- inv
  #get the value of the inverse matrix
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}
## Write a short comment describing this function
cacheSolve <- function(x, ...) {
    m <- x$getinv()
    #check to see if the inverse has already been calculated
    if(!is.null(m)) {   
        #If so, it gets the mean from the cache and skips the computation
        message("getting cached data")
        return(m)
    }
    #Otherwise, it calculates the inverse of the data,
    #and sets the inverse in the cache via the setinv function
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
