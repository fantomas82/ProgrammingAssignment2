## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # Initializing inverse and new variables
  inverse <- NULL
  new <- TRUE
  
  # Sets the matrix value received in the function to the
  # cached variable
  set <- function(y) {
    x <<- y
    inverse <<- NULL
    new <<- TRUE
  }
  
  # Retrieves the cached matrix
  get <- function() x
  
  # Retrieves the cached state "new"
  getnew <- function() new
  
  # Sets the inverse of the matrix
  # and stores it in the cache
  # Changes new flag to false
  setinverse <- function(a){
    inverse <<- a
    new <<- FALSE
  }
  
  # Retrieves the stores inverse matrix
  getinverse <- function() inverse
  
  # Creates list of functions to be returned
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse,
       getnew = getnew)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # Retrieves the variables of the special cached matrix
  inverse <- x$getinverse()
  new <- x$getnew()
  
  # Verifies if inverse is calculated and if its not a new matrix
  if(!is.null(inverse) && isFALSE(new)) {
    message("getting cached data")
    return(inverse)
  }
  
  # Calculates inverse:
  # Retrieves matrix
  data <- x$get()
  # Calculates inverse
  inverse <- solve(data, ...)
  # Saves inverse to cache
  x$setinverse(inverse)
  # Returns inverse
  inverse
}
