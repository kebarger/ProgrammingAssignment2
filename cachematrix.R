## Put comments here that give an overall description of what your
## functions do
## Assignment 2 - Kyle Barger - functions to cache, calculate, and retrieve 
##    inverse of a matrix

## Write a short comment describing this function
## Construct a list with the matrix & functions
makeCacheMatrix <- function(x = matrix()) {

# inverse of the matrix (initialize to NULL)  
    i <- NULL

# sets the value of the matrix
    set <- function(y) {
    x <<- y
    i <<- NULL
    }

# retrieves value of the matrix supplied    
  get <- function() x
  

# Calculates the inverse using solve
  setinv <- function(solve) i <<- solve
  
# Retrieves cached value previously calculated
  getinv <- function() i
  
# returns a list with all the functions defined above
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}




cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## if previously calculated, retrieve cached matrix
        ## x must be a list as output by makeCacheMatrix
  # Retrieve cached value
  i <- x$getinv()
  
  # If not null, then inverse has already been calculated. Return this value.
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  # Otherwise, retrieve the original matrix....
  data <- x$get()
  
  # Calculate the inverse....
  i <- solve(data, ...)
  
  # Cache the inverse...
  x$setinv(i)
  
  # and return the inverse.
  i
  
  }
