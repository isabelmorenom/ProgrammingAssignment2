#####################################################
##Functions that cache the inverse of a matrix
######################################################

## USAGE EXAMPLE:
## x <- matrix(1:4, nrow=2, ncol=2)
## m <- makeCacheMatrix(x)
## s <- cacheSolve(m)
## print(x)
## > x
##        [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## print(s)
## s should return:
##     [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##
## Check is x%*%s is the identity matrix as
##> x%*%s
##      [,1] [,2]
##[1,]    1    0
##[2,]    0    1

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # set function
  # Sets the matrix itself but not the inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get function
  # Gets the matrix itself but not the inverse
  get <- function() x
  
  # Manually set the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Get the inverse
  getinverse <- function() inv
  
  # Encapsulate into a list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)	
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
       
  inv <- x$getinverse()
  
  # If it has...
  if(!is.null(inv)) {
    # Simply return the computed inverse		
    message("Getting cached matrix")
    return(inv)
  }
  
  # If it hasn't...
  # Get the matrix itself
  data <- x$get()
  
  # Find the inverse
  inv <- solve(data, ...)
  
  # Cache this result in the object
  x$setinverse(inv)
  
  # Return this new result
  inv    
}
