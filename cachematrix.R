# The first function, makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of matrix 
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(matrix = matrix())
{
  inversion <- NULL

  set <- function(parm)
  {
    matrix <<- parm
    inversion <<- NULL
  }
  
  get <- function() matrix
  
  setinverse <- function(inverse) inversion <<- inverse
  getinverse <- function() inversion
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# The following function calculates returns the inverse of the matrix. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse
# in the cache via the setinverse function.

cacheSolve <- function(matrix, ...)
{
  inversion <- matrix$getinverse()
  
  if(!is.null(inversion))
  {
    message("Getting cached data!")
    return(inversion)
  }
  
  data <- matrix$get()
  inversion <- solve(data)
  matrix$setinverse(inversion)
  
  inversion
}
