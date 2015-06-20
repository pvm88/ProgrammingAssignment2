## The makeCacheMatrix functioncreates a special "vector", 
## which is really a list containing functions to
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() { 
    x
  }
  
  setinverse <- function(inverse) { 
    m <<- inverse
  }
  
  getinverse <- function()  {
    m 
  }
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The following function calculates the inverse matrix of the special
## "vector" created and returns a matrix (possibly cached) that is the 
## inverse of 'x'
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse of matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## code to test 

my_matrix <- matrix(c(1,5,1,1,1,1,1,1,2),3,3)
mcm <- makeCacheMatrix(my_matrix)
mm_inv <- cacheSolve(mcm)
mm_inv <- cacheSolve(mcm)
mm_inv %*% my_matrix
my_matrix %*% mm_inv
