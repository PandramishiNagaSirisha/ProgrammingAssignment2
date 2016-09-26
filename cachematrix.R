## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here).

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y = matrix()) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

    
# Assuming the matrix is always invertible.
# The following function returns the inverse of the matrix. 
# First checks if the inverse has already been computed. If so, it gets the result and skips
# Else  computes inverse sets the value in the cache via setinverse function.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

# Sample run
# Initialize a matrix
# > x = matrix(1:4,nrow = 2)
    
# Make cache matrix
# > mat = makeCacheMatrix(x)
# > mat$get()
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# mat variable contains matrix
    
# > mat$getinverse()
# NULL
# Inverse has not been calculated yet.
    
# > cacheSolve(mat)
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# Inverse of the matrix is cached
    
# > cacheSolve(mat)
# getting cached data.
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# Cached inverse matrix is retrieved without calculating it for the second time.
