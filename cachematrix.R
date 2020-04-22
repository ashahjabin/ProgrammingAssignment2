##This assignment is a requirement for the Coursera R programming course. 
##The following function creates a special "matrix" object that can cache its inverse. 
##This function Function makeCacheMatrix gets a matrix as an input, set the value
##of the matrix, get the value of the matrix, set the inverse Matrix and get the inverse Matrix. 
###The matrix object can cache its own object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<-NULL
  }
  get <- function() x
  
  setinverse <- function(inverse)  inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The next function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.

> cacheSolve <- function(x, ...) {
         invMatrix <- x$getInverse()
        if(!is.null(invMatrix)) {                       
        message("Getting Cached Invertible Matrix")   
              return(invMatrix)                             
        }
        MatrixData <- x$getMatrix()                     
      invMatrix <- solve(MatrixData, ...)            
      x$setInverse(invMatrix)                         
    return(invMatrix)                               
}
