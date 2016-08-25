## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve(x)
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
      
    }else{
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)}
    i
    
        ## Return a matrix that is the inverse of 'x'
}
  mat <- matrix (c(1,2,3,4), nrow =2, ncol =2)
  
  mymatrix <- makeCacheMatrix(mat)
  
  cacheSolve(mymatrix)