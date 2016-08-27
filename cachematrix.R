## These two functions, makeCacheMatrix() and cacheSole(),
## are designed to improve efficiency by not repeating the
## computation of the inverse of a matrix by cacheing it 
## and retrieving it as necessary.

## There are constructed by modifying a template of those 
## given in the example in Assignment 2, Week 3, from
## the Coursera R Programming course.

## makeCacheMatrix

## This takes a matrix as an argument. It sets the matrix as 
## the matrix and the inverse as NULL. It returns a list of 
## 4 nested functions that can be used to get and set the matrix 
## and its inverse.

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    setmatrix <- function(y) {
      x <<- y
      i <<- NULL
    }
    getmatrix <- function() x
    setinverse <- function(solve) i <<- solve(x)
    getinverse <- function() i
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setinverse = setinverse,
         getinverse = getinverse)
    }

## cacheSolve()

## This function takes the makeCaheMatrix() as an argument
## It searches the parent environment for a cached inverse
## and returns it if it is not NULL. If it is NULL it uses
## the relevent nested function to return the compute, nest
## and return the inverse. 

cacheSolve <- function(x, ...) {
  
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
    
  }else{
    data <- x$getmatrix()
    i <- solve(data, ...)
    x$setinverse(i)}
  i
  }
