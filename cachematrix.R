## Put comments here that give an overall description of what your
## functions do
## The functions below inverse a matrix, with a caching logic to reduce compute power.
## The assignment is for Coursera Data Science: R Programming
## Week 3 Assignment; GitHub user: Kisyaclark

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  
    inv <- NULL            ## creating a variable that will hold value of matrix inversion 
    set <- function(y) {     ## creating a function that assign new value of matrix in parent env
      x <<- y 
      inv <<- NULL          ## if there is a new matrix, reset inv to NULL
    }
    get <- function() x     ## define the get function
  
  setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
  getinverse <- function() inv                     ## gets the value of inv 
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ##use to refer to function
    
  }
  



## Write a short comment describing this function
## This function looks to see if there is a cached inverse matrix
## If nothing in cache it calculates the inverse of teh matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}
