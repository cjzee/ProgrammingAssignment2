#==========================
## cachematrix.R
#==========================

## Functions to cache results from time-consuming computations.
## Basic concept is to create a "make" function that stores cached objects in a list and a "cache" function that saves time by computing  the cached object(s) and storing them in the "make"object or retrieving them from the "make"object if already cached.

## The "make" function can be called once to store the original object and placeholders for objects to be computed and set by the "cache" function (like mean, inverse matrix, etc)

#==========================
## Sample data and commands
#==========================

## m <- matrix(1:4,2,2)
## minv <- makeCacheMatrix(m) ## or minv <- makeCacheMatrix(matrix(1:4,2,2))
## minv$get()
## minv$getinverse() ## will return NULL the first time b/c the inverse of minv has not been cached yet
## cacheSolve(minv) ## first time called will compute the inverse, save it to makeCacheMatrix.getinverse() and return the inverse
## cacheSolve(minv) ## second time called will figure out 
## minv$get() %*% minv$getinverse() ## return the identity matrix from minv

## "make" function that accepts a matrix and stores it and the inverse if called ither 
## Note: the inverse can be loaded directly without using caseSolve 
## To do this
## mm <- makeCacheMatrix(matrix(1:4,2,2))
## mm$getinverse() ## returns NULL
## mm$setinverse(solve(m)) ## sets inverse
## mm$getinverse() ## returns cached inverse matrix
#==========================

## "make" function to store a matrix and it's inverse, with methods to get and set the inverse and get the original
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y = matrix()) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve(x)
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## "cache" function to get the cached inverse from the "make" object or to compute and set it if not found.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m  
}
