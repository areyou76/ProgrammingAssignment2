## Assignment 2 - areyou76

## makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.

## makeCacheMatrix creates a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}



## cacheSolve
## The following function returns the inverse of the matrix. It first checks if
## the inverse has already been computed. If so, it gets the result and skips the
## computation. If not, it computes the inverse, sets the value in the cache via
## setinverse function.


## This function assumes that the matrix is always invertible.


cacheSolve <- function(x, ...) {
  

  ## Return a matrix that is the inverse of 'x'
  
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    
    message("getting cached data")
    
    return(inv)
    
  }
  data <- x$get()
  
  
  inv <- solve(data)
  
  x$setinverse(inv)
  
  
  inv
  
  
}


##-----------------------------------------------------

## Output test:

## > x = rbind(c(1, -2/4), c(-2/4, 1))
## > t = makeCacheMatrix(x)

## > t$get()
## [,1] [,2]
## [1,]  1.0 -0.5
## [2,] -0.5  1.0

## > cacheSolve(t)
## [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333

##-----------------------------------------------------


