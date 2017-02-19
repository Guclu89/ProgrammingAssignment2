## R Programming Assignment 2
## Assignment: Caching the Inverse of a Matrix

## Mehmet Guclu

## Example Assignment (caching the mean of a vector) is done and this assignment is implemented through the similar way within makeCacheMatrix and cacheSolve functions to cache the inverse a matrix

##Catche Matrix Function
#A matrix object is created by cachematrix function and it can cache its inverse
#Object Initializing

makeCacheMatrix <- function(x = matrix()) { 
  
  m <- NULL
        
  set <- function(y){
    x <<- y 
    m <<- NULL  
  }
        
#makeCacheMatrix is defined by getter and setter for data objects.
  
  get <- function() x
  
  setinverse <- function(inverse) m <<- inverse
  
  getinverse <- function() m
  
#Creating a new object by returning a list
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)    
}

##catchsolve function
#Calculating the inverse of the matrix returned by makeCacheMatrix via cacheSolve function

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  
  if(!is.null(m)) {
    message(" inverse of the matrix returned by makeCacheMatrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

##Computing

#Computing the inverse of a square matrix can be done within the solve(X) function in R. 
#For example, if X is a square invertible matrix, then solve(X) returns its inverse.


X <- matrix(c(1,2,3,4), c(2,2))
det(X)
#[1] -2
solve(X)
#      [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
          
X <- matrix(c(3,4,1,2), c(2,2))       
solve(X)
#      [,1] [,2]
[1,]    1 -0.5
[2,]   -2  1.5


