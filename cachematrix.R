## Assignment 2: cache the inverse of a matrix to avoid computing it repeatedly
## The program requires 2 functions

## Funtion 1: This function creates a "matrix" object that can cache its inverse
## i - variable for the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
   i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Funtion 2: This function computes the inverse of the "matrix" returned in Function 1 

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

## Test: testing the solution requires 3 steps
## Step 1: Create a variable of type 'matrix'
## diag() is used to create a diagonal matrix 

matrixa <- diag(2,2)

## Step 2: Create a new variable using Function 1 with matrix from Step 1

cachematrix <- makeCacheMatrix(matrixa)

## Step 3: Generate inverse matrix with Function 2 and the variable from Step 2

cacheSolve(cachematrix)
