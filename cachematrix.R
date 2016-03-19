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

## Test: testing the solution requires 5 steps

## Step 1: Run Function 1 (see above) to create makeCacheMatrix()

## Step 2: Run Function 2 (see above) to create cacheSolve()

## Step 3: Create a variable of class 'matrix'
## diag() is used to create a diagonal matrix 

matrix <- diag(2,2)

## Step 4: Create a new variable using Function 1 with matrix variable from Step 3

cachematrix <- makeCacheMatrix(matrix)

## Step 5: Generate inverse matrix with Function 2 with variable from Step 4

cacheSolve(cachematrix)
