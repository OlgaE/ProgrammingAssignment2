## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## If a matrix is given to makeCacheMatrix function, it returns an objects which contains the 
## matrix (with which the function was called), and four functions: set, get, setinverse and getinverse.
## These functions can be used to save the data to cache and get it back when needed.

makeCacheMatrix <- function(x = matrix()) {
  
  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) invm <<- solve
  getinverse <- function() invm
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function
## Suppose, we created a 2x2 matrix: mat=matrix(1:4,nrow=2) 
## and an object: mm=makeCacheMatrix(mat).
## Function cacheSolve(mm) will try to get the inverse first: invm <- x$getinverse() and will get NULL.
## Then it computes the inverse (invm <- solve(data)) and stores it in the cache: x$setinverse(invm).
## If we run thif function again with the same matrix, then the inverse will be take from cache:
## x$getinverse(), which is not NULL this time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  invm <- x$getinverse()
  if(!is.null(invm)) {
    message("getting cached data.")
    return(invm)
  }
  data <- x$get()
  invm <- solve(data)
  x$setinverse(invm)
  invm
}

## Example:
## > mat=matrix(1:4,nrow=2)
## > mat
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4

## > mm=makeCacheMatrix(mat)
## > cacheSolve(mm)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5

## > cacheSolve(mm)
## getting cached data.
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
