## Put comments here that give an overall description of what your
## functions do

## I am creating two functions which will be used to create a special object that's stored as
      ## a matrix and cache its inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 # Inverse of matrix begins as NULL
      i <- NULL
   # Set function to change values in matrix
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
   #Get the matrix
      get <- function() x
      
      #Set the inverse of the matrix to i after obtaining inverse
      setinverse <- function(inverse) i <<- inverse
      
      # Obtain inverse of matrix from cache
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## Write a short comment describing this function

## This function calculates the inverse of the special matrix created from the above function.
## It first checks to see if it has already been calculated. If so, it returns the inverse of the
## matrix and skips the computation. If not, then it takes the special matrix created from the 
## above function, computes and returns the inverse of that matrix

## This function is creating an object that will be stored as a matrix
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
      if(!is.null(i)) {                    ## If inverse is already calculated, skip computing and return inverse
            message("getting cached data")
            return(i)
      }
      matrixdata <- x$get()         ## Get the matrix and solve and return the inverse
      i <- solve(matrixdata, ...)
      x$setinverse(i)
      i
        ## Return a matrix that is the inverse of 'x'
}
