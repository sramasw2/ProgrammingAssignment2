## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse. 
## The object is really a list containing a function to: 
## 1) set the value of the matrix.
## 2) get the value of the matrix.
## 3) set the value of the inverse.
## 4) get the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
         x <<- y
         i <<- NULL
     }
     get <- function() x
     setinv <- function(inverse) i <<- inverse
     getinv <- function() i
     list(set = set, get = get, 
          setinv = setinv, 
          getinv = getinv)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cacheSolve function will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
     i <- x$getinv()
     if (!is.null(i)) {
         message("Getting cached data.")
         return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinv(i)
     i
}

## Here is a sample run.
## 
## > origMat <- matrix(rnorm(16), 4, 4)
## > origMat
##             [,1]        [,2]       [,3]       [,4]
## [1,] -0.49204781  0.60856303 -0.3840946 -0.3332996
## [2,]  0.50400617  0.41860442  0.2587557  1.1107699
## [3,] -1.97179308  0.02151116  0.2056339 -0.4992796
## [4,]  0.04483006 -1.25782635 -1.1584693  1.3165704
##
## > invMat <- makeCacheMatrix(origMat)
## > cacheSolve(invMat)
##             [,1]       [,2]       [,3]        [,4]
## [1,] -0.08388985 -0.1453316 -0.5255932 -0.09794265
## [2,]  0.89107738  0.4679589 -0.1075229 -0.21000238
## [3,] -1.03509439  0.3035742  0.3269350 -0.39417995
## [4,] -0.05661959  0.7191470  0.2028462  0.21540730
##
## > cacheSolve(invMat)
## Getting cached data.
##             [,1]       [,2]       [,3]        [,4]
## [1,] -0.08388985 -0.1453316 -0.5255932 -0.09794265
## [2,]  0.89107738  0.4679589 -0.1075229 -0.21000238
## [3,] -1.03509439  0.3035742  0.3269350 -0.39417995
## [4,] -0.05661959  0.7191470  0.2028462  0.21540730

## > solve(origMat)
##             [,1]       [,2]       [,3]        [,4]
## [1,] -0.08388985 -0.1453316 -0.5255932 -0.09794265
## [2,]  0.89107738  0.4679589 -0.1075229 -0.21000238
## [3,] -1.03509439  0.3035742  0.3269350 -0.39417995
## [4,] -0.05661959  0.7191470  0.2028462  0.21540730