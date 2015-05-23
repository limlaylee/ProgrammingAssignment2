## The purpose of this assignment is to find an inverse of a square matrix

##  The first function makeCacheMatrix is a function 
##   where the input is x: a square invertible matrix and 
##   return: a list containing functions to
##      1. set the matrix
##      2. get the matrix
##      3. set the inverse
##      4. get the inverse
##  This list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
inv = NULL
set = function(y) {
x <<- y
inv <<- NULL
}
get = function() x
setinv = function(inverse) inv <<- inverse 
getinv = function() inv
list(set=set, get=get, setinv=setinv, getinv=getinv)
}

##  This function find the inverse of the matrix
##  The output x: output of makeCacheMatrix()
##  Return :the inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
inv = x$getinv()       
# if the inverse has already been calculated
if (!is.null(inv)){
# get it from the cache and skips the computation. 
message("getting cached data")
return(inv)
}
# otherwise, calculates the inverse 
mat.data = x$get()
inv = solve(mat.data, ...)
# sets the value of the inverse in the cache via the setinv function.
x$setinv(inv)
 
return(inv)
}
