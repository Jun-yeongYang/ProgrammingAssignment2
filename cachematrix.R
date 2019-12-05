makeCacheMatrix <- function(x = matrix())
{
 m <- NULL
 set <- function(y) {
   x <<- y
   m <<- NULL
 }
 get <- function() x
 setInverse <- function(solveMatrix) inverse <<- solveMatrix
 getInverse <- function() m
 list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the matrix which is created by makeCacheMatrix above mentioned. If the inverse has solved before, then it recoveries the inverse from the cache.

cacheSolve <- function(x, ...)
{
 ## Return a matrix that is the inverse of 'x'
 m <- x$getInverse()
 if(!is.null(m)) {
   message("getting cached data")
   return(m)
 }
 data <- x$get()
 m <- solve(data)
 x$setInverse(m)
 m
}
