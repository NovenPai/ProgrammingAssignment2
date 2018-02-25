## makeCacheMatrix function creates a special "matrix" object that can cache its inverse matrix in i

makeCacheMatrix <- function(x = matrix()) {
   i<- NULL
   set <- function(y) {
     x <<- y
     i <<- NULL
   }
   get <- function() x
   setinverse <- function(inverse) i <<- inverse
   getinverse <- function() i
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


##cacheSolve function first checks whether the inverse of the "matrix" created with makeCacheMatrix function has already been solved,
##if so,it gets the result from i and skips the computation
##if not, it computes the inverse and returns the inverse matrix.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  ## Return a matrix that is the inverse of 'x'
  i
}

a<-matrix(1:4,2,2)
cachedmatrix = makeCacheMatrix(a)
cacheSolve(cachedmatrix)
