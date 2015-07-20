## This function creates a list with a function that sets, then gets the value of the matrix. It then sets and gets the value of the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function (m) {
        x<<- m
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list( set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##This function computes the inverse of the special matrix unless it has already been computed.
## If it already has been computed then it skips this computation and pulls it out of the cache.

cacheSolve <- function(x, ...) {
  inv<- x$getinverse()
  if(!is.null(inv)){
    message("get cached data")
    return(inv)
  }
  data <- x$get()
  inv<- solve(data)
  x$setinverse(inv)
  inv
}
