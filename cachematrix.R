## A pair of functions which together can be used to compute the
## inverse of a matrix x in such a way that if x inverse has
## already been computed, it simply retrieves it from memory

## A matrix x is passed to makeCacheMatrix, which creates a list
## containing functions which allow you to:
## 1) Change the cached matrix x (set), this also clears the 
##    previously stored inverse
## 2) Retrieve the matrix x (get)
## 3) Cache a new inverse (setinverse)
## 4) Retrieve a the cached inverse (getinverse)

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) xinv <<- inv
  getinverse <- function() xinv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve is passed the list returned from makeCacheMatrix()
## and retrieves the inverse of x if it is stored, or computes the
## inverse and uses the setinverse function from the list to store
## it.

cacheSolve <- function(x, ...) {
  
  xinv <- x$getinverse()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data, ...)
  x$setinverse(xinv)
  xinv
  
}
