##Damien Edwards
## The cachematrix.R script contains the below functions
## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


## Function that prepares a matrix object to caches its inverse function

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversem <- function(inversem) m <<- inversem
  getinversm <- function() m
  list(set = set, get = get,
       setinversem, = setinversem,
       getinversem = getinversem)

}


## Function will run the inverse if the matrix isn't cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m<-x$getinversem()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-inversem(matrix, ...)
  x$inversem(m)
  m
}
