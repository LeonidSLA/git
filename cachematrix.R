makeCacheMatrix <- function(x = matrix()) {
  ## create a matrix object x and get/set sub-functions
  
  ## define the cache m
  m <- NULL
  set <- function(y) {
    x <<- y ## assign the input matrix y to the variable x in the
    ## parent environment
    m <<- NULL ## re-initialize m in the parent environment to null
  }
  get <- function() x ## return the matrix x
  setinverse <- function(inverse) m <<- inverse ## set the cache m equal
  ## to the inverse of the matrix x
  getinverse <- function() m ## return the cached inverse of x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created
## with the above function. However, it first checks to see if the inverse
## has already been caclulated. If so, it 'get's the inverse from the cache
## and skips the computation. Otherwise, it calculates the matrix inverse
## and sets the value of the inverse in the cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ##check the cached inverse value.
  m <- x$getinverse()
  if(!is.null(m)) {
    
    ## if it is not null return  the value of cached inverse
    
    message("getting cached data")
    return(m)
  }
  
  ## if null calculate and set the inverse value of 
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}