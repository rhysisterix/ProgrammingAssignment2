## The first function sets & gets the value of the matrix. 
## Then it sets the Inverse and finally gets the inverse.
## The second function checks whether the value has already been cached
## If so then it returns the cache if not then does the calculation

## This function returns a spcial list of functions.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function checks whether the matrix has already been cached and hasn't changed.
## If it's not in the cache then it makes the calculation
cacheSolve <- function(x = matrix(), ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    print("Cache already exists")
    return(m)
  } else {
    print("calculating Inverse")
    data <- x$get()
    x$set(data)
    m <- solve(data)
    x$setInverse(m)
    return(m)
  }
}
