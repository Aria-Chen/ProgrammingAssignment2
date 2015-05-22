## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #solve
  #n阶  diag(n)#  <-solve(a,diag(n))   <- solve(x)
  get <- function() x
  setsolve <- function(solvea) m <<- solvea
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setsolve(m)
  m
}

#numbers = matrix(c(1,2,1,2,3,4,5,1,2,3,4,1,2,8,1,5),nrow=4,ncol=4)
#cachedNumbers = makeCacheMatrix(numbers)
#cacheSolve(cachedNumbers)
#matrix(numbers)
