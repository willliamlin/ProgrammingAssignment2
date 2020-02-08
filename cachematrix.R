## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinv() #get the cached inverse first
  if(!is.null(inverse)) { #if the cached inverse is not null
    message("getting cached inverse") #message
    return(inverse) #return cached inverse 
  }
  data <- x$get() #else get the matrix stored in "get"
  inverse <- solve(data,...) #solve the matrix to get the inverse
  x$setinv(inverse) #store the new solved inverse
  inverse #return inverse
  }

#test run
test<- makeCacheMatrix(matrix(data = rnorm(9), nrow = 3, ncol = 3))
test
cacheSolve(test)

