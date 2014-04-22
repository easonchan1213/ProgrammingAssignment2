## Put comments here that give an overall description of what your
## functions do

# if a user calls cacheSolve function to invert a matrix, the function will first look
# up if "i", which is the inverse of the matrix, may or may not exist in 
# memory/environment. If yes, function will return that value; if not, function will
# then use solve() function to find the inverse of the matrix and store it in memory for
# cache.




## Write a short comment describing this function

# this function takes in a matrix and serves as support function for 
# cacheSolve() function. It can get or set the inverse of a matrix which may
# or may not be found in memory.



makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
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


## Write a short comment describing this function

# this function calls makeCacheMatrix() function to see if the inverse of a matrix
# is pre-existent. If yes, it'll skip calling solve() function and get the inverse
# of the matrix that has been in memory instead. In this way it saves time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached inverse matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
