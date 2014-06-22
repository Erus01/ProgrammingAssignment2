# makeCacheMatrix creates a matrix object via makeCacheMatrix(matrix name). It should be assigned
# such as x<-makeCacheMatrix(matrix).

# cacheSolve calculates the inverse of the matrix and caches it via cacheSolve(x), if x is the
# same as above.

# This function creates an object from the matrix you input that you can use to cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  invMat <- NULL
  set <- function(y) {
    x <<- y
    invMat <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) invMat <<- inverse
  getInv <- function() invMat
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

# This function returns the inverse of the matrix input into makeCacheMatrix via the object
# created by running makeCacheMatrix(the matrix) if the resultant object is passed to it.
# If the matrix was previously calculated, it returns a message indicating it is retrieving
# rather than calculating it and prints that instead.
cacheSolve <- function(x, ...) {
  invMat <- x$getInv()
  ## Return a matrix that is the inverse of 'x'
  if(!is.null(invMat)) {
    message("getting cached inverse matrix")
    return(invMat)
  } else {
    
    invMat <- solve(x$get())
    x$setInv(invMat)
    return(invMat)
  }
}
