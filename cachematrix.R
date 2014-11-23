## The functions below create a matrix, and solve for it's inverse
## While at the same time, try to reduce the time taken for repetitive computations
## By caching already calculated inverses

## This function defines a bunch of functions which are store in a list, to:
# 1. Set the value of inverse as NULL everytime a new matrix is passed (first statement)
# 2. Define the matrix and pass it to the cacheSolve fn. (get)
# 3. Cache the value of inverse calculated (setinv)
# 4. Return the value of cached inverse (getinv)

makeCacheMatrix <- function(x = matrix()) {
  inv_cached <- NULL
  getmat <- function() {x}
  setcacheinv <- function(inv) {inv_cached <<- inv}
  getcacheinv <- function() {inv_cached}
  list(getmat = getmat,
       setinv = setcacheinv,
       getinv = getcacheinv)
}

## This function does the following, using the functions defined in makeCacheMatrix:
# 1. Checks if the inverse of the matrix passed is already calculated
# 2.1 If yes, it simple returns the cached value and displays a message
# 2.2 If not, it calculates the inverse using the solve function, and 
# 3. Caches the new value of inverse calculated
# 4. Finally, returns the inverse of the matrix passed

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting cached inverse:")
    return(inv)
  }
  newmatrix <- x$getmat()
  inv <- solve(newmatrix, ...)
  x$setinv(inv)
  inv
}