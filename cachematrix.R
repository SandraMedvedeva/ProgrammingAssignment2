## this stack of fuctions allows you to cache the invert of the matrix
## use makeCacheMatrix to initiate the cache structure
## use cacheSolve to get the cached value

## makeCacheMatrix returns a list of 4 functions as an output value
## set(x)   - settint the value x
## get()    - gettint the value x
## set_solve(x) - setting the value of some "property" of x 
##              (in this case it would be the inverse)
## get_solve()  - getting the inverse of the matrix x

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  ## setting the original matrix
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  ## getting the original matrix
  get <- function() x
  ## setting the income value (it should be the result of solve(x))
  set_solve <- function(solve) s <<- solve
  ## extracting the value of the cache
  get_solve <- function() s
  list(get = get, set = set, get_solve = get_solve, set_solve = set_solve)
}

## cacheSolve returns a cached result of solve() function for matrix
## If cached value has not been found, cacheSolve exists it
## !!!attention - argument x is the result of makeCacheMatrix function 
##                but not an original matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$get_solve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data_matrix <- x$get()
  s <- solve(data_matrix, ...)
  x$set_solve(s)
  s
}
