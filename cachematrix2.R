## This code consists of two functions:
## 1) makeCacheMatrix to 
## 2) cacheSolve to solve or 

## makeCacheMatrix provides a set of functions that, when run, create a list of functions that perform
## parts of the inverse-matrix caching process. Individual functions are commented beside the representative
## code

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL                                            # nullify matrix inverse when function is called 
  reset <- function(xreset) {                             # function to reset data and nullify matrix inverse globally
    x <<- xreset                              
    minv <<- NULL
  }
  get <- function() x                                     # gets a new matrix for recomputation
  resetminv <- function(minvnew) minv <<- minvnew         # resets the matrix inverse globally from the recomputation
  getminv <- function() minv                              # gets the matrix inverse (which is cached)
  list(reset = reset, get = get,                          # list construction for each function
       resetminv = resetminv,
       getminv = getminv)

}

## cacheSolve 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  minv <- x$getminv()
  if(!is.null(minv)) {                                   # function checking if the matrix inverse isn't null, pull from cache if it exists
    message("getting cached data")
    return(minv)                                         # pull matrix inverse from cache
  }
  else {                                                 #if matrix inverse does not exist, calculate new
    data <- x$get()
    minv <- solve(data, ...)
    x$resetminv(minv)
  }
  minv                                                   #return matrix inverse
}
