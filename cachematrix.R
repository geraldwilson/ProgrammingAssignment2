
## Short comment describing this function
##The first function, makeCacheMatrix creates a special "vector", 
##which is really a list containing 4 functions to 
##1.set -- set the value of the matrix
##2.get -- gets the input matrix
##3.setinv -- set the invsersed matrix
##4.getinv -- returns the inversed matrix



makeCacheMatrix <- function(x = matrix()) {
    ##m_inv is the inverted matrix. Initialise to null
     m_inv <- NULL
    set <- function(y) {
      x <<- y
      m_inv <<- NULL
    }
    get <- function() x  ## Returns the input matrix
    setinv <- function(inv) m_inv <<- inv  ##Sets the inverse of the matix
    getinv <- function() m_inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  
  
  
}


## Short comment describing this function
##The following function calculates the inverse of the matrix
##created with the above function. However, it first checks to see if the inverse 
##has already been calculated. If so, it gets the inverse from the cache and 
##skips the computation. Otherwise, it calculates the inverse of the matrix and 
##sets the value of the inverse in the cache via the set function


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    m_inv <- x$getinv()
    if(!is.null(m_inv)) {     ##if cached 
      message("getting cached data")
      return(m_inv)
    }
    data <- x$get()
    m_inv <- solve(data, ...)
    x$setinv(m_inv)
    m_inv   
  

}
