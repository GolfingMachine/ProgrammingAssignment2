## The makeCacheMatrix function below creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL ## m will represent the matrix that is reset to NULL each time makeCacheMatrix is called
       set <- function(y) {
              x <<- y
              m <<- NULL
       }
       get <- function() x  ## this function will return the value of the original matrix
       setsolve <- function(solve) m <<- solve   ## setsolve is called by cacheSolve during its first access and it will store the matrix using superassignment
       getsolve <- function() m    ## getsolve will return the cached matrix to cacheSolve after first access if assignments do not change
       list(set = set, get = get,
            setsolve = setsolve,
            getsolve = getsolve)
}             ## Each time that makeCacheMatrix is called this list of internal functions is accessed in order for the calling function to be able to access the methods


## The cacheSolve function below computes the inverse of the special matrix returned by the function above (makeCacheMatrix)
## If the inverse of the matrix has already been calculated, then the function below retrieves the inverse from the cache.

cacheSolve <- function(x, ...) { ## the input x is an object created by makeCacheMatrix
       m <- x$getsolve() ## this line access x and calculates the inverse of the matrix
       if(!is.null(m)) { 
              message("getting cached data")
              return(m) ## these three lines mean that if the inverse of the matrix has already been cached, then print the message "getting cached data" and return the inverse of the matrix
       }
       
       ## this section of the code is only necessary if the return value in the first part of cacheSolve = NULL because that means the inverse of the matrix was not in the cache and will need to be calculated
       data <- x$get()
       m <- solve(data, ...)
       x$setsolve(m)
       m ## Return a matrix that is the inverse of 'x'

      
}
