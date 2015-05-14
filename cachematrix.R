                    #######################
                    #    Inverse Matrix   #
                    #######################

## This functions inverse square matrix an cache result in an environment which is different from the current environment

## The first fonction "makeCacheMatrix" make tree actions
# 1. It test if x is a matrix
# 2. It test if x is a square matrix
# 3. It test if x is inversible
                    

makeCacheMatrix <- function(x = matrix()) {
 
        if(class(x)!="matrix"){
          message("'x' must be class of matrix")
        } else {
                if(nrow(x)!=ncol(x)){
                  message("'x' must be square matrix")
                } else {
                          if(det(x)==0){
                          message("'x' is not inversible")
                        } else {
                                m <- NULL
                                set <- function(y) {
                                  x <<- y
                                  m <<- NULL
                                  }
                                  get <- function() x
                                  setsolve <- function(solve) m <<- solve
                                  getsolve <- function() m
                                  list(set = set, get = get,
                                       setsolve = setsolve,
                                       getsolve = getsolve)
                                 }
                        }
                }
     }


## The second fonction "cacheSolve" make two actions
# 1. it first checks to see if inverse of matrix  has already been calculated. If so, it gets result from the cache and skips the computation.
# 2. if inverse has not yet calculate, it inverse the data and sets the value the cache via the setsolve function

cacheSolve <- function(x, ...) {
          m <- x$getsolve()
          if(!is.null(m)) {
            message("getting cached data")
            return(m)
                     }
                      data <- x$get()
                      m <- solve(data, ...)
                      x$setsolve(m)
                      m
    }
