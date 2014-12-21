## This functions is designed to caching the inverse of a matrix
## so that it will not be computed repeatedly when needed to avoid a costly computation.

## to creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x) {
       
       if(!is.matrix(x)) {return(message("not a matrix"))}
       if(!ncol(x)==nrow(x)) {return(message("not a square"))}
       
       s <- NULL
       set <-function(y) {              ## for potential debugging
              x <<- y
              s <<-NULL
       }
       get <-function()  {x}            ## Those functions will be used in cacheSolve(x) below.
       setmatrix <-function(solve) {s <<-solve}
       getmatrix <-function() {s}
       list(set = set, get = get, 
            setmatrix = setmatrix, getmatrix = getmatrix)
}

## to compute the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse is an old one, the cachesolve would retrieve result directy.
cacheSolve <-function (x, ...) {
       s <- x$getmatrix()
       if(!is.null(s)) {
              message("getting cached data")
              return(s)
       }
       data <-x$get()       ## If x$getmatrix() get NULL, than we caculate it.
       s <-solve(data, ...)
       x$setmatrix(s)       ## to store the result so that can be recalled.
       s             ## to return a matrix that is the inverse of "x"
}


