## This functions is designed to caching the inverse of a matrix
## so that it will not be computed repeatedly when accessed again to  avoid a costly computation.

## This function will create a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x) {
              ## to check simply whether x can be inversed 
       if(!is.matrix(x)) {return(message("not a matrix"))}
       if(!ncol(x)==nrow(x)) {return(message("not a square"))}
       
       s <- NULL
       set <-function(y) {              ## for potential debugging
              x <<- y
              s <<-NULL
       }
       get <-function()  {x}            ## Those functions will be used in the cacheSolve() below.
       setmatrix <-function(solve) {s <<-solve}
       getmatrix <-function() {s}
       list(set = set, get = get, 
            setmatrix = setmatrix, getmatrix = getmatrix)
}

## This function will compute the inverse of the special "matrix" returned by makeCacheMatrix above. 
## Inter alia, if the matrix is an old one, the cacheSolve() can retrieve the result directy.
cacheSolve <-function (x, ...) {
       s <- x$getmatrix()
       if(!is.null(s)) {           ## to check whether x has been caculatetd
              message("getting cached data")
              return(s)            ## If it has, give the inverse dircetly; skip all beneath.
       }
       data <-x$get()       ## If x is a new one(x$getmatrix=NULL), than we caculate it,
       s <-solve(data, ...)
       x$setmatrix(s)       ## and store the result so that can be recalled.
       s             ## to return a matrix that is the inverse of "x"
}


