makeCacheMatrix <- function(x = matrix()) {
     mtx <- NULL
     set <- function(y){
          x <<- y
          mtx <<- NULL
     } 
     get <- function() x
     setmtx <- function(solve) mtx <<- solve
     getmtx <- function() mtx
     list(set = set, get = get, setmtx = setmtx, getmtx = getmtx)
}

cacheSolve <- function(x, ...) {
     mtx <- x$getmtx()
     if(!is.null(mtx)){
          message("getting cached data")
          return(mtx)
     }        
     resultmtx <- x$get()
     mtx <- solve(resultmtx, ...)
     x$setmtx(mtx)
     mtx
}
