## Peer-graded Assignment: Programming Assignment 2: Lexical Scoping
## 

## Creates special vector, containing list of functions to set/get matrix, 
## set/get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
               x <<- y
               inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, 
             getinverse = getinverse)
}


## If exists returns cached value of inversed matrix, otherwise computes the 
## inverse makeCacheMatrix 

cacheSolve <- function(x, ...) {
       m <- x$getinverse()
       if (!is.null(m)) {
               message("gettign cached data")
               return(m)
       }
       data <- x$get()
       m <- solve(data)
       x$setinverse(m)
       m
}
