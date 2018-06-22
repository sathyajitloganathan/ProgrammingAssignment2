## When performing functions to a vector/ matrix of small size the computation is quite fast. As the size
## of the objects increases it becomes a computational load specially if the task needs to be 
## repeated on the same object. Therefore to make things more efficient 
## the computation's outputs are cached in another object which could be used if the same computation 
## on the previously stored object is required again.

## This create, returns a matrix and also gets or sets the inverse of that matrix on request. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks if the inverse computations of the matrix is cached 
## If cached it retrieves tht value or else it computes the inverse

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("Getting Cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setmean(i)
        i
}
