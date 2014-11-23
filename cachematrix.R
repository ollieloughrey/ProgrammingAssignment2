# makeCacheMatrix
# 1. sets the value of the matrix
# 2. gets the value of the matrix
# 3. sets the value of inverse of the matrix
# 4. gets the value of inverse of the matrix

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

#This function returns the inverse of the matrix [solves]
#the answer is stored in i, if i already has a value,
#that is return or if not then inverse is caculated with the solve function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
