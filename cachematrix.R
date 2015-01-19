## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function creates the special matrix object.
# Instead of getmean/setmean, I use getinv/setinv.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        # Make sure the function names are setinv/getinv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
# Caches a matrix that is created by makeCacheMatrix function
# Compared to the vector example, i used setinv and getinv instead of
# getmean and setmean.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        #Compute the inverse of matrix x$get()
        m <- solve(data, ...)
        #set the inverse matrix to the x data structure
        x$setinv(m)
        m
}
