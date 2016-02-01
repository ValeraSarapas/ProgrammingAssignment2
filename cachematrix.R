## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL # Set the inverse equal to NULL
        
        set <- function(y) {
                x <<- y  # set function assigns the argument to x
                m <<- NULL
        }
        get <- function() x # get function returns the matrix
        
        setinverse <- function(solve) m <<- solve # setInverse overrides the previous value 
                                                  # of m and assigns the argument to Inverse (which is supposed to be the inverse of matrix x)
        getinverse <- function() m                # getInverse returns the Inverse
        
        list(set = set, get = get,                # creates a list of the functions
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        # If the value of Inverse is NOT null (was previously calculated), cacheSolve returns that value
        if(!is.null(m)) {            
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        # Sets Inverse to the newly calculated value   
        x$setinverse(m)
        #Returns the new Inverse value
        m        
}

