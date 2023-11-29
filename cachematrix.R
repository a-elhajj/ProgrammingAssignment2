# Function to create a special type of matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    # Check if the input 'x' is a matrix
    if(is.matrix(x)) {
        print("It is a matix, continue...")
        m <- NULL  # Initialize 'm' to store the inverse of the matrix
        
        # Setter function to set the value of the matrix
        set <- function(y) {
            x <<- y  # Assign new matrix to 'x'
            m <<- NULL  # Reset cached inverse as the matrix has changed
        }
        
        # Getter function to get the value of the matrix
        get <- function() x
        
        # Setter function to set the cached inverse of the matrix
        setinverse <- function(solve) m <<- solve
        
        # Getter function to get the cached inverse of the matrix
        getinverse <- function() m
        
        # Return a list of all the above functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
    } else {
        # Print a message and return if 'x' is not a matrix
        print("x is not a matrix... input a matrix to use the function...")
        return(message("The matrix isn't invertible."))
    }
}

## Function to compute the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
    # Retrieve the cached inverse from the object 'x'
    m <- x$getinverse()
    print(paste0(cat("cacheSolve m1: "), m))
    # Check if the cached inverse is already available
    if(!is.null(m)) {
        message("getting cached data")
        return(m)  # Return the cached inverse if available
    }
    # Retrieve the matrix data from 'x' if cached inverse is not available
    data <- x$get()
    print(paste0(cat("cacheSolve data: "), data))
    # Calculate the inverse of the matrix
    m <- solve(data, ...)
    print(paste0(cat("cacheSolve m1: "), m))
    # Store the calculated inverse in the cache
    x$setinverse(m)
    print(paste0(cat("cacheSolve x$setinverse(m): "), x$setinverse(m)))
    # Return the calculated inverse
    m
    print(paste0(cat("cacheSolve m: "), m))
}
