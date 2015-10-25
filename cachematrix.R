##  Coursera programming Assignment 3 

# These programs create a cache for a matrix, and then invert the matrix. There are two functions:
#  1) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#  2) cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#     If the inverse has already been calculated (and the matrix has not changed), then the cachesolve will
#     retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) 
{
    m_inverse <- NULL                       # variable for returning the inverse matrix to. 
    set <- function(y)                      # Function to set the value of the matrix.
    { 
        x <<- y                             # function is used to change the value of the matrix stored in the function.
        m_inverse <<- NULL
    }
    
    get <- function() x                     # Function to get the value of the matrix.
    
    setinv <- function(matrix_inverse)
    { 
        m_inverse <<- matrix_inverse        # Function to set the value of the inverse.
    }  
    
    getinv <- function() m_inverse          # Function to get the value of the inverse.
    
    list(set = set, get = get,setinv = setinv,getinv = getinv)
}



cacheSolve <- function(x, ...) 
{
    m_inverse <- x$getinv()
    if(!is.null(m_inverse)) {               # Check if the inverse already has been calculated.
        message("getting cached data")
        return(m_inverse)
    }
    data <- x$get()                         # Get the matrix to calculate it's inverse.
    
    m_inverse <- solve(data, ...)           # Calculate the inverse.
    
    x$setinv(m_inverse)                     # Set the value of the inverse in the cache.
    
    m_inverse                               # Return inverse of matrix.
    
}
