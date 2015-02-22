## Assignment: Caching the Inverse of a Matrix
# 
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than computing it repeatedly (there
# are also alternatives to matrix inversion that we will not discuss here). Your
# assignment is to write a pair of functions that cache the inverse of a matrix.
# 
# Write the following functions:
# 
# makeCacheMatrix: This function creates a special "matrix" object that can
# cache its inverse. 
#
# cacheSolve: This function computes the inverse of the
# special "matrix" returned by makeCacheMatrix above. If the inverse has already
# been calculated (and the matrix has not changed), then cacheSolve should
# retrieve the inverse from the cache. 
#
# Computing the inverse of a square matrix
# can be done with the solve function in R. For example, if X is a square
# invertible matrix, then solve(X) returns its inverse.
# 
# For this assignment, assume that the matrix supplied is always invertible.
# 
# In order to complete this assignment, you must do the following:
# 
# 1. Fork the GitHub repository containing the stub R files at
# https://github.com/rdpeng/ProgrammingAssignment2 to create a copy under your
# own account. 
# 2. Clone your forked GitHub repository to your computer so that you
# can edit the files locally on your own machine. 
# 3. Edit the R file contained in
# the git repository and place your solution in that file (please do not rename
# the file). 
# 4. Commit your completed R file into YOUR git repository and push your
# git branch to the GitHub repository under your account. 
# 5. Submit to Coursera the
# URL to your GitHub repository that contains the completed R code for the
# assignment.

# Sunday, 22February, 2015 # 9:02 AM
# Testing Comments
# use x <- matrix(sample.int(15, 9*100, TRUE), numberOfRows, NumberOfCols) 
# if numberOfRows = NumberOfCols then the matrix will be invertible.
##

# -----------------------------------------------------------------------------
# makeCacheMatrix: This function creates a special "matrix" object that can
# cache its inverse. 
# -----------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
    # Test to see if the matrix is invertible
    x <- as.data.frame(x)
    numRows <- nrow(x)
    numCols <- ncol(x)
    if ( numRows != numCols ) {
        warning("This matrix is not invertible. Check number of rows and columns.")
    }
    
    m <- NULL
    set <- function(y) {
        # set the value of the vector    
        x <<- y
        m <<- NULL
    }
    
    #get the value of the vector
    get <- function() x #as.matrix(x)
    
    #set the value of the inverse
    setinverse <- function(solve) m <<- solve
    
    # get the value of the inverse
    getinverse <- function() m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# -----------------------------------------------------------------------------
# cacheSolve: This function computes the inverse of the
# special "matrix" returned by makeCacheMatrix. If the inverse has already
# been calculated (and the matrix has not changed), then cacheSolve should
# retrieve the inverse from the cache. 
# -----------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
    # Get the inverse
    m <- x$getinverse()
    # Check to see if the inverse exists
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # Get the dataframe
    data <- x$get()
    
    # Use the matrix version of the dataframe
    data <- as.matrix(data)

    # Get the inverse
    m <- solve(data, ...)
    x$setinverse(m)
    
    # Return a matrix that is the inverse of 'x'
    as.matrix(m)
}