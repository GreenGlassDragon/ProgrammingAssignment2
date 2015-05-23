## Work by Christian Gilbert for Coursera R Programming Assignment 2

## Matrix is input to function makeCacheMatrix, it assumes an invertable matrix is supplied
## The inverse of the matrix is calculated if
##     either the matrix has changed or not been calculated before
##     call solve function via cacheSolve and cache it for future requests
##  or 
##     the matrix is the same
##     store the value of the matrix for comparison, that means we should cache it
## if th matrix is new, there will be a difference between, cache and the new matrix
## if the inverse of the matrix has been calculated 
## return the result of the solve function


## Tests for new matrix, a new matrix is cached 
makeCacheMatrix <- function(x = matrix()) {
    
    ## initialise m as null
    m <- NULL
    
    ## create set matrix function in the environment 
    setmtrx <- function(y) {
                        
        ##print(x);
        ##print(y)
        
        ## set x as input and store in environment
                        x <<- y
                        
                        ## ideally an if statement here that checks to see if x = y? 
                        ##so that matrix is not unncessarily reassigned
                        ## we can try that at later date
                        
                        ## inverse has not been calculated wipe out any existing inverse in the environment
                        m <<- NULL
                        }

    ## this creates a function that calls the supplied matrix from cache
    getmtrx <- function() x

    ## this calculates the inverse of matrix using solve
    ## makes it available through function setinverse
    ## and stores it in environment and is assigned to m currently set to null
    setinverse <- function(solve) m <<- solve
    
    ## creates function getinverse to obtain the stored value 
    getinverse <- function() m
    
    ## this list stores the functions in the environment 
    ## when makeCacheMatrix is called
    list(  setmtrx = setmtrx
         , getmtrx = getmtrx
         , setinverse = setinverse
         , getinverse = getinverse)
}


## Returns the invesrse of a matrix previously submitted if available

## if the matrix is not new and is the same as one from the previous call
##    obtain solved inverse value from cache
##    return value from cache and stop
## if the matrix is new
##    build the new inverse value with solve function
##    cache the solve inverse result
##    return the inverse as last value of function

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is an inverse of matrix 
        ##submited to makeCacheMatrix from cached environment 'x'
        
        ## lets see if we can get the inverse matrix from the environment
        m <- x$getinverse()
        
        ## we expect value, value needs to be tested for null to see if it is populated
        if(!is.null(m)) {
            
            ## so if the inverse matrix exists we will return it
            ## tell the audience where the return output is comming from
            message("getting cached inverse matrix")
        
            ## this stops the function because it is told to return a value
            ## this is sloppy code and the if statement does not need to be completed with else
            ## the else is implied by continuation as the value will not be returned if condition is not met
            return(m)
        }
        ## as we are still running cached data was not available
        ## therefore we have to get the matrix and solve it to obtain inverse
        datamatrix <- x$getmtrx()
        
        ## solve matrix and assign to variable m
        m <- solve(datamatrix)
        
        ## call setinverse to put the result into cache
        x$setinverse <- (m)        
        
        ## this is the last call in the function and so should be returned
        m
}
