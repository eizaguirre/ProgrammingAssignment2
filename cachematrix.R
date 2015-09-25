## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function takes a invertible matrix as an input parameter an creates a 
## special matrix type that store the value of the matrix and also the value for
## the inverse of the matrix, calculated using the solve function, just like 
## the example provided on the Assignment description.

makeCacheMatrix <- function(x = matrix()) {
        ## creation of local variable m. This variable is used to store the 
        ## value for the inverse of the matrix 
        m <- NULL
        ## Assigns the initial value for the special matrix and the cache for 
        ## the inverse of the matrix.
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## returns the values of the special matrix
        get <- function() x
        ## stores the value of the inverse of the matrix in a local variable.
        setsolve <- function(solve) m <<- solve
        ## returns the current value stored for the inverse of the matrix
        getsolve <- function() m
        # Creates a list with the definition of the available functions for 
        ##the matrix object.
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Write a short comment describing this function
## This function takes an object of the special matrix type created in the 
## previous function and then calculates its inverse value using the solve 
## function. It then stores the result of the calculation using the "setsolve" 
## function of the special matrix object. Everytime this function is called, 
## it checks if there are any stored results in the object from previous 
## executions. If there are any stored results from previous executions, it 
## returns the stored value instead of recalculating the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
        ## Gets the current value of m using the function getsolve() 
        m <- x$getsolve()
        
        ## If m is null, a message is printed the data is from the cache. 
        ## Then, it returns the current value of the m function.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## If m is null (empty cache), we calculate the inverse of the matrix. 
        ## First, the values of special matrix are stored in a variable named 
        ## "data". This variable is used to calculate the inverse of the matrix 
        ## using the "solve" function. The result is stored in the m variable. 
        ## The value of m is then stored in the special matrix using the 
        ## "setsolve" function. Finally, the value of the local variable m is 
        ## printed with the value of the inverse of the matrix.
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
