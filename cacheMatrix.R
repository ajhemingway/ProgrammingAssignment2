makeCacheMatrix <- function(x = matrix()){ #create a function with a matrix argument
        
        m <- NULL #assign a NULL value to a variable to be used later
        set <- function(y){ #nest a new function named anything other than 'x'
                
                x <<- y #the value y gets assigned using a different environment
                m <<- NULL #resets / clears any previous value
        }
        get <- function() x #retrieves the argument
        setinv <- function(inv) m <<- inv #assigns the value to the NULL m using a different environment
        getinv <- function() m #retrieves m
        list(set = set, get = get, setinv = setinv, getinv = getinv) #create a list of names so the $ extractor can be used later
}
cacheSolve <- function(x, ...){ #create the solve (inverse) matrix function
        
        m <- x$getinv() #pulls the value from the list created above
        if(!is.null(m)){ #logical test of whether a cached value can be retrieved
                message("getting cached data") #text display of correct operation
                return(m) 
        }
        data <- x$get() #retrieves the argument from above
        m <- solve(data, ...) #calculates the matrix inverse
        x$setinv(m) #returns the inverse of the matrix used for input
        m
}
