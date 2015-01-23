##      These functions demonstrate how lexical scoping works

##      makeCacheMatrix takes takes a matrix 
##      and caches it in a new environment with variables getsolve & setsolve. 

makeCacheMatrix <- function(x = matrix()) {

        s <- NULL
        
#         evn <- environment()
#         
#         print (environment())
#         
#         print(parent.env(evn))

    
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
        
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
##      getevn <- function() environment()

#         print (evn)
#         print (parent.env(evn))
        
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
}


##      cacheSolve returns the inverse of a matrix.
##      It takes a matrix, & checks if the inverse has been calculated
##      (ie variable called getsolve() has a value attached)

##      If a value already exists in the cached namevalue pair, then it returns that result 
##      with a message stating "getting cached data" 

##      If there is no value, then it computes the inverse, updates the cached value 
##      and returns the result.

cacheSolve <- function(x, ...) {
#         env <- environment()
#         print (env)
        s <- x$getsolve()
#         print (s)
      
       if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
         data <- x$get()
         s <- solve(data, ...)
         x$setsolve(s)
         s
        
}