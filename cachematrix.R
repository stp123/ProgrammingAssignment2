## makeCacheMatrix will make a function that creates a matrix that can be cached
## then I'll see if the matrix can be inverted and check on whether it worked

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
inv = NULL
        set = function(y) {
              
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}
}


## Now I'm going to invert the matrix created by the cache matrix function using inverse 
	##fucntion in R

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'


       inv = x$getinv()
        
        #check to see if it is already done
        if (!is.null(inv)){
                # check to see if it is done. 
                message("getting cached data")
                return(inv)
        }
        
        # if not, do the matrix inversion 
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # use the set invers function to get the calculation started
        x$setinv(inv)
        
        return(inv)
}
test = function(mat){
        ## make a mmatrix to test then see how long it takes vs the cache
        
        temp = makeCacheMatrix(mat)
        
        start.time = Sys.time()
        cacheSolve(temp)
        dur = Sys.time() - start.time
        print(dur)
        
        start.time = Sys.time()
        cacheSolve(temp)
        dur = Sys.time() - start.time
        print(dur)

}



