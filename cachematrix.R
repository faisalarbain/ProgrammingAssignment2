## Factory method to create a `cachable matrix` that able to store and clear cache for a matrix
makeCacheMatrix <- function(x = matrix()) {
    
    x_solve <- NULL
    
    getMatrix <- function(){
        x ## return the matrix
    }
    
    setMatrix <- function(new_x = matrix()){
        
        ## if the matrix is identical, then ignore to prevent the cache being unnecessary cleared
        if(!identical(x, new_x)){ 
            
            x <<- new_x ## assign new_x to x
            x_solve <<- NULL ## clear cache since the matrix changed
        
        }
    }
    
    getCached <- function(){
        x_solve ## return cached solve
    }
    
    setCache <- function(s){
        x_solve <<- s ## cache solve value
    }
    
    # register public method for the `object`
    list(
        getMatrix=getMatrix,
        setMatrix=setMatrix, 
        getCached=getCached,
        setCache=setCache
    )  
}


## Return the inverse matrix 
## if the value previously computed, cacheSolve will return value from matrix cached
## if no cached, cacheSolve will compute the inverse and store the value into cache
cacheSolve <- function(x, ...) {
    
    cached <- x$getCached()
    
    if(!is.null(cached)){
        message("return from cache")
        return(cached)
    }
    
    ## compute inverse matrix
    x_matrix <- x$getMatrix()
    ix <- solve(x_matrix,...)
    
    ## store into cache
    x$setCache(ix)
    
    return(ix)
}
