##First function sets up a cached value for the inverse
makeCacheMatrix <- function(x=numeric()){
	inv <- NULL
	set <- function(y){
		x <<-y
		inv <<-NULL
	}
	get <- function() x
	setinverse <-function(inverse) inv<<-inverse
	getinverse <-function() inv
	list(set=set,get=get,
		setinverse=setinverse,getinverse=getinverse)
}

##Second function calculates the inverse of the matrix, and sets it to the cached value
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        	inv <- data.frame()
		inv <- lapply(inv,rbind(inv,data[,i]))
        x$setinverse(inv)
        inv
}